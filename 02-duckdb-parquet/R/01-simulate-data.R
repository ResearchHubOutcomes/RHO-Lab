# ---------------------------------------------------------------------------
# RHO Lab 02 - Interrogare grandi archivi senza caricarli in memoria
# Step 1: simulate a realistic hospital discharge dataset (SDO) and write it
#         as CSV, parquet and partitioned parquet.
#
# Simulated data only. No real hospital or patient data is used.
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(duckdb)
  library(DBI)
})

set.seed(20260924)

data_dir <- "data"
dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)

# Two volume scenarios, to show where the difference starts to matter.
SCENARIOS <- c(small = 1e6, large = 1e7)

N_PATIENTS_RATIO <- 0.55   # patients are fewer than admissions (readmissions)
YEARS            <- 2021:2025
ASL_CODES        <- sprintf("ASL%03d", 1:12)
N_DAYS           <- as.integer(
  as.Date(sprintf("%d-12-31", max(YEARS))) -
  as.Date(sprintf("%d-01-01", min(YEARS))) + 1L)

# ---------------------------------------------------------------------------
# Data generation is done inside DuckDB itself: generating 10 million rows in
# base R would be slow and memory hungry, and the point of the article is that
# this kind of work does not need to happen in R memory.
# ---------------------------------------------------------------------------

# Patients belong to an ASL, and the size of each ASL's patient pool varies:
# a smaller pool means the same patients come back more often, so the simulated
# 30-day readmission rate differs between ASLs instead of being flat.
# Admissions of the same patient are clustered around a patient-specific base
# date, with a minority of short gaps, so that readmissions are not an artefact
# of uniformly random dates.
generate_table <- function(con, n_rows, table_name) {
  n_patients <- round(n_rows * N_PATIENTS_RATIO)
  sql <- sprintf("
    CREATE OR REPLACE TABLE %s AS
    WITH ids AS (
      SELECT
        i AS record_id,
        (abs(hash(i * 104729)) %% 12) + 1                       AS asl_idx,
        (abs(hash(i * 15485863)) %% 40) + 1                     AS hospital_idx
      FROM range(1, %d + 1) t(i)
    ),
    pat AS (
      SELECT
        record_id, asl_idx, hospital_idx,
        -- pool size shrinks as asl_idx grows: 100%% down to about 45%%
        asl_idx * 10000000
          + (abs(hash(record_id * 7919))
             %% CAST(%d * (1.0 - 0.05 * (asl_idx - 1)) / 12 AS BIGINT))
                                                                AS patient_id
      FROM ids
    ),
    base AS (
      SELECT
        record_id, asl_idx, hospital_idx, patient_id,
        DATE '%d-01-01'
          + CAST((abs(hash(patient_id * 32452843)) %% %d
                  + CASE WHEN abs(hash(record_id * 611953)) %% 100 < 35
                         THEN abs(hash(record_id * 2971215073)) %% 45
                         ELSE abs(hash(record_id * 433494437)) %% %d
                    END) %% %d AS INTEGER)                      AS admission_date,
        CAST(abs(hash(record_id * 49979687)) %% 25 AS INTEGER)  AS los_raw,
        (abs(hash(record_id * 86028121)) %% 25) + 1             AS mdc,
        (abs(hash(record_id * 217645199)) %% 100) + 1           AS age_raw,
        abs(hash(record_id * 512927357)) %% 100                 AS sex_raw,
        abs(hash(record_id * 100000007)) %% 100                 AS regime_raw,
        abs(hash(record_id * 179424673)) %% 100                 AS urgency_raw,
        abs(hash(record_id * 373587883)) %% 100                 AS outcome_raw
      FROM pat
    )
    SELECT
      record_id,
      patient_id,
      'ASL' || lpad(CAST(asl_idx AS VARCHAR), 3, '0')           AS asl_code,
      'H' || lpad(CAST(hospital_idx AS VARCHAR), 4, '0')        AS hospital_code,
      admission_date,
      admission_date + los_raw                                  AS discharge_date,
      los_raw                                                   AS length_of_stay,
      mdc                                                       AS mdc_code,
      'DRG' || lpad(CAST(mdc * 10 + (los_raw %% 9) AS VARCHAR), 3, '0') AS drg_code,
      age_raw                                                   AS patient_age,
      CASE WHEN sex_raw < 52 THEN 'F' ELSE 'M' END              AS patient_sex,
      CASE WHEN regime_raw < 78 THEN 'ORD' ELSE 'DH' END        AS admission_regime,
      CASE WHEN urgency_raw < 44 THEN 'URG' ELSE 'PRG' END      AS admission_type,
      CASE WHEN outcome_raw < 93 THEN 'HOME'
           WHEN outcome_raw < 98 THEN 'TRANSFER'
           ELSE 'DECEASED' END                                  AS discharge_outcome,
      CAST(EXTRACT(year FROM admission_date) AS INTEGER)        AS admission_year
    FROM base
  ", table_name,
     n_rows,          # rows to generate
     n_patients,      # total patient pool, split across ASLs
     min(YEARS),      # first year
     N_DAYS,          # spread of patient base dates, in days
     800L,            # spread of the ordinary (non-clustered) gap, in days
     N_DAYS)          # wrap, so admissions are uniform across the five years
  dbExecute(con, sql)
  invisible(NULL)
}

con <- dbConnect(duckdb(), dbdir = ":memory:")
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

manifest <- list()

for (scn in names(SCENARIOS)) {
  n_rows <- SCENARIOS[[scn]]
  message(sprintf("[%s] generating %s rows ...", scn, format(n_rows, big.mark = ".")))

  generate_table(con, n_rows, "sdo")

  csv_path  <- file.path(data_dir, sprintf("sdo_%s.csv", scn))
  pq_path   <- file.path(data_dir, sprintf("sdo_%s.parquet", scn))
  part_path <- file.path(data_dir, sprintf("sdo_%s_partitioned", scn))

  dbExecute(con, sprintf("COPY sdo TO '%s' (FORMAT CSV, HEADER)", csv_path))
  dbExecute(con, sprintf(
    "COPY sdo TO '%s' (FORMAT PARQUET, COMPRESSION ZSTD)", pq_path))

  unlink(part_path, recursive = TRUE)
  dbExecute(con, sprintf(
    "COPY sdo TO '%s' (FORMAT PARQUET, COMPRESSION ZSTD,
       PARTITION_BY (admission_year), OVERWRITE_OR_IGNORE)", part_path))

  part_size <- sum(file.info(
    list.files(part_path, recursive = TRUE, full.names = TRUE))$size)

  manifest[[scn]] <- data.frame(
    scenario       = scn,
    n_rows         = n_rows,
    n_cols         = ncol(dbGetQuery(con, "SELECT * FROM sdo LIMIT 0")),
    csv_bytes      = file.info(csv_path)$size,
    parquet_bytes  = file.info(pq_path)$size,
    part_bytes     = part_size,
    stringsAsFactors = FALSE
  )
  message(sprintf("[%s] csv %.1f MB | parquet %.1f MB | partitioned %.1f MB",
                  scn,
                  manifest[[scn]]$csv_bytes / 1024^2,
                  manifest[[scn]]$parquet_bytes / 1024^2,
                  manifest[[scn]]$part_bytes / 1024^2))
}

manifest <- do.call(rbind, manifest)
write.csv(manifest, file.path(data_dir, "manifest.csv"), row.names = FALSE)
print(manifest)
