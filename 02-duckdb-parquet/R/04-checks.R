# ---------------------------------------------------------------------------
# RHO Lab 02 - Step 4: the two checks quoted in the advanced section.
#   a) how much of the parquet file a single question actually has to read
#   b) what happens if the year filter is applied BEFORE the window function
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(duckdb); library(DBI); library(dplyr); library(dbplyr)
})

con <- dbConnect(duckdb(), dbdir = ":memory:")
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)
dbExecute(con, "PRAGMA threads=2")

pq <- "data/sdo_large.parquet"

# (a) the four columns Q1 needs, written on their own
tmp <- tempfile(fileext = ".parquet")
dbExecute(con, sprintf(
  "COPY (SELECT admission_regime, admission_year, admission_date, asl_code
         FROM read_parquet('%s'))
   TO '%s' (FORMAT PARQUET, COMPRESSION ZSTD)", pq, tmp))
cat(sprintf("Colonne usate da Q1: %.1f MB su %.0f MB del file completo\n",
            file.info(tmp)$size / 1024^2, file.info(pq)$size / 1024^2))

# (b) the window-function trap
src <- tbl(con, sql(sprintf("SELECT * FROM read_parquet('%s')", pq)))

readmission_summary <- function(filter_first) {
  x <- src |> filter(admission_regime == "ORD",
                     discharge_outcome != "DECEASED")
  if (filter_first) x <- x |> filter(admission_year == 2024L)
  x <- x |>
    group_by(patient_id) |>
    window_order(admission_date, record_id) |>
    mutate(previous_discharge = lag(discharge_date)) |>
    ungroup() |>
    mutate(days_since_discharge =
             as.numeric(admission_date - previous_discharge))
  if (!filter_first) x <- x |> filter(admission_year == 2024L)
  x |>
    summarise(n_admissions = n(),
              n_readmissions = sum(ifelse(
                !is.na(days_since_discharge) &
                  days_since_discharge >= 0 &
                  days_since_discharge <= 30, 1L, 0L), na.rm = TRUE)) |>
    collect()
}

ok    <- readmission_summary(filter_first = FALSE)
wrong <- readmission_summary(filter_first = TRUE)

cat(sprintf("Finestra su tutta la storia : %d su %d = %.2f%%\n",
            ok$n_readmissions, ok$n_admissions,
            100 * ok$n_readmissions / ok$n_admissions))
cat(sprintf("Filtro applicato prima      : %d su %d = %.2f%%\n",
            wrong$n_readmissions, wrong$n_admissions,
            100 * wrong$n_readmissions / wrong$n_admissions))
cat(sprintf("Differenza                  : %d riammissioni non viste (%.1f%%)\n",
            ok$n_readmissions - wrong$n_readmissions,
            100 * (ok$n_readmissions - wrong$n_readmissions) / ok$n_readmissions))
