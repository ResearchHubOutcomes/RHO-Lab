# ---------------------------------------------------------------------------
# RHO Lab 02 - Step 2: benchmark three strategies on three questions.
#
# Strategies
#   A) CSV read fully into R memory (readr), then dplyr
#   B) parquet file queried by DuckDB through dplyr/dbplyr
#   C) parquet partitioned by year, queried by DuckDB through dplyr/dbplyr
#
# The CSV queried directly by DuckDB - the middle road quoted in the article -
# is timed in R/04-checks.R, not here.
#
# Questions
#   Q1 ordinary admissions by ASL and month, one year
#   Q2 mean length of stay for one MDC, one year, by ASL
#   Q3 share of 30-day readmissions by ASL (window function)
#
# Simulated data only.
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(duckdb); library(DBI); library(dplyr); library(dbplyr)
  library(readr); library(tidyr); library(lubridate)
})

options(dplyr.summarise.inform = FALSE)

data_dir <- "data"
out_dir  <- "output"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

TARGET_YEAR <- 2024L
TARGET_MDC  <- 5L
N_REPS      <- c(small = 3L, large = 2L)

manifest <- read.csv(file.path(data_dir, "manifest.csv"), stringsAsFactors = FALSE)

# ---------------------------------------------------------------------------
# Query definitions, written once against a generic dplyr source.
# The same code runs on a local data frame and on a DuckDB-backed table:
# this is the point of dbplyr.
# ---------------------------------------------------------------------------

q1_monthly_admissions <- function(tbl_src) {
  tbl_src |>
    filter(admission_regime == "ORD", admission_year == TARGET_YEAR) |>
    mutate(admission_month = month(admission_date)) |>
    count(asl_code, admission_month, name = "n_admissions") |>
    arrange(asl_code, admission_month)
}

q2_mean_los <- function(tbl_src) {
  tbl_src |>
    filter(admission_regime == "ORD",
           admission_year == TARGET_YEAR,
           mdc_code == TARGET_MDC) |>
    group_by(asl_code) |>
    summarise(n_admissions = n(),
              mean_los = mean(length_of_stay, na.rm = TRUE)) |>
    arrange(asl_code)
}

# Readmissions need the full history: an admission in January 2024 can be a
# readmission after a discharge in December 2023. The window is therefore
# computed on all years and only the reporting is restricted to TARGET_YEAR.
q3_readmissions <- function(tbl_src, is_remote) {
  base <- tbl_src |>
    filter(admission_regime == "ORD", discharge_outcome != "DECEASED")

  if (is_remote) {
    base <- base |>
      group_by(patient_id) |>
      window_order(admission_date, record_id) |>
      mutate(previous_discharge = lag(discharge_date)) |>
      ungroup()
  } else {
    base <- base |>
      arrange(patient_id, admission_date, record_id) |>
      group_by(patient_id) |>
      mutate(previous_discharge = lag(discharge_date)) |>
      ungroup()
  }

  base |>
    mutate(days_since_discharge =
             as.numeric(admission_date - previous_discharge)) |>
    filter(admission_year == TARGET_YEAR) |>
    group_by(asl_code) |>
    summarise(
      n_admissions   = n(),
      n_readmissions = sum(
        ifelse(!is.na(days_since_discharge) &
                 days_since_discharge >= 0 &
                 days_since_discharge <= 30, 1L, 0L), na.rm = TRUE)) |>
    mutate(readmission_rate = 100 * n_readmissions / n_admissions) |>
    arrange(asl_code)
}

# Columns each question actually needs (to show what parquet can skip).
COLS_USED <- list(
  Q1 = c("admission_regime", "admission_year", "admission_date", "asl_code"),
  Q2 = c("admission_regime", "admission_year", "mdc_code", "asl_code",
         "length_of_stay"),
  Q3 = c("admission_regime", "discharge_outcome", "patient_id",
         "admission_date", "discharge_date", "admission_year", "asl_code",
         "record_id")
)

# ---------------------------------------------------------------------------
# Runners
# ---------------------------------------------------------------------------

run_csv <- function(csv_path, qfun, ...) {
  d <- read_csv(csv_path, show_col_types = FALSE, progress = FALSE)
  res <- qfun(d, ...)
  as.data.frame(res)
}

run_duckdb <- function(con, source_sql, qfun, ...) {
  src <- tbl(con, sql(source_sql))
  res <- qfun(src, ...) |> collect()
  as.data.frame(res)
}

timed <- function(expr) {
  gc(verbose = FALSE)
  t0 <- proc.time()[["elapsed"]]
  value <- force(expr)
  list(elapsed = proc.time()[["elapsed"]] - t0, value = value)
}

# ---------------------------------------------------------------------------
# Main loop
# ---------------------------------------------------------------------------

results   <- list()
memory_df <- list()
example_results <- list()

for (scn in manifest$scenario) {
  row_info  <- manifest[manifest$scenario == scn, ]
  csv_path  <- file.path(data_dir, sprintf("sdo_%s.csv", scn))
  pq_path   <- file.path(data_dir, sprintf("sdo_%s.parquet", scn))
  part_glob <- file.path(data_dir, sprintf("sdo_%s_partitioned", scn),
                         "**", "*.parquet")

  con <- dbConnect(duckdb(), dbdir = ":memory:")
  dbExecute(con, "PRAGMA threads=2")

  sources <- list(
    csv_memory = list(
      label = "CSV in memoria (readr + dplyr)",
      run   = function(qfun, is_remote) run_csv(csv_path, qfun,
                                                is_remote = is_remote)),
    duckdb_parquet = list(
      label = "Parquet + DuckDB (dplyr)",
      run   = function(qfun, is_remote)
        run_duckdb(con, sprintf("SELECT * FROM read_parquet('%s')", pq_path),
                   qfun, is_remote = is_remote)),
    duckdb_partitioned = list(
      label = "Parquet partizionato + DuckDB (dplyr)",
      run   = function(qfun, is_remote)
        run_duckdb(con, sprintf(
          "SELECT * FROM read_parquet('%s', hive_partitioning = true)",
          part_glob), qfun, is_remote = is_remote))
  )

  queries <- list(
    Q1 = list(label = "Ricoveri per ASL e mese",
              fun = function(x, is_remote) q1_monthly_admissions(x)),
    Q2 = list(label = "Degenza media per un MDC",
              fun = function(x, is_remote) q2_mean_los(x)),
    Q3 = list(label = "Riammissioni a 30 giorni",
              fun = function(x, is_remote) q3_readmissions(x, is_remote))
  )

  # in-memory footprint of the full CSV table, measured once
  d_full <- read_csv(csv_path, show_col_types = FALSE, progress = FALSE)
  memory_df[[scn]] <- data.frame(
    scenario = scn,
    n_rows   = nrow(d_full),
    n_cols   = ncol(d_full),
    memory_bytes = as.numeric(object.size(d_full)),
    stringsAsFactors = FALSE)
  rm(d_full); gc(verbose = FALSE)

  for (qn in names(queries)) {
    for (sn in names(sources)) {
      is_remote <- sn != "csv_memory"
      reps <- N_REPS[[scn]]
      times <- numeric(reps)
      val <- NULL
      for (i in seq_len(reps)) {
        tt <- timed(sources[[sn]]$run(queries[[qn]]$fun, is_remote))
        times[i] <- tt$elapsed
        val <- tt$value
      }
      results[[length(results) + 1]] <- data.frame(
        scenario = scn, query = qn, query_label = queries[[qn]]$label,
        strategy = sn, strategy_label = sources[[sn]]$label,
        median_sec = median(times), min_sec = min(times), max_sec = max(times),
        n_reps = reps, stringsAsFactors = FALSE)
      example_results[[paste(scn, qn, sn, sep = "|")]] <- val
      message(sprintf("[%s] %s / %-34s median %7.2f s",
                      scn, qn, sn, median(times)))
    }
  }

  dbDisconnect(con, shutdown = TRUE)
}

bench <- do.call(rbind, results)
mem   <- do.call(rbind, memory_df)

write.csv(bench, file.path(out_dir, "benchmark_results.csv"), row.names = FALSE)
write.csv(mem,   file.path(out_dir, "memory_footprint.csv"), row.names = FALSE)

# ---------------------------------------------------------------------------
# Consistency check: the three strategies must return the same numbers.
# ---------------------------------------------------------------------------
message("\n--- consistency check ---")
for (scn in manifest$scenario) {
  for (qn in c("Q1", "Q2", "Q3")) {
    ref <- example_results[[paste(scn, qn, "csv_memory", sep = "|")]]
    for (sn in c("duckdb_parquet", "duckdb_partitioned")) {
      cmp <- example_results[[paste(scn, qn, sn, sep = "|")]]
      ref2 <- ref[order(ref[[1]], if (ncol(ref) > 1) ref[[2]] else NULL), ]
      cmp2 <- cmp[order(cmp[[1]], if (ncol(cmp) > 1) cmp[[2]] else NULL), ]
      rownames(ref2) <- NULL; rownames(cmp2) <- NULL
      ok <- isTRUE(all.equal(ref2, cmp2[, names(ref2), drop = FALSE],
                             tolerance = 1e-8, check.attributes = FALSE))
      message(sprintf("[%s] %s  csv_memory vs %-20s : %s",
                      scn, qn, sn, if (ok) "IDENTICO" else "DIVERSO"))
    }
  }
}

# Save the analytical results used in the article's charts (large scenario).
saveRDS(example_results, file.path(out_dir, "example_results.rds"))

message("\n--- benchmark table ---")
print(bench, row.names = FALSE)
message("\n--- memory footprint ---")
print(mem, row.names = FALSE)
message("\n--- columns used per query ---")
print(data.frame(query = names(COLS_USED),
                 n_cols_used = lengths(COLS_USED),
                 n_cols_total = manifest$n_cols[1]), row.names = FALSE)
