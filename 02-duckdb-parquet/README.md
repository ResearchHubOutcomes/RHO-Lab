# RHO Lab 02 — Il file non si apre

Interrogare grandi archivi senza caricarli in memoria: `dplyr` + `dbplyr` + DuckDB su file parquet.

All data is **simulated**. No real hospital or patient data is used.

## How to run

```r
# from this folder
source("R/01-simulate-data.R")   # generates CSV, parquet, partitioned parquet
source("R/02-benchmark.R")       # runs the benchmark and the consistency check
source("R/03-plots.R")           # writes the two charts used in the article
source("R/04-checks.R")          # the two numbers quoted in the advanced section
source("R/05-cover.R")           # 1920x1080 newsletter cover
```

`R/05-cover.R` holds `rho_cover()`, the cover template shared by the series,
plus three optional panel textures (`texture_grid()`, `texture_code()`,
`texture_columns()`).

Requires: `duckdb`, `DBI`, `dplyr`, `dbplyr`, `readr`, `tidyr`, `lubridate`,
`ggplot2`, `scales`.

## What the benchmark compares

| Strategy | What it does |
|---|---|
| `csv_memory` | full CSV read into R with `readr`, then `dplyr` in memory |
| `duckdb_parquet` | parquet file queried by DuckDB through `dplyr`/`dbplyr` |
| `duckdb_partitioned` | same, on a parquet dataset partitioned by year |

| Question | What it computes |
|---|---|
| Q1 | ordinary admissions by ASL and month, one year |
| Q2 | mean length of stay for one MDC, one year, by ASL |
| Q3 | share of 30-day readmissions by ASL (window function) |

The script checks that the three strategies return identical results.

The readmission rates are deliberately **not** plotted. They are an artefact of
the generator, which varies the patient-pool size per ASL on purpose; charting
them would produce a ranking of twelve simulated providers. What the charts show
is the procedure: file size, query time, and how much of the file a single
question has to read.

## Results on the machine used for the article

2 cores, 7 GB RAM, DuckDB 1.5.5, R 4.3.3. Ten million rows, fifteen columns.

| Question | CSV in memory | Parquet + DuckDB | Partitioned |
|---|---|---|---|
| Q1 | 13.87 s | 0.23 s | 0.11 s |
| Q2 | 14.54 s | 0.21 s | 0.12 s |
| Q3 | 202.19 s | 1.92 s | 1.86 s |

File size: 833 MB (CSV), 123 MB (parquet), 1,144 MB (in R memory).

These numbers depend on the simulated data and on the hardware. Re-run them.
