# ---------------------------------------------------------------------------
# RHO Lab 02 - Step 3: charts.
#
# Palette and white background follow the RHO Lab identity (article 01).
# Canvas is deliberately small relative to the type size: LinkedIn displays
# the image at column width, so what matters is the ratio between text and
# canvas, not the pixel count.
# Italian number formatting (decimal comma, thousands dot).
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(ggplot2); library(dplyr); library(tidyr); library(scales)
  library(duckdb); library(DBI)
})

out_dir <- "output"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# Palette sampled from the article 01 cover
NAVY   <- "#1f3a5f"
ORANGE <- "#e67e22"
GREEN  <- "#16a085"   # same Flat UI family as the rest of the RHO palette
GREY   <- "#7f8c8d"
INK    <- "#1f3a5f"
INK_SOFT <- "#5c5c5c"
GRID   <- "#e2e5ea"
BG     <- "#ffffff"

# Output geometry: one width for all three charts, so they look like a set.
W   <- 8
DPI <- 200

it_num <- function(x, digits = 1) {
  formatC(x, format = "f", digits = digits,
          big.mark = ".", decimal.mark = ",")
}

theme_rho <- function(base_size = 17) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.background    = element_rect(fill = BG, colour = NA),
      panel.background   = element_rect(fill = BG, colour = NA),
      plot.title         = element_text(face = "bold", size = base_size + 4,
                                        colour = NAVY, margin = margin(b = 3)),
      plot.subtitle      = element_text(size = base_size - 1, colour = INK_SOFT,
                                        margin = margin(b = 12)),
      plot.caption       = element_text(size = base_size - 5, colour = INK_SOFT,
                                        hjust = 0, margin = margin(t = 10)),
      axis.title         = element_text(size = base_size - 2, colour = INK_SOFT),
      axis.text          = element_text(size = base_size - 1, colour = INK_SOFT),
      panel.grid.minor   = element_blank(),
      panel.grid.major   = element_line(colour = GRID, linewidth = 0.4),
      strip.text         = element_text(face = "bold", size = base_size,
                                        colour = NAVY),
      legend.position    = "top",
      legend.title       = element_blank(),
      legend.text        = element_text(size = base_size - 2, colour = INK_SOFT),
      legend.justification = "left",
      legend.margin      = margin(b = 4),
      plot.title.position = "plot",
      plot.caption.position = "plot"
    )
}

NOTE <- "Dati simulati a scopo didattico. Nessun dato reale di strutture o pazienti."

manifest <- read.csv("data/manifest.csv", stringsAsFactors = FALSE)
bench    <- read.csv("output/benchmark_results.csv", stringsAsFactors = FALSE)
mem      <- read.csv("output/memory_footprint.csv", stringsAsFactors = FALSE)

SCN <- "large"   # the scenario used in the article's headline numbers

# ---------------------------------------------------------------------------
# Chart 1 - the problem: how much the archive weighs, and how much of it a
# single question actually has to read.
#
# The last bar is measured by writing the four columns question Q1 uses on
# their own: that is what a columnar reader has to touch.
# ---------------------------------------------------------------------------
m  <- manifest[manifest$scenario == SCN, ]
mm <- mem[mem$scenario == SCN, ]

Q1_COLS <- c("admission_regime", "admission_year", "admission_date", "asl_code")

pq_path <- file.path("data", sprintf("sdo_%s.parquet", SCN))
con <- dbConnect(duckdb(), dbdir = ":memory:")
tmp <- tempfile(fileext = ".parquet")
invisible(dbExecute(con, sprintf(
  "COPY (SELECT %s FROM read_parquet('%s')) TO '%s'
   (FORMAT PARQUET, COMPRESSION ZSTD)",
  paste(Q1_COLS, collapse = ", "), pq_path, tmp)))
cols_bytes <- file.info(tmp)$size
dbDisconnect(con, shutdown = TRUE)

lab_cols <- sprintf("Le %d colonne che servono\nalla domanda", length(Q1_COLS))

# The in-memory footprint is deliberately NOT plotted here: this chart sits
# before the first reading level, where the article also speaks to people who
# do not use R. It stays in the text, where it can be explained in a clause.
p1_data <- data.frame(
  what = factor(
    c("Il file CSV", "Lo stesso file in parquet", lab_cols),
    levels = c(lab_cols, "Lo stesso file in parquet", "Il file CSV")),
  mb   = c(m$csv_bytes, m$parquet_bytes, cols_bytes) / 1024^2,
  fill = c(NAVY, ORANGE, GREEN)
)

p1 <- ggplot(p1_data, aes(x = what, y = mb, fill = fill)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = paste0(it_num(mb, 0), " MB"), colour = fill),
            hjust = -0.2, size = 5.2, fontface = "bold", show.legend = FALSE) +
  coord_flip() +
  scale_fill_identity() +
  scale_colour_identity() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.30)),
                     breaks = c(0, 200, 400, 600, 800),
                     labels = function(x) it_num(x, 0)) +
  labs(
    title = "Quanto pesa, e quanto se ne legge davvero",
    subtitle = sprintf("%s righe e %d colonne di schede di dimissione simulate",
                       it_num(m$n_rows, 0), m$n_cols),
    x = NULL, y = "Megabyte",
    caption = NOTE) +
  theme_rho(base_size = 15)

ggsave(file.path(out_dir, "01-pesi.png"), p1,
       width = W, height = 4.6, dpi = DPI, bg = BG)

# ---------------------------------------------------------------------------
# Chart 2 - benchmark: time per question and strategy
# ---------------------------------------------------------------------------
# Two series only: the two situations the article compares. The partitioned
# variant and the CSV queried directly by DuckDB are numbers in the text, not
# lines in the chart.
strategy_levels <- c("csv_memory", "duckdb_parquet")
strategy_labels <- c("CSV letto in memoria in R", "Parquet con DuckDB")
strategy_cols   <- setNames(c(NAVY, ORANGE), strategy_labels)

scn_labels <- c(small = "1 milione di righe", large = "10 milioni di righe")

b <- bench |>
  filter(strategy %in% strategy_levels) |>
  mutate(
    strategy_f = factor(strategy, levels = strategy_levels,
                        labels = strategy_labels),
    query_f = factor(query, levels = c("Q1", "Q2", "Q3"),
                     labels = c("Ricoveri per ASL e mese",
                                "Degenza media per una MDC",
                                "Riammissioni a 30 giorni")),
    scenario_f = factor(scenario, levels = c("small", "large"),
                        labels = scn_labels[c("small", "large")]))

# reverse factor levels so the first question sits at the top of the panel
forcats_rev <- function(f) factor(f, levels = rev(levels(f)))

# A logarithmic x axis: the three strategies differ by orders of magnitude,
# and on a linear scale everything except the CSV read collapses onto zero.
dodge <- position_dodge(width = 0.75)

p2 <- ggplot(b, aes(x = median_sec, y = forcats_rev(query_f),
                    colour = strategy_f)) +
  geom_point(size = 3.8, position = dodge) +
  geom_text(aes(label = paste0(it_num(median_sec, 1), " s")),
            position = dodge, hjust = -0.35, size = 4,
            show.legend = FALSE, fontface = "bold") +
  facet_wrap(~ scenario_f, ncol = 1) +
  scale_colour_manual(values = strategy_cols) +
  scale_x_log10(limits = c(0.07, 2200),
                breaks = c(0.1, 1, 10, 100),
                labels = c("0,1", "1", "10", "100"),
                expand = expansion(mult = c(0.02, 0.02))) +
  guides(colour = guide_legend(nrow = 1)) +
  labs(x = "Secondi, scala logaritmica", y = NULL,
       title = "Quanto costa rispondere alla stessa domanda",
       subtitle = "Tempo totale, mediana delle ripetizioni, lettura dei dati inclusa",
       caption = NOTE) +
  theme_rho(base_size = 14)

ggsave(file.path(out_dir, "02-benchmark.png"), p2,
       width = W, height = 5.8, dpi = DPI, bg = BG)

message("charts written to ", out_dir)
