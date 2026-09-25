# ---------------------------------------------------------------------------
# RHO Lab 02 - Step 3: charts.
# Palette: three categorical slots, validated for colour-vision deficiency.
# Italian number formatting (decimal comma, thousands dot).
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(ggplot2); library(dplyr); library(tidyr); library(scales)
})

out_dir <- "output"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

COL_CSV  <- "#2a78d6"   # slot 1, blue
COL_PQ   <- "#eb6834"   # slot 2, orange
COL_PART <- "#1baf7a"   # slot 3, aqua
INK      <- "#0b0b0b"
INK_SOFT <- "#52514e"
SURFACE  <- "#fcfcfb"

it_num <- function(x, digits = 1) {
  formatC(x, format = "f", digits = digits,
          big.mark = ".", decimal.mark = ",")
}

theme_rho <- function(base_size = 15) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.background    = element_rect(fill = SURFACE, colour = NA),
      panel.background   = element_rect(fill = SURFACE, colour = NA),
      plot.title         = element_text(face = "bold", size = base_size + 4,
                                        colour = INK, margin = margin(b = 4)),
      plot.subtitle      = element_text(size = base_size, colour = INK_SOFT,
                                        margin = margin(b = 14)),
      plot.caption       = element_text(size = base_size - 4, colour = INK_SOFT,
                                        hjust = 0, margin = margin(t = 12)),
      axis.title         = element_text(size = base_size - 1, colour = INK_SOFT),
      axis.text          = element_text(size = base_size - 1, colour = INK),
      panel.grid.minor   = element_blank(),
      panel.grid.major   = element_line(colour = "#e6e5e1", linewidth = 0.4),
      strip.text         = element_text(face = "bold", size = base_size,
                                        colour = INK),
      legend.position    = "top",
      legend.title       = element_blank(),
      legend.text        = element_text(size = base_size - 1, colour = INK),
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
# Chart 1 - the problem: how much the same archive weighs
# ---------------------------------------------------------------------------
m <- manifest[manifest$scenario == SCN, ]
mm <- mem[mem$scenario == SCN, ]

p1_data <- data.frame(
  what = factor(
    c("In memoria in R\n(data frame completo)", "Su disco: CSV",
      "Su disco: parquet"),
    levels = c("In memoria in R\n(data frame completo)", "Su disco: CSV",
               "Su disco: parquet")),
  mb = c(mm$memory_bytes, m$csv_bytes, m$parquet_bytes) / 1024^2,
  fill = c(COL_CSV, COL_CSV, COL_PQ)
)

p1 <- ggplot(p1_data, aes(x = what, y = mb, fill = fill)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0(it_num(mb, 0), " MB")),
            vjust = -0.5, size = 5.2, fontface = "bold", colour = INK) +
  scale_fill_identity() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18)),
                     labels = function(x) it_num(x, 0)) +
  labs(
    title = sprintf("Lo stesso archivio, tre pesi diversi"),
    subtitle = sprintf(
      "%s righe e %d colonne di schede di dimissione simulate",
      it_num(m$n_rows, 0), m$n_cols),
    x = NULL, y = "Megabyte",
    caption = NOTE) +
  theme_rho()

ggsave(file.path(out_dir, "01-file-size.png"), p1,
       width = 9, height = 6, dpi = 150, bg = SURFACE)

# ---------------------------------------------------------------------------
# Chart 2 - benchmark: time per question and strategy
# ---------------------------------------------------------------------------
strategy_levels <- c("csv_memory", "duckdb_parquet", "duckdb_partitioned")
strategy_labels <- c("CSV in memoria",
                     "Parquet + DuckDB",
                     "Parquet partizionato")
strategy_cols <- setNames(c(COL_CSV, COL_PQ, COL_PART), strategy_labels)

scn_labels <- c(small = "1 milione di righe", large = "10 milioni di righe")

b <- bench |>
  mutate(
    strategy_f = factor(strategy, levels = strategy_levels,
                        labels = strategy_labels),
    query_f = factor(query,
                     levels = c("Q1", "Q2", "Q3"),
                     labels = c("Ricoveri per ASL e mese",
                                "Degenza media per un MDC",
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
  geom_point(size = 4.5, position = dodge) +
  geom_text(aes(label = paste0(it_num(median_sec, 1), " s")),
            position = dodge, hjust = -0.3, size = 4,
            show.legend = FALSE, fontface = "bold") +
  facet_wrap(~ scenario_f, ncol = 1) +
  scale_colour_manual(values = strategy_cols) +
  scale_x_log10(limits = c(0.06, 900),
                breaks = c(0.1, 1, 10, 100),
                labels = c("0,1", "1", "10", "100"),
                expand = expansion(mult = c(0.02, 0.02))) +
  guides(colour = guide_legend(nrow = 1)) +
  theme(legend.justification = "left",
        legend.text = element_text(size = 12),
        legend.key.spacing.x = grid::unit(6, "pt")) +
  labs(x = "Secondi, scala logaritmica (mediana delle ripetizioni)", y = NULL,
       title = "Quanto costa rispondere alla stessa domanda",
       subtitle = "Tempo totale, lettura dei dati inclusa",
       caption = NOTE) +
  theme_rho()

ggsave(file.path(out_dir, "02-benchmark.png"), p2,
       width = 10, height = 8, dpi = 150, bg = SURFACE)

# ---------------------------------------------------------------------------
# Chart 3 - the analytical result: 30-day readmission rate by ASL
# ---------------------------------------------------------------------------
ex <- readRDS("output/example_results.rds")
re <- ex[[paste(SCN, "Q3", "duckdb_parquet", sep = "|")]]

p3 <- ggplot(re, aes(x = reorder(asl_code, readmission_rate),
                     y = readmission_rate)) +
  geom_col(fill = COL_PQ, width = 0.65) +
  geom_text(aes(label = paste0(it_num(readmission_rate, 1), "%")),
            hjust = -0.2, size = 4.6, fontface = "bold", colour = INK) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)),
                     labels = function(x) it_num(x, 0)) +
  labs(title = "Il risultato: riammissioni entro 30 giorni",
       subtitle = "Ricoveri ordinari 2024, calcolo eseguito interamente sul file parquet",
       x = NULL, y = "Quota di riammissioni (%)",
       caption = NOTE) +
  theme_rho()

ggsave(file.path(out_dir, "03-readmissions.png"), p3,
       width = 9, height = 7, dpi = 150, bg = SURFACE)

message("charts written to ", out_dir)
