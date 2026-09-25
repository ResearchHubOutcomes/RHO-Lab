# ---------------------------------------------------------------------------
# RHO Lab 02 - Step 5: newsletter cover, 1920x1080 PNG.
#
# Layout and palette follow the cover of article 01 (funnel plot):
#   white background, navy bold title, two-line grey subtitle,
#   the article's own chart used as an illustration underneath,
#   "RHO Lab" bottom right.
#
# Two variants are produced:
#   A) the two-bar time comparison  -> 00-cover-a.png
#   B) the benchmark dot plot       -> 00-cover-b.png
# All content stays inside the central area: LinkedIn can crop the edges.
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({ library(ggplot2) })

out_dir <- "output"

# Palette sampled from the article 01 cover
NAVY   <- "#1f3a5f"
ORANGE <- "#e67e22"
RED    <- "#c0392b"
GREY   <- "#7f8c8d"
INK    <- "#5c5c5c"
BG     <- "#ffffff"

TITLE    <- "Il file non si apre"
SUB_1    <- "Interrogare grandi archivi"
SUB_2    <- "senza caricarli in memoria"

b <- read.csv("output/benchmark_results.csv", stringsAsFactors = FALSE)
big <- b[b$scenario == "large", ]

it <- function(x, d = 1) formatC(x, format = "f", digits = d,
                                 big.mark = ".", decimal.mark = ",")

# Shared page furniture: title block and signature, on a 0-100 canvas.
page <- function() {
  list(
    annotate("text", x = 6, y = 90, hjust = 0, vjust = 1, size = 21,
             colour = NAVY, fontface = "bold", label = TITLE),
    annotate("text", x = 6, y = 77.5, hjust = 0, vjust = 1, size = 9.4,
             colour = INK, label = SUB_1),
    annotate("text", x = 6, y = 70.5, hjust = 0, vjust = 1, size = 9.4,
             colour = INK, label = SUB_2),
    annotate("text", x = 94, y = 4, hjust = 1, vjust = 0, size = 7,
             colour = NAVY, fontface = "bold", label = "RHO Lab"),
    scale_x_continuous(limits = c(0, 100), expand = c(0, 0)),
    scale_y_continuous(limits = c(0, 100), expand = c(0, 0)),
    theme_void(),
    theme(plot.background  = element_rect(fill = BG, colour = NA),
          panel.background = element_rect(fill = BG, colour = NA),
          plot.margin = margin(0, 0, 0, 0))
  )
}

save_cover <- function(p, file) {
  ggsave(file.path(out_dir, file), p,
         width = 1920 / 150, height = 1080 / 150, dpi = 150, bg = BG)
  message("written: ", file.path(out_dir, file))
}

# ---------------------------------------------------------------------------
# Variant A - two bars: the same question, read from CSV or from parquet.
# Bar length is on a square-root scale, otherwise the short bar disappears.
# The printed numbers are the real medians.
# ---------------------------------------------------------------------------
slow <- big$median_sec[big$query == "Q3" & big$strategy == "csv_memory"]
fast <- big$median_sec[big$query == "Q3" & big$strategy == "duckdb_parquet"]

X0 <- 6; XMAX <- 72
bars <- data.frame(
  ymid = c(45, 25),
  xend = X0 + XMAX * sqrt(c(slow, fast)) / sqrt(slow),
  col  = c(NAVY, ORANGE),
  lab  = paste0(it(c(slow, fast)), " s"),
  what = c("CSV letto per intero in memoria",
           "Parquet interrogato con DuckDB")
)

pA <- ggplot() +
  geom_rect(data = bars,
            aes(xmin = X0, xmax = xend, ymin = ymid - 4, ymax = ymid + 4,
                fill = col)) +
  scale_fill_identity() +
  geom_text(data = bars, aes(x = xend + 1.2, y = ymid, label = lab),
            hjust = 0, vjust = 0.5, size = 11, fontface = "bold",
            colour = NAVY) +
  geom_text(data = bars, aes(x = X0, y = ymid + 8, label = what),
            hjust = 0, vjust = 0, size = 6.2, colour = INK) +
  annotate("text", x = 6, y = 11, hjust = 0, vjust = 0, size = 5.4,
           colour = GREY,
           label = "Stessa domanda, stesso risultato. 10 milioni di righe simulate.") +
  page()

save_cover(pA, "00-cover-a.png")

# ---------------------------------------------------------------------------
# Variant B - the article's own chart, stripped down: three questions,
# three strategies, logarithmic time axis.
# ---------------------------------------------------------------------------
strategy_levels <- c("csv_memory", "duckdb_parquet", "duckdb_partitioned")
strategy_cols   <- c(csv_memory = NAVY, duckdb_parquet = ORANGE,
                     duckdb_partitioned = GREY)
query_rows      <- c(Q1 = 50, Q2 = 37, Q3 = 24)
query_labels    <- c(Q1 = "Ricoveri per ASL e mese",
                     Q2 = "Degenza media per una MDC",
                     Q3 = "Riammissioni a 30 giorni")

# map seconds (0.1 - 300) onto x in [30, 88] on a log scale
xmap <- function(s) 30 + 58 * (log10(s) - log10(0.08)) /
  (log10(300) - log10(0.08))

d <- big
d$y   <- query_rows[d$query]
d$x   <- xmap(d$median_sec)
d$col <- strategy_cols[d$strategy]
# draw order: the orange series last, so it stays on top where two values
# are practically identical (1,86 s and 1,92 s on the readmissions row)
d <- d[order(match(d$strategy,
                   c("csv_memory", "duckdb_partitioned", "duckdb_parquet"))), ]

rows <- data.frame(y = query_rows, lab = query_labels[names(query_rows)])

pB <- ggplot() +
  geom_segment(data = rows, aes(x = 29, xend = 90, y = y, yend = y),
               colour = "#e2e5ea", linewidth = 0.8) +
  geom_text(data = rows, aes(x = 27, y = y, label = lab),
            hjust = 1, vjust = 0.5, size = 5.6, colour = INK) +
  # a white ring keeps two nearly identical values readable as two points
  geom_point(data = d, aes(x = x, y = y, fill = col), shape = 21,
             size = 8, colour = BG, stroke = 1.6) +
  scale_fill_identity() +
  # only the two extremes are labelled, so the cover stays readable small
  annotate("text", x = xmap(slow), y = query_rows[["Q3"]] - 6, size = 6.4,
           colour = NAVY, fontface = "bold", label = paste0(it(slow), " s")) +
  annotate("text", x = xmap(fast), y = query_rows[["Q3"]] - 6, size = 6.4,
           colour = ORANGE, fontface = "bold", label = paste0(it(fast), " s")) +
  annotate("text", x = 6, y = 11, hjust = 0, vjust = 0, size = 5.4,
           colour = GREY,
           label = "Tre domande, tre strategie. Tempo in scala logaritmica.") +
  page()

save_cover(pB, "00-cover-b.png")
