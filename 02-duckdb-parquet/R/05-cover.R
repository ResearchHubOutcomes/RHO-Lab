# ---------------------------------------------------------------------------
# RHO Lab - newsletter cover template, 1920x1080 PNG.
#
# One layout for the whole series: navy panel on the left third with the
# number, title, subtitle and signature; the article's own chart on the white
# right side; a thin navy border, so the white area does not bleed into the
# LinkedIn feed.
#
# The illustration is passed in as a list of ggplot layers drawn on the same
# 0-100 canvas, inside x = 42..98.
#
# Output:
#   output/00-cover.png
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({ library(ggplot2) })

out_dir <- "output"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

NAVY   <- "#1f3a5f"
ORANGE <- "#e67e22"
GREEN  <- "#16a085"
GREY   <- "#7f8c8d"
INK    <- "#5c5c5c"
PALE   <- "#c9d2dd"
SLATE  <- "#8ea4bd"   # quieter text on the navy panel
LEV_G  <- "#2ecc8f"   # the three reading levels, stepped for the dark panel
LEV_B  <- "#5dade2"
LEV_P  <- "#b07cc6"
RULE   <- "#e2e5ea"
WHITE  <- "#ffffff"

it <- function(x, d = 1) formatC(x, format = "f", digits = d,
                                 big.mark = ".", decimal.mark = ",")

PANEL_W <- 38   # width of the navy panel, in canvas units
TEXT_BG <- "#2a4a72"   # barely lighter than NAVY: reads as texture, not as text

# ---------------------------------------------------------------------------
# Optional textures for the navy panel. Each returns ggplot layers drawn
# before the title, so the type always sits on top.
# ---------------------------------------------------------------------------

# 1. the article's own code, in filigree
texture_code <- function(lines, size = 2.5, y_top = 97, step = 3.05,
                         max_chars = 62) {
  lines <- substr(rep(lines, length.out = 32), 1, max_chars)
  y <- y_top - step * (seq_along(lines) - 1)
  keep <- y > 2
  annotate("text", x = 2.5, y = y[keep], hjust = 0, vjust = 0.5,
           size = size, family = "mono", colour = TEXT_BG,
           label = lines[keep])
}

# 2. a table grid: rows and columns, the shape of every dataset
texture_grid <- function(step_x = 4.2, step_y = 4.2, lw = 0.35) {
  vx <- seq(0, PANEL_W, by = step_x)
  hy <- seq(0, 100, by = step_y)
  list(
    annotate("segment", x = vx, xend = vx, y = 0, yend = 100,
             colour = TEXT_BG, linewidth = lw),
    annotate("segment", x = 0, xend = PANEL_W, y = hy, yend = hy,
             colour = TEXT_BG, linewidth = lw)
  )
}

# 3. columns of different length: how a columnar format stores a table
texture_columns <- function(n = 26, seed = 20260925) {
  set.seed(seed)
  x <- seq(1.2, PANEL_W - 1.2, length.out = n)
  h <- runif(n, 12, 92)
  y0 <- runif(n, 0, 100 - h)
  annotate("rect", xmin = x - 0.5, xmax = x + 0.5,
           ymin = y0, ymax = y0 + h, fill = TEXT_BG)
}


# ---------------------------------------------------------------------------
# The template
# ---------------------------------------------------------------------------
rho_cover <- function(number, title, subtitle, illustration, file,
                      title_size = 12.5, texture = NULL) {
  p <- ggplot() +
    annotate("rect", xmin = 0, xmax = PANEL_W, ymin = 0, ymax = 100,
             fill = NAVY) +
    texture +
    # eyebrow + rule
    annotate("rect", xmin = 5, xmax = 9.5, ymin = 90.6, ymax = 91.3,
             fill = ORANGE) +
    annotate("text", x = 5, y = 95, hjust = 0, vjust = 0.5, size = 4.8,
             colour = ORANGE, fontface = "bold",
             label = sprintf("RHO LAB / %s", number)) +
    annotate("text", x = 5, y = 86, hjust = 0, vjust = 1, size = title_size,
             colour = WHITE, fontface = "bold", lineheight = 0.92,
             label = title) +
    annotate("text", x = 5, y = 67, hjust = 0, vjust = 1, size = 7,
             colour = PALE, lineheight = 1.12, label = subtitle) +
    # the three reading levels: the same promise in every article
    annotate("text", x = 5, y = 46, hjust = 0, vjust = 0.5, size = 3.9,
             colour = SLATE, fontface = "bold",
             label = "TRE LIVELLI DI LETTURA") +
    annotate("point", x = 6, y = c(38, 31, 24), size = 3.6,
             colour = c(LEV_G, LEV_B, LEV_P)) +
    annotate("text", x = 8.4, y = c(38, 31, 24), hjust = 0, vjust = 0.5,
             size = 4.6, colour = PALE,
             label = c("Per tutti", "Per chi usa R",
                       "Per chi vuole andare a fondo")) +
    annotate("text", x = 5, y = 8, hjust = 0, vjust = 0.5, size = 5.6,
             colour = WHITE, fontface = "bold", label = "RHO Lab") +
    illustration +
    # border last, so nothing paints over it
    annotate("rect", xmin = 0.35, xmax = 99.65, ymin = 0.6, ymax = 99.4,
             fill = NA, colour = NAVY, linewidth = 1.4) +
    scale_x_continuous(limits = c(0, 100), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
    theme_void() +
    theme(plot.background  = element_rect(fill = WHITE, colour = NA),
          panel.background = element_rect(fill = WHITE, colour = NA),
          plot.margin = margin(0, 0, 0, 0))

  ggsave(file.path(out_dir, file), p,
         width = 1920 / 150, height = 1080 / 150, dpi = 150, bg = WHITE)
  message("written: ", file.path(out_dir, file))
}

# The question that makes the chart readable before the article is opened.
cover_question <- function(text, y = 95, size = 5.6) {
  annotate("text", x = 47, y = y, hjust = 0, vjust = 1, size = size,
           colour = NAVY, lineheight = 1.18, label = text)
}

# A small horizontal legend drawn by hand, so it sits exactly where we want.
legend_row <- function(x0, y, labels, cols, size = 3.9, gap = 2.6) {
  layers <- list(); x <- x0
  for (i in seq_along(labels)) {
    layers <- c(layers, list(
      annotate("point", x = x, y = y, colour = cols[i], size = 3.4),
      annotate("text", x = x + 1.1, y = y, hjust = 0, vjust = 0.5,
               size = size, colour = INK, label = labels[i])))
    x <- x + 1.1 + nchar(labels[i]) * size * 0.19 + gap
  }
  layers
}

# ===========================================================================
# Article 02 - benchmark dot plot
# ===========================================================================
b <- read.csv("output/benchmark_results.csv", stringsAsFactors = FALSE)
big <- b[b$scenario == "large", ]

strategy_cols <- c(csv_memory = NAVY, duckdb_parquet = ORANGE,
                   duckdb_partitioned = GREEN)
strategy_off  <- c(csv_memory = 3.4, duckdb_parquet = 0,
                   duckdb_partitioned = -3.4)   # so the three never overlap
query_rows    <- c(Q1 = 65, Q2 = 46, Q3 = 27)
query_labels  <- c(
  Q1 = "Calcolare il numero di ricoveri per ASL e mese",
  Q2 = "Calcolare la degenza media per una MDC",
  Q3 = "Calcolare le riammissioni entro 30 giorni")

xmap2 <- function(s) 50 + 30 * (log10(s) - log10(0.08)) /
  (log10(300) - log10(0.08))

d <- big
d$y   <- query_rows[d$query] + strategy_off[d$strategy]
d$x   <- xmap2(d$median_sec)
d$col <- strategy_cols[d$strategy]

rows <- data.frame(y = query_rows, lab = query_labels[names(query_rows)])

grid_s <- c(0.1, 1, 10, 100)

illu_02 <- list(
  cover_question(paste("Stessa domanda, stesso risultato.",
                       "Perche\u0301 una costa 202 secondi e l\u0027altra 2?",
                       sep = "\n")),
  annotate("segment", x = xmap2(grid_s), xend = xmap2(grid_s),
           y = 21, yend = 74, colour = RULE, linewidth = 0.7),
  annotate("text", x = xmap2(grid_s), y = 18, vjust = 0.5, size = 3.6,
           colour = GREY,
           label = c("0,1 s", "1 s", "10 s", "100 s")),
  legend_row(47, 11,
             c("CSV in memoria", "Parquet + DuckDB", "Parquet partizionato"),
             c(NAVY, ORANGE, GREEN)),
  geom_text(data = rows, aes(x = 47, y = y + 9, label = lab),
            hjust = 0, vjust = 0.5, size = 4.4, colour = INK),
  geom_point(data = d, aes(x = x, y = y, colour = col), size = 5),
  geom_text(data = d, aes(x = x + 1.4, y = y,
                          label = paste0(it(median_sec, 1), " s"),
                          colour = col),
            hjust = 0, vjust = 0.5, size = 3.9, fontface = "bold"),
  scale_colour_identity(),
  annotate("text", x = 47, y = 5, hjust = 0, vjust = 0.5, size = 3.7,
           colour = GREY,
           label = "Tempo in scala logaritmica. 10 milioni di righe simulate.")
)

rho_cover("02", "Il file\nnon si apre",
          "Interrogare grandi archivi\nsenza caricarli in memoria",
          illu_02, "00-cover.png", texture = texture_grid())
