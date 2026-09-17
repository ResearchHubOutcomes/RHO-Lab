# =============================================================
# RHO Lab #1 - Funnel plot in R with ggplot2
# Comparing hospitals on a proportion indicator
# (e.g. 30-day mortality) without falling into league tables
# Data are SIMULATED for teaching purposes.
# =============================================================

library(dplyr)
library(ggplot2)
library(ggrepel)

set.seed(2026)

# -------------------------------------------------------------
# 1. Simulated data: 40 hospitals, very different volumes
# -------------------------------------------------------------
n_hosp <- 40
hospitals <- tibble(
  hospital = sprintf("H%02d", 1:n_hosp),
  n        = round(exp(runif(n_hosp, log(30), log(900))))   # cases treated
) |>
  mutate(
    # true rate: common baseline (8%) + small natural between-hospital variation
    p_true = plogis(qlogis(0.08) + rnorm(n_hosp, 0, 0.28)),
    # two hospitals that are genuinely different
    p_true = case_when(
      hospital == "H07" ~ 0.14,
      hospital == "H23" ~ 0.04,
      TRUE ~ p_true
    ),
    # force H07 and H23 to be high-volume
    n = case_when(hospital %in% c("H07", "H23") ~ c(620, 780)[match(hospital, c("H07", "H23"))],
                  TRUE ~ n),
    deaths = rbinom(n_hosp, n, p_true),
    rate   = deaths / n
  )

# Target: overall (pooled) rate
p0 <- sum(hospitals$deaths) / sum(hospitals$n)

# -------------------------------------------------------------
# 2. The misleading league table
# -------------------------------------------------------------
league <- hospitals |>
  arrange(desc(rate)) |>
  mutate(hospital = factor(hospital, levels = rev(hospital)),
         top5 = row_number() <= 5)

# Show only the top 8 and the bottom 3, so labels stay readable in the article
league_plot <- league |>
  mutate(pos = row_number()) |>
  filter(pos <= 8 | pos > n() - 3) |>
  mutate(group = if_else(pos <= 8, "I primi 8", "Gli ultimi 3"),
         group = factor(group, levels = c("I primi 8", "Gli ultimi 3")),
         label = paste0(scales::percent(rate, 0.1, decimal.mark = ","), "  (n = ", n, ")"))

p_league <- ggplot(league_plot, aes(x = rate, y = hospital, fill = top5)) +
  geom_vline(xintercept = p0, linetype = "dashed", colour = "grey30") +
  geom_col(width = 0.7) +
  geom_label(aes(label = label), hjust = -0.05, size = 5, colour = "grey20",
             fill = "white", label.size = 0, label.padding = unit(0.12, "lines")) +
  facet_grid(group ~ ., scales = "free_y", space = "free_y") +
  scale_x_continuous(labels = scales::label_percent(decimal.mark = ","),
                     expand = expansion(mult = c(0, 0.28))) +
  scale_fill_manual(values = c(`TRUE` = "#C0392B", `FALSE` = "#9DB4CC"), guide = "none") +
  labs(title = "La classifica: chi sono i 5 \"peggiori\"?",
       subtitle = paste0("Mortalità a 30 giorni su 40 ospedali (dati simulati). Tratteggio: media regionale ",
                         scales::percent(p0, 0.1, decimal.mark = ",")),
       x = NULL, y = NULL) +
  theme_minimal(base_size = 17) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text.y = element_text(angle = 0, face = "bold", size = 14),
        axis.text.y = element_text(face = "bold"),
        plot.title = element_text(face = "bold", size = 22),
        plot.subtitle = element_text(size = 14, colour = "grey30"))

ggsave("01_classifica.png", p_league, width = 10, height = 7, dpi = 200, bg = "white")

# -------------------------------------------------------------
# 3. Funnel plot - exact binomial control limits
# -------------------------------------------------------------
# For each volume n: which death rates are "compatible" with the
# regional rate p0? qbinom() gives the binomial quantiles; the
# interpolation step (Spiegelhalter, 2005) smooths the "saw-tooth"
# caused by the discreteness of counts.
# 95% limits   -> warning
# 99.8% limits -> alarm

binom_limit <- function(prob, n, p0) {
  r     <- qbinom(prob, n, p0)
  alpha <- (pbinom(r, n, p0) - prob) / (pbinom(r, n, p0) - pbinom(r - 1, n, p0))
  pmax((r - alpha) / n, 0)
}

funnel_limits <- tibble(n = 20:950) |>
  mutate(
    lo95  = binom_limit(0.025, n, p0),
    hi95  = binom_limit(0.975, n, p0),
    lo998 = binom_limit(0.001, n, p0),
    hi998 = binom_limit(0.999, n, p0)
  )

hospitals <- hospitals |>
  mutate(
    status = case_when(
      rate > binom_limit(0.999, n, p0) | rate < binom_limit(0.001, n, p0) ~ "Oltre 99,8%",
      rate > binom_limit(0.975, n, p0) | rate < binom_limit(0.025, n, p0) ~ "Oltre 95%",
      TRUE ~ "Nella norma"
    ),
    top5 = hospital %in% league$hospital[league$top5]
  )

status_cols <- c("Nella norma" = "#7F8C8D", "Oltre 95%" = "#E67E22", "Oltre 99,8%" = "#C0392B")

p_funnel <- ggplot() +
  geom_line(data = funnel_limits, aes(n, hi998), colour = "#C0392B", linewidth = 0.6) +
  geom_line(data = funnel_limits, aes(n, lo998), colour = "#C0392B", linewidth = 0.6) +
  geom_line(data = funnel_limits, aes(n, hi95), colour = "#E67E22", linetype = "dashed") +
  geom_line(data = funnel_limits, aes(n, lo95), colour = "#E67E22", linetype = "dashed") +
  geom_hline(yintercept = p0, colour = "#1F3A5F", linewidth = 0.7) +
  geom_point(data = hospitals, aes(n, rate, colour = status), size = 2.8) +
  geom_point(data = filter(hospitals, top5), aes(n, rate),
             shape = 21, size = 4.6, stroke = 0.9, colour = "black", fill = NA) +
  geom_text_repel(data = filter(hospitals, top5 | status != "Nella norma"),
                  aes(n, rate, label = hospital), size = 3.2, seed = 1,
                  box.padding = 0.4, min.segment.length = 0) +
  scale_colour_manual(values = status_cols, name = NULL) +
  scale_y_continuous(labels = scales::label_percent(decimal.mark = ",")) +
  labs(title = "Lo stesso dato, letto con un funnel plot",
       subtitle = paste0("Linea blu: media regionale (", scales::percent(p0, 0.1, decimal.mark = ","),
                         "). Arancio: limiti 95%. Rosso: limiti 99,8% (binomiale esatta).\n",
                         "Cerchiati: i 5 \"peggiori\" della classifica."),
       x = "Numero di casi trattati", y = "Mortalità a 30 giorni") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top", plot.title = element_text(face = "bold"))

ggsave("02_funnel_plot.png", p_funnel, width = 9, height = 6.5, dpi = 200, bg = "white")

# -------------------------------------------------------------
# 4. ADVANCED - normal approximation vs exact + overdispersion
# -------------------------------------------------------------
# 4a. The classic formula p0 +/- z * sqrt(p0 (1 - p0) / n) is symmetric
#     and becomes unreliable when n is small or p0 is close to 0.

hospitals <- hospitals |>
  mutate(se = sqrt(p0 * (1 - p0) / n),
         z  = (rate - p0) / se)

approx_limits <- tibble(n = 20:950) |>
  mutate(se    = sqrt(p0 * (1 - p0) / n),
         lo998 = pmax(p0 - 3.09 * se, 0),
         hi998 = p0 + 3.09 * se)

# 4b. Overdispersion (Spiegelhalter, 2005 - additive random effects)
#     Real data almost always vary more than the binomial model predicts
#     (case mix, coding, organisation): too-narrow limits flag too many units.
I    <- nrow(hospitals)
zw   <- pmin(pmax(hospitals$z, quantile(hospitals$z, 0.10)),
             quantile(hospitals$z, 0.90))             # winsorised z-scores (10%)
phi  <- sum(zw^2) / I                                  # overdispersion factor
w    <- 1 / hospitals$se^2
tau2 <- max(0, (I * phi - (I - 1)) / (sum(w) - sum(w^2) / sum(w)))

cat(sprintf("Target p0 = %.4f | phi = %.2f | tau = %.4f\n", p0, phi, sqrt(tau2)))

od_limits <- approx_limits |>
  mutate(lo998_od = pmax(p0 - 3.09 * sqrt(se^2 + tau2), 0),
         hi998_od = p0 + 3.09 * sqrt(se^2 + tau2))

hospitals <- hospitals |>
  mutate(z_od   = (rate - p0) / sqrt(se^2 + tau2),
         out_od = abs(z_od) > 3.09)

p_adv <- ggplot() +
  geom_line(data = funnel_limits, aes(n, hi998), colour = "grey55", linewidth = 0.5) +
  geom_line(data = funnel_limits, aes(n, lo998), colour = "grey55", linewidth = 0.5) +
  geom_line(data = approx_limits, aes(n, hi998), colour = "#C0392B", linetype = "dashed") +
  geom_line(data = approx_limits, aes(n, lo998), colour = "#C0392B", linetype = "dashed") +
  geom_line(data = od_limits, aes(n, hi998_od), colour = "#1F3A5F", linewidth = 0.9) +
  geom_line(data = od_limits, aes(n, lo998_od), colour = "#1F3A5F", linewidth = 0.9) +
  geom_hline(yintercept = p0, colour = "grey30") +
  geom_point(data = hospitals, aes(n, rate, fill = out_od), shape = 21, size = 2.8, colour = "grey20") +
  geom_text_repel(data = filter(hospitals, abs(z) > 3.09 | out_od),
                  aes(n, rate, label = hospital), size = 3.2, seed = 1) +
  scale_fill_manual(values = c(`TRUE` = "#C0392B", `FALSE` = "white"), guide = "none") +
  scale_y_continuous(labels = scales::label_percent(decimal.mark = ",")) +
  labs(title = "Versione avanzata: tre modi di tracciare i limiti al 99,8%",
       subtitle = "Rosso tratteggiato: approssimazione normale. Grigio: binomiale esatta.\nBlu: correzione per sovradispersione (Spiegelhalter, 2005). In rosso gli ospedali oltre il limite blu.",
       x = "Numero di casi trattati", y = "Mortalit\u00e0 a 30 giorni") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

ggsave("03_funnel_avanzato.png", p_adv, width = 9, height = 6.5, dpi = 200, bg = "white")

# -------------------------------------------------------------
# 5. Summary
# -------------------------------------------------------------
print(hospitals |>
        filter(top5 | status != "Nella norma" | abs(z) > 3.09 | out_od) |>
        select(hospital, n, deaths, rate, status, z, z_od, out_od) |>
        arrange(desc(rate)), n = Inf)
