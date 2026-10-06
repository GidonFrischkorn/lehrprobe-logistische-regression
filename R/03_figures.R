# 03_figures.R
# Erstellt alle Abbildungen der Folien (und die Offline-Fallbacks für den Slider).

library(ggplot2)
library(patchwork)

remission_data <- read.csv("data/remission.csv")
sim_params     <- read.csv("output/sim_params.csv")
beta0 <- sim_params$value[sim_params$parameter == "beta0"]
beta1 <- sim_params$value[sim_params$parameter == "beta1"]

col_data  <- "grey35"
col_lm    <- "#D55E00"   # orange: lineare Regression
col_logit <- "#0072B2"   # blau: logistisches Modell
col_alt   <- "#009E73"   # grün: Vergleichskurve

theme_lecture <- theme_minimal(base_size = 20) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"))

set.seed(1)  # nur für das vertikale Jittern der Punkte
remission_data$y_jit <- remission_data$remission +
  runif(nrow(remission_data), -0.03, 0.03)

base_scatter <- ggplot(remission_data, aes(sitzungen, y_jit)) +
  geom_point(alpha = 0.45, size = 2.4, colour = col_data) +
  scale_y_continuous("Remission",
                     breaks = c(0, 0.5, 1),
                     labels = c("0 = nein", "0.5", "1 = ja")) +
  scale_x_continuous("Anzahl Therapiesitzungen", breaks = seq(0, 40, 10)) +
  theme_lecture

save_fig <- function(plot, file, w = 10, h = 5.6) {
  ggsave(file.path("figures", file), plot, width = w, height = h,
         dpi = 200, bg = "white")
}

# 1) Rohdaten (Klicker-Folie) -----------------------------------------------
save_fig(base_scatter + coord_cartesian(ylim = c(-0.15, 1.2)),
         "fig1_daten.png")

# 2) Lineare Regression auf 0/1 ---------------------------------------------
fit_lm <- lm(remission ~ sitzungen, data = remission_data)
p_lpm <- base_scatter +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 1, ymax = Inf,
           fill = col_lm, alpha = 0.10) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 0,
           fill = col_lm, alpha = 0.10) +
  geom_abline(intercept = coef(fit_lm)[1], slope = coef(fit_lm)[2],
              colour = col_lm, linewidth = 1.5) +
  coord_cartesian(ylim = c(-0.15, 1.2), xlim = c(0, 41))
save_fig(p_lpm, "fig2_lineare_regression.png")

# 2b) Aufbau-Ebenen für Folie 4 (gleiche Leinwand, Achsen und Grenzen wie fig2)
# Jede Ebene ist das vollständige Bild bis zu diesem Schritt, damit die
# Ebenen im .r-stack deckungsgleich übereinanderliegen.
lm_numbers <- read.csv("output/lm_numbers.csv")
lm_val <- function(key) lm_numbers$value[lm_numbers$quantity == key]
pred_marks <- data.frame(
  x = c(lm_val("x_min_obs"), lm_val("x_max_obs")),
  y = c(lm_val("lm_pred_min_x"), lm_val("lm_pred_max_x"))
)
pred_marks$label <- sub("-", "−", formatC(pred_marks$y, format = "f",
                                               digits = 2))
lim_lpm <- coord_cartesian(ylim = c(-0.15, 1.2), xlim = c(0, 41))
shade_lpm <- list(
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 1, ymax = Inf,
           fill = col_lm, alpha = 0.10),
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 0,
           fill = col_lm, alpha = 0.10)
)
line_lpm <- geom_abline(intercept = coef(fit_lm)[1], slope = coef(fit_lm)[2],
                        colour = col_lm, linewidth = 1.5)
marks_lpm <- list(
  geom_point(data = pred_marks, aes(x, y), inherit.aes = FALSE,
             shape = 21, size = 5, stroke = 1.8, fill = "white",
             colour = col_lm),
  geom_label(data = pred_marks, aes(x, y, label = label), inherit.aes = FALSE,
             hjust = c(0, 1), nudge_x = c(1.2, -1.2), size = 7,
             fontface = "bold", colour = col_lm, fill = "white",
             linewidth = 0)
)
save_fig(base_scatter + lim_lpm, "fig2_0_punkte.png")
save_fig(base_scatter + line_lpm + lim_lpm, "fig2_1_gerade.png")
save_fig(base_scatter + shade_lpm + line_lpm + lim_lpm, "fig2_2_bereiche.png")
save_fig(base_scatter + shade_lpm + line_lpm + marks_lpm + lim_lpm,
         "fig2_3_vorhersagen.png")

# 3) Zwei Modelle nebeneinander ---------------------------------------------
# Links: illustrative stetige AV (Symptomwert) ~ N(mu_i, sigma^2)
# Rechts: binäre AV (Remission) ~ B(1, pi_i), pi_i aus dem Simulationsmodell
x_show <- c(5, 20, 35)
mu_fun <- function(x) 30 - 0.5 * x      # illustrativ: Symptomwert
sigma  <- 5
scale_w <- 25                            # horizontale Skalierung der Dichten

dens <- do.call(rbind, lapply(x_show, function(x0) {
  y <- seq(mu_fun(x0) - 3 * sigma, mu_fun(x0) + 3 * sigma, length.out = 200)
  data.frame(x0 = x0, y = y, x = x0 - dnorm(y, mu_fun(x0), sigma) * scale_w)
}))
p_norm <- ggplot() +
  geom_abline(intercept = 30, slope = -0.5, colour = col_lm, linewidth = 1.3) +
  geom_path(data = dens, aes(x, y, group = x0), colour = col_lm,
            linewidth = 1) +
  geom_segment(data = data.frame(x0 = x_show),
               aes(x = x0, xend = x0, y = -5, yend = 45),
               colour = "grey70", linetype = "dashed") +
  scale_x_continuous("Anzahl Therapiesitzungen", breaks = seq(0, 40, 10),
                     limits = c(-2, 42)) +
  scale_y_continuous("Symptomwert", limits = c(-5, 45)) +
  theme_lecture

pi_show <- plogis(beta0 + beta1 * x_show)
bars <- data.frame(
  x0 = rep(x_show, each = 2),
  y  = rep(c(0, 1), times = length(x_show)),
  p  = as.vector(rbind(1 - pi_show, pi_show))
)
bar_w <- 8
p_bern_bars <- ggplot(bars) +
  geom_segment(data = data.frame(x0 = x_show),
               aes(x = x0, xend = x0, y = -0.2, yend = 1.2),
               colour = "grey70", linetype = "dashed") +
  geom_rect(aes(xmin = x0 - p * bar_w, xmax = x0, ymin = y - 0.07,
                ymax = y + 0.07), fill = col_logit, alpha = 0.85) +
  scale_x_continuous("Anzahl Therapiesitzungen", breaks = seq(0, 40, 10),
                     limits = c(-2, 42)) +
  scale_y_continuous("Remission", breaks = c(0, 0.5, 1),
                     labels = c("0 = nein", "0.5", "1 = ja"),
                     limits = c(-0.2, 1.2)) +
  theme_lecture
p_bern <- p_bern_bars +
  geom_point(data = data.frame(x0 = x_show, pi = pi_show),
             aes(x0, pi), shape = 21, size = 4, fill = "white",
             colour = col_logit, stroke = 1.5)
save_fig(p_norm + p_bern, "fig3_modelle.png", w = 14, h = 4.8)
save_fig(p_norm, "fig3a_normal.png", w = 7, h = 4.4)
# Folie 5 baut rechts in zwei Ebenen auf: erst die Balken, dann die Kreise (pi)
save_fig(p_bern_bars, "fig3b_1_balken.png", w = 7, h = 4.4)
save_fig(p_bern, "fig3b_binomial.png", w = 7, h = 4.4)

# 4) S-Kurve und Offline-Fallback für den Slider -----------------------------
curve_df <- function(b0, b1, label) {
  x <- seq(0, 41, length.out = 300)
  data.frame(x = x, pi = plogis(b0 + b1 * x), modell = label)
}
lab_ref <- sprintf("β₀ = %s, β₁ = %s", beta0, beta1)
lab_dbl <- sprintf("β₀ = %s, β₁ = %s", beta0, 2 * beta1)

p_s <- base_scatter +
  geom_line(data = curve_df(beta0, beta1, lab_ref),
            aes(x, pi), colour = col_logit, linewidth = 1.6) +
  coord_cartesian(ylim = c(-0.15, 1.2), xlim = c(0, 41))
save_fig(p_s, "fig4_s_kurve.png")

p_s2 <- base_scatter +
  geom_line(data = rbind(curve_df(beta0, beta1, lab_ref),
                         curve_df(beta0, 2 * beta1, lab_dbl)),
            aes(x, pi, colour = modell, linetype = modell), linewidth = 1.6) +
  scale_colour_manual(NULL, values = setNames(c(col_logit, col_alt),
                                              c(lab_ref, lab_dbl))) +
  scale_linetype_manual(NULL, values = setNames(c("solid", "longdash"),
                                                c(lab_ref, lab_dbl))) +
  coord_cartesian(ylim = c(-0.15, 1.2), xlim = c(0, 41)) +
  theme(legend.position = "top")
save_fig(p_s2, "fig5_s_kurve_beta1_doppelt.png")

# 5) Zahlenstrahlen für die Leiter auf Folie 6 ---------------------------------
# Gleiche Leinwand und gleicher Ausschnitt für alle drei Zeilen, damit 0 und 1
# untereinander liegen und man sieht, wie der Wertebereich wächst.
zs_lim <- c(-4, 6)
zahlenstrahl <- function(from, to) {
  lo <- max(from, zs_lim[1] + 0.15)
  hi <- min(to, zs_lim[2] - 0.15)
  ends <- if (is.finite(from) && is.finite(to)) NULL else
    arrow(length = unit(0.18, "inches"), type = "closed",
          ends = if (is.finite(from)) "last" else if (is.finite(to)) "first"
                 else "both")
  finite_ends <- c(from, to)[is.finite(c(from, to))]
  ggplot() +
    annotate("segment", x = zs_lim[1], xend = zs_lim[2], y = 0, yend = 0,
             colour = "grey85", linewidth = 1) +
    annotate("segment", x = lo, xend = hi, y = 0, yend = 0,
             colour = col_logit, linewidth = 3.2, arrow = ends,
             linejoin = "mitre") +
    annotate("point", x = finite_ends, y = rep(0, length(finite_ends)),
             colour = col_logit, size = 4.5) +
    annotate("segment", x = c(0, 1), xend = c(0, 1), y = -0.35, yend = 0.35,
             colour = "grey30", linewidth = 0.9) +
    annotate("text", x = c(0, 1), y = -0.95, label = c("0", "1"), size = 7,
             colour = "grey30") +
    coord_cartesian(xlim = zs_lim, ylim = c(-1.35, 0.6), expand = FALSE) +
    theme_void()
}
save_fig(zahlenstrahl(0, 1), "zahlenstrahl_1_pi.png", w = 4, h = 0.8)
save_fig(zahlenstrahl(0, Inf), "zahlenstrahl_2_odds.png", w = 4, h = 0.8)
save_fig(zahlenstrahl(-Inf, Inf), "zahlenstrahl_3_logodds.png", w = 4, h = 0.8)

cat("Abbildungen gespeichert in figures/:\n")
print(list.files("figures"))
