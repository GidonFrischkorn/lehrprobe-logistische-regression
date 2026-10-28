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
  scale_y_continuous("Symptomwert", breaks = seq(0, 40, 20), limits = c(-5, 45)) +
  theme_lecture

pi_show <- plogis(beta0 + beta1 * x_show)
bars <- data.frame(
  x0 = rep(x_show, each = 2),
  y  = rep(c(0, 1), times = length(x_show)),
  p  = as.vector(rbind(1 - pi_show, pi_show))
)
# Balkenlänge = Wahrscheinlichkeit * bar_w. Der längste Balken (p ≈ .90 bei 5
# Sitzungen) muss links von x = 5 noch ins Bild passen: 5 - .90 * 7 ≈ -1.3.
bar_w <- 7
p_bern_bars <- ggplot(bars) +
  geom_segment(data = data.frame(x0 = x_show),
               aes(x = x0, xend = x0, y = -0.2, yend = 1.2),
               colour = "grey70", linetype = "dashed") +
  geom_rect(aes(xmin = x0 - p * bar_w, xmax = x0, ymin = y - 0.07,
                ymax = y + 0.07), fill = col_logit, alpha = 0.85) +
  scale_x_continuous("Anzahl Therapiesitzungen", breaks = seq(0, 40, 10)) +
  scale_y_continuous("Remission", breaks = c(0, 0.5, 1),
                     labels = c("0 = nein", "0.5", "1 = ja")) +
  # Grenzen über coord_cartesian(): scale-limits würden Balken, die über den
  # Rand reichen, ohne Fehlermeldung ganz weglassen (so fehlte der Balken bei 5).
  coord_cartesian(xlim = c(-2, 42), ylim = c(-0.2, 1.2)) +
  theme_lecture
p_bern <- p_bern_bars +
  geom_point(data = data.frame(x0 = x_show, pi = pi_show),
             aes(x0, pi), shape = 21, size = 4, fill = "white",
             colour = col_logit, stroke = 1.5)
save_fig(p_norm + p_bern, "fig3_modelle.png", w = 14, h = 4.8)
# Folie 6 zeigt die Einzelbilder je in einer halben Spalte: kleiner gespeichert
# und mit grösserer Schrift, damit Achsen und Beschriftungen lesbar bleiben.
gross <- theme(text = element_text(size = 24))
save_fig(p_norm + gross, "fig3a_normal.png", w = 6.4, h = 2.8)
# Folie 6 baut rechts in zwei Ebenen auf: erst die Balken, dann die Kreise (pi)
save_fig(p_bern_bars + gross, "fig3b_1_balken.png", w = 6.4, h = 2.8)
save_fig(p_bern + gross, "fig3b_binomial.png", w = 6.4, h = 2.8)

# 4) S-Kurve und Offline-Fallback für den Slider -----------------------------
curve_df <- function(b0, b1, label) {
  x <- seq(0, 41, length.out = 300)
  data.frame(x = x, pi = plogis(b0 + b1 * x), modell = label)
}
lab_ref <- sprintf("β₀ = %s, β₁ = %s", beta0, beta1)
lab_dbl <- sprintf("β₀ = %s, β₁ = %s", beta0, 2 * beta1)

# Folie 8 baut in zwei Ebenen auf: erst nur die Kurve, am Ende des Rechenbeispiels
# der Punkt pi = .5 bei x = -beta0/beta1 (Log-Odds 0, Odds 1) mit den gestrichelten
# Linien, in Grün wie der Marker im Slider auf Folie 9.
x_mid <- -beta0 / beta1
p_s <- base_scatter +
  geom_line(data = curve_df(beta0, beta1, lab_ref),
            aes(x, pi), colour = col_logit, linewidth = 1.6) +
  coord_cartesian(ylim = c(-0.15, 1.2), xlim = c(0, 41))
save_fig(p_s, "fig4_1_s_kurve.png")

p_s_punkt <- p_s +
  annotate("segment", x = x_mid, xend = x_mid, y = -0.15, yend = 0.5,
           colour = col_alt, linetype = "dashed", linewidth = 0.8) +
  annotate("segment", x = 0, xend = x_mid, y = 0.5, yend = 0.5,
           colour = col_alt, linetype = "dashed", linewidth = 0.8) +
  annotate("point", x = x_mid, y = 0.5, colour = col_alt, size = 4.5)
save_fig(p_s_punkt, "fig4_2_s_kurve_punkt.png")

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
# Drei Ankerwerte wandern mit, jeder in seiner Farbe: pi = 0, .5 und 1 und ihre
# Werte auf der Odds- und der Log-Odds-Skala. Grün für pi = .5 wie auf dem Slider.
# pi = .8 bewusst nicht: Odds 4 ist die Antwort der Denkpause auf derselben Folie.
# Ausschnitt symmetrisch um 0, damit 0 in der Mitte des Zahlenstrahls liegt.
zs_lim <- c(-3.5, 3.5)
anker <- data.frame(
  pi  = c(0, 0.5, 1),
  col = c("#E69F00", "#009E73", "#CC79A7")   # Okabe-Ito: orange, grün, lila
)
anker$odds     <- anker$pi / (1 - anker$pi)   # 0, 1, Inf
anker$log_odds <- log(anker$odds)             # -Inf, 0, Inf
fmt_anker <- function(v) {
  out <- ifelse(is.infinite(v), ifelse(v > 0, "∞", "−∞"),
                sub("^0\\.", ".", as.character(v)))
  ifelse(v == 0, "0", out)
}
zahlenstrahl <- function(from, to, werte) {
  lo <- max(from, zs_lim[1] + 0.15)
  hi <- min(to, zs_lim[2] - 0.15)
  ends <- if (is.finite(from) && is.finite(to)) NULL else
    arrow(length = unit(0.18, "inches"), type = "closed",
          ends = if (is.finite(from)) "last" else if (is.finite(to)) "first"
                 else "both")
  # Unendliche Ankerwerte sitzen an der Pfeilspitze
  x_anker <- pmin(pmax(werte, zs_lim[1] + 0.35), zs_lim[2] - 0.35)
  ggplot() +
    annotate("segment", x = zs_lim[1], xend = zs_lim[2], y = 0, yend = 0,
             colour = "grey85", linewidth = 1) +
    # geom_segment statt annotate: annotate(..., arrow = NULL) erzeugt in
    # ggplot2 4.0 einen leeren Layer, der Balken von pi fehlte dann stillschweigend
    geom_segment(data = data.frame(x = lo, xend = hi),
                 aes(x = x, xend = xend, y = 0, yend = 0),
                 colour = col_logit, linewidth = 3.2, arrow = ends,
                 linejoin = "mitre") +
    # Ankerwerte als farbige Striche quer zur Linie (Punkte würden den kurzen
    # blauen Bereich von pi ganz verdecken)
    annotate("segment", x = x_anker, xend = x_anker, y = -0.45, yend = 0.45,
             colour = anker$col, linewidth = 2.4) +
    annotate("text", x = x_anker, y = -0.95, label = fmt_anker(werte),
             size = 7.5, fontface = "bold", colour = anker$col) +
    coord_cartesian(xlim = zs_lim, ylim = c(-1.35, 0.6), expand = FALSE) +
    theme_void()
}
save_fig(zahlenstrahl(0, 1, anker$pi), "zahlenstrahl_1_pi.png", w = 4, h = 0.8)
save_fig(zahlenstrahl(0, Inf, anker$odds), "zahlenstrahl_2_odds.png",
         w = 4, h = 0.8)
save_fig(zahlenstrahl(-Inf, Inf, anker$log_odds), "zahlenstrahl_3_logodds.png",
         w = 4, h = 0.8)

# 6) Abbildungen für die Backup-Folien (G 13 bis 19) ---------------------------
# Zahlen aus output/backup_numbers.csv (R/02_compute_numbers.R, Abschnitt f).
bn   <- read.csv("output/backup_numbers.csv")
bval <- function(key) bn$value[bn$quantity == key]
x_grid <- seq(0, 41, length.out = 300)
fmt_pi <- function(x) sub("^0", "", formatC(x, format = "f", digits = 2))

# G 13: dasselbe Modell auf drei Skalen (Analogon zu Eid Abb. 22.5 bis 22.7).
# Log-Odds: Gerade (+ beta1 pro Sitzung); Odds: Exponentialkurve (mal e^beta1);
# pi: S-Kurve. Drei Stellen markiert: x = 20, 21, 30.
x_or   <- c(bval("x_or_1"), bval("x_or_2"), bval("x_or_3"))
skalen <- data.frame(x = x_grid, lo = beta0 + beta1 * x_grid)
skalen$odds <- exp(skalen$lo)
skalen$pi   <- plogis(skalen$lo)
marks <- data.frame(x = x_or, lo = beta0 + beta1 * x_or)
marks$odds <- exp(marks$lo)
marks$pi   <- plogis(marks$lo)
panel <- function(y, ylab, titel, lab_fun) {
  ggplot(skalen, aes(x, .data[[y]])) +
    geom_line(colour = col_logit, linewidth = 1.6) +
    geom_point(data = marks, colour = col_alt, size = 4) +
    # x = 21 rechts beschriften, x = 20 und 30 links, sonst überlappen 20 und 21
    geom_text(data = marks, aes(label = lab_fun(.data[[y]]),
                                x = x + ifelse(x == 21, 1.2, -1.2),
                                hjust = ifelse(x == 21, 0, 1)),
              colour = col_alt, fontface = "bold", size = 6.5) +
    scale_x_continuous("Anzahl Therapiesitzungen", breaks = seq(0, 40, 10)) +
    labs(y = ylab, title = titel) +
    coord_cartesian(xlim = c(0, 41)) +
    theme_lecture +
    theme(plot.title = element_text(size = 20))
}
p_lo <- panel("lo", "Log-Odds",
              sprintf("Log-Odds: +%s pro Sitzung", beta1),
              function(v) formatC(v, format = "f", digits = 2)) +
  scale_y_continuous(breaks = seq(-3, 3, 1))
p_od <- panel("odds", "Odds",
              sprintf("Odds: × %s pro Sitzung", formatC(bval("or_1"), format = "f", digits = 2)),
              function(v) formatC(v, format = "f", digits = 2)) +
  coord_cartesian(xlim = c(0, 41), ylim = c(0, 12))
p_pi <- panel("pi", "Wahrscheinlichkeit π", "π: kein fester Schritt", fmt_pi) +
  scale_y_continuous(breaks = c(0, 0.5, 1), limits = c(0, 1))
save_fig(p_lo + p_od + p_pi, "fig6_drei_skalen.png", w = 15, h = 4.6)

# G 14: gleicher Schritt in pi, ungleich viele Sitzungen. Zwei Fenster auf der
# wahren S-Kurve: .50 -> .60 (am Wendepunkt) und .90 -> .95 (am Rand).
xs <- c(bval("x_50"), bval("x_60"), bval("x_90"), bval("x_95"))
fenster <- data.frame(
  xmin = xs[c(1, 3)], xmax = xs[c(2, 4)], ymin = c(0.50, 0.90), ymax = c(0.60, 0.95),
  lab = sprintf("%s Sitzungen", formatC(c(bval("dx_50_60"), bval("dx_90_95")),
                                        format = "f", digits = 1)),
  pi_lab = c("π: .50 → .60", "π: .90 → .95"),
  y_lab  = c(0.55, 0.84)   # oberes Label unter die .90-Linie, sonst auf den Linien
)
p_schritt <- ggplot(skalen, aes(x, pi)) +
  geom_rect(data = fenster, inherit.aes = FALSE,
            aes(xmin = xmin, xmax = xmax, ymin = -0.05, ymax = ymax),
            fill = col_alt, alpha = 0.22) +
  geom_segment(data = fenster, inherit.aes = FALSE,
               aes(x = 0, xend = xmax, y = ymin, yend = ymin),
               colour = col_alt, linetype = "dashed", linewidth = 0.7) +
  geom_segment(data = fenster, inherit.aes = FALSE,
               aes(x = 0, xend = xmax, y = ymax, yend = ymax),
               colour = col_alt, linetype = "dashed", linewidth = 0.7) +
  geom_line(colour = col_logit, linewidth = 1.6) +
  geom_text(data = fenster, inherit.aes = FALSE,
            aes(x = (xmin + xmax) / 2, y = 0.02, label = lab),
            colour = col_alt, fontface = "bold", size = 6, vjust = 0) +
  geom_text(data = fenster, inherit.aes = FALSE,
            aes(x = 1, y = y_lab, label = pi_lab),
            colour = col_alt, fontface = "bold", size = 6, hjust = 0, vjust = 0.5) +
  scale_x_continuous("Anzahl Therapiesitzungen", breaks = seq(0, 40, 10)) +
  scale_y_continuous("Wahrscheinlichkeit π", breaks = c(0, 0.5, 0.6, 0.9, 0.95, 1),
                     labels = c("0", ".50", ".60", ".90", ".95", "1")) +
  coord_cartesian(xlim = c(0, 41), ylim = c(-0.05, 1.02)) +
  theme_lecture
save_fig(p_schritt, "fig7_gleicher_schritt.png")

# G 16: bedingte Varianz pi(1 - pi) über x, maximal .25 am Wendepunkt
# (Analogon zu Eid Abb. 22.4).
varianz <- rbind(
  data.frame(x = x_grid, y = skalen$pi, was = "π (Wahrscheinlichkeit)"),
  data.frame(x = x_grid, y = skalen$pi * (1 - skalen$pi), was = "π (1 − π) (Varianz)")
)
p_var <- ggplot(varianz, aes(x, y, colour = was)) +
  geom_line(linewidth = 1.6) +
  annotate("segment", x = x_mid, xend = x_mid, y = 0, yend = 0.25,
           colour = col_lm, linetype = "dashed", linewidth = 0.8) +
  annotate("text", x = x_mid + 1, y = 0.29, label = "max .25 bei π = .5",
           colour = col_lm, fontface = "bold", size = 6, hjust = 0) +
  scale_colour_manual(NULL, values = setNames(c(col_logit, col_lm),
                                              unique(varianz$was))) +
  scale_x_continuous("Anzahl Therapiesitzungen", breaks = seq(0, 40, 10)) +
  scale_y_continuous(NULL, breaks = c(0, 0.25, 0.5, 1),
                     labels = c("0", ".25", ".5", "1")) +
  coord_cartesian(xlim = c(0, 41), ylim = c(0, 1)) +
  theme_lecture +
  theme(legend.position = "top")
save_fig(p_var, "fig8_varianz.png")

# G 18: Logit und Probit, beide an die Daten angepasst (glm, zwei Link-Funktionen)
glm_numbers <- read.csv("output/glm_numbers.csv")
gval <- function(key) glm_numbers$value[glm_numbers$quantity == key]
links <- rbind(
  data.frame(x = x_grid, link = "Logit (glm, family = binomial)",
             pi = plogis(gval("glm_intercept") + gval("glm_slope") * x_grid)),
  data.frame(x = x_grid, link = "Probit (link = \"probit\")",
             pi = pnorm(bval("probit_b0") + bval("probit_b1") * x_grid))
)
p_link <- base_scatter +
  geom_line(data = links, aes(x, pi, colour = link, linetype = link),
            linewidth = 1.6) +
  scale_colour_manual(NULL, values = setNames(c(col_logit, col_alt),
                                              unique(links$link))) +
  scale_linetype_manual(NULL, values = setNames(c("solid", "longdash"),
                                                unique(links$link))) +
  coord_cartesian(ylim = c(-0.15, 1.2), xlim = c(0, 41)) +
  theme(legend.position = "top")
save_fig(p_link, "fig9_logit_probit.png")

# G 19: perfekte Trennung. Konstruierte Daten wie in 02 (0 unter sep_cut, 1 ab
# sep_cut); die glm()-Kurve ist fast senkrecht, die Koeffizienten laufen weg.
sep_cut <- bval("sep_cut")
sep_df <- data.frame(sitzungen = remission_data$sitzungen)
sep_df$remission <- as.integer(sep_df$sitzungen >= sep_cut)
sep_df$y_jit <- sep_df$remission + runif(nrow(sep_df), -0.03, 0.03)
sep_curve <- data.frame(x = x_grid,
                        pi = plogis(bval("sep_b0") + bval("sep_b1") * x_grid))
p_sep <- ggplot(sep_df, aes(sitzungen, y_jit)) +
  annotate("rect", xmin = -Inf, xmax = sep_cut - 0.5, ymin = -Inf, ymax = Inf,
           fill = col_lm, alpha = 0.08) +
  geom_point(alpha = 0.45, size = 2.4, colour = col_data) +
  geom_line(data = sep_curve, aes(x, pi), colour = col_logit, linewidth = 1.6) +
  annotate("text", x = sep_cut - 1.5, y = 0.75, label = "alle 0", hjust = 1,
           colour = col_lm, fontface = "bold", size = 7) +
  annotate("text", x = sep_cut + 0.5, y = 0.25, label = "alle 1", hjust = 0,
           colour = col_lm, fontface = "bold", size = 7) +
  scale_y_continuous("Remission", breaks = c(0, 0.5, 1),
                     labels = c("0 = nein", "0.5", "1 = ja")) +
  scale_x_continuous("Anzahl Therapiesitzungen", breaks = seq(0, 40, 10)) +
  coord_cartesian(ylim = c(-0.15, 1.2), xlim = c(0, 41)) +
  theme_lecture
save_fig(p_sep, "fig10_trennung.png")

cat("Abbildungen gespeichert in figures/:\n")
print(list.files("figures"))
