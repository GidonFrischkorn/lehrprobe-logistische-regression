# 02_compute_numbers.R
# Berechnet alle Zahlen, die auf den Folien erscheinen:
#   (a) Vorhersagen der linearen Regression (lineares Wahrscheinlichkeitsmodell)
#   (b) Wahrscheinlichkeit -> Odds -> Log-Odds für ausgewählte Werte von pi
#   (c) Rücktransformation Log-Odds -> Odds -> Wahrscheinlichkeit (Kontrolle)
#   (d) logistische Regression mit glm() (Ausblick: nächster Abschnitt)

remission_data <- read.csv("data/remission.csv")
sim_params     <- read.csv("output/sim_params.csv")
beta0 <- sim_params$value[sim_params$parameter == "beta0"]
beta1 <- sim_params$value[sim_params$parameter == "beta1"]

# (a) Lineare Regression auf die 0/1-Variable --------------------------------
fit_lm <- lm(remission ~ sitzungen, data = remission_data)
x_range <- range(remission_data$sitzungen)
lm_pred <- data.frame(sitzungen = x_range)
lm_pred$vorhersage <- predict(fit_lm, newdata = lm_pred)
n_outside <- sum(fitted(fit_lm) < 0 | fitted(fit_lm) > 1)

lm_numbers <- data.frame(
  quantity = c("lm_intercept", "lm_slope",
               "lm_pred_min_x", "lm_pred_max_x",
               "x_min_obs", "x_max_obs",
               "n_fitted_outside_01", "n_total"),
  value = c(coef(fit_lm)[1], coef(fit_lm)[2],
            lm_pred$vorhersage[1], lm_pred$vorhersage[2],
            x_range[1], x_range[2],
            n_outside, nrow(remission_data))
)
write.csv(lm_numbers, "output/lm_numbers.csv", row.names = FALSE)
print(lm_numbers)

# (b) Transformationstabelle -------------------------------------------------
pi_values <- c(0.05, 0.20, 0.50, 0.80, 0.95)
transform_table <- data.frame(
  pi       = pi_values,
  odds     = pi_values / (1 - pi_values),
  log_odds = log(pi_values / (1 - pi_values))
)
# Kontrolle: qlogis() ist die Logit-Funktion in R
stopifnot(isTRUE(all.equal(transform_table$log_odds, qlogis(pi_values))))
write.csv(transform_table, "output/transform_table.csv", row.names = FALSE)
print(transform_table)

# (c) Rücktransformation (Kontrolle der Formeln auf den Folien) -------------
back_odds <- exp(transform_table$log_odds)
back_pi   <- back_odds / (1 + back_odds)                 # odds / (1 + odds)
back_pi2  <- 1 / (1 + exp(-transform_table$log_odds))    # inverse Logit
stopifnot(isTRUE(all.equal(back_pi, pi_values)),
          isTRUE(all.equal(back_pi2, pi_values)),
          isTRUE(all.equal(back_pi, plogis(transform_table$log_odds))))
cat("Rücktransformation stimmt für alle pi-Werte.\n")

# Wahre Kurve: Wendepunkt (pi = .5) und Werte an den Rändern
true_curve <- data.frame(
  quantity = c("x_at_pi_50", "pi_true_at_x_min", "pi_true_at_x_max",
               "x_at_pi_50_beta1_doubled"),
  value = c(-beta0 / beta1,
            plogis(beta0 + beta1 * x_range[1]),
            plogis(beta0 + beta1 * x_range[2]),
            -beta0 / (2 * beta1))
)
write.csv(true_curve, "output/true_curve.csv", row.names = FALSE)
print(true_curve)

# (d) Logistische Regression (Ausblick) --------------------------------------
fit_glm <- glm(remission ~ sitzungen, family = binomial, data = remission_data)
glm_numbers <- data.frame(
  quantity = c("glm_intercept", "glm_slope"),
  value    = unname(coef(fit_glm))
)
write.csv(glm_numbers, "output/glm_numbers.csv", row.names = FALSE)
print(summary(fit_glm)$coefficients)

# (e) Zahlen für die Klicker-Items (Selbsttest am Ende) ----------------------
# Der Selbsttest verwendet einen Wert, der auf den Folien nicht vorkommt,
# damit die Aussage gerechnet und nicht erinnert wird.
pi_check <- 0.75
klicker_numbers <- data.frame(
  quantity = c("pi_check", "odds_check", "log_odds_check"),
  value    = c(pi_check, pi_check / (1 - pi_check), qlogis(pi_check))
)
stopifnot(isTRUE(all.equal(klicker_numbers$value[2], 3)))
write.csv(klicker_numbers, "output/klicker_numbers.csv", row.names = FALSE)
print(klicker_numbers)

# (f) Zahlen für die Backup-Folien (G 13 bis 19) ------------------------------
# Seitenangaben beziehen sich auf Eid, Gollwitzer & Schmitt (2017), Kap. 22.

# G 13, Odds Ratio: e^beta1 wirkt multiplikativ auf die Odds (Eid S. 804).
# Drei Stellen der wahren Kurve: x = 20 (pi = .5), 21 (eine Sitzung mehr), 30.
x_or   <- c(20, 21, 30)
lo_or  <- beta0 + beta1 * x_or
or_1   <- exp(beta1)          # Faktor pro Sitzung
or_10  <- exp(10 * beta1)     # Faktor für 10 Sitzungen
stopifnot(isTRUE(all.equal(or_10, or_1^10)),
          isTRUE(all.equal(exp(lo_or[3]) / exp(lo_or[1]), or_10)))

# G 14, gleicher Schritt, ungleiche Wirkung: .50 -> .60 und .90 -> .95 auf der
# Log-Odds-Skala und in Sitzungen der wahren Kurve (Eid S. 801, Punkt 4).
pi_step <- c(0.50, 0.60, 0.90, 0.95)
lo_step <- qlogis(pi_step)
stopifnot(isTRUE(all.equal(plogis(lo_step), pi_step)))
x_step  <- (lo_step - beta0) / beta1         # Sitzungen, bei denen pi erreicht wird
d_lo_50_60 <- lo_step[2] - lo_step[1]
d_lo_90_95 <- lo_step[4] - lo_step[3]
dx_50_60   <- d_lo_50_60 / beta1
dx_90_95   <- d_lo_90_95 / beta1
stopifnot(isTRUE(all.equal(dx_50_60, x_step[2] - x_step[1])),
          isTRUE(all.equal(dx_90_95, x_step[4] - x_step[3])))

# G 16, Varianz einer 0/1-Variable: pi * (1 - pi), maximal .25 bei pi = .5 (F 22.2)
pi_var <- c(0.50, 0.80, 0.95)

# G 15, Pseudo-R² am glm-Fit: McFadden, Cox-Snell, Nagelkerke (F 22.18 bis F 22.21).
# L_S = 1 beim saturierten Modell, deshalb McFadden = 1 - ln(L_M) / ln(L_0).
fit_null <- glm(remission ~ 1, family = binomial, data = remission_data)
ll_m <- as.numeric(logLik(fit_glm))
ll_0 <- as.numeric(logLik(fit_null))
n_obs <- nrow(remission_data)
r2_mcfadden   <- 1 - ll_m / ll_0
r2_coxsnell   <- 1 - exp(2 * (ll_0 - ll_m) / n_obs)      # 1 - (L_0 / L_M)^(2/n)
r2_nagelkerke <- r2_coxsnell / (1 - exp(2 * ll_0 / n_obs))
# Kontrolle über die Devianzen: McFadden = 1 - Devianz / Null-Devianz
stopifnot(isTRUE(all.equal(r2_mcfadden,
                           1 - fit_glm$deviance / fit_glm$null.deviance)),
          r2_coxsnell < r2_nagelkerke, r2_nagelkerke < 1)
hit_rate <- mean((fitted(fit_glm) > 0.5) == remission_data$remission)
tjur     <- mean(fitted(fit_glm)[remission_data$remission == 1]) -
            mean(fitted(fit_glm)[remission_data$remission == 0])
r2_lm    <- summary(fit_lm)$r.squared                    # zum Vergleich: lm() auf 0/1

# G 18, Probit als Alternative zum Logit (nicht bei Eid; Materialseite)
fit_probit <- glm(remission ~ sitzungen, family = binomial(link = "probit"),
                  data = remission_data)

# G 19, perfekte Trennung (Eid S. 804 f.): konstruierte Daten mit denselben
# Sitzungszahlen, 0 unter 20 Sitzungen, 1 ab 20. glm() warnt, die Schätzung läuft weg.
sep_cut  <- 20
sep_data <- data.frame(sitzungen = remission_data$sitzungen)
sep_data$remission <- as.integer(sep_data$sitzungen >= sep_cut)
sep_warn <- character(0)
fit_sep <- withCallingHandlers(
  glm(remission ~ sitzungen, family = binomial, data = sep_data),
  warning = function(w) {
    sep_warn <<- c(sep_warn, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
stopifnot(length(sep_warn) > 0)
writeLines(sep_warn, "output/separation_warning.txt")

backup_numbers <- data.frame(
  quantity = c("or_1", "or_10",
               "x_or_1", "x_or_2", "x_or_3",
               "lo_or_1", "lo_or_2", "lo_or_3",
               "odds_or_1", "odds_or_2", "odds_or_3",
               "pi_or_1", "pi_or_2", "pi_or_3",
               "lo_50", "lo_60", "lo_90", "lo_95",
               "odds_50", "odds_60", "odds_90", "odds_95",
               "x_50", "x_60", "x_90", "x_95",
               "d_lo_50_60", "d_lo_90_95", "dx_50_60", "dx_90_95",
               "var_50", "var_80", "var_95",
               "r2_mcfadden", "r2_coxsnell", "r2_nagelkerke",
               "hit_rate", "tjur", "r2_lm",
               "probit_b0", "probit_b1",
               "sep_cut", "sep_b0", "sep_b1"),
  value = c(or_1, or_10,
            x_or, lo_or, exp(lo_or), plogis(lo_or),
            lo_step, pi_step / (1 - pi_step), x_step,
            d_lo_50_60, d_lo_90_95, dx_50_60, dx_90_95,
            pi_var * (1 - pi_var),
            r2_mcfadden, r2_coxsnell, r2_nagelkerke, hit_rate, tjur, r2_lm,
            unname(coef(fit_probit)),
            sep_cut, unname(coef(fit_sep)))
)
write.csv(backup_numbers, "output/backup_numbers.csv", row.names = FALSE)
print(backup_numbers)
