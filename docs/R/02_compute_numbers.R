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
