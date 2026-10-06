# 01_simulate_data.R
# Simuliert das laufende Beispiel der Lehrprobe:
# Remission nach Psychotherapie (ja/nein) in Abhängigkeit der Anzahl Sitzungen.
#
# Datengenerierendes Modell (logistische Regression):
#   Y_i ~ B(1, pi_i)
#   logit(pi_i) = beta0 + beta1 * x_i
#
# Die Daten sind SIMULIERT und illustrieren das Modell. Sie sind keine
# Schätzung realer Remissionsraten.

set.seed(2910)

n_patients <- 200    # Anzahl Patient:innen
x_min      <- 1      # minimale Anzahl Sitzungen
x_max      <- 40     # maximale Anzahl Sitzungen
beta0      <- -3     # Log-Odds der Remission bei x = 0
beta1      <- 0.15   # Zunahme der Log-Odds pro zusätzlicher Sitzung

sessions <- sample(x_min:x_max, size = n_patients, replace = TRUE)
eta      <- beta0 + beta1 * sessions      # linearer Prädiktor (Log-Odds)
pi_true  <- 1 / (1 + exp(-eta))           # inverse Logit-Funktion
remission <- rbinom(n_patients, size = 1, prob = pi_true)

remission_data <- data.frame(
  id        = seq_len(n_patients),
  sitzungen = sessions,
  remission = remission
)

dir.create("data", showWarnings = FALSE)
write.csv(remission_data, "data/remission.csv", row.names = FALSE)

# Parameter für Folien und Begleitmaterial festhalten
sim_params <- data.frame(
  parameter = c("seed", "n_patients", "x_min", "x_max", "beta0", "beta1"),
  value     = c(2910, n_patients, x_min, x_max, beta0, beta1)
)
dir.create("output", showWarnings = FALSE)
write.csv(sim_params, "output/sim_params.csv", row.names = FALSE)

cat("N =", nrow(remission_data), "\n")
cat("Anteil Remission:", round(mean(remission_data$remission), 3), "\n")
print(table(remission_data$remission))
