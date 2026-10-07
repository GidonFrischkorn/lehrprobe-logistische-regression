# Lehrprobe: Logistische Regression

Material für einen zehnminütigen Ausschnitt aus einer BSc-Statistikvorlesung
(Psychologie) zur logistischen Regression. Der Ausschnitt setzt die lineare
Regression voraus und endet dort, wo die Interpretation der Koeffizienten als
Odds Ratios und die Schätzung mit `glm()` beginnen würden.

Folien und Begleitseite: <https://gidonfrischkorn.github.io/lehrprobe-logistische-regression/>

## Lernziel

Die Studierenden können erklären, warum für eine binäre abhängige Variable die
Normalverteilung durch die Verteilung $\mathcal{B}(1, \pi_i)$ ersetzt wird und
warum $\pi_i$ über Odds und Log-Odds mit dem linearen Prädiktor
$\beta_0 + \beta_1 x_i$ verknüpft wird.

Zwei Ideen tragen den Ausschnitt:

1. **Jedes statistische Modell ist eine Verteilungsannahme.** Lineare
   Regression: $Y_i \sim \mathcal{N}(\mu_i, \sigma^2)$ mit
   $\mu_i = \beta_0 + \beta_1 x_i$. Binäre abhängige Variable:
   $Y_i \sim \mathcal{B}(1, \pi_i)$. Die Struktur bleibt gleich, eine
   Verteilung und ein Parameter, der von $x$ abhängt.
2. **$\pi_i$ liegt zwischen 0 und 1, der lineare Prädiktor nicht.** Deshalb
   wird $\pi_i$ transformiert: Wahrscheinlichkeit (0 bis 1), Odds (0 bis ∞),
   Log-Odds (−∞ bis ∞). Das Modell lautet
   $\ln\!\left(\pi_i / (1 - \pi_i)\right) = \beta_0 + \beta_1 x_i$.

## Laufendes Beispiel

200 simulierte Patient:innen nach einer Psychotherapie. Die abhängige Variable
ist Remission (1 = ja, 0 = nein), der Prädiktor die Anzahl Therapiesitzungen
(1 bis 40). Die Daten folgen dem logistischen Modell mit $\beta_0 = -3$ und
$\beta_1 = 0.15$ (Seed 2910). Sie illustrieren das Modell und sind keine
Schätzung realer Remissionsraten.

## Aufbau der Folien

| Abschnitt | Inhalt | Aktivierung |
|---|---|---|
| Einordnung | Was vorher kam, was danach folgt, das Beispiel | |
| Intuition | Was passiert, wenn man eine lineare Regression auf 0/1 rechnet? | Abstimmung (KlickerUZH oder Handzeichen) |
| Auflösung | Die Gerade sagt Werte unter 0 und über 1 vorher | |
| Idee 1 | Normalverteilung vs. $\mathcal{B}(1, \pi_i)$, gleiche Struktur | Frage in den Raum |
| Idee 2 | Wertebereiche von Wahrscheinlichkeit, Odds, Log-Odds | Denkpause: $\pi = .8$, Odds? |
| Modell | Logit-Gleichung, Rücktransformation, S-Kurve | |
| Slider | Was passiert, wenn sich $\beta_1$ verdoppelt? | Vorhersage mit Nachbar:in, dann prüfen |
| Selbsttest | Vier Aussagen, richtig oder falsch (K-Prim) | Abstimmung (KlickerUZH oder Handzeichen); die Auflösung ist die Zusammenfassung |
| Mitnehmen | Die zwei Ideen, Ausblick auf `glm()` | |

Die Folien bauen sich schrittweise auf. Im Browser öffnet die Taste `s` die
Sprechernotizen mit dem Ablauf pro Folie. Der Slider auf der vorletzten Folie
ist in reinem JavaScript/SVG geschrieben und läuft ohne Internet; die Formeln
laden MathJax aus dem Netz.

## Struktur des Repositoriums

| Pfad | Inhalt |
|------|--------|
| `slides.qmd` | Folien (Quarto reveal.js); führt beim Rendern alle Skripte aus |
| `index.qmd` | Begleitseite: Einordnung, Modell, Notation, Daten |
| `explorer.qmd`, `explorer.js`, `explorer.css` | Explorer der logistischen Regression und Übungsfragen (exams2forms) |
| `uebungen/` | Übungsfragen als R/exams-Dateien (`.Rmd`), Lösungen werden in R berechnet |
| `webex/` | `webex.css`, `webex.js` aus exams2forms 0.2-2 (Tooltips übersetzt) |
| `r-code.qmd` | eigene Seite mit dem R-Code (Simulation, Zahlen, Abbildungen) und Aufgaben |
| `custom.scss` | Theme der Folien |
| `title-slide.html` | Titelfolie (Quarto-Partial) mit dem KlickerUZH-QR, wenn `klicker-join-url` in `slides.qmd` gesetzt ist |
| `R/01_simulate_data.R` | simuliert `data/remission.csv`, Parameter in `output/sim_params.csv` |
| `R/02_compute_numbers.R` | alle Zahlen der Folien (lineares Wahrscheinlichkeitsmodell, Transformationstabelle, `glm()`), inkl. Kontrolle der Transformationsformeln mit `stopifnot()` |
| `R/03_figures.R` | alle Abbildungen in `figures/`, inkl. der Aufbauschritte und der Offline-Fallbacks für den Slider |
| `data/remission.csv` | simulierte Daten (`id`, `sitzungen`, `remission`) |
| `output/*.csv` | berechnete Kennzahlen; jede Zahl auf den Folien wird von hier gelesen |
| `docs/` | gerenderte Website (GitHub Pages) |

Keine Zahl auf den Folien ist von Hand eingetragen: `slides.qmd` liest alle
Werte aus `output/`, die beim Rendern neu berechnet werden.

## Reproduzieren

```sh
Rscript R/01_simulate_data.R
Rscript R/02_compute_numbers.R
Rscript R/03_figures.R
quarto render
```

`quarto render` allein genügt ebenfalls, weil `slides.qmd` die drei Skripte
beim Rendern ausführt. Die Website wird nach `docs/` geschrieben
(GitHub Pages: Branch `main`, Ordner `/docs`).

Getestet mit R 4.6.1, ggplot2 4.0.3, patchwork 1.3.2, jsonlite 2.0.0,
knitr 1.52, qrcode 0.3.0, exams 2.4-4, exams2forms 0.2-2 und Quarto 1.10.18. Ohne das Paket `qrcode` wird
die letzte Folie ohne QR-Code gerendert. Der KlickerUZH-QR auf Titelfolie, Folie 4
und Folie 11 entsteht aus dem YAML-Feld `klicker-join-url` in `slides.qmd`; ist es
leer, zeigen die Folien einen Platzhalter.

## Selbst ausprobieren

- Verändern Sie in `R/01_simulate_data.R` den Wert von `beta1`. Wie verändert
  sich `figures/fig2_lineare_regression.png`? Ab welchem Wert liegen keine
  Vorhersagen der linearen Regression mehr ausserhalb von [0, 1]?
- Berechnen Sie Odds und Log-Odds für $\pi = .1$ und $\pi = .9$. Was fällt auf?
- Schätzen Sie das Modell mit
  `glm(remission ~ sitzungen, family = binomial, data = remission_data)`.
  Wie nahe liegen die Schätzungen an den wahren Werten?

## Lizenz

- Folien, Texte, Abbildungen (`slides.qmd`, `index.qmd`, `r-code.qmd`, `custom.scss`,
  `figures/`): [CC BY 4.0](LICENSE-CC-BY-4.0.txt)
- `webex/` (aus dem R-Paket exams2forms, Achim Zeileis): GPL-3
- Fotos (`images/`): CC0 1.0, Quellen in [images/QUELLEN.md](images/QUELLEN.md)
- R-Code (`R/`): [MIT](LICENSE)

## Zitieren

Frischkorn, G. T. (2026). *Lehrprobe: Logistische Regression* [Lehrmaterial].
GitHub. https://github.com/GidonFrischkorn/lehrprobe-logistische-regression
