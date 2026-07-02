# ==============================================================================
# Zentrale Funktionen definieren
# ==============================================================================

# p-Wert des Hochrechnungsfaktors aus dem Modell extrahieren
# Die vierte Spalte der Koeffiziententabelle enthält den p-Wert
get_p_value <- function(model) {
  summary(model)$coefficients["strava_day_total", 4]
}

# 80 %-Konfidenzintervall des Hochrechnungsfaktors berechnen
# level = 0.80 entspricht einem zentralen Intervall von 80 %
get_factor_ci <- function(model) {
  ci <- confint(model, level = 0.80)
  as.numeric(ci["strava_day_total", ])
}

# Sichere Dateinamen erzeugen
# Sonderzeichen und Leerzeichen werden durch "_" ersetzt
clean_filename <- function(x) {
  x <- ifelse(is.na(x), "ohne_bezeichnung", x)
  gsub("[^[:alnum:]]", "_", x)
}

# Achsenmaximum bestimmen
# Die Achsen starten bei 0 und werden leicht über Maximalwert hinaus erweitert
get_axis_max <- function(x) {
  x_max <- max(x, na.rm = TRUE)
  ifelse(is.finite(x_max) && x_max > 0, x_max * 1.05, 1)
}