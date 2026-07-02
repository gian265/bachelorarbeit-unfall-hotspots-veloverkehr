# ==============================================================================
# Hauptskript Bachelorarbeit
# ==============================================================================

# Grundeinstellungen, Pakete und zentrale Parameter laden
source("R/00_setup.R")

# Hilfsfunktionen laden
source("R/functions.R")

# Rohdaten einlesen
source("R/01_read_data.R")

# Daten bereinigen und vorbereiten
source("R/02_prepare_data.R")

# Räumliche Zuordnung von Zählstellen zu Strava-Segmenten
source("R/03_spatial_assignment.R")

# Gültige Zählstellen auswählen und ungeeignete ausschliessen
source("R/04_station_filtering.R")

# Hochrechnungsmodelle zwischen Zähldaten und Strava-Daten berechnen
source("R/05_models.R")

# Strava-Daten auf geschätzte Velonutzung und Personenkilometer hochrechnen
source("R/06_usage_estimation.R")

# Unfälle Segmenten zuordnen und Unfallraten berechnen
source("R/07_accident_assignment.R")

# Tabellen, GeoPackage-Dateien und Plots exportieren
source("R/08_exports.R")
