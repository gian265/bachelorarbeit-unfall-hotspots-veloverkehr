# ==============================================================================
# Pakete laden
# ==============================================================================

# install.packages("sf")
# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("purrr")
# install.packages("ggplot2")
# install.packages("data.table")
# install.packages("lubridate")

library(sf)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(data.table)
library(lubridate)

# ==============================================================================
# Zentrale Einstellungen definieren
# ==============================================================================

# Koordinatensystem
crs_lv95 <- 2056

# Untersuchungsgebiet
study_area_name <- "Zürich"
study_area_bfs <- 261

# Analysejahr für Strava-Daten
analysis_year_strava <- 2025

# Analysezeitraum für Unfalldaten
accident_year_min <- 2020
accident_year_max <- 2024
n_accident_years <- accident_year_max - accident_year_min + 1

# Minimalwerte für Zählstellenausschluss
min_strava_trips <- 1000
min_count_days <- 30

# Manueller Zählstellenausschluss
manual_exclude_station_ids <- c(4269)

# Maximal zulässige Distanz bei der Nächste-Segment-Methode
# Unfälle mit grösserer Distanz werden ausgeschlossen
max_accident_dist_m <- 15

# Radius für die Umkreis-Methode
# Ein Unfall wird allen Segmenten innerhalb dieses Radius zugeordnet
buffer_accident_dist_m <- 5

# Hotspot-Klassifikation
hotspot_min_accidents <- 3
hotspot_rate_quantile <- 0.95
hotspot_min_pkm_quantile <- 0.25

# ==============================================================================
# Zusätzliche Funktionen steuern
# ==============================================================================

# Segment-ID für die  Analyse:
# "edgeUID" = Analyse auf Strava-Segmentebene
# "osmId"   = Analyse auf OSM-Segmentebene
segment_analysis_id <- "edgeUID"

# Regelt die Ausführung der Umkreis-Methode
run_buffer_method <- TRUE

# Regelt die Ausführung der Modell-Plots
run_model_plots <- TRUE
