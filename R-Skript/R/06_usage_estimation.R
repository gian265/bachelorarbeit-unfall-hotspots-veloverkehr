# ==============================================================================
# Hochrechnung der Strava-Daten auf geschätzte jährliche Velonutzung
# ==============================================================================

# ------------------------------------------------------------------------------
# Tageswerte der Strava-Segmente mit passenden Faktoren verknüpfen
# ------------------------------------------------------------------------------

# Den Strava-Tageswerten wird anhand des Datums der passende Tagestyp
# und die passende Jahreszeit zugewiesen.
strava_daily_estimated <- strava_daily_segment %>%
  mutate(
    day_type = ifelse(
      wday(date, week_start = 1) > 5,
      "Wochenende",
      "Werktag"),
    season = ifelse(
      month(date) %in% 4:9,
      "Sommer",
      "Winter")
    ) %>%
  left_join(
    calibration_factors,
    by = c("season", "day_type")
  )

# ------------------------------------------------------------------------------
# Geschätzte Velonutzung pro Segment und Tag berechnen
# ------------------------------------------------------------------------------

# Alle Strava-Tageswerte mit dem passenden Hochrechnungsfaktor multiplizieren
# Berechnung minimaler und maximaler Schätzwerte über das Konfidenzintervall
strava_daily_estimated <- strava_daily_estimated %>%
  mutate(
    est_bikes_day = strava_day_total * count_factor,
    est_bikes_day_min = strava_day_total * count_factor_min,
    est_bikes_day_max = strava_day_total * count_factor_max
  )

# ------------------------------------------------------------------------------
# Geschätzte jährliche Velonutzung pro Segment berechnen
# ------------------------------------------------------------------------------

# Anzahl Tage im Analysejahr
n_days_year <- length(seq(
  as.Date("2025-01-01"), as.Date("2025-12-31"), by = "day")
  )

# Die geschätzten Tageswerte werden pro Segment aufsummiert.
# Daraus entsteht die geschätzte jährliche Velonutzung pro Strava-Segment
# und die täglichen Durchschnittswerte
usage_estimated <- strava_daily_estimated %>%
  group_by(edge_uid, osm_reference_id) %>%
  summarise(
    strava_trips_year = sum(strava_day_total, na.rm = TRUE),
    est_bikes_year = sum(est_bikes_day, na.rm = TRUE),
    est_bikes_year_min = sum(est_bikes_day_min, na.rm = TRUE),
    est_bikes_year_max = sum(est_bikes_day_max, na.rm = TRUE),
    est_bikes_mean_day = est_bikes_year / n_days_year,
    est_bikes_mean_day_min = est_bikes_year_min / n_days_year,
    est_bikes_mean_day_max = est_bikes_year_max / n_days_year,
    .groups = "drop"
  )

# ------------------------------------------------------------------------------
# Geschätzte Velonutzung an Strava-Geometrie anhängen
# ------------------------------------------------------------------------------

# Geschätzte jährliche Velonutzung wird der Strava-Segmentgeometrie angehängt
# NAs werden durch 0 ersetzt
usage_estimated_sf <- strava_shape_2056 %>%
  left_join(
    usage_estimated,
    by = c("edgeUID" = "edge_uid", "osmId" = "osm_reference_id")
  ) %>%
  mutate(
    strava_trips_year = replace_na(strava_trips_year, 0),
    est_bikes_year = replace_na(est_bikes_year, 0),
    est_bikes_year_min = replace_na(est_bikes_year_min, 0),
    est_bikes_year_max = replace_na(est_bikes_year_max, 0),
    est_bikes_mean_day = replace_na(est_bikes_mean_day, 0),
    est_bikes_mean_day_min = replace_na(est_bikes_mean_day_min, 0),
    est_bikes_mean_day_max = replace_na(est_bikes_mean_day_max, 0)
  )

# ------------------------------------------------------------------------------
# Segmentlänge und geschätzte Personenkilometer berechnen
# ------------------------------------------------------------------------------

usage_estimated_sf <- usage_estimated_sf %>%
  mutate(
    segment_length_m = as.numeric(st_length(geometry)),
    segment_length_km = segment_length_m / 1000,
    
    # Geschätzte Personenkilometer pro Jahr
    pkm_year = est_bikes_year * segment_length_km,
    pkm_year_min = est_bikes_year_min * segment_length_km,
    pkm_year_max = est_bikes_year_max * segment_length_km
  )

# ------------------------------------------------------------------------------
# Analyseebene für Segmentauswertung erstellen
# ------------------------------------------------------------------------------

if (segment_analysis_id == "edgeUID") {
  
  # Analyse auf Strava-Segmentebene
  segment_analysis_sf <- usage_estimated_sf %>%
    mutate(
      segment_id = as.character(edgeUID),
      segment_id_type = "edgeUID"
    )
  
} else if (segment_analysis_id == "osmId") {
  
  # Analyse auf OSM-ID-Ebene
  # Wichtig:
  # Die Personenkilometer werden zuerst pro edgeUID berechnet
  # und anschliessend nach osmId aufsummiert.
  segment_analysis_sf <- usage_estimated_sf %>%
    filter(!is.na(osmId)) %>%
    mutate(
      segment_id = as.character(osmId),
      segment_id_type = "osmId"
    ) %>%
    group_by(segment_id, osmId, segment_id_type) %>%
    summarise(
      strava_trips_year = sum(strava_trips_year, na.rm = TRUE),
      est_bikes_year = sum(est_bikes_year, na.rm = TRUE),
      est_bikes_year_min = sum(est_bikes_year_min, na.rm = TRUE),
      est_bikes_year_max = sum(est_bikes_year_max, na.rm = TRUE),
      
      # Länge und Personenkilometer
      segment_length_m = sum(segment_length_m, na.rm = TRUE),
      segment_length_km = sum(segment_length_km, na.rm = TRUE),
      pkm_year = sum(pkm_year, na.rm = TRUE),
      pkm_year_min = sum(pkm_year_min, na.rm = TRUE),
      pkm_year_max = sum(pkm_year_max, na.rm = TRUE),
      
      # Tagesmittelwerte
      est_bikes_mean_day = est_bikes_year / n_days_year,
      est_bikes_mean_day_min = est_bikes_year_min / n_days_year,
      est_bikes_mean_day_max = est_bikes_year_max / n_days_year,
      
      n_edgeUID = n_distinct(edgeUID),
      .groups = "drop"
    )
  
} else {
  
  stop("segment_analysis_id muss entweder 'edgeUID' oder 'osmId' sein.")
}
