# ==============================================================================
# Daten vorbereiten
# ==============================================================================

# ------------------------------------------------------------------------------
# Strava-Daten vorbereiten
# ------------------------------------------------------------------------------

# Berechnung der Gesamtanzahl Strava-Trips pro Tag und Segment
# Aggregation mit data.table statt dplyr aufgrund von Performance
strava_daily_segment <- strava_table_all[, .(
  strava_day_total = sum(total_trip_count, na.rm = TRUE),
  osm_reference_id = first(osm_reference_id)
), by = .(edge_uid, date)]
setDF(strava_daily_segment)

# ------------------------------------------------------------------------------
# Unfall-Daten vorbereiten
# ------------------------------------------------------------------------------

# Relevante Unfall-Daten filtern
accident_filtered <- accident_table %>%
  filter(
    # Zeitraum 2020 bis 2024
    AccidentYear >= accident_year_min,
    AccidentYear <= accident_year_max,
    # Kanton Zürich
    CantonCode == "ZH",
    # Unfälle mit Velobeteiligung
    AccidentInvolvingBicycle == TRUE
  )

# Unfall-Daten als Punktobjekte erstellen
accident_filtered_sf <- st_as_sf(
  accident_filtered,
  coords = c("AccidentLocation_CHLV95_E", "AccidentLocation_CHLV95_N"),
  crs = crs_lv95,
  # Koordinatenspalten beibehalten
  remove = FALSE
)

# Unfälle räumlich auf Stadt Zürich filtern
accident_filtered_sf <- accident_filtered_sf %>%
  st_filter(city_zurich_sf, .predicate = st_intersects)

# ------------------------------------------------------------------------------
# Zeitliche Einteilung nach Werktag/WE & Sommer/Winter
# ------------------------------------------------------------------------------

# Velo-Zählwerte (In/Out) addieren und pro Tag und Standort gruppieren
# Metadaten anhängen
# Jahreszeiten und Werktage/Wochenende definieren
count_daily <- count_table %>%
  mutate(count_velo_total = coalesce(VELO_IN, 0) + coalesce(VELO_OUT, 0)) %>%
  group_by(FK_STANDORT, count_day) %>%
  summarise(
    count_day_total = sum(count_velo_total, na.rm = TRUE),
    count_ost = first(OST),
    count_nord = first(NORD),
    .groups = "drop"
  ) %>%
  left_join(count_metadata, by = c("FK_STANDORT" = "id1")) %>%
  mutate(
    # Werktag vs Wochenende
    day_type = ifelse(wday(count_day, week_start = 1) > 5, "Wochenende", "Werktag"),
    # Sommer (April-September) vs Winter (Oktober-März)
    season = ifelse(month(count_day) %in% 4:9, "Sommer", "Winter")
  )