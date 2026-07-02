# ==============================================================================
# Räumliche Zuordnung
# ==============================================================================

# Zählstellen als Punkt-Geometrie
count_station_sf <- count_daily %>%
  distinct(FK_STANDORT, count_ost, count_nord, bezeichnung) %>%
  st_as_sf(coords = c("count_ost", "count_nord"), crs = crs_lv95)

# Welches Strava-Segment liegt einer Zählstelle am nächsten?
nearest_idx <- st_nearest_feature(count_station_sf, strava_shape_2056)

# Zuordnung mit jeweiliger Distanz speichern
count_station_assignment <- count_station_sf %>%
  bind_cols(st_drop_geometry(strava_shape_2056[nearest_idx, ])) %>%
  mutate(dist_m = as.numeric(st_distance(geometry, strava_shape_2056[nearest_idx, ], by_element = TRUE)))


# ==============================================================================
# Datensätze zusammenführen
# ==============================================================================

# Zählstellen-Tageswerte mit räumlicher Zuordnung verknüpfen
# und entsprechende Strava-Werte für denselben Tag anhängen.
count_strava_join <- count_daily %>%
  left_join(
    st_drop_geometry(count_station_assignment),
    by = c("FK_STANDORT", "bezeichnung")
  ) %>%
  left_join(
    strava_daily_segment,
    by = c("edgeUID" = "edge_uid", "count_day" = "date")
  ) %>%
  mutate(
    # TRUE, wenn für diesen Tag und dieses Segment ein Strava-Wert vorhanden
    has_strava_data = !is.na(strava_day_total)
  )