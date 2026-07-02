# ==============================================================================
# Zuordnung der Unfalldaten auf die Strava-Segmente
# ==============================================================================

# ------------------------------------------------------------------------------
# Bestimmung des nächsten Strava-Segment pro Unfall
# ------------------------------------------------------------------------------

segment_layer <- segment_analysis_sf

# Welches Strava-Segment liegt dem Unfallpunkt am nächsten?
accident_nearest_idx <- st_nearest_feature(
  accident_filtered_sf,
  segment_layer
)

# Berechnung der Distanz zwischen Unfallpunkt und nächstem Segment
accident_nearest_dist <- st_distance(
  accident_filtered_sf,
  segment_layer[accident_nearest_idx, ],
  by_element = TRUE
)

# Segment-IDs und Distanz an die Unfalldaten anhängen
accident_with_segment <- accident_filtered_sf %>%
  mutate(
    segment_id = segment_layer$segment_id[accident_nearest_idx],
    segment_id_type = segment_layer$segment_id_type[accident_nearest_idx],
    osmId = segment_layer$osmId[accident_nearest_idx],
    accident_dist_m = as.numeric(accident_nearest_dist)
  )

# Die ersten 20 Unfälle mit grösster Distanz zum zugeordneten Segment anzeigen
accident_with_segment %>%
  st_drop_geometry() %>%
  arrange(desc(accident_dist_m)) %>%
  select(
    AccidentUID,
    AccidentYear,
    AccidentSeverityCategory_de,
    AccidentLocation_CHLV95_E,
    AccidentLocation_CHLV95_N,
    segment_id_type,
    segment_id,
    osmId,
    accident_dist_m
  ) %>%
  head(20)

# ------------------------------------------------------------------------------
# Filtern der Unfälle mit plausibler Segmentzuordnung
# ------------------------------------------------------------------------------

# Unfälle mit plausibler Zuordnung behalten
accident_with_segment_clean <- accident_with_segment %>%
  filter(accident_dist_m <= max_accident_dist_m)

# Ausgeschlossene Unfälle separat speichern
accident_with_segment_removed <- accident_with_segment %>%
  filter(accident_dist_m > max_accident_dist_m)

# ------------------------------------------------------------------------------
# Berechnung der Unfallanzahl pro Strava-Segment
# ------------------------------------------------------------------------------

accidents_per_segment <- accident_with_segment_clean %>%
  st_drop_geometry() %>%
  group_by(segment_id, segment_id_type) %>%
  summarise(
    accident_count_2020_2024 = n(),
    accident_count_per_year = accident_count_2020_2024 / n_accident_years,
    .groups = "drop"
  )
  
# ------------------------------------------------------------------------------
# Verknüpfung der Unfallzahlen mit der geschätzten Velonutzung
# ------------------------------------------------------------------------------

usage_accidents_sf <- segment_analysis_sf %>%
  left_join(
    accidents_per_segment,
    by = c("segment_id", "segment_id_type")
  ) %>%
  mutate(
    accident_count_2020_2024 = replace_na(accident_count_2020_2024, 0),
    accident_count_per_year = replace_na(accident_count_per_year, 0)
  )

# ------------------------------------------------------------------------------
# Berechnung der Unfallrate pro 100 Mio. Personenkilometer
# ------------------------------------------------------------------------------

usage_accidents_sf <- usage_accidents_sf %>%
  mutate(
    accident_rate_per_100m_pkm = ifelse(
      pkm_year > 0,
      accident_count_per_year / pkm_year * 100000000,
      NA
    ),
    accident_rate_per_100m_pkm_min = ifelse(
      pkm_year_max > 0,
      accident_count_per_year / pkm_year_max * 100000000,
      NA
    ),
    accident_rate_per_100m_pkm_max = ifelse(
      pkm_year_min > 0,
      accident_count_per_year / pkm_year_min * 100000000,
      NA
    )
  )

# ------------------------------------------------------------------------------
# Hotspot-Klassifikation nach Unfallrate pro 100 Mio. Personenkilometer
# ------------------------------------------------------------------------------

# Hotspot-Definition:
# Ein Segment gilt als Hotspot, wenn es
# 1. mindestens die definierte Mindestanzahl Velounfälle aufweist,
# 2. eine ausreichende geschätzte Exposition besitzt und
# 3. zu den obersten Unfallraten gehört.

# Mindest-Exposition berechnen:
# Segmente im untersten Quantil der geschätzten Personenkilometer werden
# nicht für die Hotspot-Klassifikation berücksichtigt.
hotspot_min_pkm_threshold <- unname(
  quantile(
    usage_accidents_sf$pkm_year[
      !is.na(usage_accidents_sf$pkm_year) &
        usage_accidents_sf$pkm_year > 0
    ],
    probs = hotspot_min_pkm_quantile,
    na.rm = TRUE
  )
)

# Gültige Unfallraten für die Berechnung des Hotspot-Schwellenwerts:
# - gültige und positive Personenkilometer
# - mindestens Mindest-Exposition
# - gültige und positive Unfallrate
valid_hotspot_rates <- usage_accidents_sf %>%
  st_drop_geometry() %>%
  filter(
    !is.na(pkm_year),
    pkm_year > 0,
    pkm_year >= hotspot_min_pkm_threshold,
    !is.na(accident_rate_per_100m_pkm),
    is.finite(accident_rate_per_100m_pkm),
    accident_rate_per_100m_pkm > 0
  ) %>%
  pull(accident_rate_per_100m_pkm)

# Schwellenwert der Unfallrate berechnen
hotspot_rate_threshold <- unname(
  quantile(
    valid_hotspot_rates,
    probs = hotspot_rate_quantile,
    na.rm = TRUE
  )
)

# Segmente als gültige Hotspot-Kandidaten markieren und anhand der definierten
# Mindestanzahl Unfälle sowie des Unfallraten-Schwellenwerts klassifizieren.
usage_accidents_sf <- usage_accidents_sf %>%
  mutate(
    valid_hotspot_rate = !is.na(pkm_year) &
      pkm_year > 0 &
      pkm_year >= hotspot_min_pkm_threshold &
      !is.na(accident_rate_per_100m_pkm) &
      is.finite(accident_rate_per_100m_pkm) &
      accident_rate_per_100m_pkm > 0,
    
    is_hotspot = valid_hotspot_rate &
      accident_count_2020_2024 >= hotspot_min_accidents &
      accident_rate_per_100m_pkm >= hotspot_rate_threshold
  )


# ==============================================================================
# Alternative Unfallzuordnung: Umkreis-Methode
# ==============================================================================

if (run_buffer_method) {

  # Mit dieser Methode wird ein Unfall allen Segmenten innerhalb eines definierten
  # Radius zugeordnet. Ein Unfall zählt für jedes Segment im Umkreis als 1.
  # Ein Unfall kann dadurch mehreren Segmenten zugeordnet werden.

  # Für jeden Unfall alle Strava-Segmente innerhalb des Radius suchen
  accident_segment_list <- st_is_within_distance(
    accident_filtered_sf,
    segment_layer,
    dist = buffer_accident_dist_m
  )

  # Beziehungstabelle Unfall - Segment erstellen
  accident_segment_relation <- data.frame(
    accident_index = rep(seq_along(accident_segment_list), lengths(accident_segment_list)),
    segment_index = unlist(accident_segment_list)
  )

  # Segment- und Unfallinformationen anhängen
  accident_segment_relation <- accident_segment_relation %>%
    mutate(
      AccidentUID = accident_filtered_sf$AccidentUID[accident_index],
      segment_id = segment_layer$segment_id[segment_index],
      segment_id_type = segment_layer$segment_id_type[segment_index],
      osmId = segment_layer$osmId[segment_index]
    )

  # Unfallanzahl pro Segment mit Umkreis-Methode berechnen
  accidents_per_segment_buffer <- accident_segment_relation %>%
    group_by(segment_id, segment_id_type) %>%
    summarise(
      accident_count_buffer_2020_2024 = n(),
      accident_count_buffer_per_year = accident_count_buffer_2020_2024 / n_accident_years,
      .groups = "drop"
    )

  # Unfallrate mit Umkreis-Methode berechnen
  usage_accidents_buffer_sf <- segment_analysis_sf %>%
    left_join(
      accidents_per_segment_buffer,
      by = c("segment_id", "segment_id_type")
    ) %>%
    mutate(
      accident_count_buffer_2020_2024 = replace_na(accident_count_buffer_2020_2024, 0),
      accident_count_buffer_per_year = replace_na(accident_count_buffer_per_year, 0)
    ) %>%
    mutate(
      accident_rate_buffer_per_100m_pkm = ifelse(
        pkm_year > 0,
        accident_count_buffer_per_year / pkm_year * 100000000,
        NA
      ),
      accident_rate_buffer_per_100m_pkm_min = ifelse(
        pkm_year_max > 0,
        accident_count_buffer_per_year / pkm_year_max * 100000000,
        NA
      ),
      accident_rate_buffer_per_100m_pkm_max = ifelse(
        pkm_year_min > 0,
        accident_count_buffer_per_year / pkm_year_min * 100000000,
        NA
      )
    )
  
  # ---------------------------------------------------------------------------
  # Hotspot-Klassifikation für Umkreis-Methode
  # ---------------------------------------------------------------------------
  
  # Für die Vergleichbarkeit wird dieselbe Mindest-Exposition verwendet
  # wie bei der Nearest-Methode.
  hotspot_min_pkm_threshold_buffer <- hotspot_min_pkm_threshold
  
  # Gültige positive Unfallraten der Umkreis-Methode
  valid_hotspot_rates_buffer <- usage_accidents_buffer_sf %>%
    st_drop_geometry() %>%
    filter(
      !is.na(pkm_year),
      pkm_year > 0,
      pkm_year >= hotspot_min_pkm_threshold_buffer,
      !is.na(accident_rate_buffer_per_100m_pkm),
      is.finite(accident_rate_buffer_per_100m_pkm),
      accident_rate_buffer_per_100m_pkm > 0
    ) %>%
    pull(accident_rate_buffer_per_100m_pkm)
  
  if (length(valid_hotspot_rates_buffer) == 0) {
    stop("Keine gültigen Unfallraten für die Hotspot-Klassifikation der Umkreis-Methode vorhanden.")
  }
  
  # Schwellenwert der Unfallrate für die Umkreis-Methode
  hotspot_rate_threshold_buffer <- unname(
    quantile(
      valid_hotspot_rates_buffer,
      probs = hotspot_rate_quantile,
      na.rm = TRUE
    )
  )
  
  # Hotspot-Definition für Umkreis-Methode anwenden
  usage_accidents_buffer_sf <- usage_accidents_buffer_sf %>%
    mutate(
      valid_hotspot_rate_buffer = !is.na(pkm_year) &
        pkm_year > 0 &
        pkm_year >= hotspot_min_pkm_threshold_buffer &
        !is.na(accident_rate_buffer_per_100m_pkm) &
        is.finite(accident_rate_buffer_per_100m_pkm) &
        accident_rate_buffer_per_100m_pkm > 0,
      
      is_hotspot_buffer = valid_hotspot_rate_buffer &
        accident_count_buffer_2020_2024 >= hotspot_min_accidents &
        accident_rate_buffer_per_100m_pkm >= hotspot_rate_threshold_buffer
    )

  # Anzahl zugeordneter Segmente pro Unfall prüfen
  segments_per_accident <- accident_segment_relation %>%
    group_by(AccidentUID) %>%
    summarise(
      n_segments = n(),
      .groups = "drop"
    )

  # ---------------------------------------------------------------------------
  # Verteilung: Wie viele Segmente werden pro Unfall zugeordnet?
  # ---------------------------------------------------------------------------
  
  segments_per_accident_distribution <- segments_per_accident %>%
    count(n_segments, name = "n_accidents") %>%
    mutate(
      share_percent = n_accidents / sum(n_accidents) * 100
    ) %>%
    arrange(n_segments)
  
  # Kennwerte zur Mehrfachzuordnung
  n_accidents_total_buffer <- nrow(segments_per_accident)
  n_accidents_multi_segment <- segments_per_accident %>%
    filter(n_segments > 1) %>%
    nrow()
  
  share_accidents_multi_segment <- n_accidents_multi_segment / n_accidents_total_buffer * 100
  
  # Plot erstellen
  plot_segments_per_accident_buffer <- ggplot(
    segments_per_accident_distribution,
    aes(x = n_segments, y = n_accidents)
  ) +
    geom_col() +
    scale_x_continuous(
      breaks = segments_per_accident_distribution$n_segments
    ) +
    labs(
      title = "Anzahl zugeordneter Segmente pro Unfall",
      subtitle = paste0(
        n_accidents_multi_segment,
        " von ",
        n_accidents_total_buffer,
        " Unfällen wurden mehreren Segmenten zugeordnet (",
        round(share_accidents_multi_segment, 1),
        " %)."
      ),
      x = "Anzahl zugeordneter Segmente",
      y = "Anzahl Unfälle"
    ) +
    theme_minimal()
  
  # Plot anzeigen
  print(plot_segments_per_accident_buffer)
}