# ==============================================================================
# Export & PDF-Generierung
# ==============================================================================

# Exportverzeichnis erstellen, falls es noch nicht existiert
if (!dir.exists("export")) {
  dir.create("export")
}

# Unterordner für Zählstellen-Plots
if (!dir.exists("export/plots_zaehlstellen")) {
  dir.create("export/plots_zaehlstellen", recursive = TRUE)
}

# Unterordner für globale Modellplots
if (!dir.exists("export/plots_globalmodelle")) {
  dir.create("export/plots_globalmodelle", recursive = TRUE)
}

# Unterordner für Tabellen im Ergebnisteil
if (!dir.exists("export/tables")) {
  dir.create("export/tables", recursive = TRUE)
}

# Unterordner für GeoPackages im Ergebnisteil
if (!dir.exists("export/gpkg")) {
  dir.create("export/gpkg", recursive = TRUE)
}

# ------------------------------------------------------------------------------
# GeoPackage-Export: Zählstellen mit totalen Jahreswerten
# ------------------------------------------------------------------------------

# Jahreswerte pro Zählstelle berechnen
count_station_year_totals <- count_daily %>%
  filter(year(count_day) == analysis_year_strava) %>%
  group_by(
    FK_STANDORT,
    bezeichnung,
    count_ost,
    count_nord
  ) %>%
  summarise(
    count_year_total = sum(count_day_total, na.rm = TRUE),
    count_mean_day = mean(count_day_total, na.rm = TRUE),
    n_count_days = n_distinct(count_day),
    first_count_day = min(count_day, na.rm = TRUE),
    last_count_day = max(count_day, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    used = FK_STANDORT %in% valid_station_ids
  ) %>%
  left_join(
    removed_station_ids %>%
      select(FK_STANDORT, reason),
    by = "FK_STANDORT"
  ) %>%
  mutate(
    reason = case_when(
      used ~ "verwendet",
      is.na(reason) ~ "ausgeschlossen",
      TRUE ~ reason
    )
  )

# Als Punkt-Geometrie erstellen
count_station_year_totals_sf <- count_station_year_totals %>%
  st_as_sf(
    coords = c("count_ost", "count_nord"),
    crs = crs_lv95,
    remove = FALSE
  )

# GeoPackage für QGIS exportieren
st_write(
  count_station_year_totals_sf,
  paste0(
    "export/gpkg/zaehlstellen_jahreswerte_",
    analysis_year_strava,
    ".gpkg"
  ),
  delete_dsn = TRUE
)

# Ausgeschlossene Zählstellen exportieren
write.table(
  removed_station_ids,
  "export/tables/ausgeschlossene_zaehlstellen.csv",
  sep = ";",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# ------------------------------------------------------------------------------
# Plot 1: Gesamtmodell pro Zählstelle
# ------------------------------------------------------------------------------

if (run_model_plots) {
  # Für jede Zählstelle wird ein PDF mit allen Tageswerten und 
  # der Regressionsgeraden erstellt.
  pwalk(
    list(
      station_models_all$FK_STANDORT,
      station_models_all$bezeichnung,
      station_models_all$data,
      station_models_all$count_factor
    ),
    function(id, name, df, count_factor) {
      # Achsenbereiche bestimmen
      x_max <- get_axis_max(df$strava_day_total)
      y_max <- get_axis_max(df$count_day_total)
      # Plot erstellen
      p <- ggplot(df, aes(x = strava_day_total, y = count_day_total)) +
        geom_point(alpha = 0.4, color = "darkslategrey") +
        # Regressionsgerade durch den Nullpunkt zeichnen
        geom_abline(
          intercept = 0,
          slope = count_factor,
          color = "firebrick",
          linewidth = 1
        ) +
        # Achsen bei 0 starten lassen
        coord_cartesian(
          xlim = c(0, x_max),
          ylim = c(0, y_max)
        ) +
        # Titel und Achsenbeschriftung
        labs(
          title = paste("Gesamtmodell - Zählstelle:", name),
          x = "Strava-Aktivitäten pro Tag",
          y = "Gezählte Velos pro Tag"
        ) +
        theme_minimal()
      # Dateiname definieren
      file_name <- file.path(
        "export/plots_zaehlstellen",
        paste0(
          "Zaehlstelle_",
          id,
          "_",
          clean_filename(name),
          "_gesamt.png"
        )
      )
      # PNG speichern
      ggsave(
        filename = file_name,
        plot = p,
        width = 7,
        height = 6,
        dpi = 300,
        bg = "white"
      )
    }
  )
}

# ------------------------------------------------------------------------------
# Plot 2: Vergleich Werktag vs. Wochenende
# ------------------------------------------------------------------------------

if (run_model_plots) {
  # Plotdaten pro Zählstelle vorbereiten
  daytype_plot_data <- data_model %>%
    group_by(FK_STANDORT, bezeichnung, dist_m) %>%
    nest() %>%
    ungroup()
  
  # Plot pro Zählstelle, in dem Werktage und Wochenenden farblich unterschieden
  # und mit separaten Regressionsgeraden dargestellt werden.
  pwalk(
    list(
      daytype_plot_data$FK_STANDORT,
      daytype_plot_data$bezeichnung,
      daytype_plot_data$data
    ),
    function(id, name, df) {
      # Modellfaktoren für Werktag und Wochenende holen
      model_info <- station_models_daytype %>%
        filter(FK_STANDORT == id) %>%
        select(day_type, count_factor) %>%
        mutate(abline_intercept = 0)
      # Achsenbereiche bestimmen
      x_max <- get_axis_max(df$strava_day_total)
      y_max <- get_axis_max(df$count_day_total)
      # Plot erstellen
      p <- ggplot(df, aes(x = strava_day_total, y = count_day_total, color = day_type)) +
        geom_point(alpha = 0.4) +
        # Separate Regressionsgeraden für Werktag und Wochenende
        geom_abline(
          data = model_info,
          aes(
            slope = count_factor,
            intercept = abline_intercept,
            color = day_type
          ),
          linewidth = 1,
          inherit.aes = FALSE
        ) +
        # Achsen bei 0 starten lassen
        coord_cartesian(
          xlim = c(0, x_max),
          ylim = c(0, y_max)
        ) +
        # Titel und Achsenbeschriftung
        labs(
          title = paste("Werktag / Wochenende - Zählstelle:", name),
          x = "Strava-Aktivitäten pro Tag",
          y = "Gezählte Velos pro Tag",
          color = NULL
        ) +
        theme_minimal()
      # Dateiname definieren
      file_name <- file.path(
        "export/plots_zaehlstellen",
        paste0(
          "Zaehlstelle_",
          id,
          "_",
          clean_filename(name),
          "_werktag_wochenende.png"
        )
      )
      # PNG speichern
      ggsave(
        filename = file_name,
        plot = p,
        width = 7,
        height = 6,
        dpi = 300,
        bg = "white"
      )
    }
  )
}

# ------------------------------------------------------------------------------
# Plot 3: Vergleich Sommer vs. Winter
# ------------------------------------------------------------------------------

if (run_model_plots) {
  # Plotdaten pro Zählstelle vorbereiten
  season_plot_data <- data_model %>%
    group_by(FK_STANDORT, bezeichnung, dist_m) %>%
    nest() %>%
    ungroup()
  # Plot pro Zählstelle, in dem Sommer und Winter farblich unterschieden
  # und mit separaten Regressionsgeraden dargestellt werden.
  pwalk(
    list(
      season_plot_data$FK_STANDORT,
      season_plot_data$bezeichnung,
      season_plot_data$data
    ),
    function(id, name, df) {
      # Modellfaktoren für Sommer und Winter holen
      model_info <- station_models_season %>%
        filter(FK_STANDORT == id) %>%
        select(season, count_factor) %>%
        mutate(abline_intercept = 0)
      # Achsenbereiche bestimmen
      x_max <- get_axis_max(df$strava_day_total)
      y_max <- get_axis_max(df$count_day_total)
      # Plot erstellen
      p <- ggplot(df, aes(x = strava_day_total, y = count_day_total, color = season)) +
        geom_point(alpha = 0.4) +
        # Separate Regressionsgeraden für Sommer und Winter
        geom_abline(
          data = model_info,
          aes(
            slope = count_factor,
            intercept = abline_intercept,
            color = season
          ),
          linewidth = 1,
          inherit.aes = FALSE
        ) +
        # Achsen bei 0 starten lassen
        coord_cartesian(
          xlim = c(0, x_max),
          ylim = c(0, y_max)
        ) +
        # Titel und Achsenbeschriftung
        labs(
          title = paste("Sommer / Winter - Zählstelle:", name),
          x = "Strava-Aktivitäten pro Tag",
          y = "Gezählte Velos pro Tag",
          color = NULL
        ) +
        theme_minimal()
      # Dateiname definieren
      file_name <- file.path(
        "export/plots_zaehlstellen",
        paste0(
          "Zaehlstelle_",
          id,
          "_",
          clean_filename(name),
          "_sommer_winter.png"
        )
      )
      # PNG speichern
      ggsave(
        filename = file_name,
        plot = p,
        width = 7,
        height = 6,
        dpi = 300,
        bg = "white"
      )
    }
  )
}

# ------------------------------------------------------------------------------
# Plot 4: Globale Plots nach Jahreszeit und Tagestyp
# ------------------------------------------------------------------------------

if (run_model_plots) {
  # Gemeinsame Achsenlimiten über alle vier globalen Modelle berechnen
  global_model_axis_data <- global_models_season_daytype %>%
    select(data) %>%
    unnest(cols = data)
  x_max_global <- get_axis_max(global_model_axis_data$strava_day_total)
  y_max_global <- get_axis_max(global_model_axis_data$count_day_total)
  # Für jede Kombination aus Jahreszeit und Tagestyp wird ein Plot erstellt.
  # Alle Plots verwenden dieselben Achsenlimiten.
  pwalk(
    list(
      global_models_season_daytype$season,
      global_models_season_daytype$day_type,
      global_models_season_daytype$data,
      global_models_season_daytype$count_factor,
      global_models_season_daytype$r_squared,
      global_models_season_daytype$p_value
    ),
    function(season_i, day_type_i, df, count_factor, r_squared, p_value) {
      # Plot erstellen
      p <- ggplot(df, aes(x = strava_day_total, y = count_day_total)) +
        geom_point(alpha = 0.35, color = "darkslategrey") +
        geom_abline(
          intercept = 0,
          slope = count_factor,
          color = "firebrick",
          linewidth = 1
        ) +
        coord_cartesian(
          xlim = c(0, x_max_global),
          ylim = c(0, y_max_global)
        ) +
        labs(
          title = paste("Globales Modell:", season_i, "/", day_type_i),
          subtitle = paste0(
            "Faktor gezählte Velos pro Strava-Trip: ",
            round(count_factor, 2),
            " | R² = ",
            round(r_squared, 3),
            " | p = ",
            format.pval(p_value, digits = 3, eps = 0.001)
          ),
          x = "Strava-Aktivitäten pro Tag",
          y = "Gezählte Velos pro Tag"
        ) +
        theme_minimal()
      # Dateiname definieren
      file_name <- file.path(
        "export/plots_globalmodelle",
        paste0(
          "Globalmodell_",
          clean_filename(season_i),
          "_",
          clean_filename(day_type_i),
          ".png"
        )
      )
      # PNG speichern
      ggsave(
        filename = file_name,
        plot = p,
        width = 7,
        height = 6,
        dpi = 300,
        bg = "white"
      )
    }
  )
}


# ------------------------------------------------------------------------------
# Plot 5: Anzahl zugeordneter Segmente pro Unfall bei Umkreis-Methode
# ------------------------------------------------------------------------------

if (run_buffer_method) {
  ggsave(
    filename = paste0(
      "export/plot_segmente_pro_unfall_umkreis_",
      segment_analysis_id,
      ".png"
    ),
    plot = plot_segments_per_accident_buffer,
    width = 8,
    height = 6,
    dpi = 300,
    bg = "white"
  )
  write.table(
    segments_per_accident_distribution,
    paste0(
      "export/tables/verteilung_segmente_pro_unfall_umkreis_",
      segment_analysis_id,
      ".csv"
    ),
    sep = ";",
    dec = ".",
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )
}

# ------------------------------------------------------------------------------
# CSV-Export: eine Zeile pro Zählstelle
# ------------------------------------------------------------------------------

# Gesamtmodell vorbereiten
export_total <- station_models_all %>%
  transmute(
    FK_Standort = FK_STANDORT,
    bezeichnung,
    dist_m,
    r_squared_total = r_squared,
    p_total = p_value,
    factor_total = count_factor,
    factor_total_min = count_factor_min,
    factor_total_max = count_factor_max
  )

# Werktag/Wochenende vorbereiten
export_daytype <- station_models_daytype %>%
  mutate(
    day_type_clean = case_when(
      day_type == "Werktag" ~ "werktag",
      day_type == "Wochenende" ~ "wochenende",
      TRUE ~ day_type
    )
  ) %>%
  select(
    FK_STANDORT,
    day_type_clean,
    r_squared,
    p_value,
    count_factor,
    count_factor_min,
    count_factor_max
  ) %>%
  pivot_wider(
    names_from = day_type_clean,
    values_from = c(
      r_squared,
      p_value,
      count_factor,
      count_factor_min,
      count_factor_max
    ),
    names_glue = "{.value}_{day_type_clean}"
  ) %>%
  rename(
    FK_Standort = FK_STANDORT,
    p_werktag = p_value_werktag,
    p_wochenende = p_value_wochenende,
    factor_werktag = count_factor_werktag,
    factor_werktag_min = count_factor_min_werktag,
    factor_werktag_max = count_factor_max_werktag,
    factor_wochenende = count_factor_wochenende,
    factor_wochenende_min = count_factor_min_wochenende,
    factor_wochenende_max = count_factor_max_wochenende
  )

# Sommer/Winter vorbereiten
export_season <- station_models_season %>%
  mutate(
    season_clean = case_when(
      season == "Sommer" ~ "sommer",
      season == "Winter" ~ "winter",
      TRUE ~ season
    )
  ) %>%
  select(
    FK_STANDORT,
    season_clean,
    r_squared,
    p_value,
    count_factor,
    count_factor_min,
    count_factor_max
  ) %>%
  pivot_wider(
    names_from = season_clean,
    values_from = c(
      r_squared,
      p_value,
      count_factor,
      count_factor_min,
      count_factor_max
    ),
    names_glue = "{.value}_{season_clean}"
  ) %>%
  rename(
    FK_Standort = FK_STANDORT,
    p_sommer = p_value_sommer,
    p_winter = p_value_winter,
    factor_sommer = count_factor_sommer,
    factor_sommer_min = count_factor_min_sommer,
    factor_sommer_max = count_factor_max_sommer,
    factor_winter = count_factor_winter,
    factor_winter_min = count_factor_min_winter,
    factor_winter_max = count_factor_max_winter
  )

# Alle Modellresultate zu einer Tabelle zusammenführen
model_summary_export <- export_total %>%
  left_join(export_daytype, by = "FK_Standort") %>%
  left_join(export_season, by = "FK_Standort") %>%
  select(
    FK_Standort,
    bezeichnung,
    dist_m,
    factor_total,
    factor_total_min,
    factor_total_max,
    factor_werktag,
    factor_werktag_min,
    factor_werktag_max,
    factor_wochenende,
    factor_wochenende_min,
    factor_wochenende_max,
    factor_sommer,
    factor_sommer_min,
    factor_sommer_max,
    factor_winter,
    factor_winter_min,
    factor_winter_max,
    r_squared_total,
    r_squared_werktag,
    r_squared_wochenende,
    r_squared_sommer,
    r_squared_winter,
    p_total,
    p_werktag,
    p_wochenende,
    p_sommer,
    p_winter
  ) %>%
  arrange(FK_Standort) %>%
  mutate(
    across(
      where(is.numeric),
      ~ ifelse(is.infinite(.) | is.nan(.), NA, .)
    )
  )

# Als CSV exportieren
write.table(
  model_summary_export,
  "export/tables/modellergebnisse_pro_zaehlstelle.csv",
  sep = ";",
  dec = ".",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# ------------------------------------------------------------------------------
# CSV-Export: globale Faktoren nach Jahreszeit und Tagestyp
# ------------------------------------------------------------------------------

# Die vier globalen Modellfaktoren als Tabelle vorbereiten
global_model_export <- global_models_season_daytype %>%
  transmute(
    season,
    day_type,
    n_observations,
    strava_sum,
    count_sum,
    count_factor,
    count_factor_min,
    count_factor_max,
    r_squared,
    p_value,
    p_value_formatted = format.pval(p_value, digits = 3, eps = 0.001)
  ) %>%
  mutate(
    across(
      where(is.numeric),
      ~ ifelse(is.infinite(.) | is.nan(.), NA, .)
    )
  )

# Als CSV exportieren
write.table(
  global_model_export,
  "export/tables/globalmodelle.csv",
  sep = ";",
  dec = ".",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# ------------------------------------------------------------------------------
# Export der geschätzten Strassennutzung
# ------------------------------------------------------------------------------

# Export als GeoPackage für QGIS
st_write(
  segment_analysis_sf,
  paste0("export/gpkg/geschaetzte_velonutzung_strassennetz_", segment_analysis_id, ".gpkg"),
  delete_dsn = TRUE
)

# Optionaler Export als CSV ohne Geometrie
write.table(
  st_drop_geometry(segment_analysis_sf),
  paste0("export/tables/geschaetzte_velonutzung_strassennetz_", segment_analysis_id, ".csv"),
  sep = ";",
  dec = ".",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# ------------------------------------------------------------------------------
# Export der berechneten Unfallraten (Nearest-Methode)
# ------------------------------------------------------------------------------

# Export als GeoPackage für QGIS
st_write(
  usage_accidents_sf,
  paste0("export/gpkg/unfallrate_100m_pkm_", segment_analysis_id, ".gpkg"),
  delete_dsn = TRUE
)

# Optionaler Export als CSV ohne Geometrie
write.table(
  st_drop_geometry(usage_accidents_sf),
  paste0("export/tables/unfallrate_100m_pkm_", segment_analysis_id, ".csv"),
  sep = ";",
  dec = ".",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# ------------------------------------------------------------------------------
# Histogramm: Distanzen der Unfallzuordnung bei Nearest-Methode
# ------------------------------------------------------------------------------

# Plotdaten vorbereiten:
# Nur Unfälle verwenden, die innerhalb der maximal zulässigen Distanz liegen.
accident_distance_hist_data <- accident_with_segment_clean %>%
  st_drop_geometry() %>%
  filter(
    !is.na(accident_dist_m),
    accident_dist_m <= max_accident_dist_m
  )

# Histogramm erstellen
plot_accident_distance_hist <- ggplot(
  accident_distance_hist_data,
  aes(x = accident_dist_m)
) +
  geom_histogram(
    binwidth = 1,
    boundary = 0
  ) +
  geom_vline(
    xintercept = max_accident_dist_m,
    linetype = "dashed"
  ) +
  labs(
    title = "Distanzen der Unfallzuordnung nach Nearest-Methode",
    subtitle = paste0(
      "Berücksichtigt wurden nur Unfälle bis ",
      max_accident_dist_m,
      " m Distanz zum zugeordneten Segment."
    ),
    x = "Distanz zum zugeordneten Segment [m]",
    y = "Anzahl Unfälle"
  ) +
  theme_minimal()

# Plot exportieren
ggsave(
  filename = paste0(
    "export/plot_distanzen_unfallzuordnung_nearest_",
    max_accident_dist_m,
    "m_",
    segment_analysis_id,
    ".png"
  ),
  plot = plot_accident_distance_hist,
  width = 8,
  height = 6,
  dpi = 300,
  bg = "white"
)

# ------------------------------------------------------------------------------
# Tabelle: Verteilung der Anzahl Unfälle pro Segment
# ------------------------------------------------------------------------------

accidents_per_segment_table <- usage_accidents_sf %>%
  st_drop_geometry() %>%
  filter(accident_count_2020_2024 > 0) %>%
  count(accident_count_2020_2024, name = "n_segments") %>%
  mutate(
    share_percent = n_segments / sum(n_segments) * 100,
    share_percent = round(share_percent, 1)
  ) %>%
  arrange(accident_count_2020_2024) %>%
  rename(
    accidents_per_segment = accident_count_2020_2024
  )

write.table(
  accidents_per_segment_table,
  "export/tables/accidents_per_segment_distribution.csv",
  sep = ";",
  dec = ".",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# ------------------------------------------------------------------------------
# Export der Unfallraten (Umkreis-Methode)
# ------------------------------------------------------------------------------

if (run_buffer_method) {
  
  # Export als GeoPackage für QGIS
  st_write(
    usage_accidents_buffer_sf,
    paste0("export/gpkg/unfallrate_umkreis_100m_pkm_", segment_analysis_id, ".gpkg"),
    delete_dsn = TRUE
  )
  
  # Optionaler Export als CSV ohne Geometrie
  write.table(
    st_drop_geometry(usage_accidents_buffer_sf),
    paste0("export/tables/unfallrate_umkreis_100m_pkm_", segment_analysis_id, ".csv"),
    sep = ";",
    dec = ".",
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )
  
  # Hotspots der Umkreis-Methode exportieren
  hotspots_buffer_export <- usage_accidents_buffer_sf %>%
    st_drop_geometry() %>%
    filter(is_hotspot_buffer) %>%
    arrange(desc(accident_rate_buffer_per_100m_pkm))
  
  write.table(
    hotspots_buffer_export,
    paste0("export/tables/hotspots_umkreis_100m_pkm_", segment_analysis_id, ".csv"),
    sep = ";",
    dec = ".",
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )
  
  # Hotspot-Definition der Umkreis-Methode exportieren
  hotspot_definition_buffer_export <- data.frame(
    segment_analysis_id = segment_analysis_id,
    hotspot_min_accidents = hotspot_min_accidents,
    hotspot_rate_quantile = hotspot_rate_quantile,
    hotspot_min_pkm_quantile = hotspot_min_pkm_quantile,
    hotspot_min_pkm_threshold_buffer = hotspot_min_pkm_threshold_buffer,
    hotspot_rate_threshold_buffer = hotspot_rate_threshold_buffer,
    n_hotspots_buffer = sum(usage_accidents_buffer_sf$is_hotspot_buffer, na.rm = TRUE)
  )
  
  write.table(
    hotspot_definition_buffer_export,
    paste0("export/tables/hotspot_definition_umkreis_", segment_analysis_id, ".csv"),
    sep = ";",
    dec = ".",
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )
}

# ------------------------------------------------------------------------------
# Kontrolltabelle: Unfallrate pro 100 Mio. Personenkilometer
# ------------------------------------------------------------------------------

accident_rate_pkm_check <- usage_accidents_sf %>%
  st_drop_geometry() %>%
  select(
    segment_id_type,
    segment_id,
    edgeUID,
    osmId,
    segment_length_km,
    est_bikes_year,
    pkm_year,
    accident_count_2020_2024,
    accident_count_per_year,
    accident_rate_per_100m_pkm,
    accident_rate_per_100m_pkm_min,
    accident_rate_per_100m_pkm_max
  ) %>%
  arrange(desc(accident_rate_per_100m_pkm))

write.table(
  accident_rate_pkm_check,
  paste0("export/tables/kontrolle_unfallrate_100m_pkm_", segment_analysis_id, ".csv"),
  sep = ";",
  dec = ".",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# ==============================================================================
# Zusätzliche Tabellenexporte für den Ergebnisteil der Arbeit
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. Kompakte Kennwert-Tabelle
# ------------------------------------------------------------------------------

results_metrics <- data.frame(
  section = c(
    "Datengrundlage",
    "Kalibrierung",
    "Kalibrierung",
    "Unfälle",
    "Unfälle",
    "Unfälle",
    "Unfallrate",
    "Unfallrate",
    "Velonutzung",
    "Velonutzung",
    "Hotspots",
    "Hotspots",
    "Hotspots",
    "Hotspots"
  ),
  metric = c(
    "Anzahl Strava-Segmente im Untersuchungsgebiet",
    "Anzahl verwendete Zählstellen nach Bereinigung",
    "Anzahl ausgeschlossene Zählstellen",
    "Anzahl Velounfälle 2020–2024 im Stadtgebiet Zürich",
    "Anzahl wegen Distanzschwelle ausgeschlossene Unfallpunkte",
    "Anzahl Segmente mit mindestens einem Unfall",
    "Anzahl Segmente mit gültiger Unfallrate",
    "Anzahl Segmente mit positiver Unfallrate",
    "Geschätzte Velofahrten 2025 total",
    "Geschätzte Personenkilometer 2025 total",
    "Mindestanzahl Unfälle für Hotspots",
    "Mindestexposition: 25-%-Quantil der Personenkilometer",
    "Schwellenwert: 95-%-Quantil der positiven Unfallraten",
    "Anzahl Hotspot-Segmente"
  ),
  value = c(
    nrow(strava_shape_2056),
    length(valid_station_ids),
    nrow(removed_station_ids),
    nrow(accident_filtered_sf),
    nrow(accident_with_segment_removed),
    sum(usage_accidents_sf$accident_count_2020_2024 > 0, na.rm = TRUE),
    sum(
      !is.na(usage_accidents_sf$pkm_year) &
        usage_accidents_sf$pkm_year > 0 &
        !is.na(usage_accidents_sf$accident_rate_per_100m_pkm) &
        is.finite(usage_accidents_sf$accident_rate_per_100m_pkm),
      na.rm = TRUE
    ),
    sum(
      !is.na(usage_accidents_sf$pkm_year) &
        usage_accidents_sf$pkm_year > 0 &
        !is.na(usage_accidents_sf$accident_rate_per_100m_pkm) &
        is.finite(usage_accidents_sf$accident_rate_per_100m_pkm) &
        usage_accidents_sf$accident_rate_per_100m_pkm > 0,
      na.rm = TRUE
    ),
    sum(usage_accidents_sf$est_bikes_year, na.rm = TRUE),
    sum(usage_accidents_sf$pkm_year, na.rm = TRUE),
    hotspot_min_accidents,
    hotspot_min_pkm_threshold,
    hotspot_rate_threshold,
    sum(usage_accidents_sf$is_hotspot, na.rm = TRUE)
  ),
  unit = c(
    "Segmente",
    "Zählstellen",
    "Zählstellen",
    "Unfälle",
    "Unfälle",
    "Segmente",
    "Segmente",
    "Segmente",
    "Velofahrten/Jahr",
    "Personenkilometer/Jahr",
    "Unfälle",
    "Personenkilometer/Jahr",
    "Unfälle pro 100 Mio. Personenkilometer",
    "Segmente"
  )
)

write.table(
  results_metrics,
  "export/tables/results_metrics.csv",
  sep = ";",
  dec = ".",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# ------------------------------------------------------------------------------
# 2. Hotspot-Tabelle
# ------------------------------------------------------------------------------

# Vollständiger Datensatz ohne Geometrie
usage_accidents_table <- usage_accidents_sf %>%
  st_drop_geometry()

# Relevante Spalten für den Export.
# any_of() ignoriert Spalten automatisch, falls sie im Datensatz nicht vorhanden sind.
hotspot_columns <- c(
  "segment_id_type",
  "segment_id",
  "edgeUID",
  "osmId",
  "strava_trips_year",
  "est_bikes_year",
  "segment_length_km",
  "pkm_year",
  "accident_count_2020_2024",
  "accident_count_per_year",
  "accident_rate_per_100m_pkm",
  "accident_rate_per_100m_pkm_min",
  "accident_rate_per_100m_pkm_max",
  "valid_hotspot_rate",
  "is_hotspot",
  
  # Falls solche Attribute später im Datensatz vorhanden sind
  "name",
  "strassenname",
  "Strassenname",
  "street_name",
  "road_name",
  "strassentyp",
  "Strassentyp",
  "road_type",
  "RoadType",
  "RoadType_de",
  "highway"
)

# Vollständige Tabelle aller Hotspot-Segmente
hotspots_full <- usage_accidents_table %>%
  filter(is_hotspot) %>%
  arrange(desc(accident_rate_per_100m_pkm)) %>%
  select(any_of(hotspot_columns))

write.table(
  hotspots_full,
  "export/tables/hotspots_full.csv",
  sep = ";",
  dec = ".",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# ------------------------------------------------------------------------------
# 3. Hotspot-Definition dokumentieren
# ------------------------------------------------------------------------------

hotspot_definition_export <- data.frame(
  segment_analysis_id = segment_analysis_id,
  hotspot_min_accidents = hotspot_min_accidents,
  hotspot_rate_quantile = hotspot_rate_quantile,
  hotspot_min_pkm_quantile = hotspot_min_pkm_quantile,
  hotspot_min_pkm_threshold = hotspot_min_pkm_threshold,
  hotspot_rate_threshold = hotspot_rate_threshold,
  n_hotspots = sum(usage_accidents_sf$is_hotspot, na.rm = TRUE)
)

write.table(
  hotspot_definition_export,
  paste0("export/tables/hotspot_definition_", segment_analysis_id, ".csv"),
  sep = ";",
  dec = ".",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# ==============================================================================
# Tabellenexporte für den Anhang der Arbeit
# ==============================================================================

# ------------------------------------------------------------------------------
# Anhang: Vergleich der Unfallzuordnungsmethoden
# ------------------------------------------------------------------------------

# Hinweis:
# Für den Methodenvergleich muss die Umkreis-Methode im Hauptskript ausgeführt
# worden sein, damit accident_segment_relation vorhanden ist.
if (!run_buffer_method || !exists("accident_segment_relation")) {
  stop(
    "Für den Methodenvergleich muss in 00_setup.R run_buffer_method = TRUE gesetzt sein, ",
    "damit die Umkreis-Methode berechnet und exportiert werden kann."
  )
}

# Gemeinsame Ausgangsmenge: alle Unfallpunkte im Untersuchungsgebiet.
# Die stabile ID basiert auf der Zeilenreihenfolge nach der räumlichen Filterung.
# accident_segment_relation$accident_index bezieht sich auf dieselbe Reihenfolge.
accident_assignment_base <- accident_filtered_sf %>%
  st_drop_geometry() %>%
  mutate(accident_id_method = row_number()) %>%
  select(accident_id_method)

n_accidents_total_assignment <- nrow(accident_assignment_base)

# ------------------------------------------------------------------------------
# Nearest-Methode: pro Unfall höchstens ein Segment
# ------------------------------------------------------------------------------

# Für jeden Unfall wird geprüft, ob die Zuordnung innerhalb der Distanzschwelle liegt.
# Gültige Zuordnung = 1 Segment, sonst 0 Segmente.
nearest_assignment_counts <- accident_with_segment %>%
  st_drop_geometry() %>%
  mutate(
    accident_id_method = row_number(),
    anzahl_segmente_pro_unfall = ifelse(
      !is.na(accident_dist_m) & accident_dist_m <= max_accident_dist_m,
      1L,
      0L
    )
  ) %>%
  select(accident_id_method, anzahl_segmente_pro_unfall)

nearest_unfaelle_zugeordnet <- sum(
  nearest_assignment_counts$anzahl_segmente_pro_unfall > 0,
  na.rm = TRUE
)

nearest_zuordnungen_total <- sum(
  nearest_assignment_counts$anzahl_segmente_pro_unfall,
  na.rm = TRUE
)

nearest_mehrfach_zugeordnet <- sum(
  nearest_assignment_counts$anzahl_segmente_pro_unfall > 1,
  na.rm = TRUE
)

# ------------------------------------------------------------------------------
# Umkreis-Methode: pro Unfall können mehrere Segmente im Buffer liegen
# ------------------------------------------------------------------------------

# Anzahl Segmentzuordnungen pro Unfall zählen.
# Unfälle ohne Segment im 5-m-Umkreis werden danach mit 0 ergänzt.
buffer_assignment_counts <- accident_segment_relation %>%
  mutate(accident_id_method = accident_index) %>%
  count(accident_id_method, name = "anzahl_segmente_pro_unfall") %>%
  right_join(accident_assignment_base, by = "accident_id_method") %>%
  mutate(
    anzahl_segmente_pro_unfall = replace_na(anzahl_segmente_pro_unfall, 0L)
  ) %>%
  arrange(accident_id_method)

buffer_unfaelle_zugeordnet <- sum(
  buffer_assignment_counts$anzahl_segmente_pro_unfall > 0,
  na.rm = TRUE
)

buffer_zuordnungen_total <- sum(
  buffer_assignment_counts$anzahl_segmente_pro_unfall,
  na.rm = TRUE
)

buffer_mehrfach_zugeordnet <- sum(
  buffer_assignment_counts$anzahl_segmente_pro_unfall > 1,
  na.rm = TRUE
)

# ------------------------------------------------------------------------------
# Tabelle 1: Methodenvergleich
# ------------------------------------------------------------------------------

unfallzuordnung_methodenvergleich <- data.frame(
  methode = c("Nearest", "Umkreis"),
  prinzip = c(
    "Zuordnung zum nächstgelegenen Segment",
    "Zuordnung zu allen Segmenten innerhalb des Umkreises"
  ),
  distanzregel = c(
    paste0("nächstgelegenes Segment, max. ", max_accident_dist_m, " m"),
    paste0("Segmente innerhalb ", buffer_accident_dist_m, " m")
  ),
  unfaelle_gesamt = c(
    n_accidents_total_assignment,
    n_accidents_total_assignment
  ),
  unfaelle_zugeordnet = c(
    nearest_unfaelle_zugeordnet,
    buffer_unfaelle_zugeordnet
  ),
  unfaelle_ausgeschlossen = c(
    n_accidents_total_assignment - nearest_unfaelle_zugeordnet,
    n_accidents_total_assignment - buffer_unfaelle_zugeordnet
  ),
  zuordnungen_total = c(
    nearest_zuordnungen_total,
    buffer_zuordnungen_total
  ),
  mehrfach_zugeordnete_unfaelle = c(
    nearest_mehrfach_zugeordnet,
    buffer_mehrfach_zugeordnet
  ),
  bemerkung = c(
    "Hauptmethode; jeder Unfall wird höchstens einem Segment zugeordnet.",
    "Vergleichsmethode; ein Unfall kann mehreren Segmenten zugeordnet werden."
  )
)

write.table(
  unfallzuordnung_methodenvergleich,
  "export/tables/anhang_unfallzuordnung_methodenvergleich.csv",
  sep = ";",
  dec = ".",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# ------------------------------------------------------------------------------
# Tabelle 2: Verteilung der Anzahl Segmentzuordnungen pro Unfall
# ------------------------------------------------------------------------------

unfallzuordnung_mehrfachzuordnungen <- bind_rows(
  nearest_assignment_counts %>%
    count(anzahl_segmente_pro_unfall, name = "anzahl_unfaelle") %>%
    mutate(methode = "Nearest"),
  buffer_assignment_counts %>%
    count(anzahl_segmente_pro_unfall, name = "anzahl_unfaelle") %>%
    mutate(methode = "Umkreis")
) %>%
  select(
    methode,
    anzahl_segmente_pro_unfall,
    anzahl_unfaelle
  ) %>%
  arrange(
    factor(methode, levels = c("Nearest", "Umkreis")),
    anzahl_segmente_pro_unfall
  )

write.table(
  unfallzuordnung_mehrfachzuordnungen,
  "export/tables/anhang_unfallzuordnung_mehrfachzuordnungen.csv",
  sep = ";",
  dec = ".",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
