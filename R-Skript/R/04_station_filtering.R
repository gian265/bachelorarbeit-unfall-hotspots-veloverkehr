# ==============================================================================
# Filterung gültiger Zählstellen
# ==============================================================================

# ------------------------------------------------------------------------------
# Übersicht pro Zählstelle erstellen
# ------------------------------------------------------------------------------

# Für jede Zählstelle werden zentrale Kennwerte berechnet:
# - nur basierend auf Tagen, an denen Strava- und auch Zähldaten vorhanden sind
# - Gesamtanzahl Strava-Trips auf diesen gemeinsamen Tagen
# - Gesamtanzahl gezählte Velos auf diesen gemeinsamen Tagen
# - Anzahl Tage mit vollständigen Daten (beide Quellen vorhanden)
# - Zeitraum der gemeinsamen Datenverfügbarkeit
station_summary <- count_strava_join %>%
  filter(has_strava_data) %>%
  group_by(FK_STANDORT, bezeichnung) %>%
  summarise(
    strava_sum = sum(strava_day_total, na.rm = TRUE),
    count_sum = sum(count_day_total, na.rm = TRUE),
    n_days = n_distinct(count_day),
    first_count_day = min(count_day, na.rm = TRUE),
    last_count_day  = max(count_day, na.rm = TRUE),
    strava_mean_per_day = mean(strava_day_total, na.rm = TRUE),
    .groups = "drop"
  )

# ------------------------------------------------------------------------------
# Gültige Zählstellen definieren
# ------------------------------------------------------------------------------

# Bedingungen:
# - genügend Strava-Daten vorhanden
# - Zähldaten vorhanden
# - genügend Tage vorhanden (wichtig für Teiljahres-Zählstellen)
# - nicht manuell ausgeschlossen
valid_station_ids <- station_summary %>%
  filter(
    strava_sum > min_strava_trips,
    count_sum > 0,
    n_days >= min_count_days,
    !FK_STANDORT %in% manual_exclude_station_ids
  ) %>%
  pull(FK_STANDORT)

# ------------------------------------------------------------------------------
# Entfernte Zählstellen identifizieren und begründen
# ------------------------------------------------------------------------------

# Alle Zählstellen, die die Kriterien nicht erfüllen, werden hier erfasst
# Zusätzlich wird dokumentiert, warum sie entfernt wurden
removed_station_ids <- station_summary %>%
  mutate(
    reason = case_when(
      FK_STANDORT %in% manual_exclude_station_ids ~ "manuell ausgeschlossen: Zuordnung unsicher",
      count_sum == 0 ~ "keine Zähldaten enthalten",
      n_days < min_count_days ~ "zu wenige Zähltage enthalten",
      strava_sum <= min_strava_trips ~ "zu wenig Strava-Daten enthalten",
      TRUE ~ "ok"
    )
  ) %>%
  filter(reason != "ok")

# Ergebnis anzeigen 
list(removed_station_ids)

# ------------------------------------------------------------------------------
# Datensatz für Modellierung filtern
# ------------------------------------------------------------------------------

# Nur gültige Zählstellen und Tage mit vollständigen Daten (Strava + Zählstelle)
# für die weitere Modellierung und Visualisierung behalten
data_model <- count_strava_join %>%
  filter(
    FK_STANDORT %in% valid_station_ids,
    has_strava_data
  )