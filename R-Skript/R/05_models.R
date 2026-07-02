# ==============================================================================
# Modellierung
# ==============================================================================

# ------------------------------------------------------------------------------
# Gesamtmodell pro Zählstelle
# ------------------------------------------------------------------------------

# Für jede Zählstelle wird ein lineares Modell ohne Intercept berechnet.
# Modellform: gezählte Velos pro Tag = Faktor * Strava-Aktivitäten pro Tag
station_models_all <- data_model %>%
  group_by(FK_STANDORT, bezeichnung, dist_m) %>%
  nest() %>%
  mutate(
    # Modell pro Zählstelle berechnen
    model = map(data, ~ lm(count_day_total ~ strava_day_total - 1, data = .x)),
    # R-Squared und Faktoren extrahieren
    r_squared = map_dbl(model, ~ summary(.x)$r.squared),
    p_value = map_dbl(model, get_p_value),
    count_factor = map_dbl(model, ~ coef(.x)[1]),
    # 80 %-Konfidenzintervall des Hochrechnungsfaktors
    factor_ci = map(model, get_factor_ci),
    count_factor_min = map_dbl(factor_ci, ~ .x[1]),
    count_factor_max = map_dbl(factor_ci, ~ .x[2])
  ) %>%
  ungroup()

# ------------------------------------------------------------------------------
# Modelle pro Zählstelle und Tagestyp
# ------------------------------------------------------------------------------

# Die Daten werden zusätzlich nach Werktag und Wochenende getrennt.
station_models_daytype <- data_model %>%
  group_by(FK_STANDORT, bezeichnung, dist_m, day_type) %>%
  nest() %>%
  mutate(
    # Modell pro Zählstelle und Tagestyp berechnen
    model = map(data, ~ lm(count_day_total ~ strava_day_total - 1, data = .x)),
    # R-Squared und Faktoren extrahieren
    r_squared = map_dbl(model, ~ summary(.x)$r.squared),
    p_value = map_dbl(model, get_p_value),
    count_factor = map_dbl(model, ~ coef(.x)[1]),
    # 10–90 %-Konfidenzintervall des Hochrechnungsfaktors
    factor_ci = map(model, get_factor_ci),
    count_factor_min = map_dbl(factor_ci, ~ .x[1]),
    count_factor_max = map_dbl(factor_ci, ~ .x[2])
  ) %>%
  ungroup()

# ------------------------------------------------------------------------------
# Modelle pro Zählstelle und Jahreszeit
# ------------------------------------------------------------------------------

# Die Daten werden zusätzlich nach Sommer und Winter getrennt.
station_models_season <- data_model %>%
  group_by(FK_STANDORT, bezeichnung, dist_m, season) %>%
  nest() %>%
  mutate(
    # Modell pro Zählstelle und season berechnen
    model = map(data, ~ lm(count_day_total ~ strava_day_total - 1, data = .x)),
    # R-Squared und Faktoren extrahieren
    r_squared = map_dbl(model, ~ summary(.x)$r.squared),
    p_value = map_dbl(model, get_p_value),
    count_factor = map_dbl(model, ~ coef(.x)[1]),
    # 10–90 %-Konfidenzintervall des Hochrechnungsfaktors
    factor_ci = map(model, get_factor_ci),
    count_factor_min = map_dbl(factor_ci, ~ .x[1]),
    count_factor_max = map_dbl(factor_ci, ~ .x[2])
  ) %>%
  ungroup()

# ------------------------------------------------------------------------------
# Globale Modelle nach Jahreszeit und Tagestyp
# ------------------------------------------------------------------------------

# Im Unterschied zu den vorherigen Modellen werden hier nicht einzelne 
# Zählstellen separat modelliert, sondern alle gültigen Tageswerte aller 
# Zählstellen gemeinsam verwendet.

# Es werden vier globale Faktoren berechnet:
# - Sommer / Werktag
# - Sommer / Wochenende
# - Winter / Werktag
# - Winter / Wochenende
global_models_season_daytype <- data_model %>%
  group_by(season, day_type) %>%
  nest() %>%
  mutate(
    # Globales Modell pro Kombination aus Jahreszeit und Tagestyp
    model = map(data, ~ lm(count_day_total ~ strava_day_total - 1, data = .x)),
    # Modellkennwerte extrahieren
    r_squared = map_dbl(model, ~ summary(.x)$r.squared),
    p_value = map_dbl(model, get_p_value),
    count_factor = map_dbl(model, ~ coef(.x)[1]),
    # 10–90 %-Konfidenzintervall des Hochrechnungsfaktors
    factor_ci = map(model, get_factor_ci),
    count_factor_min = map_dbl(factor_ci, ~ .x[1]),
    count_factor_max = map_dbl(factor_ci, ~ .x[2]),
    # Zusatzinformationen zur Interpretation
    n_observations = map_int(data, nrow),
    strava_sum = map_dbl(data, ~ sum(.x$strava_day_total, na.rm = TRUE)),
    count_sum = map_dbl(data, ~ sum(.x$count_day_total, na.rm = TRUE))
  ) %>%
  ungroup() %>%
  arrange(season, day_type)

# ------------------------------------------------------------------------------
# Globale Kalibrierungsfaktoren vorbereiten
# ------------------------------------------------------------------------------

# Die globalen Faktoren werden aus den vier Modellen nach Jahreszeit und Tagestyp
# übernommen. Der count_factor entspricht dabei:
# gezählte Velos pro Strava-Trip
calibration_factors <- global_models_season_daytype %>%
  select(
    season,
    day_type,
    count_factor,
    count_factor_min,
    count_factor_max
  ) %>%
  mutate(
    across(
      c(count_factor, count_factor_min, count_factor_max),
      ~ ifelse(is.infinite(.) | is.nan(.), NA, .)
    )
  )
