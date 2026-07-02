# ==============================================================================
# Daten einlesen
# ==============================================================================

# Gemeindegrenzen einlesen
municipalities_sf <- st_read(
  "data/grenze/swissBOUNDARIES3D_1_5_LV95_LN02.gpkg",
  layer = "tlm_hoheitsgebiet"
) %>%
  st_transform(crs_lv95)

# Stadt Zürich auswählen
city_zurich_sf <- municipalities_sf %>%
  filter(
    bfs_nummer == study_area_bfs,
    name == study_area_name
  ) %>%
  st_make_valid()

# Strava-Geometrie einlesen
strava_shape_2056 <- st_read("data/strava/strava_2025-05-01-2025-07-31.shp") %>%
  st_transform(crs_lv95) %>%
  mutate(edgeUID = as.numeric(edgeUID)) %>%
  select(edgeUID, osmId)

# Strava-Geometrie auf Stadtgebiet Zürich zuschneiden
# Warnung bzgl. Zerteilung der Liniensegmente ignorieren
strava_shape_2056 <- suppressWarnings(
  st_intersection(strava_shape_2056, city_zurich_sf)
) %>%
  select(edgeUID, osmId)

# Strava-CSVs einlesen
# fread ist schneller als read.csv
strava_files <- c("data/strava/strava_2025-01-01-2025-03-31.csv", 
                  "data/strava/strava_2025-04-01-2025-05-31.csv",
                  "data/strava/strava_2025-05-01-2025-07-31.csv",
                  "data/strava/strava_2025-08-01-2025-10-31.csv",
                  "data/strava/strava_2025-11-01-2025-12-31.csv")

# CSVs zusammenführen und Dubletten im Mai entfernen
strava_table_all <- map_df(
  strava_files,
  ~ fread(.x) %>%
    mutate(date = as.Date(date))
) %>%
  distinct(edge_uid, date, .keep_all = TRUE)

# EdgeUIDs innerhalb der Stadtgrenzen
valid_city_edge_ids <- strava_shape_2056 %>%
  st_drop_geometry() %>%
  pull(edgeUID)

# Nur Strava-Daten für Segmente innerhalb der Stadt Zürich behalten
strava_table_all <- strava_table_all %>%
  filter(edge_uid %in% valid_city_edge_ids)

# Zählstellen-Daten einlesen
count_table <- read.csv("data/zaehlung/verkehrszaehlungen_2025.csv") %>% 
  mutate(count_day = as.Date(DATUM))

count_metadata <- read.csv("data/zaehlung/standorte_verkehrszaehlstellen.csv") %>%
  select(id1, bezeichnung)

# Unfall-Daten einlesen
# Definition der für die Analyse benötigten Spalten
accident_columns <- c(
  "AccidentUID",
  "AccidentType",
  "AccidentType_de",
  "AccidentSeverityCategory",
  "AccidentSeverityCategory_de",
  "AccidentInvolvingBicycle",
  "RoadType",
  "RoadType_de",
  "AccidentLocation_CHLV95_E",
  "AccidentLocation_CHLV95_N",
  "CantonCode",
  "MunicipalityCode",
  "AccidentYear",
  "AccidentMonth",
  "AccidentMonth_de",
  "AccidentWeekDay",
  "AccidentWeekDay_de",
  "AccidentHour_text")

# Einlesen der benötigten Spalten
accident_table <- fread(
  "data/unfall/RoadTrafficAccidentLocations.csv",
  select = accident_columns)