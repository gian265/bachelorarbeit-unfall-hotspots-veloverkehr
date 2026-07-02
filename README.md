# Bachelorarbeit: Identifikation von Unfall-Hotspots im Veloverkehr

Dieses Repository enthält die R-Skripte zur Datenaufbereitung, Modellierung und Analyse der Bachelorarbeit «Identifikation von Unfall-Hotspots im Veloverkehr».

## Hinweise zu den Daten

Die verwendeten Rohdaten und die vom R-Skript erstellten Exportdateien sind nicht Bestandteil dieses Repositories. 
Insbesondere die Strava-Daten unterliegen Nutzungsbeschränkungen und dürfen nicht veröffentlicht oder an Dritte weitergegeben werden.

Die Skripte setzen voraus, dass die benötigten Daten lokal in der im Projekt verwendeten Ordnerstruktur abgelegt sind.

## Lokale Datenstruktur

Im Ordner `data` wurden die verwendeten Datensätze lokal in folgender Struktur abgelegt:

```text
R-Skript/data
├── grenze
│   └── swissBOUNDARIES3D_1_5_LV95_LN02.gpkg
├── strava
│   ├── strava_2025-01-01-2025-03-31.csv
│   ├── strava_2025-01-01-2025-03-31.shp
│   ├── strava_2025-04-01-2025-05-31.csv
│   ├── strava_2025-04-01-2025-05-31.shp
│   ├── strava_2025-05-01-2025-07-31.csv
│   ├── strava_2025-05-01-2025-07-31.shp
│   ├── strava_2025-08-01-2025-10-31.csv
│   ├── strava_2025-08-01-2025-10-31.shp
│   ├── strava_2025-11-01-2025-12-31.csv
│   └── strava_2025-11-01-2025-12-31.shp
├── unfall
│   └── RoadTrafficAccidentLocations.csv
└── zaehlung
    ├── standorte_verkehrszaehlstellen.csv
    └── verkehrszaehlungen_2025.csv
```
		
Die Daten selbst sind nicht Bestandteil dieses Repositories.
Die Links zu den genutzten und veröffentlichten Datensätzen sind im Anhang der Bachelorarbeit aufgeführt.

## Hinweise zur Reproduktion

Die Analyse wurde mit R erstellt. 
Die Skripte verarbeiten die lokal abgelegten Rohdaten, führen die Modellierung der Hochrechnungsfaktoren durch, ordnen die Unfälle den Strava-Segmenten zu und exportieren die für die Bachelorarbeit verwendeten Tabellen, Karten- und Ergebnisdaten.

Die Quarto-Dateien der schriftlichen Arbeit sind nicht Bestandteil dieses Repositories, sondern werden separat als Anhang abgegeben.