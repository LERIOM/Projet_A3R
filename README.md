# AIS Vessel Data Analysis in R: Gulf of Mexico

> Cleaning, exploring and modeling AIS ship-tracking data in R: maps, trajectories, port detection, correlations and a vessel-type classifier.

![R](https://img.shields.io/badge/R-276DC3?logo=r&logoColor=white)
![ggplot2](https://img.shields.io/badge/ggplot2-data%20viz-blue)
![sf](https://img.shields.io/badge/sf-geospatial-green)

Part 1 of a 3-part ISEN project (3rd year) on vessel data, followed by
[Projet_A3IA](https://github.com/LERIOM/Projet_A3IA) (machine learning) and
[Projet_webA3](https://github.com/LERIOM/Projet_webA3) (web app).

## Data

AIS (Automatic Identification System) messages from ships in the Gulf of Mexico: position, speed over ground (`sog`), course (`cog`), heading, vessel type, size, draft and transceiver class.

- `DossierdedonnéesBigDataNantes.csv`: raw dataset
- `vessel-total-clean-final.csv`: cleaned dataset used by every script
- `DossierdedonnéesVesselType.txt`: AIS vessel-type codes

## Analyses

| Script | What it does |
|---|---|
| `CleaningData.R` | Cleans the raw file: column names, duplicates, missing values, types |
| `histogramme.R` | Univariate distributions |
| `bivarie_donnees.R` | Bivariate analysis: width vs length, mosaic plots by vessel type and transceiver class |
| `correlation.R` | Correlation matrix of numeric variables |
| `Carte.R` | Map of AIS positions over the Gulf of Mexico |
| `BoatTrajectory.R` | Vessel trajectories on a map |
| `Port.R` | Port detection by clustering stopped vessels with DBSCAN |
| `regression_logistique.R` | Multinomial logistic regression predicting vessel type from width, length and draft |

## Getting started

Open `Projet_A3R.Rproj` in RStudio and install the packages used by the scripts:

```r
install.packages(c("dplyr", "tidyr", "stringr", "janitor", "readr",
                   "ggplot2", "sf", "rnaturalearth", "rnaturalearthdata",
                   "viridis", "dbscan", "corrplot", "vcd", "nnet"))
```

The cleaned CSV is already included, so any analysis script can run directly; `CleaningData.R` shows how it was produced.
