library(sf)
library(dplyr)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)

idm <- "248392000" # MMSI du bateau

# 1) Lecture brute et filtrage sur MMSI

df <- read.csv("vessel-total-clean-final.csv", stringsAsFactors = FALSE) %>% 
  filter(mmsi == idm)
cat("Nombre de points après filtrage :", nrow(df), "\n")

# 2) Conversion en sf
pts <- st_as_sf(df, coords = c("lon","lat"), crs = 4326)

# 3) Définition d’un bbox « agrandi » de ±2° autour du Golfe
xmin0 <- -98; xmax0 <- -80; ymin0 <- 18; ymax0 <- 30
marge  <- 2
golfe_bbox <- st_as_sfc(
  st_bbox(c(xmin = xmin0 - marge,
            xmax = xmax0 + marge,
            ymin = ymin0 - marge,
            ymax = ymax0 + marge),
          crs = st_crs(4326))
)

# 4) Crop des données
pts_golfe   <- st_crop(pts,        golfe_bbox)
coast       <- ne_coastline(scale = "medium", returnclass = "sf")
coast_golfe <- st_crop(coast,      golfe_bbox)
land        <- ne_countries(scale = "medium", returnclass = "sf")
land_golfe  <- st_crop(land,       golfe_bbox)

# 5) Diagnostics
cat("Total points (MMSI", id,") :", nrow(df),      "\n")
cat("Points dans le bbox       :", nrow(pts_golfe), "\n")

# 6) Tracé final avec style appliqué
ggplot() +
  #  Terre remplie en #E5E5E5
  geom_sf(
    data  = land_golfe,
    fill  = "#E5E5E5",
    color = NA
  ) +
  #  Ligne de côte légère
  geom_sf(
    data  = coast_golfe,
    fill  = NA,
    color = "grey60",
    size  = 0.3
  ) +
  #  Points du bateau
  geom_sf(
    data  = pts_golfe,
    color = "red",
    size  = 0.7
  ) +
  coord_sf(
    xlim   = c(xmin0 - marge, xmax0 + marge),
    ylim   = c(ymin0 - marge, ymax0 + marge),
    expand = FALSE
  ) +
  theme_void() +
  theme(
    legend.position  = "none"
  )