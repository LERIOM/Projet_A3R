library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(viridis)
library(dplyr)

# 1) Lecture brute
df <- read.csv("vessel-total-clean-final.csv", stringsAsFactors = FALSE)

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

# 4) Crop
pts_golfe  <- st_crop(pts, golfe_bbox)
coast      <- ne_coastline(scale = "medium", returnclass = "sf")
coast_golfe<- st_crop(coast, golfe_bbox)

land <- ne_countries(scale = "medium", returnclass = "sf")

# 2) Rogner sur votre bbox
land_golfe <- st_crop(land, golfe_bbox)

# 3) Tracé avec terre remplie
ggplot() +
  # la « terre » en (229,229,229)
  geom_sf(
    data  = land_golfe,
    fill  = "#E5E5E5",         # équivalent hex de rgb(229,229,229)
    color = NA                 # pas de bordure pour le polygone
  ) +
  # la côte (facultatif, pour un contour léger)
  geom_sf(
    data  = coast_golfe,
    fill  = NA,
    color = "grey60",
    size  = 0.3
  ) +
  # vos points colorés par MMSI
  geom_sf(
    data    = pts_golfe,
    mapping = aes(color = factor(vessel_type)),
    size    = 0.5
  ) +
  scale_color_viridis_d(direction = 1) +
  coord_sf(
    xlim   = c(xmin0 - marge, xmax0 + marge),
    ylim   = c(ymin0 - marge, ymax0 + marge),
    expand = FALSE
  ) +
  theme_void() +
  theme(
    legend.position  = "right"
  )+
  guides(
    color = guide_legend(
      override.aes = list(size = 4)  # ici on grossit les points dans la légende
    )
  )
