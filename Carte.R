
install.packages("scales")
install.packages("ggplot2")
install.packages("sf")
install.packages("rnaturalearth")
install.packages("viridis")
install.packages("dplyr")
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

# 5) (Optionnel) Diagnostics
cat("Total points      :", nrow(df),      "\n")
cat("Points gardés     :", nrow(pts_golfe), "\n")

# 6) Tracé simple pour vérifier le cadre
ggplot() +
  geom_sf(data = world, fill = "grey90", color = "grey50") +
  geom_sf(
    data    = pts_golfe,
    mapping = aes(color = factor(vessel_type)),
    size    = 0.5
  ) +
  scale_color_viridis_d(
    name      = "VesselType",
    option    = "D",      # ou "C", "E", selon la palette
    direction = 1
  ) +
  coord_sf(
    xlim   = c(xmin0 - marge, xmax0 + marge),
    ylim   = c(ymin0 - marge, ymax0 + marge),
    expand = FALSE
  ) +
  theme_void() 