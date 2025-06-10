library(sf)
library(dplyr)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)

# 1) Lecture brute et filtrage sur MMSI = 67895
df <- read.csv("vessel-total-clean-final.csv", stringsAsFactors = FALSE) %>%
  filter(mmsi == 368063310)    # ou, si vos noms de colonnes sont passés en minuscules : filter(mmsi == 67895)

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
cat("Total points (MMSI 67895) :", nrow(df),      "\n")
cat("Points dans le bbox       :", nrow(pts_golfe), "\n")

# 6) Tracé simple pour vérifier le cadre
ggplot() +
  geom_sf(data = coast_golfe, fill = "grey30", color = "grey60") +
  geom_sf(data = pts_golfe,  colour = "red",    size  = 0.8) +
  coord_sf(
    xlim   = c(xmin0 - marge, xmax0 + marge),
    ylim   = c(ymin0 - marge, ymax0 + marge),
    expand = FALSE
  ) +
  theme_void() +
  theme(panel.background = element_rect(fill = "lightblue"))