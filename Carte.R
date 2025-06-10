library(sf)
library(dplyr)

# 1) Lecture brute
df <- read.csv("DossierdedonnéesBigDataNantes.csv", stringsAsFactors = FALSE)

# Nombre initial de points
n_total <- nrow(df)

# 2) Conversion en sf
pts <- st_as_sf(df, coords = c("LON","LAT"), crs = 4326)

# S’il y a des NA dans la géométrie, ce sont des points mal lus
n_na_geom <- sum(is.na(st_is_empty(pts)))  # ou sum(st_is_empty(pts))

# 3) Recadrage sur le Golfe
golfe_bbox <- st_as_sfc(st_bbox(c(xmin = -98, xmax = -80, ymin = 18, ymax = 30),
                                crs = st_crs(4326)))
pts_golfe <- st_crop(pts, golfe_bbox)

# Nombre après crop
n_cropped <- nrow(pts_golfe)

# 4) Vérifications
cat("Points lus au total : ", n_total, "\n")
cat("Points avec géométrie valide : ", n_total - n_na_geom, "\n")
cat("Points dans le bbox du Golfe : ", n_cropped, "\n")

# 5) Points exclus (facultatif)
pts_exclus <- pts[!st_intersects(pts, golfe_bbox, sparse = FALSE), ]
if(nrow(pts_exclus) > 0){
  message("Attention : ", nrow(pts_exclus), " point(s) hors du Golfe détecté(s).")
}

