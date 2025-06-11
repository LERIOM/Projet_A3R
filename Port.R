# Installer si besoin :
# install.packages(c("sf","dplyr","dbscan","ggplot2"))

library(sf)
library(dplyr)
library(dbscan)
library(ggplot2)


# 1) Charger les données
df <- read.csv("vessel-total-clean-final.csv", stringsAsFactors = FALSE)

# 2) Filtrer les points à l’arrêt (par exemple SOG ≤ 0.5 nœud)
df_stop <- df %>% 
  filter(!is.na(sog) & sog <= 0.5)

# 3) Conversion en sf et projection métrique
pts_stop <- st_as_sf(df_stop, coords = c("lon","lat"), crs = 4326)
pts_m    <- st_transform(pts_stop, 3857)  # Web Mercator en mètres

# 4) Préparer la matrice de coordonnées pour dbscan
coords <- st_coordinates(pts_m)

# 5) Exécuter DBSCAN
#    eps = 500 mètres (distance max pour cluster), minPts = 50 points
cl <- dbscan(coords, eps = 50000, minPts = 500)

# 6) Ajouter l’étiquette de cluster au sf
pts_m$cluster <- cl$cluster

# 7) Garder les clusters valides (cluster != 0) et calculer leur centroïde
ports_clusters <- pts_m %>% 
  filter(cluster != 0) %>% 
  group_by(cluster) %>% 
  summarize(geometry = st_combine(geometry)) %>%    # regroupe les points
  st_cast("MULTIPOINT") %>% 
  st_centroid()                                     # calcule le centroïde

# 8) Remettre en WGS84 pour obtenir lon/lat
ports_wgs84 <- st_transform(ports_clusters, 4326) %>%
  mutate(
    lon = st_coordinates(.)[,1],
    lat = st_coordinates(.)[,2]
  )

# 9) Afficher le résultat
print(ports_wgs84)

# 10) (Optionnel) Visualiser sur carte
world <- rnaturalearth::ne_countries(scale="medium", returnclass="sf")
ggplot() +
  geom_sf(data = world, fill = "grey90", color = "grey50") +
  geom_sf(data = pts_m, aes(color = factor(cluster)), size = 0.3, alpha = 0.3) +
  geom_sf(data = ports_wgs84, color = "red", size = 2) +
  coord_sf(xlim = c(-100, -75), ylim = c(18, 32), expand = FALSE) +
  scale_color_viridis_d(option = "turbo", guide = "none") +
  theme_minimal()+
  theme(
    axis.text      = element_blank(),  # supprime les étiquettes (–95°, 18°…)
    axis.ticks     = element_blank(),  # supprime les graduations
    axis.title     = element_blank(),  # supprime les titres d’axe
    axis.line      = element_blank()   # supprime les lignes d’axe
  )