# ========================================================================================
# Stage PFA :Cartographie de la  sevérité de la secheresse au maroc par télédetection
# ÉTAPE 1 : CONFIGURATION DU PROJET R ET CHARGEMENT DU SHAPEFILE DU MAROC
# ========================================================================================

# 1. Installation des packages essentiels (si non installés)
packages_requis <- c("sf", "terra", "tidyverse", "tidyterra", "ggspatial", "exactextractr")
packages_manquants <- packages_requis[!(packages_requis %in% installed.packages()[,"Package"])]

if(length(packages_manquants) > 0) {
  install.packages(packages_manquants, dependencies = TRUE)
}

# 2. Chargement des bibliothèques scientifiques
library(sf)            # Pour la manipulation des données vectorielles (Shapefiles)
library(terra)         # Pour le traitement ultra-rapide des images rasters satellites
library(tidyverse)     # Pour la manipulation de données (dplyr) et graphiques (ggplot2)
library(tidyterra)     # Pour la visualisation optimisée des objets spatiaux terra sous ggplot2
library(ggspatial)     # Pour ajouter barres d'échelle et flèches de direction professionnelles
library(exactextractr)
library(scales)
library(patchwork)
library(ggspatial)
library(ggplot2)
library(dplyr)
library(readxl)
library(ggrepel)
library(viridis)

cat("--- Tous les packages sont chargés avec succès ! ---\n")

# 3. Création de la structure des dossiers du projet
dir.create("donnees_satellites", showWarnings = FALSE)
dir.create("shapefile_maroc", showWarnings = FALSE)
dir.create("sorties_figures", showWarnings = FALSE)


# =========================================================================
#  CHARGEMENT DE  SHAPEFILE OFFICIEL DU MAROC
# =========================================================================

# 1. Spécifie le chemin vers TON fichier .shp (remplace par le nom exact de ton fichier)
chemin_shapefile <- "shapefile_maroc/regions.shp" 

# 2. Lecture du shapefile (sf va lire automatiquement le .dbf associé)
maroc_regions <- st_read(chemin_shapefile)

# 3. Sécurité scientifique : Conversion en WGS84 pour s'aligner sur GEE et MODIS
maroc_regions <- st_transform(maroc_regions, crs = 4326)

# 4. Regardons la structure du fichier (équivalent de ton tableau .dbf)
print(head(maroc_regions))

# 5. Visualisation de ta carte officielle sous R
plot_officiel <- ggplot(data = maroc_regions) +
  geom_sf(fill = "antiquewhite", color = "darkgrey", size = 0.5) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         style = north_arrow_fancy_orienteering) +
  labs(title = "Royaume du Maroc - Découpage Régional Officiel",
       subtitle = "Validation de l'intégrité territoriale sous R",
       caption = "Pipeline de reproduction méthodologique - Stratégie R-GEE") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        panel.background = element_rect(fill = "aliceblue"))

print(plot_officiel)