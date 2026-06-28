# =========================================================================
# PROJET DE RECHERCHE : REPRODUCTION DE LA MÉTHODOLOGIE OCDE
# ÉTAPE 6 : IMPORTATION DES RASTERS ET EXTRACTION DES STATISTIQUES ZONALES
# =========================================================================

library(sf)
library(terra)
library(tidyverse)
library(exactextractr)

# 1. Lister tous les fichiers GeoTIFF téléchargés depuis GEE
# Assure-toi que tes fichiers sont bien dans le dossier "donnees_satellites"
liste_fichiers <- list.files(path = "donnees_satellites", 
                             pattern = "\\.tif$", 
                             full.names = TRUE)

if(length(liste_fichiers) == 0) {
  stop("Erreur : Aucun fichier .tif trouvé dans le dossier 'donnees_satellites' !")
} else {
  cat("Nombre de fichiers satellites détectés :", length(liste_fichiers), "\n")
}

# 2. Extraction des dates à partir des noms de fichiers pour la traçabilité
# Nos fichiers GEE s'appellent : MODIS_Maroc_Indices_YYYY_MM_dd.tif
dates_series <- liste_fichiers %>% 
  str_extract("(?<=Indices_)\\d{4}_\\d{2}_\\d{2}") %>% 
  as.Date(format = "%Y_%m_%d")

# 3. Initialisation d'une table finale pour stocker les résultats 
base_donnees_drought <- data.frame()

# =========================================================================
# ÉTAPE 6 (PROPRE) : EXTRACTION AVEC LES BONS NOMS DE VARIABLES (_median)
# =========================================================================
# 1. Chargement du premier raster pour harmoniser le CRS et éliminer l'avis 1, 2, 3
premier_raster <- rast(liste_fichiers[1])
crs_exact_raster <- crs(premier_raster)

# Alignement géospatial strict de tes régions sur le CRS du satellite
maroc_regions_aligned <- st_transform(maroc_regions, crs = crs_exact_raster)

# 2. Réinitialisation de la table finale
base_donnees_drought <- data.frame()

# 3. Boucle d'extraction sans conflit de noms
for (i in 1:length(liste_fichiers)) {
  
  cat("Traitement de l'image", i, "/", length(liste_fichiers), "- Date :", as.character(dates_series[i]), "\n")
  
  # Chargement du raster courant
  raster_courant <- rast(liste_fichiers[i])
  
  # Extraction et conversion forcée en numérique pur avec as.numeric()
  # Cela supprime les étiquettes de colonnes variables et résout l'erreur match.names
  stats_ndvi <- as.numeric(exact_extract(raster_courant[[1]], maroc_regions_aligned, 'median', progress = FALSE))
  stats_ndwi <- as.numeric(exact_extract(raster_courant[[2]], maroc_regions_aligned, 'median', progress = FALSE))
  stats_nddi <- as.numeric(exact_extract(raster_courant[[3]], maroc_regions_aligned, 'median', progress = FALSE))
  
  # Nom de ta colonne officielle
  nom_colonne_region <- "nom_fr" 
  
  # Assemblage du dataframe pour la date i
  donnees_date <- data.frame(
    Region = maroc_regions_aligned[[nom_colonne_region]],
    Date = dates_series[i],
    NDVI_median = stats_ndvi,
    NDWI_median = stats_ndwi,
    NDDI_median = stats_nddi
  )
  
  # L'empilement se fait maintenant sans aucune erreur de nommage
  base_donnees_drought <- rbind(base_donnees_drought, donnees_date)
}
# 4. Sauvegarde finale du dataset doctoral
write.csv(base_donnees_drought, "sorties_figures/base_donnees_drought_maroc_2024.csv", row.names = FALSE)

# 5. Affichage du résultat de contrôle
cat("--- Extraction finale réussie sans avertissement ni erreur ! ---\n")
print(head(base_donnees_drought))