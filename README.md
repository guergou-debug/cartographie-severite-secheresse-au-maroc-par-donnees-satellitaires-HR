# Cartographie du Stress Hydrique au Maroc

## Description du projet
Analyse et cartographie de la sévérité de la sécheresse au Maroc à partir de données satellitaires haute résolution.

## Structure du projet
├── .gitignore # Fichiers ignorés par Git
├── README.md # Présentation du projet
├── Stage Stress Hydrique Maroc.Rproj # Projet RStudio
├── 01_preparation_donnees.R # Script préparation données
├── importation et calcul des stats.R # Script analyses stats
├── donnees_satellites/ # Données brutes (ignoré)
├── shapefile_maroc/ # Shapefiles Maroc
└── sorties_figures/ # Figures et résultats

## Prérequis
```r
# Packages nécessaires
install.packages(c("tidyverse", "sf", "raster", "ggplot2", 
                   "terra", "sp", "rgdal", "lubridate"))
Flux de travail
01_preparation_donnees.R - Chargement et nettoyage des données

importation et calcul des stats.R - Analyses statistiques

Statut
🚧 Projet en cours de développement

Auteur
GUERGOU Abdoul-samah
