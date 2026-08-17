# Cartographie de la sévérite de la sechéresse au Maroc à l'aide des données satellitaires (2000-2025)

Ce dépôt héberge le code source et la méthodologie de l'application de suivi du stress hydrique au Maroc, développée dans le cadre de l'évaluation des politiques publiques. Ce projet s'appuie sur l'extraction de séries satellitaires et le calcul d'indices biophysiques pour cartographier la sévérité de la sécheresse à l'échelle régionale.


## 🚀 Fonctionnalités du Code R

* **Alignement Géospatial :** Jointure et harmonisation des données de télédétection avec le découpage officiel des 12 régions du Maroc.
* **Légendes Stables :** Forçage de l'affichage de l'intégralité des palettes de couleurs dans les légendes (`drop = FALSE` et configuration des `limits`), assurant la lisibilité des classes même si elles ne sont pas représentées sur la carte.
* **Visualisation Avancée :** Cartes thématiques prêtes pour l'intégration dans le tableau de bord décisionnel.

## 📦 Dépendances et Installation

Pour reproduire les analyses et générer les graphiques, assurez-vous de disposer des packages R suivants :

```R
install.packages(c("ggplot2", "sf", "ggspatial", "igraph", "dplyr","tidyverse", "raster", 
                   "terra", "sp", "rgdal", "lubridate"))
```

## Structure du projet
```
├── .gitignore # Fichiers ignorés par Git
├── README.md # Présentation du projet
├── Stage Stress Hydrique Maroc.Rproj # Projet RStudio
├── 01_preparation_donnees.R # Script préparation données
├── importation et calcul des stats.R # Script analyses stats
├── donnees_satellites/ # Données brutes (ignoré)
├── shapefile_maroc/ # Shapefiles Maroc
└── sorties_figures/ # Figures et résultats
```

## Statut
🚧 Projet en cours de développement

## Auteurs
GUERGOU Abdoul-Samah et Aouissi Medard , Encadrés par M. Ilyes BOUMAHDI
