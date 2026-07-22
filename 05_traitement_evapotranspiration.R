# =========================================================================
# ÉTAPE 3:  CALCUL DE L'ÉVAPOTRANSPIRATION (2024)
# =========================================================================
# 1. Détection et chargement automatique des fichiers .tif
# R va lister tous les fichiers se terminant par .tif dans chaque sous-dossier
chemins_temp <- list.files(path = "Temp_2024", pattern = "\\.tif$", full.names = TRUE)
chemins_prec <- list.files(path = "Prec_2024", pattern = "\\.tif$", full.names = TRUE)

# Vérification de sécurité pour s'assurer que R trouve bien tes 12 fichiers
if (length(chemins_temp) != 12 || length(chemins_prec) != 12) {
  stop("🚨 Erreur : R n'a pas trouvé exactement 12 fichiers .tif dans 'Temp_2024' ou 'Prec_2024'. Vérifie l'emplacement de tes dossiers !")
}

# Chargement des rasters en piles (Stacks)
stack_temp <- rast(chemins_temp)
stack_prec <- rast(chemins_prec)

# Réaligner le Shapefile des régions sur le système de projection (CRS) des rasters
regions_crs <- st_transform(maroc_regions_aligned, st_crs(stack_temp))

# =========================================================================
# 2. CALCUL DES ENTRÉES ANNUELLES POUR LA FORMULE
# =========================================================================

# T = Température moyenne annuelle (°C)
T_annuelle <- mean(stack_temp)

# P = Précipitations annuelles totales (mm/an)
P_annuelle <- sum(stack_prec)

# =========================================================================
# 3. APPLICATION DES ÉQUATIONS DE L'ARTICLE (CÁRDENAS-TRISTÁN ET AL., 2023)
# =========================================================================

# Équation 4 : Calcul du paramètre thermique tau
tau <- 300 + (25 * T_annuelle) + (0.05 * (T_annuelle^3))

# Équation 3 : Calcul de l'Évapotranspiration (E)
et_raster <- P_annuelle / sqrt(0.9 + (P_annuelle^2 / tau^2))

# =========================================================================
# 4. EXTRACTION STATISTIQUE PAR RÉGION & DÉCOUPAGE EN QUARTILES
# =========================================================================

# Extraction de la moyenne de l'évapotranspiration par région
regions_crs$ET_Moyenne <- exact_extract(et_raster, regions_crs, 'mean')

# Classification stricte en Quartiles (4 classes égales)
regions_finales_et <- regions_crs %>%
  mutate(Quartile_ET = ntile(ET_Moyenne, 4)) %>%
  mutate(Classe_ET = case_when(
    Quartile_ET == 1 ~ "Faible",
    Quartile_ET == 2 ~ "Modérément faible",
    Quartile_ET == 3 ~ "Modérément élevée",
    Quartile_ET == 4 ~ "Élevée"
  )) %>%
  mutate(Classe_ET = factor(Classe_ET, levels = c(
    "Faible", "Modérément faible", "Modérément élevée", "Élevée"
  )))


# =========================================================================
# 5. ÉTAPE 3:  FIGURE 9 - RENDU CARTOGRAPHIQUE 
# =========================================================================


# Palette HEX officielle (Bleu nuit -> Bleu clair -> Beige -> Bordeaux)
palette_evapo <- c(
  "Faible"            = "#081d58", 
  "Modérément faible" = "#5c9ebc", 
  "Modérément élevée" = "#cca483", 
  "Élevée"            = "#67001f"  
)

carte_et_2024 <- ggplot(data = regions_finales_et) +
  geom_sf(aes(fill = Classe_ET), color = "grey30", linewidth = 0.3) +
  
  # Étiquettes des régions
  geom_sf_text(aes(label = nom_fr), size = 2.2, fontface = "bold", color = "black", 
               check_overlap = TRUE, show.legend = FALSE) +
  
  # Application des couleurs avec une légende configurée horizontalement
  scale_fill_manual(
    values = palette_evapo, 
    drop = FALSE, 
    name = "Évapotranspiration (Quartiles)",
    guide = guide_legend(
      direction = "horizontal",
      title.position = "top",
      title.hjust = 0.5,
      label.position = "bottom",
      nrow = 1
    )
  ) +
  
  # Éléments cartographiques repositionnés pour ne pas gêner
  ggspatial::annotation_scale(location = "br", width_hint = 0.3) + # Échelle en bas à droite
  ggspatial::annotation_north_arrow(location = "tr", which_north = "true", # Flèche Nord en haut à droite
                                    style = ggspatial::north_arrow_fancy_orienteering) +
  
  labs(
    title = " Niveaux d'évapotranspiration à travers les régions marocaines (2024)"
    ) +
  
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 11, color = "#1f4e79", hjust = 0),
    plot.subtitle = element_text(size = 9, color = "grey40", margin = margin(b = 15)),
    
    # Positionnement de la légende tout en bas à l'extérieur de la carte
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(face = "bold", size = 9),
    legend.text = element_text(size = 8),
    legend.margin = margin(t = 10, b = 10),
    
    panel.background = element_rect(fill = "aliceblue", color = NA)
  )

# Affichage immédiat du résultat corrigé
print(carte_et_2024)

# Sauvegarde de la figure
ggsave("sorties_figures/Figure_7_Carte_Evapotranspiration_Officielle.png", plot = carte_et_2024, 
       width = 11, height = 9, dpi = 300)
