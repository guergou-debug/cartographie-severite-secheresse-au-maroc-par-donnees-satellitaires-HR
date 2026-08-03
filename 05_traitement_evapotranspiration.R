# =========================================================================
# ÉTAPE 3: CALCUL DE L'ÉVAPOTRANSPIRATION (2025)
# =========================================================================

# 1. Détection et chargement automatique des fichiers .tif 2025
chemins_temp <- list.files(path = "Temp_2025", pattern = "\\.tif$", full.names = TRUE)
chemins_prec <- list.files(path = "Prec_2025", pattern = "\\.tif$", full.names = TRUE)

# Vérification de sécurité
if (length(chemins_temp) != 12 || length(chemins_prec) != 12) {
  stop("🚨 Erreur : R n'a pas trouvé exactement 12 fichiers .tif dans 'Temp_2025' ou 'Prec_2025'. Vérifie tes dossiers !")
}

# Chargement des rasters
stack_temp <- rast(chemins_temp)
stack_prec <- rast(chemins_prec)

# Réalignement du Shapefile officiel sur le CRS des rasters
regions_crs <- st_transform(maroc_regions, st_crs(stack_temp))

# =========================================================================
# 2. CALCUL DES ENTRÉES ANNUELLES POUR LA FORMULE
# =========================================================================

# T = Température moyenne annuelle (°C)
T_annuelle <- mean(stack_temp)

# P = Précipitations annuelles totales (mm/an)
P_annuelle <- sum(stack_prec)

# =========================================================================
# 3. ÉQUATIONS DE CÁRDENAS-TRISTÁN ET AL. (2023)
# =========================================================================

# Paramètre thermique tau (Éq. 4)
tau <- 300 + (25 * T_annuelle) + (0.05 * (T_annuelle^3))

# Évapotranspiration E en mm/an (Éq. 3)
et_raster <- P_annuelle / sqrt(0.9 + (P_annuelle^2 / tau^2))

# =========================================================================
# 4. EXTRACTION PAR RÉGION & CLASSIFICATION PAR QUARTILES (4 CLASSES)
# =========================================================================

# Extraction de la moyenne d'Évapotranspiration par région
regions_crs$ET_Moyenne <- exact_extract(et_raster, regions_crs, 'mean')

# Calcul des bornes exactes des 4 quartiles (0%, 25%, 50%, 75%, 100%)
bornes_et <- quantile(regions_crs$ET_Moyenne, probs = seq(0, 1, 0.25), na.rm = TRUE)

# Découpage avec libellés d'intervalles explicites
regions_finales_et <- regions_crs %>%
  mutate(
    Classe_ET = cut(
      ET_Moyenne,
      breaks = bornes_et,
      include.lowest = TRUE,
      labels = c(
        paste0("Q1 : Faible [", round(bornes_et[1], 1), " ; ", round(bornes_et[2], 1), "]"),
        paste0("Q2 : Modérément faible ]", round(bornes_et[2], 1), " ; ", round(bornes_et[3], 1), "]"),
        paste0("Q3 : Modérément élevée ]", round(bornes_et[3], 1), " ; ", round(bornes_et[4], 1), "]"),
        paste0("Q4 : Élevée ]", round(bornes_et[4], 1), " ; ", round(bornes_et[5], 1), "]")
      )
    )
  )

# =========================================================================
# 5. RENDU CARTOGRAPHIQUE (2025)
# =========================================================================

# Palette originale réalignée sur les 4 niveaux
niveaux_et_labels <- levels(regions_finales_et$Classe_ET)
palette_evapo <- setNames(
  c("#081d58", "#5c9ebc", "#cca483", "#67001f"),
  niveaux_et_labels
)

carte_et_2025 <- ggplot(data = regions_finales_et) +
  geom_sf(aes(fill = Classe_ET), color = "grey30", linewidth = 0.3) +
  geom_sf_text(
    aes(label = nom_fr), 
    size = 2.2, 
    fontface = "bold", 
    color = "black", 
    check_overlap = TRUE, 
    show.legend = FALSE
  ) +
  scale_fill_manual(
    values = palette_evapo, 
    drop = FALSE, 
    name = "Évapotranspiration moyenne (Quartiles)",
    guide = guide_legend(
      direction = "horizontal",
      title.position = "top",
      title.hjust = 0.5,
      nrow = 1
    )
  ) +
  annotation_scale(location = "br", width_hint = 0.3) +
  annotation_north_arrow(
    location = "tl", 
    which_north = "true", 
    style = north_arrow_fancy_orienteering
  ) +
  labs(
    title = "Niveaux d'évapotranspiration à travers les régions marocaines (2025)",
     ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 12, color = "#1f4e79", hjust = 0.5),
    plot.subtitle = element_text(size = 9, color = "grey40", hjust = 0.5, margin = margin(b = 12)),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(face = "bold", size = 9),
    legend.text = element_text(size = 8),
    panel.background = element_rect(fill = "aliceblue", color = NA)
  )

# Affichage et Sauvegarde
print(carte_et_2025)

ggsave(
  "sorties_figures/Figure_7_Carte_Evapotranspiration_2025.png", 
  plot = carte_et_2025, 
  width = 11, 
  height = 9, 
  dpi = 300
)