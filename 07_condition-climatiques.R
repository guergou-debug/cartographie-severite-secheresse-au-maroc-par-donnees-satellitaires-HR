# =========================================================================
# CARTOGRAPHIE DES CONDITIONS CLIMATIQUES : TEMPÉRATURES & PRÉCIPITATIONS
# =========================================================================

# 1. PALETTES DE COULEURS ET ÉTIQUETTES DES QUARTILES
# -------------------------------------------------------------------------
labels_quartiles <- c("Faible", "Modérément faible", "Modérément élevée", "Élevée")

# Palette Température (Conservée de l'évapotranspiration)
palette_temp <- c(
  "Faible"            = "#081d58", 
  "Modérément faible" = "#5c9ebc", 
  "Modérément élevée" = "#cca483", 
  "Élevée"            = "#67001f"
)

# Palette Précipitations (Adaptée aux 4 couleurs de sévérité)
palette_prec <- c(
  "Faible"            = "#801551", # Violet/Bordeaux (Très sec)
  "Modérément faible" = "#e391b8", # Rose/Magenta (Sec)
  "Modérément élevée" = "#a3d977", # Vert clair (Humidité modérée)
  "Élevée"            = "#2d7f28"  # Vert foncé (Très humide)
)

# 2. FONCTION DE TRAITEMENT DES RASTERS ET D'EXTRACTION PAR ANNÉE
# -------------------------------------------------------------------------
# =========================================================================
# CORRECTION : FONCTION ROBUSTE AVEC RÉALIGNEMENT D'EMPRISE (RESAMPLE)
# =========================================================================

traiter_climat_annee <- function(dossier_temp, dossier_prec, shapefile_base) {
  
  # 1. Détection des fichiers .tif
  fichiers_temp <- list.files(dossier_temp, pattern = "\\.tif$", full.names = TRUE)
  fichiers_prec <- list.files(dossier_prec, pattern = "\\.tif$", full.names = TRUE)
  
  if (length(fichiers_temp) == 0 || length(fichiers_prec) == 0) {
    stop(paste("🚨 Fichiers manquants dans", dossier_temp, "ou", dossier_prec))
  }
  
  # 2. Chargement individuel et correction automatique des emprises spatiales
  rasters_temp_list <- lapply(fichiers_temp, rast)
  rasters_prec_list <- lapply(fichiers_prec, rast)
  
  # On prend le 1er raster comme référence spatiale
  ref_temp <- rasters_temp_list[[1]]
  ref_prec <- rasters_prec_list[[1]]
  
  # Réalignement (resample) de tous les autres rasters sur le premier si les emprises diffèrent
  rasters_temp_corrects <- lapply(rasters_temp_list, function(r) {
    if (!ext(r) == ext(ref_temp) || !crs(r) == crs(ref_temp)) {
      return(resample(r, ref_temp, method = "bilinear"))
    }
    return(r)
  })
  
  rasters_prec_corrects <- lapply(rasters_prec_list, function(r) {
    if (!ext(r) == ext(ref_prec) || !crs(r) == crs(ref_prec)) {
      return(resample(r, ref_prec, method = "bilinear"))
    }
    return(r)
  })
  
  # Empilement sécurisé
  stack_t <- rast(rasters_temp_corrects)
  stack_p <- rast(rasters_prec_corrects)
  
  # 3. CALCULS ADAPTÉS AU NOMBRE DE MOIS DISPONIBLES (4 mois vs 12 mois)
  # Pour la température : La moyenne reste la moyenne des mois disponibles
  T_annuelle <- mean(stack_t)
  
  # Pour les précipitations : On calcule la moyenne mensuelle sur la période observée
  # afin de ne pas biaiser la comparaison due au manque de mois futurs
  P_moyenne_mensuelle <- mean(stack_p)
  
  # 4. Alignment du shapefile et extraction
  sf_aligned <- st_transform(shapefile_base, st_crs(stack_t))
  
  sf_aligned$Temp_Moyenne <- exact_extract(T_annuelle, sf_aligned, 'mean')
  sf_aligned$Prec_Moyenne <- exact_extract(P_moyenne_mensuelle, sf_aligned, 'mean')
  
  # 5. Classification en Quartiles
  labels_quartiles <- c("Faible", "Modérément faible", "Modérément élevée", "Élevée")
  
  res_sf <- sf_aligned %>%
    mutate(
      nom_affich = if_else(nom_fr == "l'Oriental", "L'Oriental", nom_fr),
      
      Q_Temp = ntile(Temp_Moyenne, 4),
      Classe_Temp = factor(labels_quartiles[Q_Temp], levels = labels_quartiles),
      
      Q_Prec = ntile(Prec_Moyenne, 4),
      Classe_Prec = factor(labels_quartiles[Q_Prec], levels = labels_quartiles)
    )
  
  return(res_sf)
}
# 3. FONCTION DE CRÉATION D'UNE CARTE INDIVIDUELLE
# -------------------------------------------------------------------------
creer_carte_climat <- function(sf_data, var_column, palette, titre_var, nom_legend) {
  ggplot(data = sf_data) +
    geom_sf(aes(fill = .data[[var_column]]), color = "grey30", linewidth = 0.3) +
    geom_sf_text(
      aes(label = nom_affich), 
      size = 2.0, 
      fontface = "bold", 
      color = "black", 
      check_overlap = FALSE, 
      show.legend = FALSE
    ) +
    scale_fill_manual(
      values = palette, 
      drop = FALSE, 
      name = nom_legend,
      guide = guide_legend(
        direction = "horizontal",
        title.position = "top",
        title.hjust = 0.5,
        nrow = 1
      )
    ) +
    annotation_scale(location = "br", width_hint = 0.3) +
    annotation_north_arrow(
      location = "tr", 
      which_north = "true", 
      style = north_arrow_fancy_orienteering
    ) +
    labs(title = titre_var) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 11, color = "#1f4e79", hjust = 0.5),
      legend.position = "bottom",
      legend.title = element_text(face = "bold", size = 9),
      legend.text = element_text(size = 8),
      panel.background = element_rect(fill = "aliceblue", color = NA)
    )
}

# 4. EXÉCUTION DU TRAITEMENT ET GÉNÉRATION DES FIGURES
# -------------------------------------------------------------------------

# Traitement 2025
sf_2025 <- traiter_climat_annee("Temp_2025", "Prec_2025", maroc_regions_aligned)

p_temp_2025 <- creer_carte_climat(sf_2025, "Classe_Temp", palette_temp, "Températures (2025)", "Température (Quartiles) :")
p_prec_2025 <- creer_carte_climat(sf_2025, "Classe_Prec", palette_prec, "Précipitations (2025)", "Précipitations (Quartiles) :")

carte_combinee_2025 <- (p_temp_2025 + p_prec_2025) +
  plot_annotation(
    title = "Conditions Climatiques Régionales au Maroc — Année 2025",
    theme = theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5, color = "#1f4e79"))
  )

print(carte_combinee_2025)

# Traitement 2026
sf_2026 <- traiter_climat_annee("Temp_2026", "Prec_2026", maroc_regions_aligned)

p_temp_2026 <- creer_carte_climat(sf_2026, "Classe_Temp", palette_temp, "Températures (2026)", "Température (Quartiles) :")
p_prec_2026 <- creer_carte_climat(sf_2026, "Classe_Prec", palette_prec, "Précipitations (2026)", "Précipitations (Quartiles) :")

carte_combinee_2026 <- (p_temp_2026 + p_prec_2026) +
  plot_annotation(
    title = "Conditions Climatiques Régionales au Maroc — Année 2026",
    theme = theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5, color = "#1f4e79"))
  )

print(carte_combinee_2026)


ggsave(
  "sorties_figures/Carte_Climat_Combinee_2025.png", 
  plot = carte_combinee_2025, 
  width = 14, 
  height = 8, 
  dpi = 300
)

ggsave(
  "sorties_figures/Carte_Climat_Combinee_2026.png", 
  plot = carte_combinee_2026, 
  width = 14, 
  height = 8, 
  dpi = 300
)