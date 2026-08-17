# =========================================================================
# CARTOGRAPHIE DES CONDITIONS CLIMATIQUES : TEMPÉRATURES & PRÉCIPITATIONS
# =========================================================================

# 1. FONCTION DE TRAITEMENT ET DE CLASSIFICATION DYNAMIQUE (QUARTILES)
# -------------------------------------------------------------------------
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
  
  ref_temp <- rasters_temp_list[[1]]
  ref_prec <- rasters_prec_list[[1]]
  
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
  
  # 3. Calculs climatiques
  T_annuelle <- mean(stack_t)
  P_moyenne_mensuelle <- mean(stack_p)
  
  # 4. Alignment du shapefile officiel (maroc_regions) et extraction
  sf_aligned <- st_transform(shapefile_base, st_crs(stack_t))
  
  sf_aligned$Temp_Moyenne <- exact_extract(T_annuelle, sf_aligned, 'mean')
  sf_aligned$Prec_Moyenne <- exact_extract(P_moyenne_mensuelle, sf_aligned, 'mean')
  
  # 5. Calcul des bornes de Quartiles et classification dynamique
  bornes_t <- quantile(sf_aligned$Temp_Moyenne, probs = seq(0, 1, 0.25), na.rm = TRUE)
  bornes_p <- quantile(sf_aligned$Prec_Moyenne, probs = seq(0, 1, 0.25), na.rm = TRUE)
  
  res_sf <- sf_aligned %>%
    mutate(
      nom_affich = if_else(nom_fr == "l'Oriental", "L'Oriental", nom_fr),
      
      Classe_Temp = cut(
        Temp_Moyenne,
        breaks = bornes_t,
        include.lowest = TRUE,
        labels = c(
          paste0("Q1 : Faible [", round(bornes_t[1], 1), " ; ", round(bornes_t[2], 1), "°C]"),
          paste0("Q2 : Mod. faible ]", round(bornes_t[2], 1), " ; ", round(bornes_t[3], 1), "°C]"),
          paste0("Q3 : Mod. élevée ]", round(bornes_t[3], 1), " ; ", round(bornes_t[4], 1), "°C]"),
          paste0("Q4 : Élevée ]", round(bornes_t[4], 1), " ; ", round(bornes_t[5], 1), "°C]")
        )
      ),
      
      Classe_Prec = cut(
        Prec_Moyenne,
        breaks = bornes_p,
        include.lowest = TRUE,
        labels = c(
          paste0("Q1 : Très sec [", round(bornes_p[1], 1), " ; ", round(bornes_p[2], 1), " mm]"),
          paste0("Q2 : Sec ]", round(bornes_p[2], 1), " ; ", round(bornes_p[3], 1), " mm]"),
          paste0("Q3 : Mod. humide ]", round(bornes_p[3], 1), " ; ", round(bornes_p[4], 1), " mm]"),
          paste0("Q4 : Humide ]", round(bornes_p[4], 1), " ; ", round(bornes_p[5], 1), " mm]")
        )
      )
    )
  
  return(res_sf)
}

# 2. FONCTION DE CRÉATION D'UNE CARTE INDIVIDUELLE
# -------------------------------------------------------------------------
creer_carte_climat <- function(sf_data, var_column, palette_hex, titre_var, nom_legend) {
  
  # Association dynamique des couleurs de la palette aux niveaux de la variable
  niveaux_labels <- levels(sf_data[[var_column]])
  palette_associee <- setNames(palette_hex, niveaux_labels)
  
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
      values = palette_associee, 
      drop = FALSE, 
      name = nom_legend,
      guide = guide_legend(
        direction = "horizontal",
        title.position = "top",
        title.hjust = 0.5,
        nrow = 2
      )
    ) +
    annotation_scale(location = "br", width_hint = 0.3) +
    annotation_north_arrow(
      location = "tl", 
      which_north = "true", 
      style = north_arrow_fancy_orienteering
    ) +
    labs(title = titre_var) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 11, color = "#1f4e79", hjust = 0.5),
      legend.position = "bottom",
      legend.title = element_text(face = "bold", size = 9),
      legend.text = element_text(size = 7.5),
      panel.background = element_rect(fill = "aliceblue", color = NA)
    )
}

# 3. PALETTES DE COULEURS BASE
# -------------------------------------------------------------------------
hex_temp <- c("#081d58", "#5c9ebc", "#cca483", "#67001f")
hex_prec <- c("#801551", "#e391b8", "#a3d977", "#2d7f28")

# 4. EXÉCUTION DU TRAITEMENT ET GÉNÉRATION DES FIGURES
# -------------------------------------------------------------------------

# --- Traitement 2025 ---
sf_2025 <- traiter_climat_annee("Temp_2025", "Prec_2025", maroc_regions)

p_temp_2025 <- creer_carte_climat(sf_2025, "Classe_Temp", hex_temp, "Températures (2025)", "Température moyenne (Quartiles) :")
p_prec_2025 <- creer_carte_climat(sf_2025, "Classe_Prec", hex_prec, "Précipitations (2025)", "Précipitations mensuelles moy. (Quartiles) :")

carte_combinee_2025 <- (p_temp_2025 + p_prec_2025) +
  plot_annotation(
    title = "Conditions Climatiques Régionales au Maroc — Année 2025",
    theme = theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5, color = "#1f4e79"))
  )

print(carte_combinee_2025)

ggsave(
  "sorties_figures/Carte_Climat_Combinee_2025.png", 
  plot = carte_combinee_2025, 
  width = 14, 
  height = 8, 
  dpi = 300
)

# --- Traitement 2026 ---
sf_2026 <- traiter_climat_annee("Temp_2026", "Prec_2026", maroc_regions)

p_temp_2026 <- creer_carte_climat(sf_2026, "Classe_Temp", hex_temp, "Températures (2026)", "Température moyenne (Quartiles) :")
p_prec_2026 <- creer_carte_climat(sf_2026, "Classe_Prec", hex_prec, "Précipitations (2026)", "Précipitations mensuelles moy. (Quartiles) :")

carte_combinee_2026 <- (p_temp_2026 + p_prec_2026) +
  plot_annotation(
    title = "Conditions Climatiques Régionales au Maroc — Année 2026",
    theme = theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5, color = "#1f4e79"))
  )

print(carte_combinee_2026)

ggsave(
  "sorties_figures/Carte_Climat_Combinee_2026.png", 
  plot = carte_combinee_2026, 
  width = 14, 
  height = 8, 
  dpi = 300
)