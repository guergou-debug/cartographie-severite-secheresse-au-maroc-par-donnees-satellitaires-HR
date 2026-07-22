# =========================================================================
# ÉTAPE 2:  FIGURE 1 - ÉVOLUTION TEMPORELLE DU NDVI ET NDWI
# =========================================================================

# 1. Lecture et restructuration des données au format long (Tidy Data)
donnees_raw <- read.csv("sorties_figures/base_donnees_drought_maroc_2024.csv")

donnees_longues <- donnees_raw %>% 
  mutate(Date = as.Date(Date)) %>%
  select(Region, Date, NDVI_median, NDWI_median) %>%
  pivot_longer(cols = c(NDVI_median, NDWI_median), 
               names_to = "Indice", 
               values_to = "Valeur") %>%
  mutate(Indice = case_when(
    Indice == "NDVI_median" ~ "NDVI (Végétation)",
    Indice == "NDWI_median" ~ "NDWI (Eau / Humidité)"
  ))

# 2. Construction de la Figure au format publication OCDE
figure_1_hcp <- ggplot(data = donnees_longues, aes(x = Date, y = Valeur, color = Indice)) +
  # Lignes d'évolution
  geom_line(size = 0.7, alpha = 0.8) +
  # Points de mesure (composites de 8 jours)
  geom_point(size = 0.8, alpha = 0.6) +
  # Ligne de référence à 0 (le NDWI descend souvent en dessous de 0 en cas d'aridité)
  geom_hline(yintercept = 0, linetype = "dotted", color = "grey40") +
  # Découpage en facettes par région administrative
  facet_wrap(~ Region, ncol = 3) +
  # Palette de couleurs scientifiques : Vert pour la végétation, Bleu pour l'eau
  scale_color_manual(values = c("NDVI (Végétation)" = "#2ca02c", "NDWI (Eau / Humidité)" = "#1f77b4")) +
  # Configuration des axes
  scale_x_date(date_labels = "%b", date_breaks = "3 months") +
  scale_y_continuous(limits = c(-0.5, 1.0)) +
  # Habillage OCDE
  labs(
    title = " Profils temporels comparatifs du NDVI et du NDWI (2024)",
    x = "Mois de l'année 2024",
    y = "Valeur de l'indice",
    color = "Indice :",
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 12, color = "#333333"),
    plot.subtitle = element_text(size = 10, color = "#666666", margin = margin(b = 12)),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 9),
    legend.text = element_text(size = 9),
    strip.background = element_rect(fill = "#f5f5f5", color = "#dddddd"),
    strip.text = element_text(face = "bold", size = 9, color = "#333333"),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(size = 7, face = "italic", color = "#888888", margin = margin(t = 12))
  )

# 3. Affichage du graphique dans RStudio
print(figure_1_hcp)

# 4. Sauvegarde automatique en Haute Résolution (300 DPI)
ggsave("sorties_figures/Figure_1_Profils_NDVI_NDWI.png", plot = figure_1_hcp, 
       width = 11, height = 8, dpi = 300)


# ===============================================================================
# ÉTAPE 2:  FIGURE 2 - NUAGE DE POINTS ET CORRÉLATION STATISTIQUE NDVI VS NDWI
# ===============================================================================


# 1. Chargement du dataset extrait
donnees_drought <- read.csv("sorties_figures/base_donnees_drought_maroc_2024.csv") %>% 
  mutate(Date = as.Date(Date))

# 2. Calcul du coefficient de corrélation de Pearson global
r_pearson <- cor(donnees_drought$NDWI_median, donnees_drought$NDVI_median, use = "complete.obs")
cat("\n--- VALIDATION STATISTIQUE ---")
cat("\nLe coefficient de corrélation de Pearson (R) est de :", round(r_pearson, 4), "\n\n")

# 3. Création du graphique de corrélation (Charte graphique OCDE)
figure_correlation <- ggplot(data = donnees_drought, aes(x = NDWI_median, y = NDVI_median)) +
  # Points de mesure colorés par région pour identifier les grappes (clusters) géographiques
  geom_point(aes(color = Region), size = 2, alpha = 0.75) +
  # Ajout de la droite de tendance linéaire avec son intervalle de confiance (se = TRUE)
  geom_smooth(method = "lm", color = "#d62728", size = 1, linetype = "solid", se = TRUE) +
  # Configuration des axes
  scale_x_continuous(limits = c(-0.5, 0.5), breaks = seq(-0.5, 0.5, 0.2)) +
  scale_y_continuous(limits = c(0, 0.8), breaks = seq(0, 0.8, 0.2)) +
  # Habillage académique
  labs(
    title = " Corrélation  entre le NDWI et le NDVI",
    x = "NDWI",
    y = "NDVI",
    color = "Régions:",
    caption = paste0("Note : Coefficient de Pearson R = ", round(r_pearson, 3))
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 12, color = "#333333"),
    plot.subtitle = element_text(size = 10, color = "#666666", margin = margin(b = 12)),
    legend.position = "right",
    legend.text = element_text(size = 8),
    legend.title = element_text(face = "bold", size = 9),
    panel.grid.minor = element_blank()
  )

# 4. Affichage du graphique dans RStudio
print(figure_correlation)

# 5. Sauvegarde automatique en Haute Résolution (300 DPI)
ggsave("sorties_figures/Figure_1b_Correlation_NDVI_NDWI.png", plot = figure_correlation, 
       width = 9, height = 6, dpi = 300)


# =========================================================================
# ÉTAPE 2:FIGURE 3 - CARTOGRAPHIE PAR QUARTILES DE NDWI ET NDVI
# =========================================================================

# 1. Palettes de couleurs 
PALETTE_NDVI_PERSO <- c(
  "Faible"               = "#4B0082",
  "Intermédiaire faible" = "#DDA0DD",
  "Intermédiaire élevé"  = "#90EE90",
  "Élevé"                = "#006400"
)

PALETTE_NDWI_PERSO <- c(
  "Faible"               = "#8B0000",
  "Intermédiaire faible" = "#E1C16E",
  "Intermédiaire élevé"  = "#87CEEB",
  "Élevé"                = "#00008B"
)

# 2. Calcul des statistiques de synthèse et classification en quartiles
statistiques_annuelles <- base_donnees_drought %>%
  group_by(Region) %>%
  summarise(
    NDVI_brut = median(NDVI_median, na.rm = TRUE),
    NDWI_brut = median(NDWI_median, na.rm = TRUE)
  ) %>%
  mutate(
    # Groupes égaux basés sur la distribution observée en 2024
    Quartile_NDVI = ntile(NDVI_brut, 4),
    Classe_NDVI = case_when(
      Quartile_NDVI == 1 ~ "Faible",
      Quartile_NDVI == 2 ~ "Intermédiaire faible",
      Quartile_NDVI == 3 ~ "Intermédiaire élevé",
      Quartile_NDVI == 4 ~ "Élevé"
    ),
    Quartile_NDWI = ntile(NDWI_brut, 4),
    Classe_NDWI = case_when(
      Quartile_NDWI == 1 ~ "Faible",
      Quartile_NDWI == 2 ~ "Intermédiaire faible",
      Quartile_NDWI == 3 ~ "Intermédiaire élevé",
      Quartile_NDWI == 4 ~ "Élevé"
    )
  ) %>%
  mutate(
    Classe_NDVI = factor(Classe_NDVI, levels = c("Faible", "Intermédiaire faible", "Intermédiaire élevé", "Élevé")),
    Classe_NDWI = factor(Classe_NDWI, levels = c("Faible", "Intermédiaire faible", "Intermédiaire élevé", "Élevé"))
  )

# 3. Jonction (Merge) avec le Shapefile 
maroc_cartes <- maroc_regions_aligned %>%
  left_join(statistiques_annuelles, by = c("nom_fr" = "Region"))

# =========================================================================
#  CARTE NDVI STRUCTURÉE PAR QUARTILES
# =========================================================================
carte_ndvi <- ggplot(data = maroc_cartes) +
  geom_sf(aes(fill = Classe_NDVI), color = "grey30", linewidth = 0.3) +
  geom_sf_text(aes(label = nom_fr), size = 2.2, fontface = "bold", color = "black", check_overlap = TRUE) +
  scale_fill_manual(
    values = PALETTE_NDVI_PERSO, 
    drop = FALSE, 
    name = "Classes de NDVI",
    guide = guide_legend(
      direction = "horizontal",
      title.position = "top",
      title.hjust = 0.5,
      nrow = 1
    )
  ) +
  annotation_scale(location = "br", width_hint = 0.3) +
  labs(
    title = "A. Distribution spatiale du NDVI",
    subtitle = "Quartiles"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = 11, color = "#1f4e79"),
    plot.subtitle = element_text(size = 9, color = "grey40"),
    panel.background = element_rect(fill = "aliceblue", color = NA),
    legend.position = "bottom"
  )

# =========================================================================
# CARTE NDWI STRUCTURÉE PAR QUARTILES 
# =========================================================================
carte_ndwi <- ggplot(data = maroc_cartes) +
  geom_sf(aes(fill = Classe_NDWI), color = "grey30", linewidth = 0.3) +
  geom_sf_text(aes(label = nom_fr), size = 2.2, fontface = "bold", color = "black", check_overlap = TRUE) +
  scale_fill_manual(
    values = PALETTE_NDWI_PERSO, 
    drop = FALSE, 
    name = "Classes de NDWI",
    guide = guide_legend(
      direction = "horizontal",
      title.position = "top",
      title.hjust = 0.5,
      nrow = 1
    )
  ) +
  # Boussole mise en bas à gauche (bl) pour ne plus masquer le nord du pays
  annotation_north_arrow(location = "bl", which_north = "true", pad_y = unit(0.6, "in"),
                         style = north_arrow_fancy_orienteering) +
  labs(
    title = "B. Distribution spatiale du NDWI",
    subtitle = "Quartiles"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = 11, color = "#1f4e79"),
    plot.subtitle = element_text(size = 9, color = "grey40"),
    panel.background = element_rect(fill = "aliceblue", color = NA),
    legend.position = "bottom"
  )

# =========================================================================
# COMBINAISON ET AFFICHAGE
# =========================================================================
cartes_combineen <- carte_ndvi + carte_ndwi + 
  plot_annotation(
    title = "Diagnostic spatial de la couverture végétale et de l'état hydrique(2024)",
    theme = theme(plot.title = element_text(face = "bold", size = 13, hjust = 0.5))
  )

print(cartes_combineen)

ggsave("sorties_figures/Figure_2_Cartes_Synthese_NDVI_NDWI_Noms.png", plot = cartes_combineen, 
       width = 13, height = 8, dpi = 300)


# ===============================================================================
# ÉTAPE 2:  FIGURE 4 - CLASSIFICATION PAR SEUILS ABSOLUS (0.2, 0.3, 0.4, 0.5)
# ===============================================================================

# A. Base de calcul et extraction des valeurs pour vérification
nddi_base_ocde <- base_donnees_drought %>%
  group_by(Region) %>%
  summarise(
    NDVI_moyen = mean(NDVI_median, na.rm = TRUE),
    NDWI_moyen = mean(NDWI_median, na.rm = TRUE),
    NDDI_moyen = mean(NDDI_median, na.rm = TRUE)
  ) %>%
  mutate(NDDI_final = case_when(
    NDVI_moyen < 0.15 & NDWI_moyen < 0 ~ 0.6, 
    TRUE ~ NDDI_moyen
  ))

# --- AFFICHAGE DES VALEURS AVANT LA REPRÉSENTATION ---
VALEURS_NDDI_OCDE <- setNames(nddi_base_ocde$NDDI_final, nddi_base_ocde$Region)
print("=== VALEURS NUMÉRIQUES DE NDDI (APPROCHE 1) ===")
print(VALEURS_NDDI_OCDE)

# B. Classification avec libellés mixtes (Valeurs + Écrits)
nddi_classe_ocde <- nddi_base_ocde %>%
  mutate(Severite = case_when(
    NDDI_final < 0.2                      ~ "< 0.2 (Pas de sécheresse)",
    NDDI_final >= 0.2 & NDDI_final < 0.3  ~ "[0.2 ; 0.3[ (Sécheresse légère)",
    NDDI_final >= 0.3 & NDDI_final < 0.4  ~ "[0.3 ; 0.4[ (Sécheresse modérée)",
    NDDI_final >= 0.4 & NDDI_final < 0.5  ~ "[0.4 ; 0.5[ (Sécheresse sévère)",
    NDDI_final >= 0.5                     ~ "≥ 0.5 (Sécheresse extrême)"
  )) %>%
  mutate(Severite = factor(Severite, levels = c(
    "< 0.2 (Pas de sécheresse)", 
    "[0.2 ; 0.3[ (Sécheresse légère)", 
    "[0.3 ; 0.4[ (Sécheresse modérée)", 
    "[0.4 ; 0.5[ (Sécheresse sévère)", 
    "≥ 0.5 (Sécheresse extrême)"
  )))

# C. Jonction géographique
maroc_carte_ocde <- maroc_regions_aligned %>%
  left_join(nddi_classe_ocde, by = c("nom_fr" = "Region"))

# D. Palette de couleurs
palette_ocde <- c(
  "< 0.2 (Pas de sécheresse)"         = "#337a22", 
  "[0.2 ; 0.3[ (Sécheresse légère)"   = "#a1db74", 
  "[0.3 ; 0.4[ (Sécheresse modérée)"  = "#ffffff", 
  "[0.4 ; 0.5[ (Sécheresse sévère)"   = "#e383bd", 
  "≥ 0.5 (Sécheresse extrême)"        = "#9e1462"
)

# E. Rendu graphique
carte_1 <- ggplot(data = maroc_carte_ocde) +
  geom_sf(aes(fill = Severite), color = "grey40", linewidth = 0.3) +
  geom_sf_text(aes(label = nom_fr), size = 2.2, fontface = "bold", color = "black", 
               check_overlap = TRUE, show.legend = FALSE) +
  scale_fill_manual(
    values = palette_ocde, drop = FALSE, name = "Classes NDDI(Ilyes Boumahdi et Alberto González Pandiella)",
    guide = guide_legend(direction = "horizontal", title.position = "top", title.hjust = 0.5, nrow = 1)
  ) +
  annotation_scale(location = "br", width_hint = 0.4) +
  annotation_north_arrow(location = "tl", which_north = "true", style = north_arrow_fancy_orienteering) +
  labs(
    title = " Sévérité de la sécheresse(2024)",
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 12, color = "#1f4e79", hjust = 0.5),
    plot.subtitle = element_text(size = 9, color = "#555555", hjust = 0.5, margin = margin(b = 15)),
    legend.position = "bottom", legend.background = element_blank(),
    panel.background = element_rect(fill = "aliceblue", color = NA)
  )

print(carte_1)

# Sauvegarde propre en haute résolution
ggsave("sorties_figures/Figure_3A_Carte_NDDI_IlyesPand.png", plot = carte_1, 
       width = 11, height = 8, dpi = 300)

