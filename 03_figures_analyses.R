# =========================================================================
# ÉTAPE 2: FIGURE 1 - ÉVOLUTION TEMPORELLE DU NDVI ET NDWI (ANNÉE 2025)
# =========================================================================

# 1. Lecture et restructuration des données au format long (Tidy Data)
donnees_raw <- read.csv("Maroc_Serie_8jours_2025.csv")

donnees_longues <- donnees_raw %>% 
  mutate(date = as.Date(date)) %>%
  rename(Region = nom_fr) %>%
  select(Region, date, NDVI, NDWI) %>%
  pivot_longer(cols = c(NDVI, NDWI), 
               names_to = "Indice", 
               values_to = "Valeur") %>%
  mutate(Indice = case_when(
    Indice == "NDVI" ~ "NDVI (Végétation)",
    Indice == "NDWI" ~ "NDWI (Eau / Humidité)"
  ))

# 2. Construction de la Figure au format publication
figure_1_hcp <- ggplot(data = donnees_longues, aes(x = date, y = Valeur, color = Indice)) +
  # Lignes d'évolution
  geom_line(linewidth = 0.7, alpha = 0.8) +
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
  # Habillage
  labs(
    title = "Profils temporels comparatifs du NDVI et du NDWI (2025)",
    x = "Mois de l'année 2025",
    y = "Valeur de l'indice",
    color = "Indice :"
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

# 4. Sauvegarde automatique en Haute Résolution (300 DPI) dans le dossier de sortie
ggsave("sorties_figures/Figure_1_Profils_NDVI_NDWI_2025.png", plot = figure_1_hcp, 
       width = 11, height = 8, dpi = 300)

# ===============================================================================
# ÉTAPE 2: FIGURE 2 - NUAGE DE POINTS ET CORRÉLATION STATISTIQUE NDVI VS NDWI (2025)
# ===============================================================================

# 1. Chargement du dataset 2025
donnees_drought <- read.csv("Maroc_Serie_8jours_2025.csv") %>% 
  mutate(date = as.Date(date)) %>% 
  rename(Region = nom_fr)

# 2. Calcul du coefficient de corrélation de Pearson global
r_pearson <- cor(donnees_drought$NDWI, donnees_drought$NDVI, use = "complete.obs")
cat("\n--- VALIDATION STATISTIQUE ---")
cat("\nLe coefficient de corrélation de Pearson (R) pour 2025 est de :", round(r_pearson, 4), "\n\n")

# 3. Création du graphique de corrélation (Charte graphique OCDE)
figure_correlation <- ggplot(data = donnees_drought, aes(x = NDWI, y = NDVI)) +
  # Points de mesure colorés par région pour identifier les grappes (clusters) géographiques
  geom_point(aes(color = Region), size = 2, alpha = 0.75) +
  # Ajout de la droite de tendance linéaire avec son intervalle de confiance (se = TRUE)
  geom_smooth(method = "lm", color = "#d62728", linewidth = 1, linetype = "solid", se = TRUE) +
  # Configuration des axes
  scale_x_continuous(limits = c(-0.5, 0.5), breaks = seq(-0.5, 0.5, 0.2)) +
  scale_y_continuous(limits = c(0, 0.8), breaks = seq(0, 0.8, 0.2)) +
  # Habillage académique
  labs(
    title = "Corrélation entre le NDWI et le NDVI (2025)",
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
ggsave("sorties_figures/Figure_2_Correlation_NDVI_NDWI_2025.png", plot = figure_correlation, 
       width = 9, height = 6, dpi = 300)


# =========================================================================
# ÉTAPE 2: FIGURE 3 - CARTOGRAPHIE PAR QUARTILES DE NDWI ET NDVI (2025)
# =========================================================================

# 1. Palettes de couleurs 
PALETTE_NDVI_PERSO <- c(
  "Faible"                = "#4B0082",
  "Intermédiaire faible" = "#DDA0DD",
  "Intermédiaire élevé"  = "#90EE90",
  "Élevé"                 = "#006400"
)

PALETTE_NDWI_PERSO <- c(
  "Faible"                = "#8B0000",
  "Intermédiaire faible" = "#E1C16E",
  "Intermédiaire élevé"  = "#87CEEB",
  "Élevé"                 = "#00008B"
)

# 2. Chargement des données 2025 & calcul des statistiques de synthèse (médiane) + quartiles
base_donnees_drought <- read.csv("Maroc_Serie_8jours_2025.csv")

statistiques_annuelles <- base_donnees_drought %>%
  group_by(nom_fr) %>%
  summarise(
    NDVI_brut = median(NDVI, na.rm = TRUE),
    NDWI_brut = median(NDWI, na.rm = TRUE)
  ) %>%
  mutate(
    # Groupes égaux basés sur la distribution observée en 2025
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

# 3. Jonction (Merge) avec le Shapefile officiel maroc_regions
maroc_cartes <- maroc_regions %>%
  left_join(statistiques_annuelles, by = "nom_fr")

# =========================================================================
# CARTE NDVI STRUCTURÉE PAR QUARTILES
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
    subtitle = "Quartiles (2025)"
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
  annotation_north_arrow(location = "bl", which_north = "true", pad_y = unit(0.6, "in"),
                         style = north_arrow_fancy_orienteering) +
  labs(
    title = "B. Distribution spatiale du NDWI",
    subtitle = "Quartiles (2025)"
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
cartes_combinees <- carte_ndvi + carte_ndwi + 
  plot_annotation(
    title = "Diagnostic spatial de la couverture végétale et de l'état hydrique (2025)",
    theme = theme(plot.title = element_text(face = "bold", size = 13, hjust = 0.5))
  )

print(cartes_combinees)

# Sauvegarde
ggsave("sorties_figures/Figure_3_Cartes_Synthese_NDVI_NDWI_2025.png", plot = cartes_combinees, 
       width = 13, height = 8, dpi = 300)


# ===============================================================================
# ÉTAPE 2: FIGURE 4 - CARTOGRAPHIE DU NDDI PAR QUINTILES (2025)
# ===============================================================================

# 1. Chargement du jeu de données 2025
base_donnees_drought <- read.csv("Maroc_Serie_8jours_2025.csv")

# 2. Calcul du NDDI moyen par région sans forcage artificiel
nddi_base_2025 <- base_donnees_drought %>%
  group_by(nom_fr) %>%
  summarise(
    NDDI_moyen = mean(NDDI_corrige, na.rm = TRUE)
  )

# --- AFFICHAGE DES VALEURS NUMÉRIQUES DE NDDI BRUT ---
VALEURS_NDDI_2025 <- setNames(nddi_base_2025$NDDI_moyen, nddi_base_2025$nom_fr)
print("=== VALEURS NUMÉRIQUES DU NDDI MOYEN 2025 PAR RÉGION ===")
print(VALEURS_NDDI_2025)

# 3. Découpage en 5 Quintiles (Relativement à la distribution réelle)
nddi_quintile <- nddi_base_2025 %>%
  mutate(
    Quintile = ntile(NDDI_moyen, 5),
    Severite_Quintile = case_when(
      Quintile == 1 ~ "Q1 : Faible sécheresse",
      Quintile == 2 ~ "Q2 : Sécheresse modérée",
      Quintile == 3 ~ "Q3 : Sécheresse intermédiaire",
      Quintile == 4 ~ "Q4 : Sécheresse forte",
      Quintile == 5 ~ "Q5 : Sécheresse très forte"
    )
  ) %>%
  mutate(Severite_Quintile = factor(Severite_Quintile, levels = c(
    "Q1 : Faible sécheresse",
    "Q2 : Sécheresse modérée",
    "Q3 : Sécheresse intermédiaire",
    "Q4 : Sécheresse forte",
    "Q5 : Sécheresse très forte"
  )))

# 4. Jonction géographique avec le Shapefile officiel
maroc_carte_quintile <- maroc_regions %>%
  left_join(nddi_quintile, by = "nom_fr")

# 5. Palette personnalisée (Hexadécimaux issus de la charte de référence)
palette_ocde_quintiles <- c(
  "Q1 : Faible sécheresse"          = "#337a22", # Vert foncé
  "Q2 : Sécheresse modérée"        = "#a1db74", # Vert clair
  "Q3 : Sécheresse intermédiaire"  = "#ffffff", # Blanc
  "Q4 : Sécheresse forte"          = "#e383bd", # Rose / Violet clair
  "Q5 : Sécheresse très forte"     = "#9e1462"  # Violet foncé
)

# 6. Construction de la carte sous ggplot2
carte_nddi_quintiles <- ggplot(data = maroc_carte_quintile) +
  geom_sf(aes(fill = Severite_Quintile), color = "grey40", linewidth = 0.3) +
  geom_sf_text(aes(label = nom_fr), size = 2.2, fontface = "bold", color = "black", check_overlap = TRUE) +
  scale_fill_manual(
    values = palette_ocde_quintiles, 
    drop = FALSE, 
    name = "Niveaux de sévérité du NDDI (Quintiles)",
    guide = guide_legend(
      direction = "horizontal", 
      title.position = "top", 
      title.hjust = 0.5, 
      nrow = 1
    )
  ) +
  annotation_scale(location = "br", width_hint = 0.3) +
  annotation_north_arrow(
    location = "bl", 
    which_north = "true", 
    pad_y = unit(0.5, "in"),
    style = north_arrow_fancy_orienteering
  ) +
  labs(
    title = "Sévérité de la sécheresse au Maroc - NDDI (2025)",
    subtitle = "Classification par quintiles régionaux"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13, color = "#1f4e79", hjust = 0.5),
    plot.subtitle = element_text(size = 10, color = "#555555", hjust = 0.5, margin = margin(b = 12)),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 9),
    legend.text = element_text(size = 8),
    panel.background = element_rect(fill = "aliceblue", color = NA)
  )

# 7. Affichage dans RStudio
print(carte_nddi_quintiles)

# 8. Sauvegarde propre en haute résolution (300 DPI)
ggsave("sorties_figures/Figure_4_Carte_NDDI_Quintiles_2025.png", plot = carte_nddi_quintiles, 
       width = 10, height = 8, dpi = 300)



# ===============================================================================
# ÉTAPE 2: FIGURE 4 - NDDI PAR QUINTELES AVEC INTERVALLES EXACTS (2025)
# ===============================================================================

# 1. Chargement du jeu de données 2025
base_donnees_drought <- read.csv("Maroc_Serie_8jours_2025.csv")

# 2. Calcul du NDDI moyen par région (brut/corrigé)
nddi_base_2025 <- base_donnees_drought %>%
  group_by(nom_fr) %>%
  summarise(
    NDDI_moyen = mean(NDDI_corrige, na.rm = TRUE)
  )

# 3. Calcul des bornes exactes des quintiles (0%, 20%, 40%, 60%, 80%, 100%)
bornes <- quantile(nddi_base_2025$NDDI_moyen, probs = seq(0, 1, 0.2), na.rm = TRUE)

# Découpage avec des libellés d'intervalles explicites
nddi_quintile <- nddi_base_2025 %>%
  mutate(
    Severite_Quintile = cut(
      NDDI_moyen,
      breaks = bornes,
      include.lowest = TRUE,
      labels = c(
        paste0("Q1 : Faible [", round(bornes[1], 2), " ; ", round(bornes[2], 2), "]"),
        paste0("Q2 : Modérée ]", round(bornes[2], 2), " ; ", round(bornes[3], 2), "]"),
        paste0("Q3 : Intermédiaire ]", round(bornes[3], 2), " ; ", round(bornes[4], 2), "]"),
        paste0("Q4 : Forte ]", round(bornes[4], 2), " ; ", round(bornes[5], 2), "]"),
        paste0("Q5 : Très forte ]", round(bornes[5], 2), " ; ", round(bornes[6], 2), "]")
      )
    )
  )

# 4. Jonction géographique avec le Shapefile officiel
maroc_carte_quintile <- maroc_regions %>%
  left_join(nddi_quintile, by = "nom_fr")

# 5. Extraction des noms exacts des niveaux pour les associer à la palette
niveaux_labels <- levels(nddi_quintile$Severite_Quintile)

palette_ocde_quintiles <- setNames(
  c("#337a22", "#a1db74", "#ffffff", "#e383bd", "#9e1462"),
  niveaux_labels
)

# 6. Rendu graphique sous ggplot2
carte_nddi_quintiles <- ggplot(data = maroc_carte_quintile) +
  geom_sf(aes(fill = Severite_Quintile), color = "grey40", linewidth = 0.3) +
  geom_sf_text(aes(label = nom_fr), size = 2.2, fontface = "bold", color = "black", check_overlap = TRUE) +
  scale_fill_manual(
    values = palette_ocde_quintiles, 
    drop = FALSE, 
    name = "Quintiles du NDDI (Intervalles des valeurs)",
    guide = guide_legend(
      direction = "horizontal", 
      title.position = "top", 
      title.hjust = 0.5, 
      nrow = 2 # Disposé sur 2 lignes pour une lisibilité parfaite des intervalles
    )
  ) +
  annotation_scale(location = "br", width_hint = 0.3) +
  annotation_north_arrow(
    location = "bl", 
    which_north = "true", 
    pad_y = unit(0.5, "in"),
    style = north_arrow_fancy_orienteering
  ) +
  labs(
    title = "Sévérité de la sécheresse au Maroc - NDDI (2025)",
    subtitle = "Classification par quintiles régionaux avec bornes numériques"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13, color = "#1f4e79", hjust = 0.5),
    plot.subtitle = element_text(size = 10, color = "#555555", hjust = 0.5, margin = margin(b = 12)),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 9),
    legend.text = element_text(size = 8),
    panel.background = element_rect(fill = "aliceblue", color = NA)
  )

# 7. Affichage et Sauvegarde
print(carte_nddi_quintiles)

ggsave("sorties_figures/Figure_4_Carte_NDDI_Quintiles_Intervalles_2025.png", 
       plot = carte_nddi_quintiles, width = 10, height = 8, dpi = 300)