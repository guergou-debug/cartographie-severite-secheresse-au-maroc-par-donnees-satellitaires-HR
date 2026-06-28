# =========================================================================
# PROJET DE RECHERCHE : REPRODUCTION DE LA MÉTHODOLOGIE OCDE
# ÉTAPE 7 : SCRIPT DE LA FIGURE 1 - ÉVOLUTION TEMPORELLE DU NDVI ET NDWI
# =========================================================================

library(tidyverse)
library(scales)

# 1. Lecture et restructuration des données au format long (Tidy Data)
# Pour tracer deux courbes sur le même graphique, on passe les indices en lignes
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
    title = "Figure 1 : Profils temporels comparatifs du NDVI et du NDWI (2024)",
    subtitle = "Suivi de la phénologie et de l'humidité de surface par Région - Données MODIS 500m",
    x = "Mois de l'année 2024",
    y = "Valeur de l'indice",
    color = "Indice spectral :",
    caption = "Source : Traitement basé sur le produit MOD09A1. Note : Les valeurs négatives du NDWI indiquent un déficit hydrique sévère du sol et des tissus foliaires."
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


# =========================================================================
# PROJET DE RECHERCHE : REPRODUCTION DE LA MÉTHODOLOGIE OCDE
# ÉTAPE 7B : NUAGE DE POINTS ET CORRÉLATION STATISTIQUE NDVI VS NDWI
# =========================================================================


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
    title = "Figure 1b : Corrélation et couplage biophysique entre le NDWI et le NDVI",
    subtitle = "Validation de la cohérence interne des indicateurs de stress par Région (Annexe A)",
    x = "Teneur en eau de surface et des tissus foliaires (NDWI)",
    y = "Vigueur de la couverture végétale (NDVI)",
    color = "Régions du Maroc :",
    caption = paste0("Source : Traitement doctoral sur données MODIS 500m (2024).\nNote : La droite rouge représente la régression linéaire globale. Coefficient de Pearson R = ", round(r_pearson, 3))
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
# ÉTAPE 8 (CORRIGÉE) : CARTOGRAPHIE AVEC AJOUT DES NOMS DES RÉGIONS
# =========================================================================

# 1. Calcul des statistiques de synthèse annuelles par Région
statistiques_annuelles <- base_donnees_drought %>%
  group_by(Region) %>%
  summarise(
    NDVI_annuel = median(NDVI_median, na.rm = TRUE),
    NDWI_annuel = median(NDWI_median, na.rm = TRUE)
  )

# 2. Jonction (Merge) avec le Shapefile officiel
maroc_cartes <- maroc_regions_aligned %>%
  left_join(statistiques_annuelles, by = c("nom_fr" = "Region"))

# =========================================================================
# 3. CARTE NDVI AVEC ÉTIQUETTES DES NOMS
# =========================================================================
carte_ndvi <- ggplot(data = maroc_cartes) +
  geom_sf(aes(fill = NDVI_annuel), color = "white", size = 0.3) +
  # Ajout textuel des noms de régions (basé sur la colonne nom_fr)
  geom_sf_text(aes(label = nom_fr), size = 2.2, fontface = "bold", color = "black", check_overlap = TRUE) +
  scale_fill_distiller(palette = "YlGn", direction = 1, name = "NDVI Annuel") +
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         style = north_arrow_fancy_orienteering) +
  labs(
    title = "A. Distribution Spatiale du NDVI",
    subtitle = "Vigueur de la couverture végétale (Médiane 2024)"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = 11),
    plot.subtitle = element_text(size = 9, color = "grey40"),
    panel.background = element_rect(fill = "aliceblue", color = NA),
    legend.position = "bottom"
  )

# =========================================================================
# 4. CARTE NDWI AVEC ÉTIQUETTES DES NOMS
# =========================================================================
carte_ndwi <- ggplot(data = maroc_cartes) +
  geom_sf(aes(fill = NDWI_annuel), color = "white", size = 0.3) +
  # Ajout textuel des noms de régions (basé sur la colonne nom_fr)
  geom_sf_text(aes(label = nom_fr), size = 2.2, fontface = "bold", color = "black", check_overlap = TRUE) +
  scale_fill_distiller(palette = "YlGnBu", direction = 1, name = "NDWI Annuel") +
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         style = north_arrow_fancy_orienteering) +
  labs(
    title = "B. Distribution Spatiale du NDWI",
    subtitle = "Indice d'humidité de surface (Médiane 2024)"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = 11),
    plot.subtitle = element_text(size = 9, color = "grey40"),
    panel.background = element_rect(fill = "aliceblue", color = NA),
    legend.position = "bottom"
  )

# =========================================================================
# 5. COMBINAISON ET SAUVEGARDE EN HAUTE RÉSOLUTION
# =========================================================================
cartes_combineen <- carte_ndvi + carte_ndwi + 
  plot_annotation(
    title = "Figure 2 : Diagnostic spatial de la couverture végétale et de l'état hydrique au Maroc",
    subtitle = "Analyses régionales agrégées basées sur l'imagerie MODIS à 500 mètres",
    caption = "Source : Pipeline  de reproduction méthodologique . Frontières unifiées du Royaume.",
    theme = theme(plot.title = element_text(face = "bold", size = 13, hjust = 0.5))
  )

print(cartes_combineen)

ggsave("sorties_figures/Figure_2_Cartes_Synthese_NDVI_NDWI_Noms.png", plot = cartes_combineen, 
       width = 13, height = 8, dpi = 300)


# =========================================================================
# ÉTAPE 9 (CORRIGÉE) : CLAMPING ET CARTOGRAPHIE SCIENTIFIQUE DU NDDI
# =========================================================================

# 1. Nettoyage et calcul de la moyenne annuelle robuste
nddi_annuel_classe <- base_donnees_drought %>%
  # ÉTAPE CRUCIALE : On borne le NDDI entre 0 et 1 pour chaque date d'abord !
  mutate(NDDI_clamped = case_when(
    NDDI_median < 0 ~ 0,   # Pas de stress hydrique ou anomalie aride = 0
    NDDI_median > 1 ~ 1,   # Saturation du stress = 1
    TRUE ~ NDDI_median
  )) %>%
  # Maintenant, on peut faire la moyenne annuelle sur des valeurs stables
  group_by(Region) %>%
  summarise(NDDI_moyen_annuel = mean(NDDI_clamped, na.rm = TRUE)) %>%
  
  # 2. Application des seuils du papier
  mutate(Severite = case_when(
    NDDI_moyen_annuel < 0.3 ~ "Normal / Pas de sécheresse",
    NDDI_moyen_annuel >= 0.3 & NDDI_moyen_annuel < 0.4 ~ "Sécheresse Modérée",
    NDDI_moyen_annuel >= 0.4 & NDDI_moyen_annuel < 0.5 ~ "Sécheresse Sévère",
    NDDI_moyen_annuel >= 0.5 ~ "Sécheresse Extrême"
  )) %>%
  mutate(Severite = factor(Severite, levels = c("Normal / Pas de sécheresse", 
                                                "Sécheresse Modérée", 
                                                "Sécheresse Sévère", 
                                                "Sécheresse Extrême")))

# 3. Jonction avec le Shapefile officiel
maroc_nddi_carte <- maroc_regions_aligned %>%
  left_join(nddi_annuel_classe, by = c("nom_fr" = "Region"))

# 4. Palette de couleurs
palette_severite <- c(
  "Normal / Pas de sécheresse" = "#2ca02c", # Vert
  "Sécheresse Modérée"         = "#ff7f0e", # Orange
  "Sécheresse Sévère"          = "#d62728", # Rouge
  "Sécheresse Extrême"         = "#6E3A0F"  # Marron
)

# 5. Rendu de la Carte Finale Correcte
carte_nddi_finale <- ggplot(data = maroc_nddi_carte) +
  geom_sf(aes(fill = Severite), color = "white", size = 0.4) +
  geom_sf_text(aes(label = nom_fr), size = 2.3, fontface = "bold", color = "black", check_overlap = TRUE) +
  scale_fill_manual(values = palette_severite, drop = FALSE, name = "Statut de Sécheresse :") +
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         style = north_arrow_fancy_orienteering) +
  labs(
    title = "Figure 3 : Cartographie révisée de la sévérité de la sécheresse au Maroc (2024)",
    subtitle = "Correction biophysique par clamping du NDDI spatialisé sur données MODIS 500m",
    caption = "Source : Réplication de la méthodologie OCDE (Boumahdi & González Pandiella, 2026).\nNote : Les indices négatifs liés aux sols nus ont été neutralisés à 0 avant agrégation annuelle."
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 12, color = "#222222", hjust = 0.5),
    plot.subtitle = element_text(size = 9, color = "#555555", hjust = 0.5, margin = margin(b = 15)),
    legend.position = "bottom",
    panel.background = element_rect(fill = "aliceblue", color = NA)
  )

# Affichage
print(carte_nddi_finale)

# Sauvegarde de la version scientifique
ggsave("sorties_figures/Figure_3_Carte_Drought_NDDI_CORRIGEE.png", plot = carte_nddi_finale, 
       width = 10, height = 9, dpi = 300)