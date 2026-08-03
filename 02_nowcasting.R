# =====================================================================================
# SECTION : NOWCASTING DU RISQUE DE SÉCHERESSE EXTRÊME (Q5) AU MAROC
# Modèle d'alerte précoce basé sur la fenêtre cumulée (Janvier à Juillet - Mois 1 à 7)
# =====================================================================================

# -------------------------------------------------------------------------------------
# 1. CHARGEMENT ET HARMONISATION DE LA BASE UNIFIÉE (2000 - 2026)
# -------------------------------------------------------------------------------------
# Lecture de la base consolidée de pas de temps 8 jours
df_global <- read_csv("Maroc_Serie_8jours_2000_2026_Consolidee.csv") %>%
  mutate(
    year = as.numeric(year),
    month = as.numeric(month),
    nom_fr = str_trim(nom_fr),
    nom_affich = if_else(nom_fr == "l'Oriental", "L'Oriental", nom_fr)
  )

# Agrégation au niveau mensuel (Médiane des valeurs à 8 jours)
df_mensuel <- df_global %>%
  group_by(nom_affich, year, month) %>%
  summarise(
    NDDI = median(NDDI_median, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.na(NDDI))

# Identification dynamique de la limite des données 2026 (Doit retourner Mois 7)
mois_limite_2026 <- df_mensuel %>%
  filter(year == 2026) %>%
  summarise(max_m = max(month)) %>%
  pull(max_m)

message(sprintf("✓ Identification du signal 2026 : Données disponibles de Janvier à Mois %d", mois_limite_2026))

# -------------------------------------------------------------------------------------
# 2. CONSTRUCTION DE LA VARIABLE CIBLE HISTORIQUE (QUINTILE 5 SUR 2000 - 2024)
# -------------------------------------------------------------------------------------
# Calcul du NDDI moyen annuel complet par région pour les années historiques
df_annuel_hist <- df_mensuel %>%
  filter(year <= 2024) %>%
  group_by(nom_affich, year) %>%
  summarise(NDDI_annuel = mean(NDDI, na.rm = TRUE), .groups = "drop")

# Définition du seuil du Quintile 5 (Top 20% des années les plus sèches)
seuil_q5_global <- quantile(df_annuel_hist$NDDI_annuel, probs = 0.80, na.rm = TRUE)

# Binarisation : 1 si l'année est en Sécheresse Extrême (Q5), 0 sinon
df_annuel_hist <- df_annuel_hist %>%
  mutate(Is_Q5 = if_else(NDDI_annuel >= seuil_q5_global, 1, 0))

# -------------------------------------------------------------------------------------
# 3. FEATURE ENGINEERING SUR LA FENÊTRE STRICTE (MOIS 1 À MOIS_LIMITE_2026)
# -------------------------------------------------------------------------------------
# Extraction des métriques cumulées uniquement sur les mois 1 à 7 pour TOUTES les années
df_features_cumul <- df_mensuel %>%
  filter(month <= mois_limite_2026) %>%
  group_by(nom_affich, year) %>%
  summarise(
    NDDI_moy_cumul = mean(NDDI, na.rm = TRUE),
    NDDI_max_cumul = max(NDDI, na.rm = TRUE),
    .groups = "drop"
  )

# Construction de la base d'apprentissage (2000 - 2024)
df_train <- df_features_cumul %>%
  filter(year <= 2024) %>%
  left_join(df_annuel_hist %>% select(nom_affich, year, Is_Q5), by = c("nom_affich", "year")) %>%
  mutate(
    Is_Q5 = factor(Is_Q5, levels = c(0, 1)),
    nom_affich = factor(nom_affich)
  )

# Construction de la base de prédiction Nowcast (2026)
df_nowcast_2026 <- df_features_cumul %>%
  filter(year == 2026) %>%
  mutate(nom_affich = factor(nom_affich))

# -------------------------------------------------------------------------------------
# 4. ENTRAÎNEMENT DU MODÈLE LOGIT ET VALIDATION PAR BACKTESTING (2020 - 2024)
# -------------------------------------------------------------------------------------
# Split temporel pour évaluer la capacité prédictive sans fuite de données (Data Leakage)
train_split <- df_train %>% filter(year < 2020)
test_split  <- df_train %>% filter(year >= 2020)

# Entraînement sur la période 2000-2019
model_eval <- glm(
  Is_Q5 ~ NDDI_moy_cumul + NDDI_max_cumul + nom_affich,
  data = train_split,
  family = binomial
)

# Test sur 2020-2024
test_split <- test_split %>%
  mutate(
    Prob_Q5 = predict(model_eval, newdata = test_split, type = "response"),
    Pred_Q5 = factor(if_else(Prob_Q5 >= 0.50, 1, 0), levels = c(0, 1))
  )

cat("\n===================================================================\n")
cat(" MATRICE DE CONFUSION DU MODÈLE DE NOWCASTING (TEST 2020-2024)\n")
cat("===================================================================\n")
print(confusionMatrix(test_split$Pred_Q5, test_split$Is_Q5, positive = "1"))

# -------------------------------------------------------------------------------------
# 5. NOWCASTING OFFICIEL DE L'ANNÉE 2026
# -------------------------------------------------------------------------------------
# Entraînement sur tout l'historique disponible (2000-2024) pour maximiser la puissance
model_final <- glm(
  Is_Q5 ~ NDDI_moy_cumul + NDDI_max_cumul + nom_affich,
  data = df_train,
  family = binomial
)

df_resultats_2026 <- df_nowcast_2026 %>%
  mutate(
    Prob_Q5 = predict(model_final, newdata = df_nowcast_2026, type = "response"),
    Niveau_Alerte = case_when(
      Prob_Q5 >= 0.75 ~ "Très Élevé (Q5 Quasi-Certain)",
      Prob_Q5 >= 0.50 ~ "Élevé (Risque Q5)",
      Prob_Q5 >= 0.25 ~ "Modéré",
      TRUE            ~ "Faible"
    )
  ) %>%
  arrange(desc(Prob_Q5))

# -------------------------------------------------------------------------------------
# 6. VISUALISATION 1 : GRAPHIQUE DES PROBABILITÉS DE NOWCASTING 2026
# -------------------------------------------------------------------------------------
fig_nowcast_2026 <- ggplot(df_resultats_2026, aes(x = reorder(nom_affich, Prob_Q5), y = Prob_Q5, fill = Prob_Q5)) +
  geom_col(width = 0.65) +
  geom_hline(yintercept = 0.50, linetype = "dashed", color = "#c0392b", linewidth = 0.9) +
  geom_text(aes(label = paste0(round(Prob_Q5 * 100, 1), "%")), 
            hjust = -0.15, size = 3.8, fontface = "bold") +
  scale_fill_gradient(low = "#2c6e1e", high = "#702353", name = "Probabilité Q5") +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1.15)) +
  coord_flip() +
  labs(
    title = "Nowcasting 2026 : Estimation du risque de Sécheresse Extrême (Q5)",
    subtitle = sprintf("Alerte précoce basée sur les données MODIS de Janvier à Juillet (Mois %d)", mois_limite_2026),
    x = NULL,
    y = "Probabilité estimée de basculement en Sécheresse Extrême (Q5)",
    caption = "Ligne rouge pointillée = Seuil critique de décision à 50%."
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 12, color = "#1f4e79"),
    plot.subtitle = element_text(size = 9, color = "grey30"),
    axis.text = element_text(color = "black", size = 9),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

print(fig_nowcast_2026)
ggsave("sorties_figures/Figure_Nowcasting_2026_Officielle.png", plot = fig_nowcast_2026, width = 10, height = 6.5, dpi = 300)

# -------------------------------------------------------------------------------------
# 7. VISUALISATION 2 : CARTOGRAPHIE SPATIALE COMPARATIVE (2025 vs 2026) - OPTIMISÉE
# -------------------------------------------------------------------------------------

# Calcul des quintiles historiques sur la fenêtre Janvier-Juillet
q_cuts_cumul <- quantile(
  df_features_cumul %>% filter(year <= 2024) %>% pull(NDDI_moy_cumul),
  probs = seq(0, 1, length.out = 6),
  na.rm = TRUE
)

# LABELS OPTIMISÉS : Avec retour à la ligne (\n) pour éviter d'étirer la légende en largeur
labels_quintiles <- c(
  sprintf("Q1 (Très Faible)\n[%.2f ; %.2f[", q_cuts_cumul[1], q_cuts_cumul[2]),
  sprintf("Q2 (Faible)\n[%.2f ; %.2f[", q_cuts_cumul[2], q_cuts_cumul[3]),
  sprintf("Q3 (Modéré)\n[%.2f ; %.2f[", q_cuts_cumul[3], q_cuts_cumul[4]),
  sprintf("Q4 (Sévère)\n[%.2f ; %.2f[", q_cuts_cumul[4], q_cuts_cumul[5]),
  sprintf("Q5 (Extrême)\n[%.2f ; %.2f]",  q_cuts_cumul[5], q_cuts_cumul[6])
)

# Blindage aux bornes (-Inf et +Inf) pour éviter tout NA
q_cuts_ext <- q_cuts_cumul
q_cuts_ext[1] <- -Inf
q_cuts_ext[6] <- Inf

df_map_data <- df_features_cumul %>%
  filter(year %in% c(2025, 2026)) %>%
  mutate(
    Classe_Quintile = cut(
      NDDI_moy_cumul,
      breaks = q_cuts_ext,
      include.lowest = TRUE,
      labels = labels_quintiles
    )
  )

# Chargement du Shapefile
maroc_regions <- st_read("shapefile_maroc/regions.shp", quiet = TRUE) %>%
  mutate(nom_fr = str_trim(nom_fr))

data_2025 <- df_map_data %>% filter(year == 2025)
data_2026 <- df_map_data %>% filter(year == 2026)

map_2025_sf <- maroc_regions %>% left_join(data_2025, by = c("nom_fr" = "nom_affich"))
map_2026_sf <- maroc_regions %>% left_join(data_2026, by = c("nom_fr" = "nom_affich"))

palette_couleurs <- setNames(
  c("#337a22", "#a1db74", "#ffffff", "#e383bd", "#9e1462"),
  labels_quintiles
)

# FONCTION DE RENDU REVISITÉE POUR LA LÉGENDE MUTUALISÉE
creer_carte_nowcast <- function(data_sf, titre_annee) {
  ggplot(data = data_sf) +
    geom_sf(aes(fill = Classe_Quintile), color = "grey30", linewidth = 0.35) +
    geom_sf_text(
      aes(label = nom_fr), 
      size = 2.2, 
      fontface = "bold", 
      color = "black",
      check_overlap = TRUE
    ) +
    scale_fill_manual(
      values = palette_couleurs, 
      drop = FALSE, 
      na.translate = FALSE,
      name = "Niveau de sévérité NDDI (Janvier - Juillet) :",
      guide = guide_legend(
        nrow = 1,                 # Tout aligner proprement ou passer à nrow = 2 si besoin
        title.position = "top", 
        title.hjust = 0.5,
        label.position = "bottom",
        keywidth = unit(1.8, "cm"),
        keyheight = unit(0.4, "cm")
      )
    ) +
    annotation_scale(location = "br", width_hint = 0.25, text_size = 7) +
    annotation_north_arrow(
      location = "bl", 
      pad_y = unit(0.2, "in"), 
      pad_x = unit(0.2, "in"),
      height = unit(0.8, "cm"),
      width = unit(0.8, "cm"),
      style = north_arrow_fancy_orienteering
    ) +
    labs(title = titre_annee) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 11, color = "#1f4e79", hjust = 0.5),
      panel.background = element_rect(fill = "aliceblue", color = NA),
      panel.grid.major = element_line(color = "grey90", linetype = "dotted"),
      axis.text = element_blank(), # Masque les coordonnées géographiques pour alléger l'espace
      axis.ticks = element_blank()
    )
}

p_map_2025 <- creer_carte_nowcast(map_2025_sf, "A. Sévérité observée à 7 mois (2025)")
p_map_2026 <- creer_carte_nowcast(map_2026_sf, "B. Perspective Nowcasting à 7 mois (2026)")

# COMBINAISON ET GESTION FINE DE LA LÉGENDE COMMUNE VIA PATCHWORK
cartes_cote_a_cote <- (p_map_2025 + p_map_2026) +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 10, color = "#1f4e79"),
    legend.text = element_text(size = 8, lineheight = 0.85),
    legend.margin = margin(t = 5, b = 5),
    legend.background = element_rect(fill = "white", color = "grey85", linewidth = 0.3)
  )

print(cartes_cote_a_cote)

# SAUVEGARDE AVEC HAUTEUR ET LARGEUR ADAPTÉES
ggsave(
  "sorties_figures/Carte_Cote_A_Cote_NDDI_2025_2026_Propre.png", 
  plot = cartes_cote_a_cote, 
  width = 13, 
  height = 7.5, 
  dpi = 300
)




# =====================================================================================
# FIGURE 15 : TRAJECTOIRE MENSUELLE DES RÉGIONS DU QUINTILE 5 (COMPARAISON 2025 vs 2026)
# =====================================================================================


# 1. CHARGEMENT ET PRÉPARATION DES DONNÉES MENSUELLES
df_global <- read_csv("Maroc_Serie_8jours_2000_2026_Consolidee.csv") %>%
  mutate(
    year = as.numeric(year),
    month = as.numeric(month),
    nom_fr = str_trim(nom_fr),
    nom_affich = if_else(nom_fr == "l'Oriental", "L'Oriental", nom_fr)
  )

# Aggrégation mensuelle
df_mensuel_regions <- df_global %>%
  group_by(nom_affich, year, month) %>%
  summarise(
    NDDI = median(NDDI_median, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.na(NDDI))

# 2. IDENTIFICATION DES RÉGIONS CLASSÉES EN Q5 (SÉCHERESSE EXTRÊME)
# Calcul du seuil Q5 historique (2000-2024) sur les 7 premiers mois
df_cumul_7m <- df_mensuel_regions %>%
  filter(year <= 2024 & month <= 7) %>%
  group_by(nom_affich, year) %>%
  summarise(NDDI_cumul = mean(NDDI, na.rm = TRUE), .groups = "drop")

# Seuil Q5 global (80ème percentile)
seuil_q5_seuil <- quantile(df_cumul_7m$NDDI_cumul, probs = 0.80, na.rm = TRUE)

# Identification des régions les plus vulnérables en 2026 (ou historiquement en Q5)
regions_q5 <- df_mensuel_regions %>%
  filter(year == 2026 & month <= 7) %>%
  group_by(nom_affich) %>%
  summarise(moy_2026 = mean(NDDI, na.rm = TRUE)) %>%
  filter(moy_2026 >= seuil_q5_seuil) %>%
  pull(nom_affich)

# Si moins de 4 régions ressortent, on prend les Top régions les plus sèches en 2026
if(length(regions_q5) < 4) {
  regions_q5 <- df_mensuel_regions %>%
    filter(year == 2026 & month <= 7) %>%
    group_by(nom_affich) %>%
    summarise(moy_2026 = mean(NDDI, na.rm = TRUE)) %>%
    arrange(desc(moy_2026)) %>%
    slice_head(n = 6) %>%
    pull(nom_affich)
}

# 3. PREPARATION DE LA DATASTRUCTURE POUR LE PLOT (2025 ET 2026)
df_plot_2026 <- df_mensuel_regions %>%
  filter(nom_affich %in% regions_q5 & year == 2026 & month <= 7) %>%
  rename(NDDI_2026 = NDDI)

df_plot_2025 <- df_mensuel_regions %>%
  filter(nom_affich %in% regions_q5 & year == 2025 & month <= 7) %>%
  rename(NDDI_2025 = NDDI)

# Fusion des deux années sur les mois 1 à 7
df_comparaison <- df_plot_2026 %>%
  left_join(df_plot_2025 %>% select(nom_affich, month, NDDI_2025), by = c("nom_affich", "month")) %>%
  mutate(
    Nom_Mois = factor(month, levels = 1:7, labels = c("Janvier", "Février", "Mars", "Avril", "Mai", "Juin", "Juillet"))
  )

# 4. CONSTRUCTION DU RENDU VISUEL (STYLE PANDIELLA ET AL.)
fig_15_trajectoires <- ggplot(df_comparaison, aes(x = Nom_Mois)) +
  
  # Barres bleues pour 2026
  geom_col(aes(y = NDDI_2026, fill = "2026"), width = 0.55, position = position_nudge(x = 0)) +
  
  # Points oranges pour 2025
  geom_point(aes(y = NDDI_2025, color = "2025"), size = 2.8, stroke = 1.1) +
  
  # Ligne pointillée horizontale pour le seuil de Sécheresse Extrême (Q5)
  geom_hline(aes(yintercept = seuil_q5_seuil, linetype = "Sécheresse extrême (Q5)"), 
             color = "#702353", linewidth = 0.8) +
  
  # Multi-panneaux par région
  facet_wrap(~ nom_affich, ncol = 2, scales = "free_y") +
  
  # Échelles manuelles de couleurs et de légende
  scale_fill_manual(name = NULL, values = c("2026" = "#1b4e6b")) +
  scale_color_manual(name = NULL, values = c("2025" = "#e66101")) +
  scale_linetype_manual(name = NULL, values = c("Sécheresse extrême (Q5)" = "dashed")) +
  
  labs(
    title = "Évolution mensuelle de la sévérité de la sécheresse pour les régions les plus exposées (Q5)",
    subtitle = "Comparaison des données de Janvier à Juillet entre 2025 et 2026",
    x = NULL,
    y = "Indice NDDI"
      ) +
  
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 12, color = "#1f4e79"),
    plot.subtitle = element_text(size = 9.5, color = "grey30"),
    strip.background = element_rect(fill = "grey95", color = "grey80"),
    strip.text = element_text(face = "bold", size = 10, color = "#1f4e79"),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 8.5, color = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.border = element_rect(color = "grey85", fill = NA, linewidth = 0.5),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.margin = margin(t = -5)
  )

print(fig_15_trajectoires)

# Sauvegarde de la figure
ggsave(
  "sorties_figures/Figure_15_Comparaison_Mensuelle_Q5_2025_2026.png",
  plot = fig_15_trajectoires,
  width = 11,
  height = 8.5,
  dpi = 300
)