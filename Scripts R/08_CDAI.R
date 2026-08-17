# ========================================================================================
# PROJET : Indice Composite de Sécheresse par Télédétection (CDAI-Maroc)
# ÉTAPE 3 : NETTOYAGE, SÉCURITÉ SCIENTIFIQUE ET CALCUL DES ANOMALIES CLIMATOLOGIQUES
# ========================================================================================

library(tidyverse)
library(lubridate)

# 1. Chargement des données brutes issues de GEE
chemin_csv <- "Maroc_Drought_12Regions_2000_2026.csv"
donnees_brutes <- read_csv(chemin_csv, show_col_types = FALSE)

cat("--- Dimensions initiales de la base :", dim(donnees_brutes), "---\n")

# 2. Filtrage Temporel et Nettoyage des Artefacts Satellitaires (0 et NA)
donnees_propres <- donnees_brutes %>%
  # Correction du cadrage temporel : Février 2000 à Juin 2026
  filter(!(year == 2000 & month == 1)) %>%    # Supprime Janvier 2000 (MODIS inactif)
  filter(!(year == 2026 & month > 6)) %>%     # Garde Nowcasting 2026 jusqu'à Juin
  # Remplacement des valeurs 0 aberrantes sur LST et NDVI par NA s'il en reste
  mutate(
    NDVI = ifelse(NDVI <= 0, NA, NDVI),
    LST  = ifelse(LST <= 0, NA, LST)
  ) %>%
  # Imputation douce par interpolation linéaire régionale si NA ponctuel
  group_by(nom_fr) %>%
  arrange(year, month) %>%
  mutate(
    NDVI = zoo::na.approx(NDVI, na.rm = FALSE),
    LST  = zoo::na.approx(LST, na.rm = FALSE),
    P    = zoo::na.approx(P, na.rm = FALSE)
  ) %>%
  ungroup()

cat("--- Dimensions après nettoyage temporel :", dim(donnees_propres), "---\n")

# 3. Calcul de la Climatologie de Référence par Région et par Mois (2000–2025)
climatologie_ref <- donnees_propres %>%
  filter(year >= 2000 & year <= 2025) %>%
  group_by(nom_fr, month) %>%
  summarise(
    P_mean    = mean(P, na.rm = TRUE),
    P_sd      = sd(P, na.rm = TRUE),
    NDVI_mean = mean(NDVI, na.rm = TRUE),
    NDVI_sd   = sd(NDVI, na.rm = TRUE),
    NDVI_min  = min(NDVI, na.rm = TRUE),
    NDVI_max  = max(NDVI, na.rm = TRUE),
    LST_mean  = mean(LST, na.rm = TRUE),
    LST_sd    = sd(LST, na.rm = TRUE),
    LST_min   = min(LST, na.rm = TRUE),
    LST_max   = max(LST, na.rm = TRUE),
    .groups   = "drop"
  )

# 4. Jointure et Calcul des Anomalies Standardisées (Z-Scores)
donnees_anomalies <- donnees_propres %>%
  left_join(climatologie_ref, by = c("nom_fr", "month")) %>%
  mutate(
    # Z-scores des anomalies climatologiques
    Z_P    = (P - P_mean) / ifelse(P_sd == 0, 1, P_sd),
    Z_NDVI = (NDVI - NDVI_mean) / ifelse(NDVI_sd == 0, 1, NDVI_sd),
    # Pour la LST, une température plus élevée indique une sécheresse -> Inversion de signe
    Z_LST  = -(LST - LST_mean) / ifelse(LST_sd == 0, 1, LST_sd),
    
    # Indices conditionnels VCI et TCI (0 à 100%)
    VCI    = 100 * (NDVI - NDVI_min) / (NDVI_max - NDVI_min + 1e-5),
    TCI    = 100 * (LST_max - LST) / (LST_max - LST_min + 1e-5)
  )

# 5. Contrôle Qualité Visuel des Anomalies Calculées
head(donnees_anomalies %>% select(nom_fr, year, month, Z_P, Z_NDVI, Z_LST, VCI, TCI))

# Sauvegarde de la base propre et enrichie
write_csv(donnees_anomalies, "sorties_figures/Maroc_Anomalies_Clean_2000_2026.csv")
cat("--- Base d'anomalies climatologiques enregistrée avec succès ! ---\n")




# ========================================================================================
# PROJET : Indice Composite de Sécheresse par Télédétection (CDAI-Maroc)
# ÉTAPE 4 : ANALYSE EN COMPOSANTES PRINCIPALES (ACP), NORMALISATION & NOWCASTING 2026
# ========================================================================================

# 1. Chargement des bibliothèques statistiques
library(tidyverse)
library(FactoMineR)
library(factoextra)
library(patchwork)

# 2. Chargement de la base nettoyée d'anomalies
donnees_anomalies <- read_csv("sorties_figures/Maroc_Anomalies_Clean_2000_2026.csv", show_col_types = FALSE)

# 3. Séparation de la base : Calibration (2000-2025) vs Nowcasting (2026)
donnees_calib <- donnees_anomalies %>% filter(year <= 2025)
donnees_2026  <- donnees_anomalies %>% filter(year == 2026)

# Reconstruction d'un dataframe unifié avec identifiants pour FactoMineR
df_acp_input <- bind_rows(donnees_calib, donnees_2026)

# Indices des lignes de calibration et des individus supplémentaires (2026)
ind_calibration <- 1:nrow(donnees_calib)
ind_supp        <- (nrow(donnees_calib) + 1):nrow(df_acp_input)

# Matrice pour FactoMineR
matrice_acp <- df_acp_input %>% 
  select(Z_P, Z_NDVI, Z_LST)

# 4. Exécution de l'ACP (Calibration 2000-2025 avec individus supplémentaires 2026)
res.acp <- PCA(
  matrice_acp,
  ind.sup = ind_supp,
  scale.unit = TRUE,
  graph = FALSE
)

# 5. Extraction et Sauvegarde des Diagnostics de l'ACP
cat("\n=== DIAGNOSTIC FACTORIEL DE L'ACP ===\n")
valeurs_propres <- get_eigenvalue(res.acp)
print(valeurs_propres)

lambda_1 <- valeurs_propres[1, 1] # Première valeur propre ( Variance de PC1 )
cat(sprintf("\nPremière Valeur Propre (Lambda 1) : %.4f (Variance expliquée : %.2f%%)\n", 
            lambda_1, valeurs_propres[1, 2]))

# Cercle des corrélations et Éboulis des valeurs propres
p_eboulis <- fviz_eig(res.acp, addlabels = TRUE, ylim = c(0, 70),
                      title = "Éboulis des Valeurs Propres (Variance Expliquée)",
                      ggtheme = theme_minimal())

p_cercle  <- fviz_pca_var(res.acp, col.var = "contrib",
                          gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                          repel = TRUE,
                          title = "Cercle des Corrélations (Variables actives)",
                          ggtheme = theme_minimal())

fig_diagnostics <- p_eboulis + p_cercle
ggsave("sorties_figures/ACP_Diagnostics_Factoriels.png", fig_diagnostics, width = 10, height = 4.5, dpi = 300)

# 6. Extraction du PC1 et Calcul du CDAI Normalisé
# Extraction des coordonnées PC1 pour tous les individus (actifs + supplémentaires)
pc1_actifs <- res.acp$ind$coord[, 1]
pc1_supp   <- res.acp$ind.sup$coord[, 1]
pc1_total  <- c(pc1_actifs, pc1_supp)

# Normalisation par la racine carrée de Lambda_1 (Z-Score Fitting)
df_cdai_final <- df_acp_input %>%
  mutate(
    PC1_raw = pc1_total,
    CDAI    = PC1_raw / sqrt(lambda_1),
    
    # Classification par seuils de sévérité officiels
    Severite = case_when(
      CDAI >= -0.50 ~ "Normal",
      CDAI >= -0.80 & CDAI < -0.50 ~ "Anormalement sec",
      CDAI >= -1.30 & CDAI < -0.80 ~ "Sécheresse modérée",
      CDAI >= -1.60 & CDAI < -1.30 ~ "Sécheresse sévère",
      CDAI >= -2.00 & CDAI < -1.60 ~ "Sécheresse extrême",
      CDAI <  -2.00 ~ "Sécheresse exceptionnelle"
    ),
    Severite = factor(Severite, levels = c(
      "Normal", "Anormalement sec", "Sécheresse modérée", 
      "Sécheresse sévère", "Sécheresse extrême", "Sécheresse exceptionnelle"
    ))
  )

# Exportation du dataset final enrichi
write_csv(df_cdai_final, "sorties_figures/Maroc_CDAI_Final_2000_2026.csv")
cat("\n--- Jeu de données final CDAI 2000-2026 exporté avec succès ! ---\n")

# 7. Graphique Chronologique du CDAI Régional (Exemple : Comparaison Nord vs Sud)
p_chronologie <- df_cdai_final %>%
  filter(nom_fr %in% c("Fès-Meknès", "Rabat-Salé-Kénitra", "Marrakech-Safi", "Souss-Massa")) %>%
  mutate(Date = ymd(paste(year, month, "01", sep = "-"))) %>%
  ggplot(aes(x = Date, y = CDAI, fill = Severite)) +
  geom_col(width = 25) +
  geom_hline(yintercept = c(-0.5, -0.8, -1.3, -1.6, -2.0), linetype = "dashed", color = "black", alpha = 0.5) +
  scale_fill_manual(values = c(
    "Normal" = "#2c7bb6", 
    "Anormalement sec" = "#abd9e9", 
    "Sécheresse modérée" = "#ffffbf", 
    "Sécheresse sévère" = "#fdae61", 
    "Sécheresse extrême" = "#d7191c", 
    "Sécheresse exceptionnelle" = "#800026"
  )) +
  facet_wrap(~nom_fr, ncol = 2) +
  labs(
    title = "Évolution Historique de l'Indice Composite de Sécheresse (CDAI-Maroc)",
    subtitle = "Séries temporelles 2000–2026 (Nowcasting inclus) avec seuils d'impact",
    x = "Année", y = "Indice CDAI (Standardisé N(0,1))",
    fill = "Niveau de Sécurité / Sévérité"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 11),
    plot.title = element_text(face = "bold", size = 14)
  )

print(p_chronologie)
ggsave("sorties_figures/CDAI_Chronologie_Regionale.png", p_chronologie, width = 12, height = 7, dpi = 300)




# ========================================================================================
# PROJET : CDAI-Maroc — Analyse Temporelle et Matrice Spatio-Temporelle
# ========================================================================================

library(tidyverse)
library(lubridate)
library(scales)

# 1. Chargement et Préparation des Données Mensuelles
df_cdai <- read_csv("sorties_figures/Maroc_CDAI_Final_2000_2026.csv", show_col_types = FALSE) %>%
  mutate(Date = ymd(paste(year, month, "01", sep = "-"))) %>%
  arrange(nom_fr, Date)

# ----------------------------------------------------------------------------------------
# FIGURE 1 : DYNAMIQUE TEMPORELLE NATIONALE (Série Continue 2000–2026)
# ----------------------------------------------------------------------------------------
df_nat <- df_cdai %>%
  group_by(Date) %>%
  summarise(CDAI_nat = mean(CDAI, na.rm = TRUE), .groups = "drop")

p1 <- ggplot(df_nat, aes(x = Date, y = CDAI_nat)) +
  # Remplissage sous la courbe (Rouge = Sécheresse, Bleu = Humide)
  geom_ribbon(aes(ymin = pmin(CDAI_nat, 0), ymax = 0), fill = "#d7191c", alpha = 0.35) +
  geom_ribbon(aes(ymin = 0, ymax = pmax(CDAI_nat, 0)), fill = "#2c7bb6", alpha = 0.25) +
  # Ligne principale
  geom_line(color = "#1f77b4", linewidth = 0.8) +
  # Lignes de référence et seuils
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", alpha = 0.7) +
  geom_hline(yintercept = -0.8, linetype = "dotted", color = "orange", linewidth = 0.8) +
  geom_hline(yintercept = -1.6, linetype = "dotted", color = "red", linewidth = 0.8) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y", expand = c(0.01, 0.01)) +
  labs(
    title = "Dynamique Temporelle Nationale de l'Indice Composite de Sécheresse (CDAI : 2000–2026)",
    x = "Chronologie (Années)",
    y = "Indice CDAI Standardisé"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90", linetype = "dashed")
  )

print(p1)

ggsave("sorties_figures/Figure1_CDAI_National_Evolution.png", p1, width = 13, height = 5, dpi = 300)

# ----------------------------------------------------------------------------------------
# FIGURE 2 : TRAJECTOIRES MENSUELLES PAR RÉGION (Panneau Facetté 4x3)
# ----------------------------------------------------------------------------------------
p2 <- ggplot(df_cdai, aes(x = Date, y = CDAI)) +
  geom_ribbon(aes(ymin = pmin(CDAI, 0), ymax = 0), fill = "#d7191c", alpha = 0.35) +
  geom_ribbon(aes(ymin = 0, ymax = pmax(CDAI, 0)), fill = "#2c7bb6", alpha = 0.25) +
  geom_line(color = "#1f77b4", linewidth = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", size = 0.4) +
  geom_hline(yintercept = -0.8, linetype = "dotted", color = "orange", size = 0.5) +
  geom_hline(yintercept = -1.6, linetype = "dotted", color = "red", size = 0.5) +
  facet_wrap(~nom_fr, ncol = 3, scales = "fixed") +
  scale_x_date(date_breaks = "4 years", date_labels = "%Y") +
  labs(
    title = "Trajectoires Mensuelles de la Sécheresse par Région au Maroc (2000–2026)",
    subtitle = "Évolution intra-annuelle du CDAI région par région avec zones d'anomalie négative",
    x = "Années",
    y = "CDAI Mensuel"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    strip.text = element_text(face = "bold", size = 10),
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.minor = element_blank()
  )
print(p2)
ggsave("sorties_figures/Figure2_CDAI_Trajectoires_Regionales.png", p2, width = 15, height = 10, dpi = 300)

# ----------------------------------------------------------------------------------------
# FIGURE 3 : MATRICE SPATIO-TEMPORELLE (HEATMAP RÉGIONS x ANNEES/MOIS)
# ----------------------------------------------------------------------------------------
p3 <- ggplot(df_cdai, aes(x = Date, y = reorder(nom_fr, CDAI, FUN = median), fill = CDAI)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "#d7191c", mid = "#ffffbf", high = "#2c7bb6",
    midpoint = 0,
    name = "Indice CDAI",
    limits = c(-3, 3), oob = squish
  ) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y", expand = c(0, 0)) +
  labs(
    title = "Matrice Spatio-Temporelle Continue des Épisodes de Sécheresse (CDAI : 2000–2026)",
    subtitle = "Identification synthétique des vagues de sécheresse généralisées vs localisées",
    x = "Chronologie (Années)",
    y = "Régions du Maroc"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.y = element_text(face = "bold", size = 10),
    legend.position = "right"
  )
print(p3)
ggsave("sorties_figures/Figure3_CDAI_Heatmap_SpatioTemporelle.png", p3, width = 14, height = 6, dpi = 300)

print("Toutes les figures ont été générées et sauvegardées avec succès.")


