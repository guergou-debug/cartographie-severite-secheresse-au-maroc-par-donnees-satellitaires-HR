# =====================================================================================
# NOWCASTING DU RISQUE DE SÉCHERESSE EXTRÊME (Q5) AU MAROC
# Modèle d'alerte précoce à partir des données partielles (Janvier - Août)
# =====================================================================================

library(tidyverse)
library(caret)     # Pour les métriques d'évaluation (Matrice de confusion)

df_mensuel <- df_raw %>%
  mutate(
    year = as.numeric(year),
    month = as.numeric(month),
    nom_affich = if_else(nom_fr == "l'Oriental", "L'Oriental", nom_fr)
  ) %>%
  group_by(nom_affich, year, month) %>%
  summarise(
    NDDI = median(NDDI_median, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.na(NDDI))

# 2. DEFINITION DE LA RÉFÉRENCE HISTORIQUE ET DES QUINTILES ANNUELS
# Calcul du NDDI moyen annuel par région sur toute la période
df_annuel <- df_mensuel %>%
  group_by(nom_affich, year) %>%
  summarise(NDDI_annuel = mean(NDDI, na.rm = TRUE), .groups = "drop")

# Calcul des seuils de quintiles sur la distribution annuelle globale
seuils_annuels <- quantile(df_annuel$NDDI_annuel, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1.0), na.rm = TRUE)

# Assignation de la classe cible finale (Is_Q5 = 1 si Sécheresse Extrême)
df_annuel_cible <- df_annuel %>%
  mutate(
    Classe_Finale = cut(
      NDDI_annuel,
      breaks = seuils_annuels,
      include.lowest = TRUE,
      labels = c("Q1", "Q2", "Q3", "Q4", "Q5")
    ),
    Is_Q5 = if_else(Classe_Finale == "Q5", 1, 0)
  )

# 3. INGÉNIERIE DES FEATURES DE NOWCASTING (FENÊTRE : JANVIER À AOÛT)
# On simule la disponibilité des données satellitaires jusqu'en août (Mois 8)
MOIS_NOWCAST <- 8 

df_nowcast_features <- df_mensuel %>%
  filter(month <= MOIS_NOWCAST) %>%
  group_by(nom_affich, year) %>%
  summarise(
    NDDI_moy_jan_aout = mean(NDDI, na.rm = TRUE),
    NDDI_max_jan_aout = max(NDDI, na.rm = TRUE),
    NDDI_sd_jan_aout  = sd(NDDI, na.rm = TRUE),
    .groups = "drop"
  )

# Fusion des features de Nowcasting avec la variable cible
df_model <- df_nowcast_features %>%
  left_join(df_annuel_cible %>% select(nom_affich, year, Is_Q5, Classe_Finale), 
            by = c("nom_affich", "year")) %>%
  mutate(
    Is_Q5 = factor(Is_Q5, levels = c(0, 1)),
    nom_affich = factor(nom_affich)
  )

# 4. MODÉLISATION ET VALIDATION CROISÉE / BACKTESTING (2000 - 2024)
# On réservez les années récentes 2020-2024 pour évaluer la capacité d'anticipation
train_data <- df_model %>% filter(year < 2020)
test_data  <- df_model %>% filter(year >= 2020)

# Modèle de régression logistique binomale pour le Nowcasting
model_nowcast <- glm(
  Is_Q5 ~ NDDI_moy_jan_aout + NDDI_max_jan_aout + nom_affich,
  data = train_data,
  family = binomial
)

# Prédiction des probabilités sur la période de test
test_data <- test_data %>%
  mutate(
    Prob_Q5 = predict(model_nowcast, newdata = test_data, type = "response"),
    Pred_Q5 = factor(if_else(Prob_Q5 >= 0.5, 1, 0), levels = c(0, 1))
  )

cat("--- MATRICE DE CONFUSION DU NOWCASTING (TEST 2020-2024) ---\n")
conf_matrix <- confusionMatrix(test_data$Pred_Q5, test_data$Is_Q5, positive = "1")
print(conf_matrix)

# 5. NOWCASTING APPLIQUÉ AUX DONNÉES RÉCENTES (2024 / Année en cours)
# Estimation de la probabilité de basculement en Q5 à la fin de l'année
df_nowcast_actuel <- df_model %>%
  filter(year == max(year)) %>%
  mutate(
    Prob_Basculement_Q5 = predict(model_nowcast, newdata = ., type = "response"),
    Niveau_Risque = case_when(
      Prob_Basculement_Q5 >= 0.75 ~ "Très Élevé (Q5 Quasi-Certain)",
      Prob_Basculement_Q5 >= 0.50 ~ "Élevé (Risque Q5)",
      Prob_Basculement_Q5 >= 0.25 ~ "Modéré",
      TRUE                        ~ "Faible"
    )
  )

# 6. VISUALISATION DES RÉSULTATS DU NOWCASTING
fig_nowcast <- ggplot(df_nowcast_actuel, aes(x = reorder(nom_affich, Prob_Basculement_Q5), 
                                             y = Prob_Basculement_Q5, 
                                             fill = Prob_Basculement_Q5)) +
  geom_col(width = 0.7) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "red", linewidth = 0.8) +
  scale_fill_gradient(low = "#8ce874", high = "#702353", name = "Probabilité Q5") +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  coord_flip() +
  labs(
    title = "Nowcasting du risque de sécheresse extrême (Q5) par région",
    subtitle = "Alerte précoce basée uniquement sur les observations satellitaires de Janvier à Août",
    x = NULL,
    y = "Probabilité estimée de terminer l'année en Sécheresse Extrême (Q5)",
    caption = "Ligne rouge = Seuil de décision de basculement à 50%."
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 12, color = "#1f4e79"),
    axis.text = element_text(color = "black", size = 9),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )

print(fig_nowcast)


ggsave(
  "sorties_figures/Figure_Nowcasting_Risque_Q5.png", 
  plot = fig_nowcast, 
  width = 10, 
  height = 6, 
  dpi = 300
)










# =====================================================================================
# NOWCASTING DIRECT 2026 : SÉCHERESSE EXTRÊME (Q5) AU MAROC (CORRIGÉ)
# =====================================================================================

library(tidyverse)

# 1. CHARGEMENT ET HARMONISATION DE LA BASE RÉCENTE (2025 - 2026)
# Remplace par le nom exact de ton fichier si besoin
df_recent_raw <- read_csv("Maroc_Serie_8jours_2025_2026_Recent.csv")

# Si les colonnes n'ont pas de noms standardisés, on renomme selon la structure observée
# (nom_fr, year, month, day, date, NDDI_median)
if (ncol(df_recent_raw) >= 6) {
  colnames(df_recent_raw)[1:6] <- c("nom_fr", "year", "month", "day", "date", "NDDI_median")
} else if (ncol(df_recent_raw) == 5) {
  colnames(df_recent_raw)[1:5] <- c("nom_fr", "year", "month", "date", "NDDI_median")
  df_recent_raw$day <- 1  # Ajout manuel si absente
}

df_recent_clean <- df_recent_raw %>%
  mutate(
    year        = as.numeric(year),
    month       = as.numeric(month),
    NDDI_median = as.numeric(NDDI_median),
    nom_affich  = if_else(nom_fr == "l'Oriental", "L'Oriental", as.character(nom_fr))
  ) %>%
  filter(!is.na(NDDI_median))

# 2. CHARGEMENT DE LA BASE HISTORIQUE (2000 - 2024)
df_historique <- read_csv("Maroc_Serie_8jours_2000_2024_Clean.csv") %>%
  mutate(
    year        = as.numeric(year),
    month       = as.numeric(month),
    NDDI_median = as.numeric(NDDI_median),
    nom_affich  = if_else(nom_fr == "l'Oriental", "L'Oriental", as.character(nom_fr))
  )

# Conserver uniquement les colonnes communes pour éviter tout conflit de types
cols_communes <- intersect(names(df_historique), names(df_recent_clean))

df_global_complet <- bind_rows(
  select(df_historique, all_of(cols_communes)),
  select(df_recent_clean, all_of(cols_communes))
)

# Sauvegarde de la base unifiée
write_csv(df_global_complet, "Maroc_Serie_8jours_2000_2026_Consolidee.csv")
message("✓ Base unifiée 2000-2026 enregistrée avec succès !")

# 3. AGRÉGATION MENSUELLE & DÉTECTION DE LA DERNIÈRE OBSERVATION 2026
df_mensuel <- df_global_complet %>%
  group_by(nom_affich, year, month) %>%
  summarise(
    NDDI = median(NDDI_median, na.rm = TRUE),
    .groups = "drop"
  )

dernier_mois_2026 <- df_mensuel %>%
  filter(year == 2026) %>%
  summarise(max_m = max(month)) %>%
  pull(max_m)

cat("-------------------------------------------------------------------\n")
cat("NOWCASTING 2026 : Données disponibles de Janvier à Mois", dernier_mois_2026, "\n")
cat("-------------------------------------------------------------------\n")

# 4. CONSTRUCTION DU SEUIL DE RÉFÉRENCE Q5 (2000 - 2024)
df_annuel_hist <- df_mensuel %>%
  filter(year <= 2024) %>%
  group_by(nom_affich, year) %>%
  summarise(NDDI_annuel = mean(NDDI, na.rm = TRUE), .groups = "drop")

seuil_q5_global <- quantile(df_annuel_hist$NDDI_annuel, probs = 0.80, na.rm = TRUE)

df_annuel_hist <- df_annuel_hist %>%
  mutate(Is_Q5 = if_else(NDDI_annuel >= seuil_q5_global, 1, 0))

# 5. FEATURES CUMULÉES SUR LA MÊME FENÊTRE (Mois 1 à dernier_mois_2026)
df_nowcast_features <- df_mensuel %>%
  filter(month <= dernier_mois_2026) %>%
  group_by(nom_affich, year) %>%
  summarise(
    NDDI_moy_cumul = mean(NDDI, na.rm = TRUE),
    NDDI_max_cumul = max(NDDI, na.rm = TRUE),
    .groups = "drop"
  )

# Base d'apprentissage (2000-2024)
df_train <- df_nowcast_features %>%
  filter(year <= 2024) %>%
  left_join(df_annuel_hist %>% select(nom_affich, year, Is_Q5), by = c("nom_affich", "year")) %>%
  mutate(
    Is_Q5 = factor(Is_Q5, levels = c(0, 1)),
    nom_affich = factor(nom_affich)
  )

# Base de prédiction (2026)
df_nowcast_2026 <- df_nowcast_features %>%
  filter(year == 2026) %>%
  mutate(nom_affich = factor(nom_affich))

# 6. ENTRAÎNEMENT DU MODÈLE ET NOWCAST 2026
model_logit <- glm(
  Is_Q5 ~ NDDI_moy_cumul + NDDI_max_cumul + nom_affich,
  data = df_train,
  family = binomial
)

df_resultats_2026 <- df_nowcast_2026 %>%
  mutate(
    Prob_Q5 = predict(model_logit, newdata = df_nowcast_2026, type = "response"),
    Niveau_Alerte = case_when(
      Prob_Q5 >= 0.75 ~ "Très Élevé (Q5 Quasi-Certain)",
      Prob_Q5 >= 0.50 ~ "Élevé (Risque Q5)",
      Prob_Q5 >= 0.25 ~ "Modéré",
      TRUE            ~ "Faible"
    )
  ) %>%
  arrange(desc(Prob_Q5))

cat("\n=== RÉSULTATS DU NOWCASTING 2026 PAR RÉGION ===\n")
print(df_resultats_2026 %>% select(nom_affich, NDDI_moy_cumul, Prob_Q5, Niveau_Alerte))

# 7. GRAPHIQUE OFFICIEL
fig_nowcast_2026 <- ggplot(df_resultats_2026, aes(x = reorder(nom_affich, Prob_Q5), 
                                                  y = Prob_Q5, 
                                                  fill = Prob_Q5)) +
  geom_col(width = 0.65) +
  geom_hline(yintercept = 0.50, linetype = "dashed", color = "#c0392b", linewidth = 0.9) +
  geom_text(aes(label = paste0(round(Prob_Q5 * 100, 1), "%")), 
            hjust = -0.15, size = 3.8, fontface = "bold") +
  scale_fill_gradient(low = "#2c6e1e", high = "#702353", name = "Probabilité Q5") +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1.2)) +
  coord_flip() +
  labs(
    title = "Nowcasting 2026 : Estimation du risque de Sécheresse Extrême (Q5)",
    subtitle = paste0("Alerte précoce construite sur les données MODIS jusqu au mois ", dernier_mois_2026, " (2026)"),
    x = NULL,
    y = "Probabilité estimée de terminer l'année en Sécheresse Extrême (Q5)",
    caption = "Seuil d'alerte critique fixé à 50% (ligne rouge)."
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

# Sauvegarde
dir.create("sorties_figures", showWarnings = FALSE)
ggsave(
  "sorties_figures/Figure_Nowcasting_2026_Officielle.png", 
  plot = fig_nowcast_2026, 
  width = 10, 
  height = 6.5, 
  dpi = 300
)



# ===============================================================================
# CARTE CÔTE-À-CÔTE : PERSPECTIVE DE SÉVÉRITÉ DU NDDI (2025 vs 2026) - ZÉRO NA
# ===============================================================================

# 1. CHARGEMENT ET HARMONISATION RIGOUREUSE
df_global <- read_csv("Maroc_Serie_8jours_2000_2026_Consolidee.csv")

# Nettoyage des chaînes de caractères pour une jointure irréprochable
df_nowcast_cumul <- df_global %>%
  filter(month <= 7) %>%
  mutate(nom_fr = str_trim(nom_fr)) %>%
  group_by(nom_fr, year) %>%
  summarise(NDDI_final = mean(NDDI_median, na.rm = TRUE), .groups = "drop")

# 2. CALCUL DES QUINTILES HISTORIQUES (2000-2024)
q_cuts <- quantile(
  df_nowcast_cumul %>% filter(year <= 2024) %>% pull(NDDI_final), 
  probs = seq(0, 1, length.out = 6), 
  na.rm = TRUE
)

labels_quintiles <- c(
  sprintf("Q1 [%.2f ; %.2f[", q_cuts[1], q_cuts[2]),
  sprintf("Q2 [%.2f ; %.2f[", q_cuts[2], q_cuts[3]),
  sprintf("Q3 [%.2f ; %.2f[", q_cuts[3], q_cuts[4]),
  sprintf("Q4 [%.2f ; %.2f[", q_cuts[4], q_cuts[5]),
  sprintf("Q5 [%.2f ; %.2f]",  q_cuts[5], q_cuts[6])
)

# BLINDAGE MATHÉMATIQUE : On remplace le min par -Inf et le max par +Inf 
# pour éviter tout NA si 2026 bat des records extrêmes !
q_cuts_extension <- q_cuts
q_cuts_extension[1] <- -Inf
q_cuts_extension[6] <- Inf

# Application du découpage étendu
df_outlook_quintiles <- df_nowcast_cumul %>%
  filter(year %in% c(2025, 2026)) %>%
  mutate(
    Classe_Quintile = cut(
      NDDI_final,
      breaks = q_cuts_extension,
      include.lowest = TRUE,
      labels = labels_quintiles
    )
  )

# 3. SHAPEFILE ET HARMONISATION
maroc_regions <- st_read("shapefile_maroc/regions.shp") %>%
  mutate(nom_fr = str_trim(nom_fr))

# VÉRIFICATION DE CORRESPONDANCE (Affiche un message d'alerte dans la console si Mismatch)
diff_noms <- setdiff(maroc_regions$nom_fr, df_outlook_quintiles$nom_fr)
if(length(diff_noms) > 0) {
  warning("ATTENTION ! Des noms de régions ne matchent pas entre Shapefile et CSV : ", paste(diff_noms, collapse = ", "))
}

# 4. CRÉATION DES DEUX MAPS SÉPARÉMENT (Anti-Duplication / Anti-NA)
data_2025 <- df_outlook_quintiles %>% filter(year == 2025)
data_2026 <- df_outlook_quintiles %>% filter(year == 2026)

carte_2025_sf <- maroc_regions %>%
  left_join(data_2025, by = "nom_fr") %>%
  mutate(nom_affich = if_else(nom_fr == "l'Oriental", "L'Oriental", nom_fr))

carte_2026_sf <- maroc_regions %>%
  left_join(data_2026, by = "nom_fr") %>%
  mutate(nom_affich = if_else(nom_fr == "l'Oriental", "L'Oriental", nom_fr))

# 5. PALETTE DE COULEURS HISTORIQUE (5 Quintiles)
palette_couleurs <- setNames(
  c("#337a22", "#a1db74", "#ffffff", "#e383bd", "#9e1462"),
  labels_quintiles
)

# 6. FONCTION DE RENDU GRAPHIQUE 
creer_carte <- function(data_sf, titre_annee) {
  ggplot(data = data_sf) +
    geom_sf(aes(fill = Classe_Quintile), color = "grey40", linewidth = 0.3) +
    geom_sf_text(
      aes(label = nom_affich), 
      size = 2.0, 
      fontface = "bold", 
      color = "black", 
      check_overlap = FALSE, 
      show.legend = FALSE
    ) +
    scale_fill_manual(
      values = palette_couleurs, 
      drop = FALSE, 
      na.translate = FALSE, # Bloque l'affichage explicite des NA dans la légende
      name = "Quintiles du NDDI :",
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
      pad_y = unit(0.4, "in"), 
      style = north_arrow_fancy_orienteering
    ) +
    labs(title = titre_annee) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 12, color = "#1f4e79", hjust = 0.5),
      panel.background = element_rect(fill = "aliceblue", color = NA),
      legend.position = "bottom"
    )
}

# 7. GENERATION ET ASSEMBLAGE
p_2025 <- creer_carte(carte_2025_sf, "Sévérité du NDDI (2025)")
p_2026 <- creer_carte(carte_2026_sf, "Perspective Nowcasting NDDI (2026)")

cartes_cote_a_cote <- (p_2025 + p_2026) +
  plot_layout(guides = "collect") &
  theme(legend.position = 'bottom')

print(cartes_cote_a_cote)

# Sauvegarde
ggsave(
  "sorties_figures/Carte_Cote_A_Cote_NDDI_2025_2026_Propre.png",
  plot = cartes_cote_a_cote,
  width = 14,
  height = 8,
  dpi = 300
)