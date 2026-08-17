# =========================================================================
# ÉTAPE 4: ANALYSE DES TENDANCES HISTORIQUES ET SAISONNIÈRES (2000 - 2025)
# =========================================================================

# 1. Chargement et préparation de la base consolidée
df_raw <- read_csv("Maroc_Serie_8jours_2000_2026_Consolidee.csv")

# Filtrage strict sur la période d'étude historique (2000 - 2025)
# 2026 est exclu pour la phase ultérieure de Nowcasting
df_etude <- df_raw %>%
  filter(year >= 2000 & year <= 2025) %>%
  mutate(Region = str_trim(nom_fr)) %>%
  filter(!is.na(Region))

# -------------------------------------------------------------------------
# AGRÉGATION ANNUELLE (2000 - 2025) AVEC GESTION DES NA
# -------------------------------------------------------------------------
df_historique <- df_etude %>%
  group_by(year, Region) %>%
  summarise(
    NDDI = mean(NDDI_median, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(Annee = year)

# =========================================================================
# FIGURE 8 : ÉVOLUTION DE NDDI (2000 - 2025)
# =========================================================================

fig_8_evolution <- ggplot(df_historique, aes(x = Annee, y = NDDI, color = Region)) +
  theme_minimal(base_size = 11) +
  geom_smooth(method = "loess", se = FALSE, span = 0.2, linewidth = 1.2) +
  
  # Alignement des étiquettes des 12 régions en bout de ligne (Année 2025)
  geom_text_repel(
    data = df_historique %>% filter(Annee == 2025 & !is.na(NDDI)), 
    aes(label = Region), 
    hjust = 0,
    direction = "y",
    nudge_x = 0.5,
    size = 2.8, 
    fontface = "bold", 
    max.overlaps = Inf
  ) +
  scale_x_continuous(breaks = seq(2000, 2025, by = 5), limits = c(2000, 2030)) +
  labs(
    title = "Évolution de la sévérité de la sécheresse par région entre 2000 et 2025",
    x = "Année",
    y = "NDDI"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 11, color = "#1f4e79"),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

print(fig_8_evolution)
ggsave("sorties_figures/Figure_8_Evolution_Drought_Severity.png", plot = fig_8_evolution, width = 11, height = 7, dpi = 300)

# =========================================================================
# FIGURE 9 : PERSISTANCE ET RANGS DE VULNÉRABILITÉ (BUMP CHART 2000 - 2025)
# =========================================================================

df_ranks <- df_historique %>%
  filter(!is.na(NDDI)) %>%
  group_by(Annee) %>%
  mutate(Rang_NDDI = rank(NDDI, ties.method = "first")) %>%
  ungroup()

fig_9_bump <- ggplot(df_ranks, aes(x = Annee, y = Rang_NDDI, color = Region, group = Region)) +
  theme_minimal(base_size = 11) +
  geom_smooth(method = "loess", se = FALSE, span = 0.2, linewidth = 1.5) +
  geom_text(data = df_ranks %>% filter(Annee == 2000),
            aes(label = Region), hjust = 1.1, size = 2.5, fontface = "bold") +
  geom_text(data = df_ranks %>% filter(Annee == 2025),
            aes(label = Region), hjust = -0.1, size = 2.5, fontface = "bold") +
  scale_x_continuous(breaks = seq(2000, 2025, by = 5), limits = c(1995, 2029)) +
  scale_y_continuous(breaks = 1:12, labels = 1:12) +
  labs(
    title = "Persistance de la vulnérabilité à la sécheresse à travers les régions (2000 - 2025)",
    x = "Année",
    y = "Classement de sévérité NDDI (Rang)",
    caption = "Note : Un rang élevé indique une exposition chronique plus intense au stress hydrique sur la période."
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 11, color = "#1f4e79"),
    panel.grid.major.y = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

print(fig_9_bump)
ggsave("sorties_figures/Figure_9_Persistence_Drought_Vulnerability.png", plot = fig_9_bump, width = 12, height = 7, dpi = 300)

# =========================================================================
# FIGURE 10 : DISTRIBUTION DE LA SÉVÉRITÉ PAR ANNÉE (BOXPLOT 2000 - 2025)
# =========================================================================

fig_10_boxplot <- ggplot(df_historique, aes(x = factor(Annee), y = NDDI)) +
  theme_light(base_size = 11) +
  geom_boxplot(fill = "#e383bd", color = "#9e1462", alpha = 0.5, 
               outlier.color = "#9e1462", outlier.size = 2, na.rm = TRUE) +
  labs(
    title = "Distribution annuelle de la sévérité de la sécheresse au Maroc (2000 - 2025)",
    x = "Année",
    y = "NDDI"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 11, color = "#1f4e79"),
    axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1),
    panel.grid.minor = element_blank()
  )

print(fig_10_boxplot)
ggsave("sorties_figures/Figure_10_Distribution_Drought_Severity.png", plot = fig_10_boxplot, width = 11, height = 6, dpi = 300)

# =========================================================================
# FIGURE 11 : FRÉQUENCE ET INTENSITÉ DE LA SÉCHERESSE (2000 - 2025)
# =========================================================================

seuil_secheresse <- 4.0

df_metriques <- df_historique %>%
  filter(!is.na(NDDI)) %>%
  group_by(Region) %>%
  summarise(
    Total_Annees = n(),
    Annees_Seches = sum(NDDI >= seuil_secheresse, na.rm = TRUE),
    Frequence = (Annees_Seches / Total_Annees) * 100,
    Intensite = mean(NDDI[NDDI >= seuil_secheresse], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Intensite = replace_na(Intensite, 0))

# Panel A - Fréquence
p_freq <- ggplot(df_metriques, aes(x = reorder(Region, -Frequence), y = Frequence)) +
  geom_bar(stat = "identity", fill = "#702353", width = 0.6) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 100)) +
  labs(
    title = "Panel A. Fréquence de la sécheresse",
    x = NULL, y = "Proportion d'années (%)"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    plot.title = element_text(face = "bold", size = 11, color = "#1f4e79"),
    panel.grid.minor = element_blank()
  )

# Panel B - Intensité
p_int <- ggplot(df_metriques, aes(x = reorder(Region, -Intensite), y = Intensite)) +
  geom_bar(stat = "identity", fill = "#702353", width = 0.6) +
  labs(
    title = "Panel B. Intensité de la sécheresse",
    x = NULL, y = "Niveau moyen de l'indice (NDDI)"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    plot.title = element_text(face = "bold", size = 11, color = "#1f4e79"),
    panel.grid.minor = element_blank()
  )

fig_11_combinee <- p_freq + p_int + 
  plot_annotation(
    title = "Fréquence et intensité de la sécheresse au Maroc (2000-2025)",
    theme = theme(plot.title = element_text(face = "bold", size = 13, color = "#1f4e79", hjust = 0))
  )

print(fig_11_combinee)
ggsave("sorties_figures/Figure_11_Drought_Frequency_Intensity.png", plot = fig_11_combinee, width = 12, height = 6, dpi = 300)

# =====================================================================================
# FIGURE 12 : SÉVÉRITÉ MENSUELLE DE LA SÉCHERESSE (EXTENSION 2020 - 2025)
# =====================================================================================

# Agrégation au pas mensuel depuis la série de 8 jours
df_mensuel <- df_etude %>%
  filter(year >= 2020 & year <= 2025) %>%
  group_by(year, month, Region) %>%
  summarise(
    NDDI = mean(NDDI_median, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.na(NDDI))

# Classification par seuils
df_classes <- df_mensuel %>%
  mutate(
    Classe_Drought = case_when(
      NDDI < 4   ~ "Pas de sécheresse",
      NDDI >= 4  & NDDI < 7  ~ "Sécheresse légère",
      NDDI >= 7  & NDDI < 10 ~ "Sécheresse modérée",
      NDDI >= 10 & NDDI < 14 ~ "Sécheresse sévère",
      NDDI >= 14             ~ "Sécheresse extrême"
    )
  )

# Compte du nombre de régions touchées par niveau de sévérité
df_counts <- df_classes %>%
  group_by(year, month, Classe_Drought) %>%
  summarise(Nb_Regions = n(), .groups = "drop")

tous_mois_classes <- expand_grid(
  year = 2020:2025,
  month = 1:12,
  Classe_Drought = c("Pas de sécheresse", "Sécheresse légère", "Sécheresse modérée", "Sécheresse sévère", "Sécheresse extrême")
)

df_complet <- tous_mois_classes %>%
  left_join(df_counts, by = c("year", "month", "Classe_Drought")) %>%
  mutate(Nb_Regions = replace_na(Nb_Regions, 0))

df_final_mensuel <- df_complet %>%
  group_by(month, Classe_Drought) %>%
  summarise(Moyenne_Regions = mean(Nb_Regions), .groups = "drop") %>%
  mutate(
    Nom_Mois = factor(month, levels = 1:12, labels = c(
      "Janvier", "Février", "Mars", "Avril", "Mai", "Juin", 
      "Juillet", "Août", "Septembre", "Octobre", "Novembre", "Décembre"
    )),
    Classe_Drought = factor(Classe_Drought, levels = c(
      "Sécheresse extrême", "Sécheresse sévère", "Sécheresse modérée", "Sécheresse légère", "Pas de sécheresse"
    ))
  )

palette_mensuelle <- c(
  "Sécheresse extrême"  = "#702353",
  "Sécheresse sévère"   = "#db62bb",
  "Sécheresse modérée"  = "#e0e0e0",
  "Sécheresse légère"   = "#8ce874",
  "Pas de sécheresse"   = "#2c6e1e"
)

fig_12_saisonniere <- ggplot(df_final_mensuel, aes(x = Nom_Mois, y = Moyenne_Regions, 
                                                   color = Classe_Drought, group = Classe_Drought)) +
  geom_line(linewidth = 1.5) +
  scale_color_manual(values = palette_mensuelle) +
  scale_y_continuous(breaks = seq(0, 12, by = 2), limits = c(0, 12.5)) +
  labs(
    title = "Sévérité mensuelle de la sécheresse à travers les régions marocaines (2020-2025)",
    x = NULL,
    y = "Nombre de régions par niveau de sévérité",
    color = NULL,
    caption = "Note : Valeur mensuelle moyenne calculée sur la période récente 2020-2025."
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 11, color = "#1f4e79", hjust = 0),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 9),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom",
    legend.box = "horizontal"
  )

print(fig_12_saisonniere)
ggsave("sorties_figures/Figure_12_Monthly_Drought_Severity.png", plot = fig_12_saisonniere, width = 11, height = 7, dpi = 300)