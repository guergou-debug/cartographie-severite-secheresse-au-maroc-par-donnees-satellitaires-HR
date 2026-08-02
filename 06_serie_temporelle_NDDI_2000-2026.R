# =========================================================================
# ÉTAPE 4:   ANALYSE DES TENDANCES HISTORIQUES (2000 - 2024)
# =========================================================================

# 1. Chargement du fichier exporté depuis GEE
df_historique <- read_csv("Serie_Temporelle_NDDI_2000_2024_Nettoye.csv") %>%
  rename(Annee = Annee, Region = nom_fr, NDDI = mean) %>%
  filter(!is.na(Region))

# Nettoyage des noms si des anomalies de caractères ou espaces subsistent
df_historique <- df_historique %>%
  mutate(Region = str_trim(Region))

# =========================================================================
# ÉTAPE 4:  FIGURE 10 -  EVOLUTION DE NDDI
# =========================================================================

fig_8_evolution <- ggplot(df_historique, aes(x = Annee, y = NDDI, color = Region)) +
  theme_minimal(base_size = 11) +
  
  # Lissage loess identique au modèle
  geom_smooth(method = "loess", se = FALSE, span = 0.2, linewidth = 1.2) +
  
  # CORRECTION : Utilisation de geom_text_repel pour forcer l'affichage des 12 régions
  geom_text_repel(
    data = df_historique %>% filter(Annee == 2024), 
    aes(label = Region), 
    hjust = 0,
    direction = "y",            # Aligne les textes verticalement pour éviter les croisements
    nudge_x = 0.5,              # Pousse le texte légèrement vers la droite après 2024
    size = 2.8, 
    fontface = "bold", 
    max.overlaps = Inf          # FORCE R à afficher TOUS les labels, sans exception
  ) +
  
  # Ajustement des limites de l'axe X pour laisser la place aux 12 étiquettes
  scale_x_continuous(breaks = seq(2000, 2024, by = 5), limits = c(2000, 2029)) +
  
  labs(
    title = "Évolution de la sévérité de la sécheresse par région entre 2000 et 2024",
    x = "Année",
    y = "NDDI",
    ) +
  theme(
    plot.title = element_text(face = "bold", size = 11, color = "#1f4e79"),
    panel.grid.minor = element_blank(),
    legend.position = "none"    # Masquée car les 12 labels sont écrits en bout de ligne
  )

print(fig_8_evolution)
ggsave("sorties_figures/Figure_8_Evolution_Drought_Severity.png", plot = fig_8_evolution, width = 11, height = 7, dpi = 300)
# =========================================================================
# ÉTAPE 4:  FIGURE 11 -  PERSISTANCE ET RANGS DE VULNÉRABILITÉ (BUMP CHART)
# =========================================================================

df_ranks <- df_historique %>%
  group_by(Annee) %>%
  # Classement des régions du plus sec (Rank 12) au moins sec (Rank 1)
  mutate(Rang_NDDI = rank(NDDI, ties.method = "first")) %>%
  ungroup()

fig_9_bump <- ggplot(df_ranks, aes(x = Annee, y = Rang_NDDI, color = Region, group = Region)) +
  theme_minimal(base_size = 11) +
  geom_smooth(method = "loess", se = FALSE, span = 0.2, linewidth = 1.5) +
  geom_text(data = df_ranks %>% filter(Annee == 2000),
            aes(label = Region), hjust = 1.1, size = 2.5, fontface = "bold") +
  geom_text(data = df_ranks %>% filter(Annee == 2024),
            aes(label = Region), hjust = -0.1, size = 2.5, fontface = "bold") +
  scale_x_continuous(breaks = seq(2000, 2024, by = 5), limits = c(1996, 2028)) +
  scale_y_continuous(breaks = 1:12, labels = 1:12) +
  labs(
    title = "Persistance de la vulnérabilité à la sécheresse à travers les régions (2000 - 2024)",
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
# FIGURE 12 : DISTRIBUTION DE LA SÉVÉRITÉ PAR ANNÉE (BOXPLOT)
# =========================================================================
fig_10_boxplot <- ggplot(df_historique, aes(x = factor(Annee), y = NDDI)) +
  theme_light(base_size = 11) +
  # Boxplot reprenant la coloration magenta/bordeaux du modèle d'origine
  geom_boxplot(fill = "#e383bd", color = "#9e1462", alpha = 0.5, 
               outlier.color = "#9e1462", outlier.size = 2) +
  labs(
    title = " Distribution annuelle de la sévérité de la sécheresse au Maroc",
    x = "Année",
    y = "NDDI",
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 11, color = "#1f4e79"),
    plot.subtitle = element_text(size = 9, color = "grey40"),
    axis.text.x = element_text(angle = 0, vjust = 0.5), # Garde les années horizontales et propres
    panel.grid.minor = element_blank()
  )

print(fig_10_boxplot)
ggsave("sorties_figures/Figure_10_Distribution_Drought_Severity.png", plot = fig_10_boxplot, width = 11, height = 6, dpi = 300)



# =========================================================================
# FIGURE 13 : FRÉQUENCE ET INTENSITÉ DE LA SÉCHERESSE (2000-2024)
# =========================================================================

# Seuil de sécheresse calibré sur la Figure 10
seuil_secheresse <- 7.0

df_metriques <- df_historique %>%
  group_by(Region) %>%
  summarise(
    Total_Annes = n(),
    Annees_Seches = sum(NDDI >= seuil_secheresse),
    Frequence = (Annees_Seches / Total_Annes) * 100,
    Intensite = mean(NDDI[NDDI >= seuil_secheresse], na.rm = TRUE)
  ) %>%
  ungroup()

# Panel A - Graphique de Fréquence
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

# Panel B - Graphique d'Intensité
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

# Combinaison des deux panels (Côte à côte comme sur le modèle)
fig_11_combinee <- p_freq + p_int + 
  plot_annotation(
    title = "Fréquence et intensité de la sécheresse au Maroc (2000-2024)",
    theme = theme(plot.title = element_text(face = "bold", size = 13, color = "#1f4e79", hjust = 0))
  )

print(fig_11_combinee)
ggsave("sorties_figures/Figure_11_Drought_Frequency_Intensity.png", plot = fig_11_combinee, width = 12, height = 6, dpi = 300)


# =====================================================================================
# ÉTAPE 4:  FIGURE 14 - SÉVÉRITÉ MENSUELLE DE LA SÉCHERESSE AU MAROC (2020-2024)
# =====================================================================================

# 1. Chargement de la data mensuelle exportée de GEE
df_mensuel <- read_csv("Serie_Mensuelle_NDDI_2020_2024.csv") %>%
  rename(Annee = Annee, Mois = Mois, Region = nom_fr, NDDI = mean) %>%
  filter(!is.na(Region))

# 2. Classification stricte selon nos seuils empiriques validés
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

# 3. Calcul du nombre de régions par classe pour chaque couple (Année, Mois)
df_counts <- df_classes %>%
  group_by(Annee, Mois, Classe_Drought) %>%
  summarise(Nb_Regions = n(), .groups = "drop")

# Remplir les combinaisons vides (au cas où une classe n'apparaît pas un mois donné)
tous_mois_classes <- expand_grid(
  Annee = 2020:2024,
  Mois = 1:12,
  Classe_Drought = c("Pas de sécheresse", "Sécheresse légère", "Sécheresse modérée", "Sécheresse sévère", "Sécheresse extrême")
)

df_complet <- tous_mois_classes %>%
  left_join(df_counts, by = c("Annee", "Mois", "Classe_Drought")) %>%
  mutate(Nb_Regions = replace_na(Nb_Regions, 0))

# 4. Calcul de la moyenne mensuelle sur la période récente 2020-2024
df_final_mensuel <- df_complet %>%
  group_by(Mois, Classe_Drought) %>%
  summarise(Moyenne_Regions = mean(Nb_Regions), .groups = "drop") %>%
  mutate(
    Nom_Mois = factor(Mois, levels = 1:12, labels = c(
      "Janvier", "Février", "Mars", "Avril", "Mai", "Juin", 
      "Juillet", "Août", "Septembre", "Octobre", "Novembre", "Décembre"
    )),
    Classe_Drought = factor(Classe_Drought, levels = c(
      "Sécheresse extrême", "Sécheresse sévère", "Sécheresse modérée", "Sécheresse légère", "Pas de sécheresse"
    ))
  )

# 5. Palette de couleurs reprenant scrupuleusement le modèle
palette_mensuelle <- c(
  "Sécheresse extrême"  = "#702353", # Pourpre foncé
  "Sécheresse sévère"   = "#db62bb", # Rose/Magenta vif
  "Sécheresse modérée"  = "#e0e0e0", # Gris clair / Blanc cassé
  "Sécheresse légère"   = "#8ce874", # Vert clair lime
  "Pas de sécheresse"   = "#2c6e1e"  # Vert foncé forêt
)

# 6. Rendu graphique
fig_12_saisonniere <- ggplot(df_final_mensuel, aes(x = Nom_Mois, y = Moyenne_Regions, 
                                                   color = Classe_Drought, group = Classe_Drought)) +
  geom_line(linewidth = 1.5) +
  scale_color_manual(values = palette_mensuelle) +
  scale_y_continuous(breaks = seq(0, 12, by = 2), limits = c(0, 12.5)) +
  
  labs(
    title = "Sévérité mensuelle de la sécheresse à travers les régions marocaines (2020-2024)",
    x = NULL,
    y = "Nombre de régions par niveau de sévérité",
    color = NULL,
    caption = "Note : Valeur mensuelle moyenne calculée sur la période 2020-2024."
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

# Affichage immédiat du graphique
print(fig_12_saisonniere)

# Sauvegarde de la figure
ggsave("sorties_figures/Figure_12_Monthly_Drought_Severity.png", plot = fig_12_saisonniere, 
       width = 11, height = 7, dpi = 300)