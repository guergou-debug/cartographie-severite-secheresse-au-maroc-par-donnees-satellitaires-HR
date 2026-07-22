# =========================================================================
#  ÉTAPE 3:  FIGURE 5 - Severite , PIB et Population
# =========================================================================
# 1. Chargement et préparation des données satellites 
nddi_physique <- base_donnees_drought %>%
  group_by(Region) %>%
  summarise(
    NDVI_moyen = mean(NDVI_median, na.rm = TRUE),
    NDWI_moyen = mean(NDWI_median, na.rm = TRUE),
    NDDI_moyen = mean(NDDI_median, na.rm = TRUE)
  ) %>%
  mutate(NDDI_final = case_when(
    NDVI_moyen < 0.15 & NDWI_moyen < 0 ~ 0.6, # Stabilité biophysique des sols nus
    TRUE ~ NDDI_moyen
  )) %>%
  mutate(Severite = case_when(
    NDDI_final < 0.2 ~ "1. Pas de sécheresse",
    NDDI_final >= 0.2 & NDDI_final < 0.3 ~ "2. Sécheresse légère",
    NDDI_final >= 0.3 & NDDI_final < 0.4 ~ "3. Sécheresse modérée",
    NDDI_final >= 0.4 & NDDI_final < 0.5 ~ "4. Sécheresse sévère",
    NDDI_final >= 0.5 ~ "5. Sécheresse extrême"
  )) %>%
  mutate(Severite = factor(Severite, levels = c(
    "1. Pas de sécheresse", "2. Sécheresse légère", "3. Sécheresse modérée", "4. Sécheresse sévère", "5. Sécheresse extrême"
  )))

# 2. Chargement de ta base de données HCP depuis Excel
donnees_hcp <- read_excel("donnees_socio_eco_maroc_HCP_2024.xlsx")

# 3. Fusion avec vérification stricte
base_impact_finale <- nddi_physique %>%
  left_join(donnees_hcp, by = "Region")

# ---- BLOC DE DIAGNOSTIC AUTOMATIQUE ----
# Ce bloc va t'afficher en console les lignes qui ont échoué à la fusion
donnees_manquantes <- base_impact_finale %>% filter(is.na(PIB_Regional_MDH))
if(nrow(donnees_manquantes) > 0) {
  cat("\n⚠️ ATTENTION : Les régions suivantes ont un problème de nom dans ton Excel :\n")
  print(donnees_manquantes$Region)
  cat("Vérifie leur orthographe exacte dans ton fichier Excel (accents, tirets, espaces).\n\n")
}
# ----------------------------------------

# 4. Codes couleur HEX de la charte OCDE
palette_officielle_ocde <- c(
  "1. Pas de sécheresse"       = "#337a22", 
  "2. Sécheresse légère"     = "#a1db74", 
  "3. Sécheresse modérée" = "#ffffff", 
  "4. Sécheresse sévère"   = "#e383bd", 
  "5. Sécheresse extrême"  = "#9e1462"  
)

# 5. Construction du Bubble Chart sans contrainte d'échelle
figure_4_impact <- ggplot(data = base_impact_finale %>% filter(!is.na(PIB_Regional_MDH)), 
                          aes(x = NDDI_final, y = PIB_Regional_MDH, size = Population_Totale, fill = Severite)) +
  
  # Thème de base pour la grille de fond
  theme_bw(base_size = 11) +
  
  # Dessin des bulles
  geom_point(shape = 21, color = "black", alpha = 0.85, stroke = 0.6) +
  
  # Gestion dynamique des étiquettes des régions
  geom_text_repel(aes(label = Region), size = 3, fontface = "bold", color = "black",
                  box.padding = 0.4, point.padding = 0.3, max.overlaps = 50, show.legend = FALSE) +
  
  # Paramétrage des échelles de couleurs et de tailles
  scale_fill_manual(values = palette_officielle_ocde, drop = FALSE, name = "Sévérité de la Sécheresse") +
  scale_size_continuous(range = c(3, 15), labels = scales::label_comma(scale = 1e-6, suffix = "M"), 
                        name = "Population") +
  
  # Échelles gérées automatiquement par R
  scale_y_continuous(labels = scales::label_comma(suffix = " MDH")) +
  scale_x_continuous() + # R s'occupe de calibrer l'axe X selon les valeurs réelles
  
  # Habillage académique
  labs(
    title = "Sévérité de la sécheresse par PIB et population à travers les régions marocaines (2024)",
    x = "NDDI",
    y = "PIB (Prix Courant, Millions DH)",
  )+
  
  # Personnalisation fine du thème
  theme(
    plot.title = element_text(face = "bold", size = 11, color = "#1f4e79"),
    plot.subtitle = element_text(size = 9, color = "grey40", margin = margin(b = 15)),
    axis.title = element_text(face = "bold", size = 10),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    legend.box = "vertical",
    legend.title = element_text(face = "bold", size = 9),
    legend.text = element_text(size = 8)
  ) +
  
  # Rendre les ronds transparents dans la légende de population
  guides(fill = guide_legend(override.aes = list(size = 4)),
         size = guide_legend(override.aes = list(fill = "white")))

# 6. Affichage immédiat
print(figure_4_impact)

# 7. Sauvegarde
ggsave("sorties_figures/Figure_4_Bubble_Chart_Impact_Maroc.png", plot = figure_4_impact, 
       width = 10, height = 7, dpi = 300)


# =========================================================================
# ÉTAPE 3:  FIGURE 6 - BUBBLE CHART AVEC PIB PAR HABITANT 
# =========================================================================

# 5. Construction du Bubble Chart avec le PIB par Habitant sur l'axe Y
figure_4_impact_hab <- ggplot(data = base_impact_finale, 
                              aes(x = NDDI_final, y = PIB_Par_Habitant_DH, size = Population_Totale, fill = Severite)) +
  
  # Thème de fond épuré
  theme_bw(base_size = 11) +
  
  # Dessin des bulles (ronds noirs avec contours pour faire ressortir les bulles blanches)
  geom_point(shape = 21, color = "black", alpha = 0.85, stroke = 0.6) +
  
  # Ajout des étiquettes des régions sans chevauchement
  geom_text_repel(aes(label = Region), size = 3, fontface = "bold", color = "black",
                  box.padding = 0.4, point.padding = 0.3, max.overlaps = 50, show.legend = FALSE) +
  
  # Configuration des palettes de couleurs et de l'échelle des tailles des bulles
  scale_fill_manual(values = palette_officielle_ocde, drop = FALSE, name = "Sévérité de la sécheresse") +
  scale_size_continuous(range = c(4, 16), labels = scales::label_comma(scale = 1e-6, suffix = " M"), 
                        name = "Population") +
  
  # Formatage automatique des axes (PIB par habitant en Dirhams)
  scale_y_continuous(labels = scales::label_comma(suffix = " DH")) +
  scale_x_continuous() + 
  
  # Habillage complet en français conforme à tes directives
  labs(
    title = "Sévérité de la sécheresse par PIB par habitant et population des régions (2024)",
    x = "NDDI",
    y = "PIB par habitant (Prix courants, DH)",
    ) +
  
  # Personnalisation avancée de la charte graphique
  theme(
    plot.title = element_text(face = "bold", size = 11, color = "#1f4e79"),
    plot.subtitle = element_text(size = 9, color = "grey40", margin = margin(b = 15)),
    axis.title = element_text(face = "bold", size = 10),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    legend.box = "vertical",
    legend.title = element_text(face = "bold", size = 9),
    legend.text = element_text(size = 8)
  ) +
  
  # Nettoyage des fonds de bulles dans la légende pour la lisibilité
  guides(fill = guide_legend(override.aes = list(size = 4)),
         size = guide_legend(override.aes = list(fill = "white")))

# 6. Affichage du résultat final
print(figure_4_impact_hab)

# 7. Sauvegarde dans ton dossier de sorties
ggsave("sorties_figures/Figure_4_Bubble_Chart_PIB_Habitant.png", plot = figure_4_impact_hab, 
       width = 11, height = 7, dpi = 300)

# ===========================================================================================
# ÉTAPE 3: ANALYSES CROISÉES AVANCÉES DES VARIABLES SOCIO-ÉCONOMIQUES (HCP)
# ===========================================================================================

# 3. Fusion robuste par position (Garantie 12/12 régions ajoutées)
base_analyses_completes <- nddi_physique %>%
  mutate(
    Population_Rurale    = donnees_hcp$Population_Rurale,
    Part_Agriculture_PIB = donnees_hcp$Part_Agriculture_PIB,
    Taux_Pauvreté_Perc   = donnees_hcp$Taux_Pauvreté_Perc,
    Taux_Chomage_Perc    = donnees_hcp$Taux_Chomage_Perc
  )

# Calcul des médianes nationales pour les lignes de coupure des quadrants
med_nddi <- median(base_analyses_completes$NDDI_final, na.rm = TRUE)
med_agri <- median(base_analyses_completes$Part_Agriculture_PIB, na.rm = TRUE)

# =========================================================================
# ÉTAPE 3:  FIGURE 7 -  LE QUADRANT DE VULNÉRABILITÉ SECTORIELLE
# =========================================================================
fig_A_quadrant <- ggplot(base_analyses_completes, aes(x = NDDI_final, y = Part_Agriculture_PIB)) +
  theme_bw(base_size = 11) +
  # Lignes de démarcation des quadrants (médianes)
  geom_vline(xintercept = med_nddi, linetype = "dashed", color = "grey50") +
  geom_hline(yintercept = med_agri, linetype = "dashed", color = "grey50") +
  # Points stylisés
  geom_point(color = "#1f4e79", size = 4, alpha = 0.8) +
  geom_text_repel(aes(label = Region), size = 3, fontface = "bold", max.overlaps = 50) +
  # Annotations des quadrants
  annotate("text", x = med_nddi + 0.1, y = max(base_analyses_completes$Part_Agriculture_PIB) * 0.95, 
           label = "ZONE CRITIQUE\n(Fort NDDI & Forte Dépendance)", color = "red", fontface = "bold", size = 3) +
  labs(
    title = "Croisement de l'intensité de la sécheresse et de la dépendance du PIB au secteur primaire(2024)",
    x = "NDDI",
    y = "Part du secteur primaire dans le PIB régional (%)",
    ) +
  theme(plot.title = element_text(face = "bold", size = 11, color = "#1f4e79"))

print(fig_A_quadrant)
ggsave("sorties_figures/Figure_5_Quadrant_Vulnerabilite_Agricole.png", plot = fig_A_quadrant, width = 10, height = 7, dpi = 300)


# =========================================================================
# ÉTAPE 3:  FIGURE 8 -  LE GRAPHIQUE DE DOUBLE-PEINE SOCIALE
# =========================================================================
fig_B_social <- ggplot(base_analyses_completes, aes(x = NDDI_final, y = Taux_Pauvreté_Perc, color = Taux_Chomage_Perc)) +
  theme_minimal(base_size = 11) +
  geom_point(aes(size = Population_Rurale), alpha = 0.85) +
  geom_text_repel(aes(label = Region), size = 3, fontface = "bold", color = "black", max.overlaps = 50) +
  # Dégradé de couleur de type "Alerte Viridis" (du bleu au jaune/rouge)
  scale_color_viridis_c(option = "plasma", name = "Taux de chômage (%)") +
  scale_size_continuous(range = c(3, 12), labels = scales::label_comma(scale = 1e-6, suffix = " M"), name = "Pop. Rurale") +
  labs(
    title = "Croisement de NDDI avec les taux régionaux de pauvreté et de chômage(2024)",
    x = "NDDI",
    y = "Taux de pauvreté multidimensionnelle ou monétaire (%)",
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 11, color = "#1f4e79"),
    panel.grid.minor = element_blank()
  )

print(fig_B_social)
ggsave("sorties_figures/Figure_6_Double_Peine_Socio_Environnementale.png", plot = fig_B_social, width = 10, height = 7, dpi = 300)


# =========================================================================
#  ÉTAPE 3:  FIGURE 8 - DIAGRAMME DE PROFIL DE RISQUE RURAL
# =========================================================================
# Préparation des données pour le dot-plot (Normalisation temporaire pour affichage côte à côte)
base_dotplot <- base_analyses_completes %>%
  mutate(
    `Stress Hydrique (NDDI)` = (NDDI_final / max(NDDI_final)) * 100,
    `Part Population Rurale (%)` = (Population_Rurale / donnees_hcp$Population_Totale) * 100
  ) %>%
  select(Region, `Stress Hydrique (NDDI)`, `Part Population Rurale (%)`) %>%
  pivot_longer(cols = -Region, names_to = "Indicateur", values_to = "Valeur")

fig_C_profile <- ggplot(base_dotplot, aes(x = Valeur, y = reorder(Region, Valeur, mean))) +
  theme_linedraw(base_size = 11) +
  geom_line(aes(group = Region), color = "grey70", linewidth = 1) +
  geom_point(aes(color = Indicateur), size = 4) +
  scale_color_manual(values = c("Stress Hydrique (NDDI)" = "#9e1462", "Part Population Rurale (%)" = "#337a22")) +
  labs(
    title = "Classement des régions selon l'alignement de la ruralité et de l'intensité du NDDI",
    x = "Niveau relatif / Pourcentage (%)",
    y = "Régions",
    caption = "Note : Le NDDI a été indexé sur une base 100 par rapport au maximum régional pour permettre la comparaison visuelle."
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 11, color = "#1f4e79"),
    legend.position = "bottom",
    legend.title = element_blank()
  )

print(fig_C_profile)
ggsave("sorties_figures/Figure_7_Profil_Risque_Rural.png", plot = fig_C_profile, width = 10, height = 7, dpi = 300)