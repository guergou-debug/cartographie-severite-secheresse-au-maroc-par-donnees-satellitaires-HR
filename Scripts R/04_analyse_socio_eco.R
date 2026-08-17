# =========================================================================
# ÉTAPE 3: ANALYSE SOCIO-ÉCONOMIQUE & SÉVÉRITÉ DE LA SÉCHERESSE (2024-2025)
# =========================================================================

# -------------------------------------------------------------------------
# 1. Préparation des données satellites (NDDI 2025 BRUT & QUINTILES)
# -------------------------------------------------------------------------

# Chargement des données 2025
base_donnees_drought <- read.csv("Maroc_Serie_8jours_2025.csv")

# Correction du nom de la colonne (nom_fr -> Region) et calcul du NDDI moyen
nddi_physique <- base_donnees_drought %>%
  rename(Region = nom_fr) %>%  # Alignement du nom de la variable géographique
  group_by(Region) %>%
  summarise(
    NDDI_moyen = mean(NDDI_corrige, na.rm = TRUE)
  )

# Calcul des bornes exactes des quintiles (0%, 20%, 40%, 60%, 80%, 100%)
bornes <- quantile(nddi_physique$NDDI_moyen, probs = seq(0, 1, 0.2), na.rm = TRUE)

# Classification par quintiles avec bornes explicites
nddi_physique <- nddi_physique %>%
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

# -------------------------------------------------------------------------
# 2. Chargement des données HCP 2024 et Fusion par la colonne 'Region'
# -------------------------------------------------------------------------

donnees_hcp <- read_excel("donnees_socio_eco_maroc_HCP_2024.xlsx")

# Fusion explicite par Region
base_impact_finale <- nddi_physique %>%
  left_join(donnees_hcp, by = "Region")

# Vérification du diagnostic de fusion
donnees_manquantes <- base_impact_finale %>% filter(is.na(PIB_Regional_MDH))
if(nrow(donnees_manquantes) > 0) {
  cat("\n⚠️ ATTENTION : Incohérence dans l'orthographe des noms de régions entre le CSV et l'Excel pour :\n")
  print(donnees_manquantes$Region)
} else {
  cat("\n✅ Fusion réussie ! Les 12 régions sont parfaitement alignées.\n")
}

# Config palette OCDE dynamisée
niveaux_labels <- levels(base_impact_finale$Severite_Quintile)
palette_officielle_ocde <- setNames(
  c("#337a22", "#a1db74", "#ffffff", "#e383bd", "#9e1462"),
  niveaux_labels
)

# =========================================================================
# FIGURE 5 - BUBBLE CHART : NDDI VS PIB RÉGIONAL
# =========================================================================

figure_4_impact <- ggplot(
  data = base_impact_finale %>% filter(!is.na(PIB_Regional_MDH)), 
  aes(x = NDDI_moyen, y = PIB_Regional_MDH, size = Population_Totale, fill = Severite_Quintile)
) +
  theme_bw(base_size = 11) +
  geom_point(shape = 21, color = "black", alpha = 0.85, stroke = 0.6) +
  geom_text_repel(aes(label = Region), size = 3, fontface = "bold", color = "black",
                  box.padding = 0.4, point.padding = 0.3, max.overlaps = 50, show.legend = FALSE) +
  scale_fill_manual(values = palette_officielle_ocde, drop = FALSE, name = "Sévérité (Quintiles NDDI)") +
  scale_size_continuous(range = c(4, 16), labels = scales::label_comma(scale = 1e-6, suffix = " M"), name = "Population") +
  scale_y_continuous(labels = scales::label_comma(suffix = " MDH")) +
  scale_x_continuous() +
  labs(
    title = "Sévérité de la sécheresse (NDDI) par PIB et Population régionale",
    subtitle = "Croisement NDDI 2025 (MODIS) et Données socio-économiques HCP 2024",
    x = "Index de Sécheresse NDDI (Moyenne 2025)",
    y = "PIB Régional (Prix Courants, Millions DH)"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 11, color = "#1f4e79"),
    plot.subtitle = element_text(size = 9, color = "grey40", margin = margin(b = 10)),
    axis.title = element_text(face = "bold", size = 10),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    legend.box = "vertical",
    legend.title = element_text(face = "bold", size = 9),
    legend.text = element_text(size = 8)
  ) +
  guides(
    fill = guide_legend(override.aes = list(size = 4)),
    size = guide_legend(override.aes = list(fill = "white"))
  )

print(figure_4_impact)
ggsave("sorties_figures/Figure_4_Bubble_Chart_Impact_Maroc.png", plot = figure_4_impact, width = 11, height = 7, dpi = 300)


# =========================================================================
# FIGURE 6 - BUBBLE CHART : NDDI VS PIB PAR HABITANT
# =========================================================================

figure_4_impact_hab <- ggplot(
  data = base_impact_finale %>% filter(!is.na(PIB_Par_Habitant_DH)), 
  aes(x = NDDI_moyen, y = PIB_Par_Habitant_DH, size = Population_Totale, fill = Severite_Quintile)
) +
  theme_bw(base_size = 11) +
  geom_point(shape = 21, color = "black", alpha = 0.85, stroke = 0.6) +
  geom_text_repel(aes(label = Region), size = 3, fontface = "bold", color = "black",
                  box.padding = 0.4, point.padding = 0.3, max.overlaps = 50, show.legend = FALSE) +
  scale_fill_manual(values = palette_officielle_ocde, drop = FALSE, name = "Sévérité (Quintiles NDDI)") +
  scale_size_continuous(range = c(4, 16), labels = scales::label_comma(scale = 1e-6, suffix = " M"), name = "Population") +
  scale_y_continuous(labels = scales::label_comma(suffix = " DH")) +
  scale_x_continuous() +
  labs(
    title = "Sévérité de la sécheresse (NDDI) par PIB par habitant et Population",
    subtitle = "Croisement NDDI 2025 (MODIS) et Données socio-économiques HCP 2024",
    x = "Index de Sécheresse NDDI (Moyenne 2025)",
    y = "PIB par habitant (Prix courants, DH)"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 11, color = "#1f4e79"),
    plot.subtitle = element_text(size = 9, color = "grey40", margin = margin(b = 10)),
    axis.title = element_text(face = "bold", size = 10),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    legend.box = "vertical",
    legend.title = element_text(face = "bold", size = 9),
    legend.text = element_text(size = 8)
  ) +
  guides(
    fill = guide_legend(override.aes = list(size = 4)),
    size = guide_legend(override.aes = list(fill = "white"))
  )

print(figure_4_impact_hab)
ggsave("sorties_figures/Figure_4_Bubble_Chart_PIB_Habitant.png", plot = figure_4_impact_hab, width = 11, height = 7, dpi = 300)


# =========================================================================
# FIGURE 7 - QUADRANT DE VULNÉRABILITÉ SECTORIELLE (SECTEUR PRIMAIRE)
# =========================================================================

med_nddi <- median(base_impact_finale$NDDI_moyen, na.rm = TRUE)
med_agri <- median(base_impact_finale$Part_Agriculture_PIB, na.rm = TRUE)

fig_A_quadrant <- ggplot(base_impact_finale, aes(x = NDDI_moyen, y = Part_Agriculture_PIB)) +
  theme_bw(base_size = 11) +
  geom_vline(xintercept = med_nddi, linetype = "dashed", color = "grey50") +
  geom_hline(yintercept = med_agri, linetype = "dashed", color = "grey50") +
  geom_point(color = "#1f4e79", size = 4, alpha = 0.8) +
  geom_text_repel(aes(label = Region), size = 3, fontface = "bold", max.overlaps = 50) +
  annotate(
    "text", x = med_nddi + (max(base_impact_finale$NDDI_moyen) - med_nddi)/2, 
    y = max(base_impact_finale$Part_Agriculture_PIB, na.rm = TRUE) * 0.95, 
    label = "ZONE CRITIQUE\n(Fort NDDI & Forte Dépendance)", color = "red", fontface = "bold", size = 3
  ) +
  labs(
    title = "Sévérité du NDDI et dépendance du PIB au secteur primaire",
    subtitle = "Lignes pointillées = Médianes nationales",
    x = "Index de Sécheresse NDDI (Moyenne 2025)",
    y = "Part du secteur primaire dans le PIB régional (%)"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 11, color = "#1f4e79"),
    plot.subtitle = element_text(size = 9, color = "grey40")
  )

print(fig_A_quadrant)
ggsave("sorties_figures/Figure_5_Quadrant_Vulnerabilite_Agricole.png", plot = fig_A_quadrant, width = 10, height = 7, dpi = 300)


# =========================================================================
# FIGURE 8 - DOUBLE-PEINE SOCIALE (PAUVRETÉ & CHÔMAGE)
# =========================================================================

fig_B_social <- ggplot(base_impact_finale, aes(x = NDDI_moyen, y = Taux_Pauvreté_Perc, color = Taux_Chomage_Perc)) +
  theme_minimal(base_size = 11) +
  geom_point(aes(size = Population_Rurale), alpha = 0.85) +
  geom_text_repel(aes(label = Region), size = 3, fontface = "bold", color = "black", max.overlaps = 50) +
  scale_color_viridis_c(option = "plasma", name = "Taux de chômage (%)") +
  scale_size_continuous(range = c(3, 12), labels = scales::label_comma(scale = 1e-6, suffix = " M"), name = "Pop. Rurale") +
  labs(
    title = "Sévérité de la sécheresse (NDDI) vs Pauvreté et Chômage",
    x = "Index de Sécheresse NDDI (Moyenne 2025)",
    y = "Taux de pauvreté (%)"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 11, color = "#1f4e79"),
    panel.grid.minor = element_blank()
  )

print(fig_B_social)
ggsave("sorties_figures/Figure_6_Double_Peine_Socio_Environnementale.png", plot = fig_B_social, width = 10, height = 7, dpi = 300)


# =========================================================================
# FIGURE 9 - DIAGRAMME DE PROFIL DE RISQUE RURAL
# =========================================================================

base_dotplot <- base_impact_finale %>%
  mutate(
    `Stress Hydrique (NDDI Relative)` = (NDDI_moyen / max(NDDI_moyen, na.rm = TRUE)) * 100,
    `Part Pop. Rurale (%)` = (Population_Rurale / Population_Totale) * 100
  ) %>%
  select(Region, `Stress Hydrique (NDDI Relative)`, `Part Pop. Rurale (%)`) %>%
  pivot_longer(cols = -Region, names_to = "Indicateur", values_to = "Valeur")

fig_C_profile <- ggplot(base_dotplot, aes(x = Valeur, y = reorder(Region, Valeur, mean))) +
  theme_linedraw(base_size = 11) +
  geom_line(aes(group = Region), color = "grey70", linewidth = 1) +
  geom_point(aes(color = Indicateur), size = 4) +
  scale_color_manual(values = c("Stress Hydrique (NDDI Relative)" = "#9e1462", "Part Pop. Rurale (%)" = "#337a22")) +
  labs(
    title = "Alignement entre l'exposition au stress hydrique et la ruralité",
    x = "Pourcentage (%) / Indice relatif base 100",
    y = "Régions",
    caption = "Note : Le NDDI est indexé sur une base 100 (relativement au maximum) pour permettre la comparaison avec la part rurale."
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 11, color = "#1f4e79"),
    legend.position = "bottom",
    legend.title = element_blank()
  )

print(fig_C_profile)
ggsave("sorties_figures/Figure_7_Profil_Risque_Rural.png", plot = fig_C_profile, width = 10, height = 7, dpi = 300)