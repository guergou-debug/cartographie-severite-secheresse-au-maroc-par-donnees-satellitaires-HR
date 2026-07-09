# =========================================================================
# GÉNÉRATION DU DIAGRAMME PIPELINE MÉTHODOLOGIQUE (HAUTE RÉSOLUTION)
# =========================================================================
if(!require(igraph)) install.packages("igraph")
library(igraph)

# 1. Définition linéaire et focalisée du pipeline (Strictement cadré)
edges <- c(
  "Google Earth Engine (GEE)\nExtraction des Séries Satellitaires", "Filtrage Cloud Masking &\nNettoyage des Données",
  "Filtrage Cloud Masking &\nNettoyage des Données",                  "Calcul des Indices\nSpatiotemporels (NDVI / NDWI)",
  "Calcul des Indices\nSpatiotemporels (NDVI / NDWI)",                  "Calcul du NDDI Brut\net Correction Biophysique",
  "Calcul du NDDI Brut\net Correction Biophysique",                  "Cartographie"
)

# 2. Création et configuration du graphe structurel
g <- graph_from_edgelist(matrix(edges, ncol=2, byrow=TRUE), directed=TRUE)

# 3. Alignement vertical strict (Pipeline en cascade descendante)
co <- layout_as_tree(g, root=1)

# 4. ÉTAPE 1 : OUVERTURE DE LA FENÊTRE À L'ÉCRAN (Lisibilité Maximale)
dev.new(width = 11, height = 9, noRStudioGD = TRUE)

# Redimensionnement des marges pour éliminer les espaces vides inutiles
par(mar = c(1, 1, 3, 1))

# 5. ÉTAPE 2 : RENDU GRAPHIQUE À L'ÉCRAN
plot(g, 
     layout = co,
     vertex.shape = "rectangle",
     
     # Dimensions des rectangles adaptées aux textes longs
     vertex.size = 58,          # Largeur de la boîte
     vertex.size2 = 14,         # Hauteur de la boîte
     
     # Code couleur sobre et professionnel (Dégradé logique)
     vertex.color = c("#1f4e79", "#2f75b5", "#5b9bd5", "#8faadc", "#9e1462"),
     
     # Propriétés du texte (Grand et bien contrasté)
     vertex.label.color = c("white", "white", "white", "black", "white"),
     vertex.label.font = 2,     # Police en Gras (Bold)
     vertex.label.cex = 1.1,    # Taille augmentée du texte pour la lisibilité
     
     # Propriétés des flèches directionnelles
     edge.arrow.size = 0.8,
     edge.arrow.width = 1.1,
     edge.width = 2.5,
     edge.color = "#444444",
     
     # Titre général du livrable académique
     main = "")

title("Pipeline de Traitement des Données et Calculs Spatiaux", 
      cex.main = 1.5, font.main = 2, col.main = "#1f4e79", line = 1)

# 6. ÉTAPE 3 : ENREGISTREMENT AUTOMATIQUE HAUTE RÉSOLUTION (Après affichage)
# Cette fonction copie fidèlement le rendu actif à l'écran vers un fichier PNG
dev.copy(png, filename = "pipeline_methodologique_highres.png", 
         width = 3300, height = 2700, res = 300)

# Fermeture du périphérique de copie (indispensable pour finaliser le fichier PNG)
dev.off()