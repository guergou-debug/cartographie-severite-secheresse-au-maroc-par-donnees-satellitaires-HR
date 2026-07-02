// =========================================================================
// PROJET DE RECHERCHE : REPRODUCTION DE LA MÉTHODOLOGIE 
// ÉTAPE 1: DÉFINITION DE LA ZONE D'ÉTUDE DU MAROC UNIFIÉ
// =========================================================================

// Definition manuelle et rigoureuse des limites du Maroc (Sahara inclus)
// Longitude Ouest, Latitude Sud, Longitude Est, Latitude Nord
var limitesMaroc = [-17.2, 20.7, -1.0, 36.0]; 

// Création de la géométrie unifiée sous forme de Rectangle (Region of Interest)
var roiGeometry = ee.Geometry.Rectangle(limitesMaroc);

// Configuration de la visualisation
Map.centerObject(roiGeometry, 5);

// Style d'affichage : Bordure verte continue pour validation
var styleZone = {
  color: 'green',
  fillColor: '00000000',
  width: 3
};

// Affichage sur la carte GEE
Map.addLayer(roiGeometry, styleZone, 'Zone d Study : Maroc Integral');

// Affichage des coordonnées pour vérification dans la console
print("Géométrie rectangulaire du Maroc unifié créée :", roiGeometry);


// =========================================================================
// ÉTAPE 2 : CHARGEMENT DE MOD09A1, FILTRAGE TEMPOREL ET MASQUAGE QUALITÉ
// =========================================================================

// 1. Définition de la période d'étude (Année complète 2024)
var dateDebut = '2024-01-01';
var dateFin = '2024-12-31';

// 2. Fonction scientifique de masquage des nuages basée sur la bande StateQA
function masquerNuagesMODIS(image) {
  // La bande 'StateQA' contient les drapeaux de qualité au format binaire
  var qa = image.select('StateQA');
  
  // Bits 0-1 : État du nuage (00 = limpide/clear, 01 = nuageux, 10 = mixte)
  // Nous créons un masque où le bit 0 et le bit 1 doivent être à 0 (pas de nuage)
  var masqueNuage = qa.bitwiseAnd(3).eq(0);
  
  // Bit 2 : Ombre de nuage (0 = non, 1 = oui)
  // Nous isolons le bit 2 et vérifions qu'il est égal à 0
  var masqueOmbre = qa.bitwiseAnd(4).eq(0);
  
  // Combinaison des masques (le pixel doit être sans nuage ET sans ombre)
  var masqueFinal = masqueNuage.and(masqueOmbre);
  
  // Application du masque et conservation des propriétés temporelles de l'image
  return image.updateMask(masqueFinal)
              .copyProperties(image, ['system:time_start']);
}

// 3. Chargement et prétraitement de la collection MODIS MOD09A1 V6.1
var collectionModis = ee.ImageCollection("MODIS/061/MOD09A1")
                        .filterBounds(roiGeometry)
                        .filterDate(dateDebut, dateFin)
                        .map(masquerNuagesMODIS);

// 4. Extraction d'une image composite médiane pour vérification visuelle
var compositeMediane = collectionModis.median().clip(roiGeometry);

// 5. Paramètres de visualisation en vraies couleurs (Bandes 1, 4, 3 pour MODIS)
// Band 1 = Rouge, Band 4 = Vert, Band 3 = Bleu
var visParams = {
  bands: ['sur_refl_b01', 'sur_refl_b04', 'sur_refl_b03'],
  min: -100,
  max: 3000,
  gamma: 1.4
};

// Affichage du résultat nettoyé sur la carte
Map.addLayer(compositeMediane, visParams, 'Mosaïque Médiane MODIS 2024 Nettoyée');

// Affichage du nombre d'images disponibles dans la console
print("Nombre d'images MODIS collectées et filtrées pour 2024 :", collectionModis.size());


// =========================================================================
// ÉTAPE 3 : CALCUL DES INDICES SPECTRAUX (NDVI, NDWI, NDDI) POUR CHAQUE IMAGE
// =========================================================================

// 1. Fonction scientifique pour calculer les 3 indices sur une image
function calculerIndicesDrought(image) {
  // Extraction des bandes MODIS requises par les auteurs
  var red = image.select('sur_refl_b01');
  var nir = image.select('sur_refl_b02');
  var swir = image.select('sur_refl_b06');
  
  // Calcul du NDVI (Equation standard : (NIR - Red) / (NIR + Red))
  var ndvi = image.normalizedDifference(['sur_refl_b02', 'sur_refl_b01']).rename('NDVI');
  
  // Calcul du NDWI (Equation de Gao 1996 utilisée par les auteurs : (NIR - SWIR) / (NIR + SWIR))
  var ndwi = image.normalizedDifference(['sur_refl_b02', 'sur_refl_b06']).rename('NDWI');
  
  // Calcul du NDDI (Equation 3 de l'article : (NDVI - NDWI) / (NDVI + NDWI))
  // Nous utilisons une expression mathématique pour éviter les divisions par zéro
  var nddi = image.expression(
    '(NDVI - NDWI) / (NDVI + NDWI)', {
      'NDVI': ndvi,
      'NDWI': ndwi
    }).rename('NDDI');
    
  // On ajoute ces 3 nouvelles bandes à l'image d'origine et on préserve la date
  return image.addBands([ndvi, ndwi, nddi])
              .copyProperties(image, ['system:time_start']);
}

// 2. Application de la fonction à toute notre collection de 46 images
var collectionAvecIndices = collectionModis.map(calculerIndicesDrought);

// 3. Extraction de la moyenne annuelle du NDDI pour observation spatiale
var nddiAnnuelMoyen = collectionAvecIndices.select('NDDI').mean().clip(roiGeometry);

// 4. Paramètres de visualisation du NDDI (Sécheresse)
// Plus le NDDI est élevé (proche de 1), plus la sécheresse est extrême.
var visNDDI = {
  min: 0.0,
  max: 1.0,
  palette: ['blue', 'yellow', 'orange', 'red', 'brown']
};

// Affichage de la carte du stress hydrique de contrôle
Map.addLayer(nddiAnnuelMoyen, visNDDI, 'NDDI Moyen Annuel 2024 (Stress Hydrique)');


// =========================================================================
// ÉTAPE 4 : AUTOMATISATION DE L'EXPORTATION DES INDICES VERS GOOGLE DRIVE
// =========================================================================

// 1. Sélection uniquement des 3 bandes d'indices dont nous avons besoin pour R
var collectionAExporter = collectionAvecIndices.select(['NDVI', 'NDWI', 'NDDI']);

// 2. Conversion de la collection en liste pour pouvoir boucler dessus
var listeImages = collectionAExporter.toList(collectionAExporter.size());
var nbImages = collectionAExporter.size().getInfo(); // Récupère le nombre d'images (46)

// 3. Boucle automatique pour créer les tâches d'exportation
for (var i = 0; i < nbImages; i++) {
  // Récupération de l'image courante
  var img = ee.Image(listeImages.get(i));
  
  // Récupération de la date de l'image pour nommer proprement le fichier
  var dateStr = ee.Date(img.get('system:time_start')).format('YYYY_MM_dd').getInfo();
  var nomFichier = 'MODIS_Maroc_Indices_' + dateStr;
  
  // Configuration de l'export vers Google Drive
  Export.image.toDrive({
    image: img.clip(roiGeometry),
    description: nomFichier,
    folder: 'Projet_Drought_Maroc_2024', // Nom du dossier qui sera créé sur ton Drive
    fileNamePrefix: nomFichier,
    region: roiGeometry,
    scale: 500, // Résolution exacte de 500m demandée par les auteurs (Règle d'or)
    maxPixels: 1e9,
    fileFormat: 'GeoTIFF'
  });
}

print("Toutes les requêtes d'exportation ont été générées. Accède à l'onglet 'Tasks' à droite !");


