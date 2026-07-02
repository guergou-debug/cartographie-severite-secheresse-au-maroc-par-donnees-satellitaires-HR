// penser a importer le shapefile des regions (table) avant d'executer ce script

// =========================================================================
// EXTRACTION COLOSSALE NETTOYÉE : SÉRIE TEMPORELLE NDDI (2000 - 2024)
// MODÉLISATION DE L'HISTORIQUE SANS NUAGES NI BRUIT ATMOSPHÉRIQUE
// =========================================================================

// 1. Définir la séquence temporelle (25 ans de données)
var annees = ee.List.sequence(2000, 2024);

// 2. Fonction de masquage de qualité (MODIS QA Cloud Mask)
function masquerNuages(image) {
  var qa = image.select('StateQA');
  // Conserver uniquement les pixels où l'état du nuage est limpide (clear)
  var masqueNuage = qa.bitwiseAnd(3).eq(0);
  // Conserver uniquement les pixels sans ombre de nuage
  var masqueOmbre = qa.bitwiseAnd(4).eq(0);
  return image.updateMask(masqueNuage).updateMask(masqueOmbre);
}

// 3. Fonction pour calculer l'NDDI sur une image MODIS nettoyée
var calculerNDDI = function(img) {
  // Masquage préalable des nuages
  var imgNettoyee = masquerNuages(img);
  
  var ndvi = imgNettoyee.normalizedDifference(['sur_refl_b02', 'sur_refl_b01']).rename('NDVI');
  var ndwi = imgNettoyee.normalizedDifference(['sur_refl_b02', 'sur_refl_b06']).rename('NDWI');
  
  // Formule officielle du NDDI
  var nddi = ndvi.subtract(ndwi).divide(ndvi.add(ndwi)).rename('NDDI');
  
  return img.addBands(nddi);
};

// Charger la collection MODIS (Terra Surface Reflectance 8-Day 500m) et appliquer les calculs
var collectionMODIS = ee.ImageCollection("MODIS/061/MOD09A1").map(calculerNDDI);

// 4. Boucle temporelle pour extraire les moyennes annuelles par région
var donneesExtraites = annees.map(function(annee) {
  var debut = ee.Date.fromYMD(annee, 1, 1);
  var fin = ee.Date.fromYMD(annee, 12, 31);
  
  // Sélectionner les images de l'année et faire la réduction temporelle par la moyenne
  var nddiAnnuel = collectionMODIS.filterDate(debut, fin).select('NDDI').mean();
  
  // Extraire la valeur moyenne pour chaque polygone régional
  var extraireParRegion = nddiAnnuel.reduceRegions({
    collection: table, // Utilisation directe de ton asset importé
    reducer: ee.Reducer.mean(),
    scale: 500 // Résolution spatiale native du capteur
  }).map(function(feature) {
    // Ajouter l'attribut de l'année à chaque ligne du tableau de sortie
    return feature.set('Annee', annee);
  });
  
  return extraireParRegion;
});

// Aplatir la liste de collections annuelles en une seule et unique table globale
var tableFinale = ee.FeatureCollection(donneesExtraites).flatten();

// =========================================================================
// EXPORTATION DES DONNÉES SÉCURISÉES VERS GOOGLE DRIVE
// =========================================================================
Export.table.toDrive({
  collection: tableFinale,
  description: 'Serie_Temporelle_NDDI_2000_2024_Nettoye',
  fileFormat: 'CSV',
  selectors: ['Annee', 'nom_fr', 'mean'] // Aligné avec ton champ 'nom_fr'
});