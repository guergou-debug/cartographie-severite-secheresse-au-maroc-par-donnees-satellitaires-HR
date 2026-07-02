// Il est important d'importer le shapefile des régions (table) avant d'exécuter ce script pour garantir que les données soient correctement extraites et analysées.
// =========================================================================
// CORRECTION INFAILLIBLE : SÉRIE MENSUELLE NDDI (2020-2024)
// =========================================================================

var annees = ee.List.sequence(2020, 2024);
var mois = ee.List.sequence(1, 12);

function masquerNuages(image) {
  var qa = image.select('StateQA');
  return image.updateMask(qa.bitwiseAnd(3).eq(0)).updateMask(qa.bitwiseAnd(4).eq(0));
}

var collectionMODIS = ee.ImageCollection("MODIS/061/MOD09A1").map(function(img) {
  var imgNettoyee = masquerNuages(img);
  var ndvi = imgNettoyee.normalizedDifference(['sur_refl_b02', 'sur_refl_b01']).rename('NDVI');
  var ndwi = imgNettoyee.normalizedDifference(['sur_refl_b02', 'sur_refl_b06']).rename('NDWI');
  return img.addBands(ndvi.subtract(ndwi).divide(ndvi.add(ndwi)).rename('NDDI'));
});

// Double boucle corrigée avec conversion explicite en FeatureCollection et double flatten
var donneesMensuelles = annees.map(function(annee) {
  var featuresDuMois = mois.map(function(m) {
    var debut = ee.Date.fromYMD(annee, m, 1);
    var fin = debut.advance(1, 'month').advance(-1, 'day');
    
    var nddiMensuel = collectionMODIS.filterDate(debut, fin).select('NDDI').mean();
    
    var extraire = nddiMensuel.reduceRegions({
      collection: table, // Ton asset importé
      reducer: ee.Reducer.mean(),
      scale: 500
    }).map(function(f) {
      return f.set({ 'Annee': annee, 'Mois': m });
    });
    
    return extraire;
  });
  
  // Aplatit les 12 FeatureCollections de l'année en cours
  return ee.FeatureCollection(featuresDuMois).flatten();
});

// Aplatit toutes les années en une seule table globale
var tableFinale = ee.FeatureCollection(donneesMensuelles).flatten();

// =========================================================================
// EXPORTATION DES DONNÉES VERS GOOGLE DRIVE
// =========================================================================
Export.table.toDrive({
  collection: tableFinale,
  description: 'Serie_Mensuelle_NDDI_2020_2024',
  fileFormat: 'CSV',
  selectors: ['Annee', 'Mois', 'nom_fr', 'mean']
});