// il faut importer tout d'abord le shapefile des régions marocaines
/**
 * ============================================================================
 * PROJET : Indice Composite de Sécheresse par Télédétection (CDAI-Maroc)
 * ÉTAPE 2 : Extraction GEE Robustifiée (2000-2026)
 * ============================================================================
 */

// 1. Utilisation de ta variable d'entrée 'table' (12 régions du Maroc)
var marocRegions = table;

Map.centerObject(marocRegions, 6);
Map.addLayer(marocRegions, {color: 'red'}, '12 Régions (nom_fr)', true);

// 2. Dates de la série historique
var startYear = 2000;
var endYear = 2026;

// 3. Masque des Sols Nus Permanents (Collection MODIS v061 Terra NDVI)
var modisNDVICol = ee.ImageCollection("MODIS/061/MOD13Q1")
                     .filterDate('2000-01-01', '2025-12-31')
                     .select('NDVI')
                     .map(function(img){ return img.multiply(0.0001); });

var p95NDVI = modisNDVICol.reduce(ee.Reducer.percentile([95]));
// Seuil : p95(NDVI) < 0.15 identifie les sols nus stables/permanents
var permanentDesertMask = p95NDVI.lt(0.15);

// 4. Génération d'une séquence mensuelle sécurisée
var years = ee.List.sequence(startYear, endYear);
var months = ee.List.sequence(1, 12);

// Construction de la grille temporelle mois par mois
var monthlyList = years.map(function(y) {
  return months.map(function(m) {
    var startDate = ee.Date.fromYMD(y, m, 1);
    var endDate = startDate.advance(1, 'month');
    
    // Vérification que la date ne dépasse pas aujourd'hui
    var isPast = startDate.millis().lte(ee.Date(Date.now()).millis());
    
    return ee.Algorithms.If(isPast, (function() {
      // A. CHIRPS Précipitations
      var p = ee.ImageCollection("UCSB-CHG/CHIRPS/DAILY")
                .filterDate(startDate, endDate)
                .select('precipitation')
                .sum()
                .rename('P');
                
      // B. MODIS NDVI (Collection 061)
      var ndvi = ee.ImageCollection("MODIS/061/MOD13Q1")
                   .filterDate(startDate, endDate)
                   .select('NDVI')
                   .mean();
      // Gestion des mois sans image NDVI
      var ndviScaled = ee.Algorithms.If(
        ndvi.bandNames().size().gt(0),
        ndvi.multiply(0.0001),
        ee.Image.constant(0)
      );
      var ndviImg = ee.Image(ndviScaled).rename('NDVI');
      
      // C. MODIS LST (Collection 061)
      var lst = ee.ImageCollection("MODIS/061/MOD11A2")
                  .filterDate(startDate, endDate)
                  .select('LST_Day_1km')
                  .mean();
      var lstScaled = ee.Algorithms.If(
        lst.bandNames().size().gt(0),
        lst.multiply(0.02).subtract(273.15),
        ee.Image.constant(0)
      );
      var lstImg = ee.Image(lstScaled).rename('LST');
      
      // D. MODIS ET/PET Ratio (Collection 061)
      var etCol = ee.ImageCollection("MODIS/061/MOD16A2").filterDate(startDate, endDate);
      var et = etCol.select('ET').mean();
      var pet = etCol.select('PET').mean();
      var etRatio = ee.Algorithms.If(
        et.bandNames().size().gt(0),
        et.multiply(0.1).divide(pet.multiply(0.1).add(0.0001)),
        ee.Image.constant(0)
      );
      var etImg = ee.Image(etRatio).rename('ET_PET');
      
      // Assemblage des 4 bandes
      return ee.Image.cat([p, ndviImg, lstImg, etImg])
                .set({
                  'year': y,
                  'month': m,
                  'date_str': startDate.format('YYYY-MM')
                });
    })(), null);
  });
}).flatten();

// Nettoyage des éléments nuls
var monthlyCollection = ee.ImageCollection(monthlyList.removeAll([null]));

// 5. Réduction Spatiale par Région (Moyenne par polygone avec Masque)
var extractStats = monthlyCollection.map(function(img) {
  var yr      = img.get('year');
  var mo      = img.get('month');
  var dateStr = img.get('date_str');
  
  // Application du masque des sols nus sur NDVI et ET_PET
  var maskedImg = img.addBands(
    img.select(['NDVI', 'ET_PET']).updateMask(permanentDesertMask.not()),
    null, true
  );
  
  var stats = maskedImg.reduceRegions({
    collection: marocRegions,
    reducer: ee.Reducer.mean(),
    scale: 5000 // Échelle régionale
  });
  
  return stats.map(function(f) {
    return f.set({
      'nom_fr': f.get('nom_fr'),
      'year': yr,
      'month': mo,
      'date': dateStr,
      'P': f.get('P'),
      'NDVI': f.get('NDVI'),
      'LST': f.get('LST'),
      'ET_PET': f.get('ET_PET')
    });
  });
}).flatten();

// 6. Exportation vers Google Drive
Export.table.toDrive({
  collection: extractStats,
  description: 'Maroc_Drought_12Regions_2000_2026',
  fileFormat: 'CSV',
  selectors: ['nom_fr', 'year', 'month', 'date', 'P', 'NDVI', 'LST', 'ET_PET']
});

print("Script prêt et sécurisé avec les collections MODIS v061.");