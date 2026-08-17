// il faut importer tout d'abord le shapefile des régions marocaines

// ============================================================
// SCRIPT GEE CORRIGÉ - SÉRIE 8-JOURS 2000-2024 (12 RÉGIONS)
// Calcul continu et sans NA du NDDI pour toutes les régions
// ============================================================

var regionAttr = 'nom_fr'; 

var startYear = 2000;
var endYear = 2024;

// Masque de qualité ajusté pour préserver les observations en zones arides
function applyQualityMasks(image) {
  var qa = image.select('StateQA');
  
  // Conservation du contrôle des nuages et ombres primordiaux
  var cloudMask = qa.bitwiseAnd(3).lt(2);        // Qualité globale acceptable
  var cloudShadow = qa.bitwiseAnd(8).eq(0);      // Pas d'ombre de nuage
  
  return image.updateMask(cloudMask.and(cloudShadow));
}

function calculerIndices(image) {
  // Facteur d'échelle MODIS
  var red = image.select('sur_refl_b01').multiply(0.0001);
  var nir = image.select('sur_refl_b02').multiply(0.0001);
  var swir = image.select('sur_refl_b06').multiply(0.0001);
  
  // Indices NDVI et NDWI
  var ndvi = nir.subtract(red).divide(nir.add(red)).rename('NDVI');
  var ndwi = nir.subtract(swir).divide(nir.add(swir)).rename('NDWI');
  
  // Calcul continu du NDDI avec epsilon (0.0001) pour empêcher la division par zéro
  // sans jamais masquer les pixels arides ou de sol nu
  var denominateur = ndvi.add(ndwi);
  
  // Si le dénominateur est quasi-nul, on injecte epsilon pour stabiliser la division
  var denominateurAjuste = denominateur.where(denominateur.abs().lt(0.0001), 0.0001);
  
  var nddi = ndvi.subtract(ndwi)
    .divide(denominateurAjuste)
    .rename('NDDI');
  
  var dateObj = image.date();
  return image
    .addBands(ndvi)
    .addBands(ndwi)
    .addBands(nddi)
    .set('date', dateObj.format('YYYY-MM-dd'))
    .set('year', dateObj.get('year'))
    .set('month', dateObj.get('month'))
    .set('day_of_year', dateObj.getRelative('day', 'year').add(1));
}

// Collection MODIS 8 jours
var modisCollection = ee.ImageCollection('MODIS/061/MOD09A1')
  .filterBounds(regions_maroc)
  .filterDate(ee.Date.fromYMD(startYear, 1, 1), ee.Date.fromYMD(endYear, 12, 31))
  .map(applyQualityMasks)
  .map(calculerIndices);

// Agrégation spatiale par région
var finalCollection = modisCollection.map(function(image) {
  var date = image.get('date');
  var year = image.get('year');
  var month = image.get('month');
  var doy = image.get('day_of_year');
  
  var indices = image.select(['NDVI', 'NDWI', 'NDDI']);
  
  var reduced = indices.reduceRegions({
    collection: regions_maroc,
    reducer: ee.Reducer.median()
      .combine(ee.Reducer.mean(), null, true)
      .combine(ee.Reducer.stdDev(), null, true),
    scale: 500,
    tileScale: 16
  });
  
  return reduced.map(function(f) {
    return f.set({
      'date': date,
      'year': year,
      'month': month,
      'day_of_year': doy
    });
  });
}).flatten();

// Exportation du CSV complet vers Google Drive
Export.table.toDrive({
  collection: finalCollection,
  description: 'Maroc_Serie_8jours_2000_2024_Clean',
  folder: 'Maroc_Drought_2000_2024',
  fileFormat: 'CSV',
  selectors: [
    regionAttr, 'year', 'month', 'day_of_year', 'date',
    'NDVI_median', 'NDWI_median', 'NDDI_median', 'NDDI_mean'
  ]
});