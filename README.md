# DIBANG VALLEY DISTRICTS RAINFALL DATA 1998 - 2019
 Tropical Rainfall Measuring Mission data extracted for a specific region of interest using Goolge Earth Engine to plot rainfall anomalies in dibang and lower dibang district arunachal pradesh
 
This is the script used in GEE to export the 3-hourly precipitation data
```////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////SCRIPT TO EXPORT TRMM 3-HOURLY PRECIPITATION ESTIMATES TO A CSV////////////////////

// CREATE A RECTANGLE OF THE GIVEN EXTENTS
var rectangle = ee.Geometry.Rectangle(95.402202,27.71712,96.19871,29.214069);

//CENTER THE VIEW TO "RECTANGLE"
Map.centerObject(rectangle);

//ADD THE LAYER
Map.addLayer(rectangle, {}, 'rectangle')

// CREATE A VARIABLE OF THE TRMM 3-HOURLY DATASET
var dataset = ee.ImageCollection('TRMM/3B42').select('precipitation').filterDate("1998-01-01", "2019-12-31")

// CREATE A DICTIONARY OF POINTS SPACED 25000 METERS APART FROM EACH OTHER (ROUGHLY THE SCALE OF 0.25 ARC DEGREES)
var dictionary = ee.Image.pixelLonLat().reduceRegion({
  reducer: ee.Reducer.toCollection(['longitude', 'latitude']), 
  geometry: rectangle, 
  scale: 25000
});

// FROM THE DICTIONARY CREATE A SET OF POINTS
var points = ee.FeatureCollection(dictionary.get('features'))
    .map(function(feature) {
      var lon = feature.get('longitude');
      var lat = feature.get('latitude');
      return ee.Feature(ee.Geometry.Point([lon, lat]), {
        'featureID': ee.Number(lon).multiply(1000).round().format('%5.0f')
            .cat('_')
            .cat(ee.Number(lat).multiply(1000).round().format('%5.0f'))
      });
    });
print('points', points)

// ADD THE POINTS LAYER TO THE MAP
Map.addLayer(points, "Rainfall sampling points");

// CREATE A DATASET FROM TRMM USING A MAP FUNCTION TO LOOP OVER EVERY DATASET AT THE POINT
var triplets = dataset.map(function(image) {
  return image.reduceRegions({
    collection: points, 
    reducer: ee.Reducer.first().setOutputs(image.bandNames()), 
    scale: 250,
  }).map(function(feature) {
    return feature.set({
      'imageID': image.id(),
      'timeMillis': ee.Date(image.get('system:time_start'))
    })
  });
}).flatten();
//print(triplets) 

// SPECIFY THE FORMAT
var format = function(table, rowId, colId, rowProperty, colProperty) {
  var rows = table.distinct(rowId); 
  var joined = ee.Join.saveAll('matches').apply({
    primary: rows, 
    secondary: table, 
    condition: ee.Filter.equals({
      leftField: rowId, 
      rightField: rowId
    })
  });
  return joined.map(function(row) {
      var values = ee.List(row.get('matches'))
        .map(function(feature) {
          feature = ee.Feature(feature);
          return [feature.get(colId), feature.get(colProperty)];
        }).flatten();
      return row.select([rowId, rowProperty]).set(ee.Dictionary(values));
    });
};


// THE RESULTS
var results = format(triplets, 'imageID', 'featureID', 'timeMillis', 'precipitation');
//print(results, 'results')

// EXPORT THE RESULTS TABLE
Export.table.toDrive({
  collection: results, 
  description: 'dibang-precipitation_1998-2019', 
  fileNamePrefix: '3B42_dibang_1998-2019', 
  fileFormat: 'CSV'
})```
