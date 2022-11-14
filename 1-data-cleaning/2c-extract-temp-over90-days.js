// Extracts number of days in each month with max temperatures >90 degrees  
// Run on Google Earth Engine

var batch = require('users/fitoprincipe/geetools:batch');

var startDate = ee.Date.fromYMD(2015, 1, 1)
var endDate = ee.Date.fromYMD(2021, 1, 1) // Exclusive
var aoi = geometry // geometry was selected using the rectangle tool, captured continental US

var collection = ee.ImageCollection('MODIS/061/MOD11A1')
.filterBounds(aoi)

var numberOfMonths = endDate.difference(startDate, 'months').floor()
var daysAbove90Collection = ee.ImageCollection(
  ee.List.sequence(0, numberOfMonths.subtract(1))
  .map(daysAbove90)
)

function daysAbove90(monthOffset) {
  var date = startDate.advance(monthOffset, 'months')
  return collection
  .select('LST_Day_1km')
  .filterDate(date, date.advance(1, 'months'))
  .map(function (image) {
    var above90 = image.gte(15268.6) // Apply conversion and scale 1/(0.2) from kelvin
    return image.updateMask(above90)
  })
  .count()
  .unmask(0)
  .clip(aoi)
  .set('date', date.format('yyyy-MM-dd'))
}

batch.Download.ImageCollection.toDrive(daysAbove90Collection, 'temps_above_90')