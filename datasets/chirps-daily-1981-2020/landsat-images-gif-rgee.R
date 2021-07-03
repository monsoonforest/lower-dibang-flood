 ### 3. Create an NDVI-animation ([JS version](https://developers.google.com/earth-engine/tutorials/community/modis-ndvi-time-series-animation/))
  
##  Install and load `sf`, after that, initialize the Earth Engine R API.


library(magick)
library(rgee)
library(sf)

ee_Initialize()

## Define the regional bounds of animation frames and a mask to clip the NDVI data by.


mask <- file.path("/home/csheth/documents/work/remote-sensing/arunachal/lower-dibang-flood/maps/animation-bounding-box.gpkg") %>% 
  st_read(quiet = TRUE) %>% 
  sf_as_ee()
region <- mask$geometry()$bounds()

## Retrieve the Landsat 5 and Landsat 8 collection and filter to the exact path and row

## try and get exactly that many images as the years rename the bands so that the visparams are the same for both collections.
colL5 <- ee$ImageCollection('LANDSAT/LT05/C01/T1_SR')$select('B7', 'B3', 'B2')$
									  filterDate('1987-01-01', '2012-01-01')$
									  filter(ee$Filter$calendarRange(11,12,'month'))$
									  filter(ee$Filter$eq('WRS_PATH', 134))$
									  filter(ee$Filter$eq('WRS_ROW', 41))$
									  filter(ee$Filter$lessThan('CLOUD_COVER', 7)) %>% 
									  ee$ImageCollection$map(function(x) {
									  	x$select('B7', 'B3', 'B2')$rename("SWIRII", "Red", "Green")
									  }
									 )

colL8 <- ee$ImageCollection('LANDSAT/LC08/C01/T1_SR')$select('B7', 'B4', 'B3')$
									  filterDate('2013-01-01', '2021-07-01')$
									  filter(ee$Filter$calendarRange(11,12,'month'))$
									  filter(ee$Filter$eq('WRS_PATH', 134))$
									  filter(ee$Filter$eq('WRS_ROW', 41))$
									  filter(ee$Filter$lessThan('CLOUD_COVER', 10))	%>%
									  ee$ImageCollection$map(function(x) {
									  	x$select('B7', 'B4', 'B3')$rename("SWIRII", "Red", "Green")
									  }
									 )
## Merge both the collections
colL5L8 <- colL5$merge(colL8)

## Define a filter that identifies which images from the complete collection match the DOY from the distinct DOY collection.

distinctyear <- colL5L8$filterDate('1987-01-01', '2021-12-12')

filter <- ee$Filter$equals(leftField = 'year', rightField = 'year')

## Define a join; convert the resulting FeatureCollection to an ImageCollection.


join <- ee$Join$saveAll('year')

joinColL5L8 <- ee$ImageCollection(join$apply(distinctyear, colL5L8, filter))

## Apply median reduction among matching DOY collections.


# comp <- joinCol$map(function(img) {
#   doyCol = ee$ImageCollection$fromImages(
#     img$get('year_matches')
#   )
#   doyCol$reduce(ee$Reducer$median())
# })

## Define RGB visualization parameters.


visParams = list(
  min = 50.0,
  max = 2000.0,
  bands = c('SWIRII', 'Red','Green')
  )


## Create RGB visualization images for use as animation frames.


rgbVis <- joinColL5L8$map(function(img) {
  do.call(img$visualize, visParams) %>% 
    ee$Image$clip(mask)
})

## Define GIF visualization parameters.


gifParams <- list(
  region = region,
  dimensions = 800,
  crs = 'EPSG:3857',
  framesPerSecond = 1.8
)

## Get month names


dates_mabbr <- distinctyear %>% 
  ee_get_date_ic %>% # Get Image Collection dates
  '[['("time_start") %>% # Select time_start column
  lubridate::year()

## Use ee_utils_gif\_\* functions to render the GIF animation and add some texts.

animation <- ee_utils_gif_creator(rgbVis, gifParams, mode = "wb")


animation_wtxt <- animation %>% 
  ee_utils_gif_annotate(
    text = "Landsat 5 & 8 1987-2020 Lower Dibang Valley",
    size = 30, color = "white",
    location = "+10+10",
    boxcolor = "#000000"
  ) %>% 
  ee_utils_gif_annotate(
    text = dates_mabbr, 
    size = 50, 
    location = "+50+50",
    color = "white", 
    font = "arial",
    boxcolor = "#000000"
  )

ee_utils_gif_save(animation_wtxt, path = "Landsat5-8_anpum_1987-2020.gif")

ee_utils_gif_save(animationL8_wtxt, path = "Landsat8_anpum_2013-2021.gif")