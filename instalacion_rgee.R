# remotes::install_github('r-spatial/rgee')
library(mapview)
library(rgee)
# rgee::ee_install()
ee_Initialize()

p1 <- c(-75.80443484849307,-48.38616390108878)
p2 <- c(-71.01439578599307,-48.38616390108878)
p3 <- c(-71.01439578599307,-40.914333811907866)
p4 <- c(-75.80443484849307,-40.914333811907866)
p5 <- c(-75.80443484849307,-48.38616390108878)

marco_trabajo <- ee$Geometry$Polygon(c(p1, p2, p3, p4, p5))
Map$addLayer(marco_trabajo)

collection <- ee$
	ImageCollection("ECMWF/ERA5/MONTHLY")$
	filter(ee$Filter$date('2010-01', '2014-12'))$
	map(function(image){image$clip(marco_trabajo)})

tas <- collection$select('mean_2m_air_temperature')

tas_ok <- ee$Image(tas$first());

Map$setCenter(-72, -44.75, 6.3);

Map$addLayer(
eeObject = tas_ok,
  visParams = list(
  min = 250.0,
  max = 320.0,
  palette = c("#000080","#0000D9","#4000FF","#8000FF","#0080FF","#00FFFF","#00FF80","#80FF00","#DAFF00","#FFFF00","#FFF500","#FFDA00","#FFB000","#FFA400","#FF4F00","#FF2500","#FF0A00","#FF00FF")
  ),
  name = 'tas'
)

