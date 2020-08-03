# remotes::install_github('r-spatial/rgee')
library(mapview)
library(rgee)
library(googleCloudStorageR)
library(googledrive)


rm(list=ls())
dev.off()


# Informacion para seleccion de imagenes ----

# rgee::ee_install()
ee_Initialize(email = 'fcomanquehual@gmail.cl', drive = TRUE, quiet = FALSE)

latitud <- c(-41,-48)
longitud <- c(-71, -76)

nombre.variable <- 'mean_2m_air_temperature'
anho.inicio <- 2010
anho.termino <- 2015
mes.inicio <- '01'
mes.termino <- '01'


p1 <- c(longitud[2],latitud[2])
p2 <- c(longitud[1],latitud[2])
p3 <- c(longitud[1],latitud[1])
p4 <- c(longitud[2],latitud[1])
p5 <- c(longitud[2],latitud[2])

marco_trabajo <- ee$Geometry$Polygon(c(p1, p2, p3, p4, p5))
Map$addLayer(marco_trabajo)

# fin ---




# Seleccion de imagenes ----

fecha.para.filtro.inicio <- paste(anho.inicio, mes.inicio, sep = '-') ; fecha.para.filtro.inicio
fecha.para.filtro.termino <- paste(anho.termino, mes.termino, sep = '-') ; fecha.para.filtro.termino

collection <- ee$
	ImageCollection("ECMWF/ERA5/MONTHLY")$
	filter(ee$Filter$date(fecha.para.filtro.inicio, fecha.para.filtro.termino))$
	map(function(image){image$clip(marco_trabajo)})
ee_print(collection)

img.variable <- collection$select(nombre.variable)
ee_print(img.variable)

# transforma a imagen 
# img.variable_ok <- ee$Image(img.variable$first())
# ee_print(img.variable_ok)

# info para nombre de salida
#id.img <- img.variable$id()$getInfo() ; id.img
# epsg.i <- img.variable$projection()$getInfo()$crs
# codigo.epsg <- gsub('EPSG:', 'EPSG_', epsg.i) ; codigo.epsg


# plot 

Map$setCenter(-72, -44.75, 6.3)

Map$addLayers( # cuando sea una sola imagen, cambia 'addLayers' por 'addLayer'
eeObject = img.variable,
  visParams = list(
  min = 250.0,
  max = 320.0,
  palette = c("#000080","#0000D9","#4000FF","#8000FF","#0080FF","#00FFFF","#00FF80","#80FF00","#DAFF00","#FFFF00","#FFF500","#FFDA00","#FFB000","#FFA400","#FF4F00","#FF2500","#FF0A00","#FF00FF")
  ),
  name = 'Imagen_del_mes'
)

# fin ---




# Guardado de imagenes ----
directorio <- "C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/raster_descargados_con_rgee/"

setwd(directorio)

list.files() # archivos del directorio de trabajo
file.remove(list.files()) # eliminando archivos
list.files()

# guardar en pc
ee_imagecollection_to_local(
  ic = img.variable,
  region = marco_trabajo,
  fileNamePrefix = nombre.salida,
  dsn = directorio,
)

# nombre.salida <- paste(id.img, nombre.variable, codigo.epsg, sep = '_') ; nombre.salida

# guardar en drive
# ee_as_raster(image = img.variable_ok, 
#              dsn = nombre.salida,
#              region = marco_trabajo, 
#              via = "drive")

# fin ---
