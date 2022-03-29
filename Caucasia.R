# Cargar librerias
library(Makurhini)
library(sf)
library(raster)
library(tmap)
library(classInt)
library(RColorBrewer)

#Cargar Shapefile
getwd()
setwd("E:/R/Conectividad/datos")

#Lectura de datos vectoriales
caucasia <- st_read("Cob2018_CaucasiaWGS_Nat.shp")
class(caucasia)
names(caucasia)
summary(caucasia)

#Número de parches
nrow(caucasia)

#Visualizar parches
tmap_mode("plot")
tm_shape(caucasia) + tm_fill(col = "#31a354")+
  tm_borders(col = "black", lwd = 0.2)+ tm_style("cobalt")

#Fragmentación
fragmentacion <- MK_Fragmentation(patches = caucasia, edge_distance = 500, plot = TRUE)
class(fragmentacion)
names(fragmentacion)

#Visualizar la estadisticas
fragmentacion$`Summary landscape metrics (Viewer Panel)`
tabla <- fragmentacion$`Summary landscape metrics (Viewer Panel)`
tabla <- as.data.frame(tabla)
head(tabla)

# Estadisticas de fragmentacion a nivel de parche
class(fragmentacion$`Patch statistics shapefile`)
names(fragmentacion$`Patch statistics shapefile`)

# Estadaisticas, el porcentaje de Area nucleo en los parches (CAPercent) y el Indice de dimension fractal (FRAC):
Parches_fragmentacion <- fragmentacion$`Patch statistics shapefile`

CA <- tmap::tm_shape(Parches_fragmentacion) +
  tmap::tm_fill("CAPercent", palette = brewer.pal(9, "RdYlGn"),
                breaks = classIntervals(Parches_fragmentacion$CAPercent, 9, "jenks")[[2]])+
  tmap::tm_style("cobalt")+ tmap::tm_layout(legend.width = 0.4, legend.height = 0.4)

FRAC <- tmap::tm_shape(Parches_fragmentacion) +
  tmap::tm_fill("FRAC", palette = brewer.pal(9, "RdYlGn"),
                breaks = classIntervals(Parches_fragmentacion$FRAC, 9, "jenks")[[2]])+
  tmap::tm_style("cobalt")+ tmap::tm_layout(legend.width = 0.4, legend.height = 0.4)

tmap::tmap_arrange(CA, FRAC)

#Umbrales de dispersion de 10 y 30 km
dispersion <- c(10000, 30000)

# Argumentos para estimar la distancia entre parches,
# para ver todos los argumentos ver distancefile
distancia <- list(type = "centroid")

# Se debe usar el argumento probability porque algunos indices son probabilisticos,
# el valor debe ser de 0 a 1, en donde 1 es un 100% de probabilidad de conectar
# bajo el umbral de dispersion (distance_thresholds)
probabilidad <- 0.5

# Aplicamos la funcion
centrality <- MK_RMCentrality(nodes = caucasia, distance = distancia, distance_thresholds = dispersion, probability = probabilidad, intern = FALSE) # intern = TRUE,
# muestra una barra de progreso
class(centrality)
names(centrality)

# visualizamos el shapefile basado en un umbral de dispersion de 30 km
tmap::tmap_mode("plot")
c <- 9
degree <- tmap::tm_shape(centrality$d30000) +
  tmap::tm_fill("degree", palette = RColorBrewer::brewer.pal(c, "RdYlGn"),
                breaks = classIntervals(centrality$d30000[["degree"]], c, "jenks")[[2]])+
  tmap::tm_style("cobalt")+ tmap::tm_layout(legend.width = 0.4, legend.height = 0.4)
close <- tmap::tm_shape(centrality$d30000) +
  tmap::tm_fill("close", palette = RColorBrewer::brewer.pal(c, "RdYlGn"),
                breaks = classIntervals(centrality$d30000[["close"]], c, "jenks")[[2]])+
  tmap::tm_style("cobalt")+ tmap::tm_layout(legend.width = 0.4, legend.height = 0.4)
BWC <- tm_shape(centrality$d30000) +
  tmap::tm_fill("BWC", palette = RColorBrewer::brewer.pal(c, "RdYlGn"),
                breaks = classIntervals(centrality$d30000[["BWC"]], c, "jenks")[[2]])+
  tmap::tm_style("cobalt")+ tmap::tm_layout(legend.width = 0.4, legend.height = 0.4)
modules <- tmap::tm_shape(centrality$d30000) +
  tmap::tm_fill("modules", palette = RColorBrewer::brewer.pal(c, "RdYlGn"),
                breaks = classIntervals(centrality$d30000[["modules"]], c, "jenks")[[2]])+
  tmap::tm_style("cobalt")+ tmap::tm_layout(legend.width = 0.4, legend.height = 0.4)

tmap::tmap_arrange(degree, close)
tmap::tmap_arrange(BWC, modules)

# IIC
# No se usa el argumento probability porque es un indice binario
# De nuevo usamos 10 y 30 km como umbrales de dispersion
IIC <- MK_dPCIIC(nodes = caucasia, attribute = NULL, area_unit = "m2", distance = list(type = "centroid"), metric = "IIC", distance_thresholds = c(10000, 30000), intern = FALSE)#intern=TRUE muestra una barra de progreso
class(IIC)
names(IIC)

#visualizacion del indice y fracciones del resultado usando un umbral de dispersion de 30 km
tmap_mode("plot")
c <-9
dIIC <- tm_shape(IIC$d30000) +
  tm_fill("dIIC", palette = RColorBrewer::brewer.pal(c, "RdYlGn"),
          breaks = classIntervals(IIC$d30000[["dIIC"]], c, "jenks")[[2]])+
  tm_style("cobalt")+ tm_layout(legend.width = 0.4, legend.height = 0.4)
dIICintra <- tm_shape(IIC$d30000) +
  tm_fill("dIICintra", palette = RColorBrewer::brewer.pal(c, "RdYlGn"),
          breaks = classIntervals(IIC$d30000[["dIICintra"]], c, "jenks")[[2]])+
  tm_style("cobalt")+ tm_layout(legend.width = 0.4, legend.height = 0.4)
dIICflux <- tm_shape(IIC$d30000) +
  tm_fill("dIICflux", palette = RColorBrewer::brewer.pal(c, "RdYlGn"),
          breaks = classIntervals(IIC$d30000[["dIICflux"]], c, "jenks")[[2]])+
  tm_style("cobalt")+ tm_layout(legend.width = 0.4, legend.height = 0.4)
dIICconect <- tm_shape(IIC$d30000) +
  tm_fill("dIICconnector", palette = RColorBrewer::brewer.pal(c, "RdYlGn"),
          breaks = classIntervals(IIC$d30000[["dIICconnector"]], c, "jenks")[[2]])+
  tm_style("cobalt")+ tm_layout(legend.width = 0.4, legend.height = 0.4)
tmap_arrange(dIIC, dIICintra)
tmap_arrange(dIICflux, dIICconect)

# Cargar archivo raster
CaucaciaR <- raster("E:/R/Conectividad/datos/CaucaciaR.tif")

# Probabilidad de dispersion bajo un umbral de distancia de 30 km
PC <- MK_dPCIIC(nodes = CaucaciaR, attribute = NULL, distance = list(type = "centroid"), metric = "PC", probability = 0.5, overall = TRUE, distance_thresholds = 5000, intern = FALSE)
class(PC) # se obtiene una lista
summary(PC) # que contiene 5 rasters y 1 tabla
plot(PC$node_importances_d5000, col = brewer.pal(11, "PiYG"), colNA="#AAB5BF")
PC$overall_d5000


