
#--------------------------#
####    Script modelR   ####
#--------------------------#


#Instalando os pacotes, se necessário ----
packages = c("devtools", "leaflet", "dplyr", "raster", "dismo")
for (p in setdiff(packages, installed.packages()[, "Package"])) {
  install.packages(p, dependencies = T)
}

# Instalando pacotes ----
devtools::install_github("Model-R/modelr_pkg")

# Carregando pacotes
require(modleR)
require(raster)
require(dplyr)
require(leaflet)

#pontos de ocorrência ----
registros <- read.table("registros.csv", sep = ";", header = TRUE, stringsAsFactors = FALSE)
registros <- dismo::gbif(genus = "guapira",species = "laxa")
registros <- registros[,c("species","lon","lat")]
names(registros)[1] <- "sp"

#verificando as ocorrências ----
head(registros)
tail(registros)

#Mapa Sem agrupar os pontos ----
registros %>%
  na.omit() %>%
  leaflet() %>%
  addTiles() %>%
  addMarkers(lng = ~lon, lat = ~lat, popup = ~sp)

#Importando variáveis preditoras
lista <- list.files("./wc2.0_10m_bio", pattern = ".tif$", full.names = TRUE)
predictors <- stack(lista)
plot(predictors[[1]])

#máscara de corte do modelo
mascara
plot(mascara, axes = T, add = T)

especies <- unique(registros$sp)
especies

dir.create("teste")

for (especie in especies) {
  ocorrencias <- coordenadas[coordenadas$sp == especie, c("lon", "lat")]

  #setup data

  #gerando os modelos

  #gerando os ensembles por algoritmo

  #gerando o modelo final

  }


#carregando o modelo final
modelo = raster("./teste/Eugenia florida DC/ensemble/Eugenia florida DC._Final.mean.bin7_ensemble.tif")

#Plotando o modelo com o leaflet
coordenadas %>%
  leaflet() %>%
  addTiles(urlTemplate="http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}") %>%
  addRasterImage(modelo, colors = rev(terrain.colors(25)), opacity = 0.4) %>%
  addMarkers(lng = ~lon, lat = ~lat, popup = ~sp, clusterOptions = markerClusterOptions())
