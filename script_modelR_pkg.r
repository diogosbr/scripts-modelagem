
#############################
####    Script modelR   ####
############################


#Instalando os pacotes, se necessário
packages = c("devtools", "leaflet", "dplyr", "raster", "dismo")
for (p in setdiff(packages, installed.packages()[, "Package"])) {
  install.packages(p, dependencies = T)
}

# Instalando pacotes 
devtools::install_github("Model-R/modelr_pkg")

# Carregando pacotes
require(modelr)
require(raster)
require(dplyr)
require(leaflet)

#pontos de ocorrência
?coordenadas
coordenadas

#Mapa Sem agrupar os pontos
coordenadas %>% 
  leaflet() %>% 
  addTiles() %>% 
  addMarkers(lng = ~lon, lat = ~lat, popup = ~sp)

#variáveis ambientais
variaveis_preditoras
plot(variaveis_preditoras[[1]])

#máscara de corte do modelo
mascara
plot(mascara, axes=T, add = T)

especies <- unique(coordenadas$sp)

especies

dir.create("teste")

for (especie in especies) {
  ocorrencias <- coordenadas[coordenadas$sp == especie, c("lon", "lat")]
  
  #gerando os modelos
  do_bioclim(especie, coordinates = ocorrencias, partitions = 3, buffer = FALSE, seed = 512, predictors = variaveis_preditoras, models.dir = "./teste", project.model = F, mask = mascara, n.back = 500, write_png = T)
  do_maxent(especie, coordinates = ocorrencias, partitions = 3, buffer = FALSE, seed = 512, predictors = variaveis_preditoras, models.dir = "./teste", project.model = F, mask = mascara, n.back = 500, write_png = T)
  do_randomForest(especie, coordinates = ocorrencias, partitions = 3, buffer = FALSE, seed = 512, predictors = variaveis_preditoras, models.dir = "./teste", project.model = F, mask = mascara, n.back = 500, write_png = T)
  do_GLM(especie, coordinates = ocorrencias, partitions = 3, buffer = FALSE, seed = 512, predictors = variaveis_preditoras, models.dir = "./teste", project.model = F, mask = mascara, n.back = 500, write_png = T)
  do_SVM(especie, coordinates = ocorrencias, partitions = 3, buffer = FALSE, seed = 512, predictors = variaveis_preditoras, models.dir = "./teste", project.model = F, mask = mascara, n.back = 500, write_png = T)
  do_SVM2(especie, coordinates = ocorrencias, partitions = 3, buffer = FALSE, seed = 512, predictors = variaveis_preditoras, models.dir = "./teste", project.model = F, mask = mascara, n.back = 500, write_png = T)
  do_domain(especie, coordinates = ocorrencias, partitions = 3, buffer = FALSE, seed = 512, predictors = variaveis_preditoras, models.dir = "./teste", project.model = F, mask = mascara, n.back = 500, write_png = T)
  do_mahal(especie, coordinates = ocorrencias, partitions = 3, buffer = FALSE, seed = 512, predictors = variaveis_preditoras, models.dir = "./teste", project.model = F, mask = mascara, n.back = 500, write_png = T)
  
  #gerando os ensembles por algoritmo
  finalModel(especie, algoritmos=c("BioClim", "maxent", "glm", "rf", "svm","svm2", "Domain", "Mahal"), TSS.value = 0.7, models.dir="./teste")
  
  #gerando o modelo final
  ensemble(especie, models.dir="./teste", occs=coordenadas)
  
  #Criando a tabela com os valores de desempenho do modelo. 
  
  lista = list.files(paste0('./teste/', especie), pattern = ".txt", full.names = T)
  aval = c()
  for(i in 1:(length(lista)-1)){
    a = read.table(lista[i])
    aval = rbind(aval,a)
    row.names(aval) = NULL
    print(head(aval,10))
    write.table(aval, paste0("./teste/", especie, ".csv"), row.names = F, sep = ";")
  }
}


#carregando o modelo final
modelo = raster("./teste/Eugenia florida DC/ensemble/Eugenia florida DC._Final.mean.bin7_ensemble.tif")

#Plotando o modelo com o leaflet
coordenadas %>% 
  leaflet() %>% 
  addTiles(urlTemplate="http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}") %>% 
  addRasterImage(modelo, colors = rev(terrain.colors(25)), opacity = 0.4) %>%
  addMarkers(lng = ~lon, lat = ~lat, popup = ~sp, clusterOptions = markerClusterOptions())
