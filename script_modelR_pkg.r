
#--------------------------#
####    Script modelR   ####
#--------------------------#


#Instalando os pacotes, se necessário ----
packages = c("devtools", "leaflet", "dplyr", "raster", "dismo")
for (p in setdiff(packages, installed.packages()[, "Package"])) {
  install.packages(p, dependencies = T)
}

# Instalando pacotes ----
devtools::install_github("Model-R/modelr_pkg", ref = "documentation")

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

result_folder <- "./teste"


for (i in 1:length(especies)) {
  ocorrencias <- registros[registros$sp == especies[i], c("lon", "lat")]

  #setup data
  sdmdata_1sp <- setup_sdmdata(species_name = especies[i],
    occurrences = ocorrencias,
    predictors = predictors,
    models_dir = result_folder,
    partition_type = "crossvalidation",
    cv_partitions = 4,
    cv_n = 1,
    seed = 512,
    buffer_type = "mean",
    plot_sdmdata = T,
    n_back = 500,
    clean_dupl = T,
    clean_uni = T,
    clean_nas = T,
    geo_filt = F,
    geo_filt_dist = 10,
    select_variables = T,
    percent = 0.5,
    cutoff = 0.7
  )

  #gerando os modelos
  do_many(species_name = especies[i],
    predictors = predictors,
    models_dir = result_folder,
    write_png = T,
    write_bin_cut = F,
    bioclim = T,
    domain = F,
    glm = T,
    svmk = T,
    svme = T,
    maxent = T,
    maxnet = T,
    rf = T,
    mahal = F,
    brt = T,
    equalize = T)

  #gerando os ensembles por algoritmo
  final_model(species_name = especies[i],
    algorithms = NULL, #if null it will take all the in-disk algorithms
    models_dir = result_folder,
    select_partitions = TRUE,
    select_par = "TSS",
    select_par_val = 0,
    which_models = c("raw_mean", "bin_consensus"),
    consensus_level = 0.5,
    uncertainty = T,
    overwrite = T)

  #gerando o modelo final
  ens <- ensemble_model(especies[i],
    occurrences = ocorrencias,
    which_final = "raw_mean",
    models_dir = result_folder,
    overwrite = TRUE) #argument from writeRaster
}

#explorando os resultados
ensemble_files <-  list.files(paste0(test_folder,"/", species[1],"/present/ensemble"),
  recursive = T,
  pattern = "raw_mean.+tif$",
  full.names = T)

ensemble_files
ens_mod <- stack(ensemble_files)
names(ens_mod) <- c("mean", "median", "range", "st.dev")
plot(ens_mod)


#carregando o modelo final
modelo = raster("./teste/Eugenia florida DC/ensemble/Eugenia florida DC._Final.mean.bin7_ensemble.tif")

#Plotando o modelo com o leaflet
coordenadas %>%
  leaflet() %>%
  addTiles(urlTemplate="http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}") %>%
  addRasterImage(modelo, colors = rev(terrain.colors(25)), opacity = 0.4) %>%
  addMarkers(lng = ~lon, lat = ~lat, popup = ~sp, clusterOptions = markerClusterOptions())
