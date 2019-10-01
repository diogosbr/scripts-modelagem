#############################
####    Script exemplo   ####
#############################

# OBSOLETO

#Instalando os pacotes, se necessário
packages = c("leaflet", "dplyr", "raster", "dismo")
for (p in setdiff(packages, installed.packages()[, "Package"])) {
  install.packages(p, dependencies = T)
}


# MaxEnt .jar#### baixa e descompacta o maxent java
jar <- paste0(system.file(package = "dismo"), "/java/maxent.jar")
if (file.exists(jar) != T) {
  url = "http://biodiversityinformatics.amnh.org/open_source/maxent/maxent.php?op=download"
  download.file(url, dest = "maxent.zip", mode = "wb")
  unzip("maxent.zip",
        files = "maxent.jar",
        exdir = system.file("java", package = "dismo"))
  unlink("maxent.zip")
}


require(dismo)

#modelagem#

#lendo dados de uma espécie (exemplo)
data(acaule, package = "dismo")
pts1 = na.exclude(acaule[, c("lon", "lat")])

#lendo predires (exemplos)
fnames <-
  list.files(
    path = paste(system.file(package = "dismo"), '/ex', sep = ''),
    pattern = 'grd',
    full.names = TRUE
  )
predictors <- raster::stack(fnames[-9])
backg <- randomPoints(predictors, n = 1000, extf = 1.25)

#número de partições
k = 3

#partições
group.p <- kfold(pts1, k)
group.a <- kfold(backg, k)


pres_train <- pts1[group.p != 1, ]
pres_test <- pts1[group.p == 1, ]
backg_train <- backg[group.a != 1, ]
backg_test <- backg[group.a == 1, ]
bc <- bioclim(predictors, pres_train)
e = evaluate(pres_test, backg_test, bc, predictors)
tr = e@t[which.max(e@TPR + e@TNR)]
TSS.calc = max(e@TPR + e@TNR) - 1

#Tabela de avaliação de desmpenho do modelo
aval = cbind(threshold(e), "Bioclim", e@auc, max(e@TPR + e@TNR) - 1, tr)

#projetando o modelo
bc.mod = predict(predictors, bc)

#salvar o raster em formato GeoTiff
writeRaster(
  bc.mod,
  paste0("./modelos/", "bc_", i, "_con.tif"),
  format = "GTiff",
  overwrite = T
)
