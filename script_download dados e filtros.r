#####################################################################
#         ROTINA PARA BAIXAR DADOS DE OCORRÊNCIA DO GBIF E          # 
#         CONFERENCIA DE SINÔNIMOS NO SITE DO FLORA 2020            #
#####################################################################

#######################################################################
#                                                                     #
#         ROTINA PARA:                                                #
#         - BAIXAR DADOS DE OCORRÊNCIA DO GBIF;                       # 
#         - CONFERENCIA DE SINÔNIMOS NO SITE DO FLORA 2020            #
#         - VERIFICAR PONTOS FORA DO MUNICÍPIO DE COLETA;             #
#         - vERIFICAR SE AS COORDENADAS ESTÃO INVERTIDAS (lon e lat)  #
#                                                                     #
#                 versão 1.2.0                                        #
#######################################################################

#Elaborado por:
#Diogo S. B. Rocha (diogosbr@gmail.com) 
#26/02/2018


#instalando pacotes, se for necessário
packages = c("dismo", "raster", "maptools", "flora", "devtools")
for (p in setdiff(packages, installed.packages()[, "Package"])) { install.packages(p, dependencies = T)}

#lista de espécies para baixar os registros 
nome.sp = c("Tapirira guianensis", "Miconia mirabilis", "Guapira opposita") #exemplo com muiots registros 
nome.sp = c("Prepusa montana", "Manilkara maxima", "Ocotea felix",
            "Parinari littoralis", "Ipomoea franciscana")#com poucos registros

#gerando objeto para armazenar os registros
lista = c()

#loop para baixar os registros em um única tabela
for(i in nome.sp){
  #baixar dados de ocorrência do GBIF pelo pacote dismo
  species=dismo::gbif(i)
  
  #selecionando as colunas de interesse
  species.sel=species[,c("species","lon","lat", "municipality", "adm1")]
  
  #excluíndo os registros que não tem longitude e latitude
  species.sel=na.exclude(species.sel)
  
  #criando a lista
  lista = rbind(lista, species.sel)
}

#número de registros com coordenadas por espécie
table(lista$species)

#visualizando os 10 primeiro registros
head(lista,10)

#plotando os registros para visualização
data(wrld_simpl, package = "maptools")
lista1=lista
sp::coordinates(lista1)  =~lon+lat
raster::plot(lista1, col = as.factor(unique(lista$species)), pch = 19, cex = 1.2);raster::plot(wrld_simpl, add = T)
legend("topleft", unique(lista$species), col = as.factor(unique(lista$species)), pch = 19, title = "Espécies")

#conferindo no Flora 2020
ex.res=flora::get.taxa(unique(lista$species))
head(ex.res)


#############
## Filtros ##
#############

#instalando pacote 'spfilt' 
devtools::install_github("diogosbr/spfilt")

#carregando pacote
require(spfilt)

#Carregando a planilha com os registros
# Planilha de exeplo pode ser encontrada em https://github.com/diogosbr/download-occurrence-data/blob/master/exemplo.csv
exemplo = read.table("https://raw.githubusercontent.com/diogosbr/download-occurrence-data/master/exemplo.csv", h = T, sep = ";")

#verificando os nomes das colunas
names(exemplo)

#excluíndo os registros sem coordenadas
exemplo.sel = na.exclude(exemplo)

#visualizando os 6 primeiros registros 
head(exemplo.sel)

#indica:
# quais são os registros que as cooredenas estão fora do municipio informado;
# registros fora do Brasil;
# se lon e lat estão invertidas
exemplo.filt = filt(exemplo.sel)

#os 6 primeiros registros 
head(exemplo.filt)

#verificando status 
table(exemplo.filt$status)

#selecionando apenas os registros com staus Ok e as colunas de lon e lat
filtrados = exemplo.filt[exemplo.filt$status=="Ok",c('lon','lat')]

#plotando os registros para visualização
raster::plot(filtrados, col = "red", pch = 19)
data(wrld_simpl, package = "maptools")
raster::plot(wrld_simpl, add=T)