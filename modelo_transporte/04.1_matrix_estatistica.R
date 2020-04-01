#
# geracao de centroides a partir da matrizes de estatisticas
#
#
# ---
rm(list=ls())
library(sf)
library(sp)
library(dplyr)
library(data.table)
library(geobr)
library(mapview)
library(readr)
setwd("L:/# DIRUR #/ASMEQ/bosistas/joaobazzo/master-thesis-repo1")
# zona censitaria
# zona <- sf::read_sf("dados/Pesquisa_OD_IPPUC/D536_005_BR/zoneamento_deslocamentos.shp")
zona <- sf::read_sf("dados/Pesquisa_OD_IPPUC/arquivos_saida/shp/estendida/viagens_geradas_atraidas.shp")
zona <- sf::st_set_crs(zona, CRS("+init=epsg:31982"))
# matriz de ponderacao
matrix <- sf::read_sf("dados/IBGE/grade_id25/grade_id25.shp")
matrix <-  sf::st_transform(matrix, 4326)
matrix <- sf::st_transform(matrix,31982)
# geobr
cod = c(4106902,4100400,4101804,4103107,4104006,4104204,4104253,
        4105805,4106209,4107652,4111258,4114302,4119152,4119509,
        4120804,4122206,4125506)
muni <- lapply(cod,function(i){geobr::read_municipality(i)}) %>% 
  data.table::rbindlist() %>%
  sf::st_as_sf() %>%
  sf::st_transform(4326) %>% 
  sf::st_transform(31982) %>% 
  sf::st_buffer(10)

muni <- sf::st_union(muni)
# first intersection
# zona_union <- sf::st_union(zona$geometry)
matrix <- sf::st_intersection(matrix,muni)
matrix <- matrix[which(
  sf::st_geometry_type(matrix$geometry) %>% as.character() %in% 
    "POLYGON"),]
# unique area
matrix$area_total <- sf::st_area(matrix$geometry) %>% as.numeric()

sf::write_sf(matrix,"dados/IBGE/grade_id25/grade_rmc.shp")
# intersection experiment
break()
temp <- lapply(1:nrow(zona),function(i){ 
  # i = 1
  temp <- sf::st_intersection(matrix,zona$geometry[i]) 
  temp <- temp[which(
    sf::st_geometry_type(temp$geometry) %>% as.character() %in% 
      "POLYGON"),]
  
  # rename
  temp$ID_UNICO <- paste0(temp$ID_UNICO,"_",i)
  
  # spatial
  temp$new_area <- sf::st_area(temp$geometry) %>% as.numeric()
  temp$area_prop <- temp$new_area / temp$area_total
  temp$new_pop <- temp$POP * temp$area_prop
  
  # temp
  temp$zona_ippuc <- zona$ZONA[i]
  return(temp)
}) %>% data.table::rbindlist() %>% sf::st_as_sf()
#
# adiciona novo centroide
# (passar pra DT)
# new centroid

temp <- setDT(temp)[,.(x = {
  aux <- sf::st_centroid(geometry) %>% 
    sf::st_coordinates() %>% data.table::as.data.table()
  xx <- weighted.mean(x = aux$X,w = new_pop)
  },
  y = {
  aux <- sf::st_centroid(geometry) %>% 
    sf::st_coordinates() %>% data.table::as.data.table()
  
  yy <- weighted.mean(x = aux$Y,w = new_pop)
  }),
  by = zona_ippuc]


data.table::fwrite(temp,"dados/Pesquisa_OD_IPPUC/arquivos_saida/csv/estendida/new_centroid.txt")


sys