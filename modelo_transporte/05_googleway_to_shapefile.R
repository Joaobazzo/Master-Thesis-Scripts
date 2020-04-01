rm(list=ls())
library(googleway)
library(googlePolylines)
library(sf)
library(data.table)
library(dplyr)
library(mapview)
library(stringi)
library(sfheaders)
# ---
# read and filtering
setwd("L:/# DIRUR #/ASMEQ/bosistas/joaobazzo/master-thesis-repo")
# car trips
flw <- read.csv("dados/Pesquisa_OD_IPPUC/arquivos_saida/csv/estendida/matrix_od_zonas.csv")
flw <- setDT(flw)[,name:=paste0(zone_org,"_to_",zone_dst)]
flw <- flw[-which(rel_auto==0),]
# routes
route <- list.files("dados/Percursos/estendido/rds_new-centroids/",full.names = TRUE)
route_name <- list.files("dados/Percursos/estendido/rds_new-centroids/",full.names = FALSE)
route_name <- stringr::str_remove(route_name,".rds")
route_name <- route_name[route_name %in% flw$name] # filter
# ---
# main code - routes
break()
merged <- lapply(1:length(route_name),function(i){
  # i = 1
  #message(route[i])
  rt <- readRDS(route[i])
  tt <- rt$routes$legs[[1]]$steps[[1]]$duration$value
  ss <-  rt$routes$legs[[1]]$steps[[1]]$distance$value
  pts <- access_result(rt, result = "points")
  pts <- pts$routes$legs[[1]]$steps[[1]]$polyline$points
  # decode
  decoded <- lapply(googlePolylines::decode( pts ),
                    function(i){data.table::data.table("lon"=i$lon,"lat"=i$lat)})
  sfc <- sf::st_sfc( lapply( decoded, sfheaders::sfg_linestring ))
  sf <- sf::st_sf(geometry = sfc) 
  # traffic index t/to [-]
  tto <- (rt$routes$legs[[1]]$duration_in_traffic$value + rt$routes$legs[[1]]$duration$value) / rt$routes$legs[[1]]$duration$value
  if(length(tto) == 0){tto <- 1}
  # columns info
  sf$name <- route_name[i]
  sf$distance_m <- ss
  sf$duration_s <- tt
  sf$speed_km_h <- 3.6 * ss/tt
  sf$trips <- flw[name == route_name[i],"rel_auto"] %>% as.numeric()
  sf$tto <- tto

  return(sf)
}) %>% data.table::rbindlist() %>% sf::st_sf(crs=4326)

write_sf(merged,"dados/Percursos/estendido/auto_new-centroid_merged.shp")
#
# ---