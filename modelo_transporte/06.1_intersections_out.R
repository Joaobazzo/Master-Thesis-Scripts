# ---
#
#
# intersection of shapefiles ####
#
#
# ---
rm(list=ls())
setwd("L:/# DIRUR #/ASMEQ/bosistas/joaobazzo/master-thesis-repo1/")
library(sf)
library(rgdal)
library(stringr)
library(mapview)
library(geobr)
library(dplyr)
library(data.table)
library(profvis) 
# -
# function
theta <- function(u3){
  p1 <- sf::st_coordinates(u3$geometry) %>% head(1) %>% as.data.table() %>% select(c("X","Y"))
  p2 <- sf::st_coordinates(u3$geometry) %>% tail(1) %>% as.data.table() %>% select(c("X","Y"))
  tang <- (p1$Y-p2$Y)/(p1$X-p2$X)
  return(tang)
}
# ---
# leitura
# --
u2 <- readr::read_rds("dados/Percursos/estendido/intersection-new_centroids_cap_adj.rds") %>% 
  sf::st_transform(4326) %>% sf::st_transform(31982)
u2$trips <- u2$trips * 0.45
muni <- geobr::read_municipality(4106902,tp="original") %>% sf::st_transform(31982) %>% sf::st_buffer(20)
u2$id_merge <- 1:nrow(u2)
# --
#
# 1) U3 MUNI CROSSES
#
# ---
# intersection with boundary
temp_u <- sf::st_crosses(u2$geometry,muni$geom)
temp_u1 <- sapply(1:length(temp_u),function(i){length(temp_u[[i]])})
temp_name_trips <- u2$name[which(temp_u1>0)]
u3f <- as.data.table(u2)
u3f <- u3f[name %in% temp_name_trips,][,.(name,trips,geometry)]
u3f <- u3f[,trips_cor := trips * (2.2 - 1)][,.(name,trips_cor,geometry)] %>% sf::st_as_sf() 
u3f$theta <- sapply(1:nrow(u3f),function(i){theta(u3f[i,])})
# --
#
# 3) 
#
# --
message("passou")
inter <- lapply(1:nrow(u3f),function(i){ # nrow(u3f) i =2 
  temp <- sf::st_intersection(u2,u3f[i,]) %>% 
    filter(n_overlaps>1) %>% 
    filter(as.character(sf::st_geometry_type(geometry)) %in% "LINESTRING") %>% 
    as.data.table()
  if(nrow(temp)>0){  return(temp)}
  message(i)
}) %>% data.table::rbindlist()
#
# depois eu vou substituir o inter, baseado no arquivo 'id_merge', dentro do u2
#
break()
u4 <- as.data.table(u2)
# cheque
(u4[inter$id_merge,]$trips/10^6) %>% sum()
(inter$trips/10^6) %>% sum()
(inter$trips_cor/10^6) %>% sum()
#
u4[inter$id_merge,]$trips <- inter$trips + inter$trips_cor
u5 <-  u4 %>% sf::st_as_sf() %>% sf::st_intersection(muni$geom)


readr::write_rds(u5,"dados/Percursos/estendido/intersection-new_centroids_cap_adjboard.rds")

(u2$trips/10^6) %>% sum()
(u4$trips/10^6) %>% sum()
merged2 <- merged1
merged2[inter$id_merge,]$trips %>% sum()
inter$trips %>% sum()
inter$id_merge




# ---
message("Theta formula")
# theta 
u3$theta <- sapply(1:nrow(u3),function(i){theta(u3[i,])})
# positive/negative angle
u3p <- u3[u3$theta>0,]
u3n <- u3[u3$theta<0,]
# --
# intersection operation
# --
system.time({
  inter <- lapply(list(u3n,u3p),function(i){
    aux_inter <- sf::st_intersection(i) 
    message("step 1")
    # yes intersection
    yes_inter <- aux_inter %>% 
      filter(n.overlaps>1) %>% 
      filter(as.character(sf::st_geometry_type(geometry)) %in% "MULTILINESTRING")
    message("step 1.1 yes")
    # no intersection
    no_inter <- aux_inter %>% 
      filter(n.overlaps==1) %>% 
      filter(as.character(st_geometry_type(geometry)) %in% "LINESTRING")
    message("step 1.2 no")
    # yes inter - fill trips
    yes_inter$trips <- sapply(1:nrow(yes_inter),function(j){
      ind <- yes_inter$origins[[j]]
      trips <- i$trips[ind] %>% sum()
      return(trips)
    })
    message("step 1.3 add trips")
    # bind (yes, no)
    output <- rbind(yes_inter,no_inter)
    message("step 1.4 output")
    return(output)
  }) %>% data.table::rbindlist() %>% sf::st_as_sf()
})
# --
#
# 2) U3 MUNI ADJUST
#
# ---
inter
# --
# export
# --
inter %>% names()
sf::write_sf(inter[1:15767,],"dados/Percursos/estendido/intersection_new-centroid.shp")
