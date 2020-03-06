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
theta <- function(u2){
  p1 <- sf::st_coordinates(u2$geometry) %>% head(1) %>% as.data.table() %>% select(c("X","Y"))
  p2 <- sf::st_coordinates(u2$geometry) %>% tail(1) %>% as.data.table() %>% select(c("X","Y"))
  tang <- (p1$Y-p2$Y)/(p1$X-p2$X)
  return(tang)
}
# ---
u2 <- sf::read_sf("dados/Percursos/estendido/auto_new-centroid_merged.shp") %>% sf::st_transform(31982)
muni <- geobr::read_municipality(4106902) %>% sf::st_transform(31982) %>% sf::st_buffer(20)
u2 <- sf::st_intersection(u2,muni)
# -------
message("Theta formula")
# theta 
u2$theta <- sapply(1:nrow(u2),function(i){theta(u2[i,])})
# positive/negative angle
u2p <- u2[u2$theta>0,]
u2n <- u2[u2$theta<0,]
# --
# intersection operation
# --
system.time({
  inter <- lapply(list(u2n,u2p),function(i){
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
# export
# --
inter %>% names()
sf::write_sf(inter[1:15767,],"dados/Percursos/estendido/intersection_new-centroid.shp")
