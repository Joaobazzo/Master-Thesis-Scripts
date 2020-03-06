# 
# 
# CAPACITY FIELD ADDED TO INTERSECTION
#
# soma ideas for calibrating the results
# https://stats.stackexchange.com/questions/171154/fitting-known-equation-to-data/171191?newreg=00e24e0321294bc0aeefda68c3ff1fe3i
#
# -----------------------------------
rm(list=ls())
setwd("L:/# DIRUR #/ASMEQ/bosistas/joaobazzo/master-thesis-repo1/")
library(mapview)
library(sf)
library(geobr)
library(data.table)
library(future.apply)
merged <- sf::read_sf("dados/Percursos/estendido/intersection_new-centroid.shp")
merged <- sf::st_set_crs(merged,4326) %>% sf::st_transform(31982)
# intersection 
muni <- geobr::read_municipality(4106902) %>% sf::st_transform(31982) %>% sf::st_buffer(150)
merged <- sf::st_intersection(merged,muni)
mapview(merged$geometry) + mapview(muni$geom)
# -
# ippuc data
trips <- sf::read_sf("dados/Pesquisa_OD_IPPUC/PVT_VISUM/shapefile/Modelo OD_Pico Manha_20180530_link.SHP") %>% 
  sf::st_transform(31982) %>% sf::st_intersection(muni)
# --
# st_cast_merged
# --
merged1 <- lapply(1:nrow(merged),function(i){
  sf::st_cast(merged[i,],"LINESTRING")
}) %>% data.table::rbindlist() %>% sf::st_as_sf()
# ---
#
# adding capacity into merged data.table
#
# ---
merged1$cap <- 0
system.time({
  cap_street <- sapply(ind,function(i){ # 
    message(i)
    output <- merged1[i,"geometry"] %>% 
      sf::st_buffer(20) %>% 
      sf::st_intersection(trips)
    #if(nrow(output)!=0){
    output <- output[output$geometry %>%
                       sf::st_length() %>%
                       which.max(),"CAPPRT"]
    cap <- output$CAPPRT
    return(cap)
    # }
  })
})
aux1 <- cap_street 
gvec <- c()
for(i in 1:length(aux1)){
  #message(i)
  if(length(aux1[[i]]) == 0){
    aux1[[i]] <- NA}else{
    if(is.na(aux1[[i]])){
      aux1[[i]] <- NA}else{
      if(aux1[[i]] == 0 | aux1[[i]] == 9999){aux1[[i]] <- NA}}}
  gvec[i] <- aux1[[i]]
}
for(i in 1:length(ind)){
  aux[ind[i]] <- gvec[i]
}
aux2 <- c()
for(i in 1:length(aux)){
  aux2[i] <- aux[[i]]
}
aux2[which(aux2 == 0)] <- NA
aux3 <- c(aux2[1])
for(i in 2:length(aux2)){
  if(is.na(aux2[i])){aux3[i] <- aux3[i-1]}else{
    aux3[i] <- aux2[i]
  }
}

merged1$cap <- aux3
# merged1 <- readr::read_rds("dados/Percursos/estendido/intersection-new_centroids_cap.rds")
# merged1$trips <- merged1$trips * 2.655
readr::write_rds(merged1,"dados/Percursos/estendido/intersection-new_centroids_cap_adj.rds")
# sf::write_sf(merged,"dados/Percursos/estendido/intersection-new_centroids_cap.shp")
