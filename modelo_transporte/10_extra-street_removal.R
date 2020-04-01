rm(list=ls())
setwd("L:/# DIRUR #/ASMEQ/bosistas/joaobazzo/master-thesis-repo1/")
library(sf)
library(readr)
library(geobr)
library(mapview)
# my model
rmc_mm <- sf::read_sf("dados/Percursos/estendido/intersection-new_centroids_cap_adj.shp")
rmc_mm <- rmc_mm %>% sf::st_transform(32722) %>% 
  sf::st_buffer(0.003) 
# osm
rmc_osm <- sf::read_sf("dados/OSM/rmc_shp/rmc_planet_osm_line_lines.shp") %>% 
  sf::st_transform(4326) %>% sf::st_transform(32722) 
osm_types <- c("trunk","motorway", "primary", "residential",          
               "secondary","tertiary", "unclassified","motorway_link",       
               "trunk_link","construction","primary_link",
               "tertiary_link","secondary_link","road")
osm_types <- "service"
rmc_osm1 <- rmc_osm[rmc_osm$highway %in% osm_types,] 
# 
rmc_bon <- geobr::read_municipality(4106902) %>% 
  sf::st_as_sf() %>%
  sf::st_transform(4326) %>% 
  sf::st_transform(32722) %>% 
  sf::st_buffer(0.003)
#
# first intersection
# 
rmc_mm <- sf::st_intersection(rmc_mm,rmc_bon$geometry)
rmc_osm1 <- sf::st_intersection(rmc_osm1,rmc_bon$geometry)
break()
#
# definetely intersection
#
rmc_osm1_buf <- sf::st_buffer(rmc_osm1$geometry,2)
mapview(rmc_osm1_buf)
rmc_mm_inter <- sf::st_intersection(rmc_mm$geometry,rmc_osm1_buf) %>% sf::st_as_sf()
#rmc_mm_inter <- rmc_mm_inter %>% unlist()
#rmc_mm_inter <- rmc_mm[rmc_mm_inter,]
rmc_mm_inter$length <- sf::st_cast(rmc_mm_inter,"LINESTRING") %>% sf::st_length() %>% as.numeric()
 
rmc_mm_inter[rmc_mm_inter$length > 50,]$length %>% sum()

rmc_mm_inter$length %>% sum()
mapview(rmc_mm_inter[rmc_mm_inter$length > 50,])
mapview(rmc_mm_inter) + mapview(rmc_osm1_buf)
#rmc_mm_inter$length <- 

mapview(rmc_mm_inter[1:100,])



rmc_mm_inter$geometry %>% sf::st_geometry_type() %>% unique()
rmc_mm_inter$geometry %>% sf::st_length() %>% as.numeric() %>% sum()

mapview(rmc_mm_inter$geometry)

mapview(rmc_osm1_buf) + mapview(rmc_mm_inter$geometry)

