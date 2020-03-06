#
# grade estatistica e hexagonos
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
#
rmc <- c("full09","full08")
#
# grade
grade <- sf::read_sf("dados/IBGE/grade_id25/grade_rmc.shp") %>% 
  sf::st_transform(31982)
grade$grad_total <- sf::st_area(grade$geometry) %>% as.numeric()
#
for(i in rmc){
  # i = rmc[1]
  print(i)
  #
  # hexagonos
  #  grid09 <- readr::read_rds("dados/IBGE/cwb_full09.rds")
  hex <- readr::read_rds(paste0("dados/IBGE/cwb_",i,".rds")) %>% 
    sf::st_transform(4326) %>% st_transform(31982)
  hex$hex_total <- sf::st_area(hex$geometry) %>% as.numeric()
  # 
  # interseccao
  inter <-  sf::st_intersection(grade,hex)
  inter <- inter[which(
    sf::st_geometry_type(inter$geometry) %>% as.character() %in% 
      "POLYGON"),]
  listhex <- unique(inter$id_hex)
  teste <- lapply(listhex,function(j){
    temp <- inter[inter$id_hex %in% j,]
    #message(j)
    temp$grad_rel <- sf::st_area(temp$geometry) %>% as.numeric()
    temp$grad_prop <- temp$grad_rel / temp$grad_total
    temp$pop_rel <- temp$grad_prop * temp$POP
    temp$pop_hex <- sum(temp$pop_rel)
    return(temp)
  }) %>% data.table::rbindlist() %>% sf::st_as_sf()
  #
  # salva DT resumo
  pop_dt <- data.table::as.data.table(teste)[,.SD[1],by = id_hex][,.(id_hex,pop_hex)]
  # hexagono populacao
  hex1 <- as.data.table(hex)
  hex_pop <- as.data.table(pop_dt)[hex1,on = "id_hex"] %>% sf::st_as_sf()
  # escreve
  readr::write_rds(hex_pop,paste0("dados/IBGE/rmc_pop_",i,".rds"))
  
  message(paste0("dados/IBGE/rmc_pop_",i,".shp"))
}
