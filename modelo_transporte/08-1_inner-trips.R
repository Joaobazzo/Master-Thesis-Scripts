# ----
#
# inner trip estimation
#
# ----
rm(list=ls())
library(readr)
library(sf)
library(data.table)
library(mapview)
library(sp)
library(dplyr)
library(geobr)
setwd("L:/# DIRUR #/ASMEQ/bosistas/joaobazzo/master-thesis-repo1/")
# ---
# ENTRADA
# ---
rmc <- c("full09","full08")
# matriz estatistica
muni1 <- sf::read_sf("dados/Pesquisa_OD_IPPUC/arquivos_saida/shp/estendida/amostras_por_zona.shp") %>% 
  as.data.table()
zona1 <- sf::read_sf("dados/Pesquisa_OD_IPPUC/arquivos_saida/shp/estendida/viagens_geradas_atraidas.shp") %>% 
  sf::st_transform(31982)
fileod1 <- data.table::fread("dados/Pesquisa_OD_IPPUC/arquivos_saida/csv/estendida/matrix_od_zonas.csv")
ods1 <- data.table::fread("dados/Pesquisa_OD_IPPUC/arquivos_saida/csv/estendida/deslocamentos_extrapolacao_tx_viagem_variavel.csv")
merged1 <- sf::read_sf("dados/Percursos/estendido/auto_new-centroid_merged.shp") %>% 
  sf::st_transform(31982)
# --
# loop
# --
for(i in rmc[2]){
  # i = rmc[1]
  grade_rmc <- readr::read_rds(paste0("dados/IBGE/rmc_pop_",i,".rds"))
  # grid09 <- readr::read_rds("dados/IBGE/cwb_full09.rds")
  # grid08 <- readr::read_rds("dados/IBGE/cwb_full08.rds")
  muni <- muni1[MUNICIPIO == "CURITIBA",ZONA] 
  # zona
  zona <- zona1[zona1$ZONA %in% muni,]
  # number of trips
  fileod <- fileod1[zone_org == zone_dst,][rel_auto > 0,][zone_org %in% muni,]
  # raw data (mean time)
  ods <- ods1[ZONA_DESTINO == ZONA_ORIGEM,][COD_MEIO %in% c(6,8),][ZONA_DESTINO %in% muni,]
  # merged files
  merged <- merged1[-which(merged1$speed_km_h %in% "Inf"),]
  # ---
  # PROCESSING
  # ---
  # ODS DATA
  ods <- ods[,c("mean_time","length_time") := list(mean(TEMPO),length(TEMPO)),
             by = .(ZONA_DESTINO,ZONA_ORIGEM)][,.SD[1],by = .(ZONA_DESTINO,ZONA_ORIGEM)]
  ods <- ods[,.(ZONA_DESTINO,ZONA_ORIGEM,total_viagem,mean_time,length_time)]
  # VERIFY MEAN TRAVELLED SPEED
  zona_cut <- zona[zona$ZONA %in% ods$ZONA_DESTINO,]
  zona_cut$speed <- NA
  zona_cut$speed <- sapply(1:nrow(zona_cut),function(i){ #
    # i = 1
    inter <- sf::st_intersection(merged,zona_cut$geometry[i])
    mts <- inter$speed_km_h %>% mean()
    return(mts)
  }) 
  # add 'mean_time','total_trips', into zona_cut
  zona_cut <- setDT(zona_cut)[,.(ZONA,speed,geometry)]
  zona_cut <- zona_cut[ods, on = c(ZONA = "ZONA_DESTINO")][,.(ZONA,total_viagem,speed,mean_time,geometry)]
  zona_cut <- zona_cut[, dist := speed * mean_time / 60] %>% sf::st_as_sf()
  zona_cut$area_total <- sf::st_area(zona_cut$geometry) %>% as.numeric()
  #readr::write_rds(zona_cut,"dados/Pesquisa_OD_IPPUC/arquivos_saida/RDS/estendido/inner_trips_by_centroid_08.rds")
  # --
  # intersection (grade_rmc x zona_cut1)
  # --
  temp_inter <- sf::st_intersection(zona_cut,grade_rmc)
  temp_inter$area_inter <- sf::st_area(temp_inter$geometry) %>% as.numeric()
  temp_inter <- setDT(temp_inter)[,area_relativ := area_inter / area_total] # (0 - 1)
  temp_inter <- temp_inter[,pop_adj := pop_hex * area_relativ]
  temp_inter <- temp_inter[,pop_rel := pop_adj / sum(pop_adj), by = ZONA]
  temp_inter <- temp_inter[,viagem_adj := total_viagem * pop_rel]
  temp_inter <- temp_inter[,.(ZONA,speed, mean_time, dist, id_hex, area_relativ, viagem_adj, geometry)] %>% 
    sf::st_as_sf()
  # --
  # salve
  # --
  #readr::write_rds(temp_inter,"dados/Pesquisa_OD_IPPUC/arquivos_saida/RDS/estendido/inner_trips_by_centroid_08.rds.")
  readr::write_rds(temp_inter,
                   paste0("dados/Pesquisa_OD_IPPUC/arquivos_saida/RDS/estendido/inner_trips_by_centroid_",i,".rds"))
  message(paste0("inner_trips_by_centroid_",i,".rds"))
  
}

