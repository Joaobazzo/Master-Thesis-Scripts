#
# geracao de percursos entre centroides
#
#
# ---
require(googleway)
require(stringr)
require(data.table)
require(sf)
require(rgdal)
# ---
#
# leitura arquivos ####
#
# ---
rm(list=ls())
setwd("L:/# DIRUR #/ASMEQ/bosistas/joaobazzo/master-thesis-repo")
fileod <- read.csv("dados/Pesquisa_OD_IPPUC/arquivos_saida/csv/estendida/matrix_od_zonas.csv")
dat <- data.table::fread("dados/Pesquisa_OD_IPPUC/arquivos_saida/csv/estendida/new_centroid.txt")
MY_API <- data.table::fread("dados/api.txt",header = F)
MY_API <- MY_API$V1
source("modelo_deslocamentos/v_estendida/09_LatLongUTM.R")
# ---
#
#
# relaciona percursos ####
#
#
# ---
# delete situation of inner zones trip
# ---
fileod <- setDT(fileod)
fileod <- fileod[,test:=zone_org-zone_dst][-which(test==0),][,-c("test")]

# add centrod of origin and destination zone
# ---
# dat$X <- sf::st_coordinates(sf::st_centroid(dat$geometry))[,1]
# dat$Y <- sf::st_coordinates(sf::st_centroid(dat$geometry))[,2]
dat[,c("x","y")] <- LongLatToUTM(x = dat$x,y = dat$y,
                  epsg_previous = "+init=epsg:31982",
                  epsg_current = "+init=epsg:4326")
# add coordinates
# ---
#dat1 <- setDT(dat[,c("ZONA","X","Y")])
fileod <- fileod[dat,on="zone_org==zona_ippuc"]
setnames(fileod, "x", "x_org")
setnames(fileod, "y", "y_org")
fileod <- fileod[dat,on="zone_dst==zona_ippuc"]
setnames(fileod, "x", "x_dst")
setnames(fileod, "y", "y_dst")
# remove no cars trips
# --
fileod <- fileod[-which(rel_auto<0.1),]
break()
# ---
#
#
# mapsapi ####
#
#
# ---
for(i in (844:nrow(fileod))){ # nrow(fileod)
  # i = 840 
  # viagem
  vgm <- paste0(fileod$zone_org[i],"_to_",fileod$zone_dst[i])
  # origem e destinos
  origin <-c(as.numeric(as.character(fileod$y_org[i])),
                as.numeric(as.character(fileod$x_org[i])))
  destin <- c(as.numeric(as.character(fileod$y_dst[i])),
                 as.numeric(as.character(fileod$x_dst[i])))
  # busca API
  rt = googleway::google_directions(origin = origin,
                      destination = destin,
                      mode = "driving",
                      units = "metric",
                      simplify = TRUE,
                      key = MY_API,
                      departure_time = as.POSIXct("2020-01-24 07:30:00 -03"),
                      traffic_model = "best_guess")
  # sleep
  Sys.sleep(3)
  # condicional
  if(rt$status=="OK"){
    #  save mp_dir
    filepath <- paste0("dados/Percursos/estendido/rds_new-centroids/",vgm,".rds")
    saveRDS(rt,filepath)
    # conferencia
    #print(paste0(i,"_",vgm))
  }

}
      


