#
# Matriz OD por zona
#
# gera dados de origem e destino, por zona
#
# ---
#
#
# leitura de arquivos ####
#
#
# ---
rm(list=ls())
require(sf)
setwd("E:/Documents/CICLO/Mestrado/PROJETO/dados/Pesquisa_OD_IPPUC/")
filenew <- read.csv("arquivos_saida/csv/estendida/deslocamentos_extrapolacao_tx_viagem_variavel.csv")
dat <- read_sf("arquivos_saida/shp/estendida/amostras_por_zona.shp")
#dat <- dat[-which(dat$amostra%in%0),]
# ---
#
# pre-processing ####
#
# ---
# data.frame OD (dfod)
nomes_dfod <- c("zone_org",
                "zone_dst",
                "num_trips",
                "num_people",
                "total",
                "rel_auto",
                "rel_bus",
                "rel_moto",
                "rel_outros")
dfod <- data.frame(matrix(NA,0,length(nomes_dfod)))
colnames(dfod) <- nomes_dfod
# ---
#
# ODS matrix ####
#
# ---
k <- 1 # indice de contagem
for(i in (1:length(dat$ZONA))){
  # arquivo de trabalho - origem [i]
  #
  # add
  filecod <- filenew[which(filenew$ZONA_ORIGEM%in%dat$ZONA[i]),]
  # todos os destinos da origem [i]
  num_dest <- unique(filecod$ZONA_DESTINO)
  # loop no destino [j]
  for(j in (1:length(num_dest))){
    # arquivo de trabalho - origem [i] %in% destino[j]
    filecod1 <- filecod[which(filecod$ZONA_DESTINO==num_dest[j]),]
    # aumenta linhas 'dfod'
    dfod[k,] <- NA
    # numero viagens | numero de pessoas |  zona origem | zona destino 
    dfod$num_trips[k] <- dim(filecod1)[1]
    dfod$num_people[k] <- length(unique(filecod1$COD_PESSOA))
    dfod$zone_org[k] <- dat$ZONA[i] 
    dfod$zone_dst[k] <- num_dest[j]
    # total viagens
    aux_tv <- as.numeric(as.character(factor(filecod1$total_viagem)))
    dfod$total[k] <- sum(aux_tv)
    # viagens relativas auto | bus | moto | outros
    dfod$rel_auto[k] <- sum(aux_tv[which(filecod1$COD_MEIO%in%c(6,8))]) 
    dfod$rel_bus[k] <- sum(aux_tv[which(filecod1$COD_MEIO%in%c(1,3,5,4,2,9,10,11))])
    dfod$rel_moto[k] <- sum(aux_tv[which(filecod1$COD_MEIO%in%c(12))])
    dfod$rel_outros[k] <- sum(aux_tv[which(filecod1$COD_MEIO%in%c(7,13:15,0))])
    # atualiza indice contador
    k=k+1
  }
}
# exporta
#
# neste caso, serão exportados as viagens geradas (independente da populacao residente).
# ou seja, as viagens geradas e atraidas (arquivo da matrix_od) serão iguais ou bem parecidas
# ---
write.csv(dfod,"arquivos_saida/csv/estendida/matrix_od_zonas.csv")
