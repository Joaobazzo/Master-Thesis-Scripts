#
#
# arquivo deslocarmentos_arq
#
# adiciona ao arquivo "deslocamento_arq.xlsx" as seguintes colunas
#
# 1) bairro de residencia
# 2) Populacao (IBGE) do bairro de residencia
# 3) Representativade (em %) do total de viagens por zona
# 4) Extrapolação 
# 5) Considera 33% de pessoas imoveis, 2.476 a taxa de viagens por pessoa
#
# ---
#
#
# leitura de dados ####
#
#
# ---
require(openxlsx)
require(sf)
library(samplesize4surveys)
#
#
rm(list=ls())
#setwd("E:/Documents/CICLO/Mestrado/PROJETO/dados/Pesquisa_OD_IPPUC/")
setwd("L:/# DIRUR #/ASMEQ/bosistas/joaobazzo/master-thesis-repo1/dados/")
filenew <- openxlsx::read.xlsx("Pesquisa_OD_IPPUC/deslocamento_arq.xlsx",sheet = 1)
# bairros
# bairros  <- read_sf("D536_012_BR/zoneamento_deslocamentos_v2.shp")
# shp_zonas
dat <- sf::read_sf("Pesquisa_OD_IPPUC/arquivos_saida/shp/estendida/zoneamento_deslocamento_pop.shp")
#dat <- data.frame("codigo_ippuc"=shp_zonas$ZONA,"pop"=shp_zonas$pop_ibge,
#                  "x"=shp_zonas$utm_x,"y"=shp_zonas$utm_y,"geometry"=shp_zonas$geometry)
# ---
#
#
# verificando a zona de residencia de cada pessoa ####
#
#
# ---
fileres_o <- filenew[which(filenew$TIPO_ORIGEM=="Residência"),]
fileres_d <- filenew[which(filenew$TIPO_DESTINO=="Residência"),]
ppl <- unique(filenew$COD_PESSOA)
zone_res <- c()
for(i in (1:length(ppl))){
  aux <- fileres_o$ZONA_ORIGEM[which(fileres_o$COD_PESSOA==ppl[i])]
  if(is.na(aux[1])==T){
    aux <- fileres_d$ZONA_DESTINO[which(fileres_d$COD_PESSOA==ppl[i])]
    if(is.na(aux[1])==T){aux <- -9999}
  }
  zone_res[i] <- aux[1]
}
zone_res[which(zone_res==-9999)] <- "SEM INFO"
z_res_ppl <- data.frame("ppl"=ppl,"zone_res"=zone_res)
# ---
#
#
# adiciona a zona de residencia/pop no arquivo filenew ####
#
#
# ---
filenew$zona_res <- 0
filenew$pop_zona_res <- 0
for(i in (1:dim(filenew)[1])){
  # zona de residencia
  filenew$zona_res[i] <- as.character(z_res_ppl$zone_res[which(z_res_ppl$ppl==filenew$COD_PESSOA[i])])
  # pop
  aux1 <- which(dat$ZONA%in%filenew$zona_res[i])
  if(length(aux1)==0){
    filenew$pop_zona_res[i] <- "SEM INFO"
  }else{
    filenew$pop_zona_res[i] <- dat$pop_ibge2[aux1]
  }
}
# remove zonas de residencia "SEM INFO"
filenew <- filenew[-which(filenew$pop_zona_res%in%"SEM INFO"),]
#break()
# ---
#
#
# adiciona bairro de residencia ####
#
#
# ---
filenew$bairro_res <- 0
#filenew$bairro_res1 <- 0
for(i in 1:length(dat$ZONA)){
  ind <- which(filenew$zona_res%in%dat$ZONA[i])
  filenew$bairro_res[ind] <- dat$BAIRRO[i]
}
# ---
#
#
# metricas bairro ####
# 
# viagens por pessoa
# ---
met_bairro <- data.frame("bairro"=unique(dat$BAIRRO),"taxa"=0)
for(i in (1:length(met_bairro$bairro))){
  # indices
  ind <- which(filenew$bairro_res%in%as.character(met_bairro$bairro[i]))
  aux <- filenew[ind,"COD_PESSOA"]
  # taxa viagens
  met_bairro$taxa[i] <- length(aux)/length(unique(aux)) # trips/people
}
# ---
#
#
# verificacao de amostras insuficientes ####
#
#
# ---
#break()
dat$erro <- NA
#dat$bairro <- NA
for(i in (1:length(dat$ZONA))){
  aux <- filenew[which(filenew$zona_res%in%dat$ZONA[i]),]
  num_people <- length(unique(aux$COD_PESSOA))
  num_trips <- dim(aux)[1]
  # checa tamanho da amostra
  aux1 <- samplesize4surveys::e4p(N = 0.66 * dat$pop_ibge2[i], # populacao movel
             n = num_people,     # tamanho da amostra
             P = 0.5,DEFF = 1,
             conf = 0.90)$Margin_of_error
  if(aux1%in%c(NaN,"Inf")){dat$erro[i] <- 99}else{dat$erro[i] <- aux1}
}
#break()
# ---
#
#
# verificacao da taxa de viagem por zona #### 09/07/2019
#
#
# ---
tx_trip <- c()
dat$amostra <- 99
dat$pessoas <- 99
for(i in (1:length(dat$ZONA))){
  aux <- filenew[which(filenew$zona_res%in%dat$ZONA[i]),]
  dat$pessoas[i] <- length(unique(aux$COD_PESSOA))
  dat$amostra[i] <- dim(aux)[1]
  # checa tamanho da amostra
  # acumula media se erro > 30
  if(dat$erro[i] > 30 && dat$erro[i] < 99){
    tx_trip[i] <- met_bairro$taxa[which(met_bairro$bairro%in%unique(aux$bairro_res))]
  }
  else{
    tx_trip[i] <- dat$amostra[i]/dat$pessoas[i]
  }
}
# taxa de viagem para situacoes de zero amostras
tx_trip[tx_trip%in%NaN] <- 0
# ---
#
#
# percentual relativo de viagens conforme zona de residencia ####| 
# total de viagens (extrapolaçao)
#
#
# ---
p_inativos <- 0.33 # percentual de pessoas imoveis
filenew$perc_viagens <- "SEM INFO"
filenew$total_viagem <- "SEM INFO"
for(i in (1:length(dat$ZONA))){
  aux2 <- which(filenew$zona_res%in%as.character(dat$ZONA[i]))
  filenew$perc_viagens[aux2] <- 1/length(aux2)
  # extrapolacao (% viagens x Tx de viagem x % pessoas móveis x Pop da zona)
  filenew$total_viagem[aux2] <- as.numeric(filenew$perc_viagens[aux2])*tx_trip[i]*(1-p_inativos)*as.numeric(filenew$pop_zona_res[aux2])
}
# ---
#
#
# exportacao ####
#
#
# ---
write.csv(filenew,"arquivos_saida/csv/estendida/deslocamentos_extrapolacao_tx_viagem_variavel.csv")
# exporta dat
write_sf(obj = dat,
         dsn = "arquivos_saida/shp/estendida/amostras_por_zona.shp",
         driver="ESRI Shapefile")
#
