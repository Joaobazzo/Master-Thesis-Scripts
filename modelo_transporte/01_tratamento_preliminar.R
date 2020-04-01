require(sf)
require(sp)
require(openxlsx)
require(stringr)
require(xtable)
rm(list=ls()) 
#setwd("E:/Documents/CICLO/Mestrado/PROJETO/dados/Pesquisa_OD_IPPUC/")
setwd("L:/# DIRUR #/ASMEQ/bosistas/joaobazzo/master-thesis-repo1/dados/")
# ---
#
#
#
# leitura arquivos ####
#
#
#
# ---
file_v2 <- sf::read_sf("Pesquisa_OD_IPPUC/D536_012_BR/zoneamento_deslocamentos_v2.shp")
file_v2$pop_ibge <- c()
file_v2 <- sf::st_set_crs(file_v2,CRS("+init=epsg:31982"))
file_v2 <- sf::st_cast(x = file_v2,to = "MULTIPOLYGON")
# arquivo ibge setores censitarios
shp_ibge <- read_sf("IBGE/pr_setores_censitarios/41SEE250GC_SIR.shp")
shp_ibge <- st_transform(shp_ibge, CRS("+init=epsg:31982"))
# file tipo 1
file_v1 <- read_sf("Pesquisa_OD_IPPUC/D536_005_BR/zoneamento_deslocamentos.shp")
file_v1 <- st_transform(file_v1, CRS("+init=epsg:31982"))
# populacao 
setibge <- read.csv("IBGE/PR_20171016/PR/Base informaçoes setores2010 universo PR/CSV/Basico_PR.csv",sep=";")
setibge$Cod_setor <- as.character(setibge$Cod_setor)
# deslocamentos
arq <- openxlsx::read.xlsx("Pesquisa_OD_IPPUC/deslocamento_arq.xlsx")

arq$COD_PESSOA %>% unique() %>% length()
arq$ZONA_ORIGEM %>% unique() %>% length()
# xtable(x=head(arq[,6:9],2),type="latex",label = "tab:raw_pesquisaod",
#        caption = "Característica das entrevistas da pesquisa O/D.") %>% print()
# ---
#
#
#
# adiciona populacao nos arquivos de deslocamento ####
#
#
#
# ---
# metodo 1
file_v2$pop_ibge <- c()
for(i in 1:length(file_v1$geometry)){
  aux <- st_equals(file_v2$geometry,file_v1$geometry[i])
  aux <- as.numeric(aux);is.na(aux) <- 0; aux <- which(aux>0)
  file_v2$pop_ibge[aux] <-  file_v1$`POP_IBGE~1`[i]
}
# ---
# metodo 2
file_v2$pop_ibge1 <- c()
aux <- as.numeric(st_area(file_v1$geometry))
for(i in 1:length(file_v2$geometry)){
  aux1 <- as.numeric(st_area(file_v2$geometry[i]))
  ind <- which.min(abs(aux-aux1))
  file_v2$pop_ibge1[i] <- file_v1$`POP_IBGE~1`[ind]
}
# ---
# metodo 3
zone_ippuc <- unique(c(arq$ZONA_ORIGEM,arq$ZONA_DESTINO))
file_v2$pop_ibge2 <- -99    # populacao_metodo 3
for(i in 1:length(zone_ippuc)){
  aux_filev2 <- which(file_v2$ZONA%in%zone_ippuc[i])
  y <- file_v2[aux_filev2,"geometry"]
  # verifica arquivos internos
  aux <- as.numeric(st_within(shp_ibge$geometry,st_buffer(y$geometry,dist = 10)))
  is.na(aux) <- 0; aux <- which(aux>0)
  aux1 <- shp_ibge$CD_GEOCODI[aux]
  # populacao zone_ippuc[i]
  file_v2$pop_ibge2[aux_filev2] <- sum(setibge$V002[which(setibge$Cod_setor%in%aux1)])
}
# ---
# visualiza metodo 3
plot(y$geometry)
plot(shp_ibge$geometry[aux],add=T,col="blue")
# ---
# zonas com problemas (zcp) 
# ---
ind <- which(file_v2$pop_ibge2%in%c(0,-99))
file_v2$pop_ibge2[ind] <- file_v2$pop_ibge[ind]
file_v2$pop_ibge2[is.na(file_v2$pop_ibge2)] <- file_v2$pop_ibge[is.na(file_v2$pop_ibge2)]
# ---
#
#
#
# alocacao do bairro ####
#
#
#
# ---
# zonas com problemas nos bairros (zcp)
zcp <- file_v2$ZONA[which(is.na(file_v2$BAIRRO))]
for(i in 1:length(zcp)){
  ind_v2 <- which(file_v2$ZONA%in%zcp[i])
  y <- file_v2[ind_v2,"geometry"]
  # verifica arquivos internos
  aux <- as.numeric(st_within(shp_ibge$geometry,st_buffer(y$geometry,dist = 10)))
  is.na(aux) <- 0; aux <- which(aux>0)
  aux1 <- shp_ibge$CD_GEOCODI[aux]
  # nome bairro
  aux2 <- setibge$Nome_do_bairro[setibge$Cod_setor%in%aux1]
  aux3 <- as.character(unique(aux2))
  # bairro de acordo com a zona
  file_v2$BAIRRO[ind_v2] <- str_to_upper(aux3)
}
# ---
#
#
#
# salva arquivos ####
#
#
#
# ---
# exclui colunas inuteis
break()
ind <- -which(colnames(file_v2)%in%c("pop_ibge","pop_ibge1"))
file_v3 <- file_v2[,ind]
write_sf(file_v3,"arquivos_saida/shp/estendida/zoneamento_deslocamento_pop.shp")






























