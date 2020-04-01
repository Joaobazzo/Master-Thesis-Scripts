# -------------------------------------
#
# temp calibration
#
# -------------------------------------
rm(list=ls())
library(vein)
library(sf)
library(sp)
library(ggplot2)
library(data.table)
library(kohonen)
library("viridis")   
library(units)
library(dplyr)
setwd("L:/# DIRUR #/ASMEQ/bosistas/joaobazzo/master-thesis-repo1/")
# importa dados
trips <- readr::read_rds("dados/Percursos/estendido/intersection-new_centroids_cap_adjboard.rds") %>% 
  sf::st_set_crs(31982) %>% sf::st_transform(4326)
#trips <- readr::read_rds("dados/Percursos/estendido/intersection-new_centroids_cap_adj.rds") %>% 
#  sf::st_set_crs(31982) %>% sf::st_transform(4326)
pc <- data.table::fread("dados/Pesquisa_OD_IPPUC/arquivos_saida/csv/estendida/fluxo_horario_deslocamentos.csv")
pc <- pc[,rel := freq/sum(freq)][,rel]
# ---
# ajuste para funcao BPR
counts <- sf::read_sf("dados/Pesquisa_OD_IPPUC/D536_015_BR/screen_line.shp") %>%
  data.table::data.table()
#counts[,razao := N_Autos_/Volume_Dia][,.(razao)] %>% as.vector() %>% summary()
#counts[,.(N_Autos_,Volume_Dia)] %>% summary()
counts <- counts[,V_dia := N_Autos_ + N_Taxis_ + N_Vans_]
adj <- (1 / counts[,V_dia/Volume_Dia]) %>%  quantile(.55) %>% as.numeric()
# --
#
# net
#
# --
data1 <- trips %>% data.table::as.data.table()
data1$lkm <- trips %>% 
  sf::st_length() %>% 
  units::set_units("m") %>% 
  units::set_units("km") 

data <- data1[1:nrow(data1),]
net <- lapply(1:nrow(data), function(i){  # nrow(data)
  # i =1
  #message(55000 + i)
  aux <- trips$geometry[[i]] %>% 
    sf::st_coordinates() %>%
    as.data.frame() %>%
    dplyr::select("X","Y") %>% 
    list() %>% 
    sp::Line() %>%
    sp::Lines(ID=i) 
  aux
  return(aux)  
}) %>% sp::SpatialLines() %>% 
  sp::SpatialLinesDataFrame(data= data) 
# --
# speed [53817 x 24] ####
# --
FATOR = 1.2
pcm <- as.matrix((FATOR * adj) *  net@data$trips) %*% t( as.matrix(pc) )
net@data$trips <- net@data$trips * FATOR
# pcm[1,] %>% sum()
# net@data$trips[1] * adj
# speed index
vel <- as.data.frame(matrix(0,nrow = nrow(trips),ncol = ncol(pcm)))
alpha <- 0.15; beta <- 4
vel <- lapply(1:nrow(trips), function(i){ # nrow(trips)
  # i = 1
  net@data$speed[i]/(1 + alpha*(pcm[i,]/net@data$cap[i])^beta) 
}) %>% simplify2array() %>% t()
# --
# save
# --
break()
readr::write_rds(vel,"simulacoes/estendida/traffic_input/speed_ADD20CI_A37_PL_C_AUT.rds.rds")
readr::write_rds(net,"simulacoes/estendida/traffic_input/net_ADD20CI_A37_PL_C_AUT.rds")
readr::write_rds(pc,"simulacoes/estendida/traffic_input/pc1.rds")







# --
# analise de agrupamento
# --
trips1 <- as.data.table(trips)[,trip_cap := (trips * max(pc)) / cap][,.(tto, trip_cap,speed_km_h)]
trips1 <- trips1[,tto := tto/sum(tto)]
trips1 <- trips1[,trip_cap := trip_cap/sum(trip_cap)]
trips1 <- trips1[,speed_km_h := speed_km_h/sum(speed_km_h)]
training <- sample(nrow(trips1), 1000)
Xtraining <- scale(trips1[training, ])
somnet <- som(Xtraining, kohonen::somgrid(2, 2, "rectangular"))
output <- map(somnet,
              scale(trips1, # trips1
                    center=attr(Xtraining, "scaled:center"),
                    scale=attr(Xtraining, "scaled:scale"))) 
#trips2 <- trips #trips[-training,]
trips$group <- output$unit.classif
# --
# net to sldf
# --
# data <- trips %>% data.table::as.data.table()
# data$lkm <- trips %>% sf::st_length() %>% units::set_units("m")
# net <- lapply(1:nrow(data), function(i){ 
#   aux <- trips$geometry[[i]] %>% 
#     sf::st_coordinates() %>%
#     as.data.frame() %>%
#     dplyr::select("X","Y") %>% 
#     list() %>% 
#     sp::Line() %>%
#     sp::Lines(ID=i) 
#   return(aux)  
# }) %>% sp::SpatialLines() %>% sp::SpatialLinesDataFrame(data=data) 
# --
# 
# parameters
# trips2 <- as.data.table(trips)

# par(mfrow=c(2,3))
temp_trips <- as.data.table(trips)
c1 <- temp_trips$tto - 1
c2 <- (temp_trips$trips * 1.46 * max(pc)) / temp_trips$cap
df1 <- data.table::data.table(c1 = c1, c2 = c2,group = temp_trips$group)
ggplot()+
  geom_point(data = df1,aes(x=c1,y=c2,color=group))+
  scale_color_viridis()
break()
for(i in unique(trips$group)){
  
  temp_trips <- as.data.table(trips)[group == i,]
  c1 <- temp_trips$tto - 1
  c2 <- (temp_trips$trips * max(pc)) / temp_trips$cap
  #   plot(c1,c2,main = paste0("group ",i),xlim=c(0,2),ylim=c(0,3))
  # }
  ds <- data.table::data.table(c1,c2)
  nlc <- nls.control(maxiter = 1000)
  m <- nls(c1 ~ I(alfa * (c2)^beta), data = ds, control = nlc,
           start = list(alfa = 1.35, beta = 5),
           trace = F)
  # m <- nls(c2 ~ I((c1/alfa)^(1/beta)), data = ds, 
  #          start = list(alfa = 0.15, beta = 2),
  #          trace = F)
  sm <- summary(m)
  alfa <- sm$parameters[[1]]
  beta <- sm$parameters[[2]]
  
  message(paste("alfa =",alfa))
  message(paste("beta =",beta))
  message(i)
}
trips2
c1 <- trips$tto - 1
c2 <- (trips$trips * max(pc[,rel])) / trips$cap
ds <- data.table::data.table(c1,c2)

# function
#ds <- ds[1:100,]
m <- nls(c1 ~ I(alfa * (c2)^beta), data = ds, 
         start = list(alfa = 0.15, beta = 5),
         trace = T)
summary(m)
dim(pcm)
# ---
# Profile Traffic Hour [24 x 1] ####
# ---
pc[,rel := freq/max(freq)][,rel]
pc[,rel]
# --
# speed [79680 x 24] ####
# --
pcm <- as.matrix(net@data$ldv) %*% 
  t( pc[,rel] %>% as.matrix() )
# speed index
vec <- c(0,30,50,70,110,130) + 1
spind <- list()
i=1
for(i in 1:5){
  spind[[i]] <- which(setDT(trips)[,speed] %between% c(vec[i],vec[i+1]))
}




# parameters
i=3
c2 <- lapply(spind[[i]],function(i){pcm[i,]}) %>% unlist()
c1 <- net@data$cap[rep(spind[[i]],each=24)]
x <- c2/c1
y <- rep(0.15,length(c1)) + rnorm(length(c1),0,1) 
ds <- data.frame(x = x,y = y)
# function
#ds <- ds[1:100,]
m <- nls(y ~ I(alfa * x^beta), data = ds, 
         start = list(alfa = 0.25, beta = 5),
         trace = T)
summary(m)
dim(pcm)
vel <- as.data.frame(matrix(0,nrow = nrow(trips),ncol = ncol(pcm)))
alpha <- 0.15; beta <- 4
vel <- lapply(1:nrow(trips), function(i){ # nrow(trips)
  net@data$speed[i]/(1+alpha*(pcm[i,]/net@data$cap[i])^beta) 
}) %>% simplify2array() %>% t()

set.seed(1485)
len <- 24
x <- runif(len)
y <- x^3 + rnorm(len, 0, 0.06)
ds <- data.frame(x = x, y = y)
str(ds)
plot(y ~ x, main = "Known cubic, with noise")
s <- seq(0, 1, length = 100)
lines(s, s^3, lty = 2, col = "green")


m <- nls(y ~ I(x^power + b), data = ds, start = list(power = 1, b= 0),trace = T)

m

summary(m)

