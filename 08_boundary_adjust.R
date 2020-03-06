# ----
#
# adjust flow based on vehicles count on boundaries
#
# ----
rm(list=ls())
setwd("L:/# DIRUR #/ASMEQ/bosistas/joaobazzo/master-thesis-repo1/")
library(mapview)
library(sf)
library(ggplot2)
library(data.table)
#merged2 <- readr::read_rds("dados/Percursos/estendido/intersection-new_centroids_cap.rds")
 merged1 <- sf::read_sf("dados/Percursos/estendido/intersection_new-centroid.shp") %>% 
   sf::st_set_crs(4326) %>% sf::st_transform(32722)
mapview(merged1["trips"])
pc <- data.table::fread("dados/Pesquisa_OD_IPPUC/arquivos_saida/csv/estendida/fluxo_horario_deslocamentos.csv")
pc <- pc[,rel := freq / max(freq)][,rel]

trips1 <- sf::read_sf("dados/Pesquisa_OD_IPPUC/PVT_VISUM/shapefile/Modelo OD_Pico Manha_20180530_link.SHP")
trips1 <- trips1[trips1$VOLVEHPR.5 > 0,] %>%
  data.table::data.table() %>%
  sf::st_as_sf() %>% sf::st_transform(32722)
trips1$trips <- trips1$VOLVEHPR.5 * sum(pc)

counts <- sf::read_sf("dados/Pesquisa_OD_IPPUC/D536_015_BR/screen_line.shp")
counts <- counts %>% data.table::data.table() %>% sf::st_as_sf() %>% sf::st_transform(32722)


# --
#
# intersection both
#
# --
bound_counts <- sf::st_buffer(counts,55)

inter <- sf::st_intersection(merged1,bound_counts)
ippuc <- sf::st_intersection(trips1,bound_counts)


inter[,c(5,34)]
table(inter$Posto)
# table(trips$Posto)

mapview(inter$geometry) + mapview(bound_counts,zcol = "Posto")
mapview(ippuc$geometry) + mapview(bound_counts,zcol = "Posto")

# --
# expected
# --
exp <- data.table("pontos" = paste0("P",1:38),
                  "num" = c(rep(1,7),2,1,2,1,1,1,1,2,1,1,1,1,2,2,
                            2,2,1,1,2,1,2,1,1,1,2,2,0,1,1,1,1))
inter1 <- lapply(seq_along(exp$pontos),function(i){
  temp_inter <- as.data.table(inter)[Posto %in% exp[i,pontos],]  
  exp[i,trips := temp_inter$trips[order(temp_inter$trips,decreasing = TRUE)[1:exp[i,num]]] %>% sum() ]
  exp[i,obs := as.data.table(bound_counts)[Posto %in% exp[i,pontos],N_Autos_ + N_Vans_]]
  return(exp[i,])
}) %>% data.table::rbindlist()
# --
# expected ippuc
# --
ippuc1 <- lapply(seq_along(exp$pontos),function(i){
  temp_inter <- as.data.table(ippuc)[Posto %in% exp[i,pontos],]  
  exp[i,trips := temp_inter$trips[order(temp_inter$trips,decreasing = TRUE)[1:exp[i,num]]] %>% sum() ]
  exp[i,obs := as.data.table(bound_counts)[Posto %in% exp[i,pontos],N_Autos_ + N_Vans_]]
  return(exp[i,])
}) %>% data.table::rbindlist()
# temp_inter$distance_m <- sf::st_length(temp_inter$geometry %>% st_cast("LINESTRING"))
# mapview(temp_inter$geometry) + mapview(bound_counts[bound_counts$Posto %in% i,]$geometry)
# 
# inter <- as.data.table(inter)[,trips := sum(trips),by = "Posto"][,.SD[1],by = "Posto"]
# trips <- as.data.table(trips)[,trips := sum(VOLVEHPR.5) * sum(pc),by = "Posto"][,.SD[1],by = "Posto"]
# 
# inter[,obs := N_Autos_ + N_Taxis_]
# trips[,obs := N_Autos_ + N_Taxis_]
# 
# 
# inter <- inter[,.(trips,obs,Posto,geometry)][,ratio := obs / trips]
# trips <- trips[,.(trips,obs,Posto,geometry)][,ratio := obs / trips]
# 
# inter <- inter[,ratio := trips / obs]
# trips <- trips[,ratio := trips / obs]

inter1 <- inter1[- which(is.na(inter1$trips)),]
ippuc1 <- ippuc1[- which(is.na(ippuc1$trips)),]


inter1[trips > 0, ratio := trips / obs]
ippuc1[trips > 0, ratio := trips / obs]


inter <- inter1
inter1[,ratio[order(ratio)]] %>% summary()
ippuc1[,ratio[order(ratio)]] %>% summary()
# trips[,ratio[order(ratio)]] %>% summary()
# --
# calibration MERGED
# --
cal <- sample(x = 1:nrow(inter1),size = 26)
val <- (1:nrow(inter1))[-cal]
inter1 <- inter1[cal,data := "Calibração"][val, data := "Validação"]
# adjust
adj <- inter1[cal,][,ratio] %>% mean()

data.lm <- inter1[cal,.(obs,trips)][,trips := trips / adj ]
flow.lm = lm(trips ~ obs,data = data.lm) # r.square
label.lm <- data.table("R2" = summary(flow.lm)$r.squared %>% round(2),
                       "Corr" = cor(data.lm$obs,data.lm$trips) %>% round(2),
                       "RMSE" = Metrics::mae(data.lm$obs,data.lm$trips) %>% round(1),
                       "data" = "Calibração")
# validation
data.lm.val <- inter1[val,.(obs,trips)][,obs := obs ]
flow.lm.val = lm(trips ~ obs,data = data.lm.val) # r.square

# data print
label.lm.val <- data.table("R2" = summary(flow.lm.val)$r.squared %>% round(2),
                           "Corr" = cor(data.lm.val$obs,data.lm.val$trips) %>% round(2),
                           "RMSE" = Metrics::mae(data.lm.val$obs,data.lm.val$trips) %>% round(1),
                           "data" = "Validação")

label <- rbind(label.lm,label.lm.val)
xtable::xtable(x = label,type="latex",label = "tab:ippuc_transp_adjust",
               caption = "Calibração e validação do modelo de transporte apresentado pelo AUT") %>% print()
# --
# calibration IPPUC
# --
ippuc1[,data := "Met IPPUC"]
data.lm.val <- ippuc1[,.(obs,trips)]
flow.lm.val = lm(trips ~ obs,data = data.lm.val) # r.square
label.lm.val <- data.table("R2" = summary(flow.lm.val)$r.squared %>% round(2),
                           "Corr" = cor(data.lm.val$obs,data.lm.val$trips) %>% round(2),
                           "RMSE" = Metrics::mae(data.lm.val$obs,data.lm.val$trips) %>% round(1),
                           "data" = "Validação")
label <- rbind(label.lm.val)
label
xtable::xtable(x = label,type = "latex",label = "tab:ippuc_transp_adjust",
               caption = "Calibração e validação do modelo de transporte apresentado pelo IPPUC.") %>%
  print()
# --
# adjust
# --
paste0("so the fleet has to be adjusted by ", round(1/adj,3));break()
inter$trips <- inter$trips / adj
# --
inter$met <- "AUT"
trips$met <- "IPPUC"
inter1 <- rbind(inter,trips)
ggplot(inter1) +
  geom_point(aes(x = trips,y = obs)) +
  geom_line(aes(x = obs,y = obs)) +
  facet_grid(rows = vars(met)) + 
  xlim(0,max(inter1$trips)) + 
  ylim(0,max(inter1$trips)) + 
  xlab("Fluxo modelado (nº veículos)") + 
  ylab("Fluxo observado (nº veículos)")

# plot
ggplot(inter1) +
  geom_point(aes(x = trips,y = N_Autos_,color = ratio)) +
  geom_line(aes(x = N_Autos_,y = N_Autos_)) +
  xlim(0,max(inter1$N_Autos_)) + 
  ylim(0,max(inter1$N_Autos_)) + 
  xlab("Fluxo modelado (nº veículos)") + 
  ylab("Fluxo observado (nº veículos)")
# -
# save
merged1$trips <- merged1$trips / adj
readr::write_rds(merged1,"dados/Percursos/estendido/intersection-new_centroids_cap_adj.rds")

