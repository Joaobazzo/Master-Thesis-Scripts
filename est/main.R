setwd("L:/# DIRUR #/ASMEQ/bosistas/joaobazzo/master-thesis-repo1/")
rm(list = ls())
path1 <- "simulacoes/estendida/N_A37_P_C_AUT/"
# library(devtools) 
library(vein)
library(stringi) # remove accents
library(data.table)
library(stringr)
library(dplyr)
library(sf)
library(openxlsx)
library(sp)
library(geobr)
library(cptcity)
library(gridExtra)
library(xtable)
library(extrafont)
library(lwgeom)
library(ggplot2)
# --
# limpa
# --
files_rds <- list.files(path = path1, pattern = ".rds", recursive = TRUE, full.names = TRUE)
files_jpg <- list.files(path = path1, pattern = ".jpg", recursive = TRUE, full.names = TRUE)
unlink(x = files_rds,recursive = FALSE,force = TRUE)
unlink(x = files_jpg,recursive = FALSE,force = TRUE)
# ---
# 1) SPEED
# ---

# --
# 2) FLEET PREP
# --
net1 <- readRDS("simulacoes/estendida/traffic_input/net_aut.rds")
frota_fe1 = "dados/CETESB_2016/Frota-Circulante_Estado-de-São-Paulo_2016.xlsx"
source(paste0(path1,"traffic.R"))
traffic(path1 = path1,net1 = net1, frota_fe1 = frota_fe1)
# --
# 2.1) Top-down method
# --

# -
# 3) EMISSIONS - inputs
# --
speed1 <- readRDS("simulacoes/estendida/traffic_input/speed_aut.rds")
pc1 <- readRDS("simulacoes/estendida/traffic_input/pc1.rds")
grid09 <- readr::read_rds("dados/IBGE/cwb_full09.rds")
grid08 <- readr::read_rds("dados/IBGE/cwb_full08.rds")
data("fkm")
data("fe2015")

# emission factor
# source("ef/ef_daemme.R")

# poluentes
pol_det <- c("COd","CO2","CH4","NMHCd","NOxd")
pol <- c("CO","CO2","CH4","NMHC","NOx")

# roda scripts

inputs <- list.files(path = paste0(path1,"est"), pattern = "input.R",
                     recursive = TRUE, full.names = TRUE)
system.time({
  lapply(inputs,function(i){message(i);source(i)})
})
break()
# -
# 3.1) EMISSIONS - INNER (apenas para SIM6)
# --
# inner08 <- readr::read_rds("dados/Pesquisa_OD_IPPUC/arquivos_saida/RDS/estendido/inner_trips_by_centroid_08.rds")
# inner09 <- readr::read_rds("dados/Pesquisa_OD_IPPUC/arquivos_saida/RDS/estendido/inner_trips_by_centroid_09.rds")
# 
# veh_inner <- readr::read_rds(paste0(path1,"veh/PC_inner_trips.rds"))
# --
# 4) MAPS E PLOTS
# --
#
# 4.1 prep plots and prints
#
# source("analysis/R/04_spacial_traffic_plot.R") # map
source(paste0(path1,"analysis/R/04_traffic_age_plot.R"))     # histogram
source(paste0(path1,"analysis/R/04_traffic_prep.R"))         # print - xtable
source(paste0(path1,"analysis/R/06_total_estimation.R"))     # print - xtable
#
# 4.2 generate folders
#
ps <- c("e_hour","e_age","e_grid08","e_grid09")
pastas <- lapply(pol_det,function(i){
  dir.create(path = paste0(path1,"emi/",i,"/"),showWarnings = F)
  lapply(ps,function(j){
      dir.create(path = paste0(path1,"emi/",i,"/",j,"/"),showWarnings = F)
  })
}); rm(pastas) 
#
# 4.3 main plots
#
emi_plot_codes <- list.files(path = paste0(path1,"analysis/R/"),pattern = "05",full.names = TRUE)

break()
system.time({
  lapply(emi_plot_codes,function(i){message(i);source(i)})
})
