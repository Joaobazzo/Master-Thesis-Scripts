# 1) pc_age20
# 2) pc_age30
# 3) pc_age37
# 4) pc_cen1 - aumento
# 5) pc_cen2 - diminuicao
# ++++++++++++++++++++++++++
#setwd("E:/Documents/CICLO/Mestrado/PROJETO/vein/simulacoes/SIM4/")
#rm(list=ls())
# ++++++++++++++++++++++++++
#
# : inicializacao ####
#
# ++++++++++++++++++++++++++
# :: pacotes
# :: entradas
tipo <- c("FE25")
for(lp in (1:2)){
  # lp =1

  ccc <- c("<=1400",">2000")[lp]
  strccc <- c("m1400","2000")[lp]
  # :: Input 
  veh_age37 <- readRDS(paste0(path1,"veh/PC_",tipo,"_A37_",strccc,".rds"))
  
  for(p in 1:length(pol_det)){ 
    # p = 1 
    message(paste0(pol_det[p],"_",tipo))
    for(i in c(37)){ 
      # i = 37;data(fe2015)
      # Fator de emissao local
      FE_local <- vein::ef_cetesb(p=pol_det[p],veh = "PC_FG",
                                  year = 2016,agemax = i) %>% as.numeric()
      # Equivalencia Euro
      EURO_q <- fe2015[fe2015$Pollutant %in% pol[p],"Euro_LDV"] %>% as.character()
      EURO_q <- c(EURO_q[1],EURO_q)[1:i]
      
      # Fator de emissao dependente da velocidade 
      LEF_v <- vein::ef_ldv_scaled(dfcol = FE_local,SDC = 34.12, v = "PC",
                                   t = "4S", cc = ccc,
                                   f = "G",p = pol[p], eu=EURO_q)
      # 6) Emissao total por exaustao
      # condicional
      if(i==20){veh1 <- veh_age20}
      if(i==30){veh1 <- veh_age30}
      if(i==37){veh1 <- veh_age37}
      # --
      # emissao poluente
      # --
      E_pol <- vein::emis(veh = veh1,lkm = net1$lkm, ef = LEF_v, speed = speed1,
                          profile = pc1,verbose = T)
      # by hour
      e_hour <- vein::emis_post(E_pol,by="streets_wide") %>% colSums() %>% 
        data.table::as.data.table()
      colnames(e_hour) <- "emi"; e_hour$hour <- 1:24
      saveRDS(e_hour,
              file=paste0(path1,"emi/",pol_det[p],"/e_hour/",tipo,"_PC_",strccc,"_age_",i,".rds"))
      
      # by street
      e_strt <- vein::emis_post(E_pol,by="streets",net = net1) %>% sf::st_set_crs(4326)
      e_strt$day <- as.data.table(e_strt)[,V1:V24] %>% rowSums() %>% units::set_units(g)
      # resolution hex '09'
      e_grid <- vein::emis_grid(spobj = e_strt["day"],g = grid09)
      saveRDS(e_grid,
              file=paste0(path1,"emi/",pol_det[p],"/e_grid09/",tipo,"_PC_",strccc,"_age_",i,".rds"))
      # resolution hex '08'
      e_grid <- vein::emis_grid(spobj = e_strt["day"],g = grid08)
      saveRDS(e_grid,
              file=paste0(path1,"emi/",pol_det[p],"/e_grid08/",tipo,"_PC_",strccc,"_age_",i,".rds"))
      
      # by fleet age
      e_age <- vein::emis_post(arra = E_pol,
                               veh = "PC",size = strccc,fuel=tipo,
                               pollutant = pol_det[p], by = "veh") %>% as.data.table()
      e_age <- e_age[,emi:=sum(g),by=age][,.SD[1],by=age][order(age)]
      saveRDS(e_age,
              file=paste0(path1,"emi/",pol_det[p],"/e_age/",tipo,"_PC_",strccc,"_age_",i,".rds"))
    }
  }
}