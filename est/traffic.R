traffic <- function(path1,net1,frota_fe1){
  abas <- openxlsx::getSheetNames(frota_fe1)
  abas <- abas[2:4]
  dtf <- lapply(abas,function(i){
    # i = abas[1]
    dt <- openxlsx::read.xlsx(frota_fe1,sheet =i) %>% setDT()
    sumdt <- dt[Municipio %in% "Total",.SDcols = `2016`:`1976`][,`2016`:`1980`] %>% as.vector()
    dtf <- data.table(fuel = i,sumdt)
    message(i)
    return(dtf)
  }) %>% data.table::rbindlist()
  dtf[,2:ncol(dtf)] <- dtf[,2:ncol(dtf)]/sum(dtf[,2:ncol(dtf)])
  dtf[,2:ncol(dtf)] %>% sum()
  # arrange
  dtf <- t(dtf)
  colnames(dtf) <- dtf[1,]
  dtf <- dtf[-1,]
  dtf <- sapply(colnames(dtf),function(i){dtf[,i] <- as.numeric(dtf[,i])})
  # flex
  dtf <- as.data.table(dtf)
  dtf[,`:=`(Autómovel_FE100 = Automóvel_flex * 0.3,
            Autómovel_FE25 = Automóvel_flex * 0.7#,
            #`Coml_Leve_FE100` = `Coml Leve_flex` * 0.7,
            #`Coml_Leve_FE25` = `Coml Leve_flex` * 0.3
            )]
  dtf[,`:=`(Automóvel_flex = NULL)]
  colnames(dtf) <- c("PC_E25","PC_E100","PC_FE100","PC_FE25")
  #
  #break()
  # anfavea engine size
  part <- c(0.36486,0.63506) 
  # ++++++++++++++
  # age 20,30,37
  # +++++++++++++++
  fuel <- c("_E25","_E100","_FE25","_FE100")
  year <- c(37)
  for(i in year){
    # i = 37
    # frota
    frota <- as.data.frame(dtf[1:i,])
    # frota <- frota[,2:5]/sum(frota[,2:5])
    frota <- frota/sum(frota)
    for(j in fuel){
      # j = fuel[3]
      for(k in c("PC")){
        # k = "PC"
        sf <- frota[,names(frota) %like% j & names(frota) %like% k]
        aux <- vein::my_age(x = net1@data$trips,y = sf, name = paste0(k,j,"_AGE",i)) 
        if(k == "PC"){
          saveRDS(sum(sf)*part[1]*aux, file = paste0(path1,"veh/",k,j,"_A",i,"_m1400.rds"))
          saveRDS(sum(sf)*part[2]*aux, file = paste0(path1,"veh/",k,j,"_A",i,"_2000.rds"))
        }else{
          saveRDS(sum(sf)*aux, file = paste0(path1,"veh/",k,j,"_A",i,".rds"))
        }
        # stats
        message(paste0(k,j,"_AGE",i))
      }
      
    }
  }
}

# 
# break()
# #
# # check traffic
# #
# veh <- list.files("veh/",pattern = ".rds",full.names = TRUE)
# 
# age20 <- veh[veh %like% "A20"]
# age30 <- veh[veh %like% "A30"]
# age37 <- veh[veh %like% "A37"]
# 
# lapply(age20,function(i){readRDS(i) %>% sum()}) %>% unlist() %>% sum()
# 
# lapply(age30,function(i){readRDS(i) %>% sum()}) %>% unlist() %>% sum()
# 
# lapply(age37,function(i){readRDS(i) %>% sum()}) %>% unlist() %>% sum()
