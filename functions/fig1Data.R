fig1Data = function(country, area) {
  
  listaReturn = data.frame()
  
  fups = if (country %in% c("HND","NIC")) {
    c("BASELINE","36M","54M")
  } else if (country %in% c("MEX","PAN","GTM")) {
    c("BASELINE","36M")
  }
  
  for (fup in fups) {
    
    load(glue("data/FIG1_{country}_{fup}_HOUSEHOLD_MODULEA.rda"))
    
    # filtro por area
    if (area != "ALL") {
      if (fup == "54M") {
        dataSMI$TX_AREA = dataSMI$ARM
        dataSMI$TX_AREA[dataSMI$TX_AREA=="Intervention"] = "1"
        dataSMI$TX_AREA[dataSMI$TX_AREA=="Comparison"] = "0"
      }
      dataSMI = dataSMI[dataSMI$TX_AREA == area,]
    }
    
    NTOT = nrow(dataSMI)
    
    dataSMI$MARITAL_FILTER = NA
    
    # filtro estado civil
    if (fup != "54M") {
      dataSMI$MARITAL_FILTER[dataSMI$MARITAL_STATUS %in% 2:3] = 1
      dataSMI$MARITAL_FILTER[is.na(dataSMI$MARITAL_FILTER)] = 0
    } else {
      if (country %in% c("NIC")) {
        dataSMI$MARITAL_STATUS_PILOT = dataSMI$MARITAL_STATUS_SELF
      }
      dataSMI$MARITAL_FILTER[dataSMI$MARITAL_STATUS_PILOT %in% 2:3] = 1
      dataSMI$MARITAL_FILTER[is.na(dataSMI$MARITAL_FILTER)] = 0
    }
    
    dataSMI = dataSMI[dataSMI$MARITAL_FILTER == 1,]
    
    REG_MARRIED = nrow(dataSMI)
    
    # filtro necesidad anticonceptiva
    variables <- c(
      'PREG1', 'FP_NOUSE2_NOSEX', 'FP_NOUSE2_VIR', 'FP_NOUSE2_MENO',
      'FP_NOUSE2_PREG', 'FP_NOUSE2_WANTPR', 'FP_PREG_DESIRE', 'FP_NOUSE2_INFERT'
    )
    
    for (var in variables) {
      cond_var <- paste0("COND_", var)
      dataSMI[[cond_var]] <- NA
      dataSMI[[cond_var]][!is.na(dataSMI[[var]]) & dataSMI[[var]] == 1] <- 1
      dataSMI[[cond_var]][is.na(dataSMI[[cond_var]])] <- 0
    }
    
    dataSMI$needCont = rowSums(dataSMI[, paste0("COND_", variables)])
    dataSMI$need = ifelse(dataSMI$needCont == 0, 1, 0)
    dataSMI = dataSMI[!is.na(dataSMI$need) & dataSMI$need == 1,]
    
    REG_INNEED = nrow(dataSMI)
    
    listaReturn = rbind(
      listaReturn,
      data.frame(
        COUNTRY = country,
        FUP = fup,
        AREA = area,
        NTOT = NTOT,
        REG_MARRIED = REG_MARRIED,
        REG_INNEED = REG_INNEED
      )
    )
  }
  
  return(listaReturn)
}