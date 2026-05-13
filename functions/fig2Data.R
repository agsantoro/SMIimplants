fig2Data = function(country, area) {
  
  message(
    glue("Working on Figure 2 data ({country})")
  )
  
  listaReturn = list()
  
  fups = if (country %in% c("HND","NIC")) {
    c("BASELINE","36M","54M")
  } else if (country %in% c("MEX","PAN","GTM")) {
    c("BASELINE","36M")
  }
  
  for (fup in fups) {
    
    load(glue("data/FIG2_{country}_{fup}_HOUSEHOLD_MODULEA.rda"))
    
    # solo columnas necesarias
    # cols_needed <- c(
    #   "TX_AREA", "ARM",
    #   "WEIGHT_WOMAN", "WTSEG",
    #   "MARITAL_STATUS", "MARITAL_STATUS_PILOT", "MARITAL_STATUS_SELF",
    #   "PREG1", "FP_NOUSE2_NOSEX", "FP_NOUSE2_VIR", "FP_NOUSE2_MENO",
    #   "FP_NOUSE2_PREG", "FP_NOUSE2_WANTPR", "FP_PREG_DESIRE", "FP_NOUSE2_INFERT",
    #   paste0("FP_MONTH", 1:24, "_METHOD")
    # )
    # cols_needed <- intersect(cols_needed, colnames(dataSMI))
    # dataSMI <- dataSMI %>% dplyr::select(dplyr::all_of(cols_needed))
    # 
    # filtro por area
    if (area != "ALL") {
      if (fup == "54M") {
        dataSMI$TX_AREA = dataSMI$ARM
        dataSMI$TX_AREA[dataSMI$TX_AREA=="Intervention"] = "1"
        dataSMI$TX_AREA[dataSMI$TX_AREA=="Comparison"] = "0"
      }
      dataSMI = dataSMI[dataSMI$TX_AREA==area,]
    }
    
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
    
    # variable uso de implante por mes
    for (i in 1:24) {
      variable = glue("USE_IMPLANT{i}")
      dataSMI[[variable]] = 0
      dataSMI[[variable]][dataSMI[[glue("FP_MONTH{i}_METHOD")]] == 9]           =  1
      dataSMI[[variable]][dataSMI[[glue("FP_MONTH{i}_METHOD")]] %in% c(4:8,10:20)] =  0
      dataSMI[[variable]][dataSMI[[glue("FP_MONTH{i}_METHOD")]] < 0]            = -1
      dataSMI[[variable]][dataSMI[[glue("FP_MONTH{i}_METHOD")]] %in% 1:3]       = -1
      dataSMI[[variable]][is.na(dataSMI[[glue("FP_MONTH{i}_METHOD")]])]         = NA
    }
    
    # reemplazar -1 por NA
    for (i in 1:24) {
      variable = glue("USE_IMPLANT{i}")
      dataSMI[[variable]][dataSMI[[variable]] == -1] = NA
    }
    
    # diseño muestral
    dataSMI = dataSMI[!is.na(dataSMI$WEIGHT_WOMAN),]
    
    if (country == "NIC") {
      dataSMI = dataSMI[substring(dataSMI$WTSEG,1,1) %in% as.character(0:9),]
      dataSMI$WEIGHT_WOMAN = as.numeric(dataSMI$WEIGHT_WOMAN)
    }
    
    design = svydesign(ids = ~WTSEG, weights = ~WEIGHT_WOMAN, data = dataSMI)
    
    # frecuencia de uso de implante por mes
    RESULTS_USE_MONTH = data.frame()
    
    for (i in 1:24) {
      value = suppressWarnings(
        svymean(
          x = ~ do::exec(glue("USE_IMPLANT{i}")),
          na.rm = T,
          design = design
        ) * 100
      ) 
      
      varText = glue("~ USE_IMPLANT{i}")
      ci_l = suppressWarnings(attr(svyciprop(eval(parse(text = varText)), design = design), "ci")[1] * 100)
      ci_u = suppressWarnings(attr(svyciprop(eval(parse(text = varText)), design = design), "ci")[2] * 100)
      
      append = data.frame(
        MONTH = i,
        VALUE = unname(value[1]),
        CI_L = as.numeric(ci_l),
        CI_U = as.numeric(ci_u)
      )
      
      RESULTS_USE_MONTH = rbind(append, RESULTS_USE_MONTH)
    }
    
    RESULTS_USE_MONTH$MONTH = rev(RESULTS_USE_MONTH$MONTH)
    listaReturn[[fup]]$resultadosMes = RESULTS_USE_MONTH
  }
  
  listaReturn
}