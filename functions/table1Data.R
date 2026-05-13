table1Data = function(country, area) {
  pais = country
  
  message(
    glue(
      "Preparing data for Table 1 ({country})"
      )
    )
  
  listaReturn = list()
  
  fups = if (country %in% c("HND","NIC")) {
    c("BASELINE","36M","54M")
  } else if (country %in% c("MEX","PAN","GTM")) {
    c("BASELINE","36M")
  }
  
  for (fup in fups) {
    
    load(glue("data/TABLE1_{country}_{fup}_HOUSEHOLD_MODULEA.rda"))
    
    # convertir todas las columnas haven_labelled a sus tipos base
    dataSMI <- dataSMI %>% 
      mutate(across(where(haven::is.labelled), as.numeric))
    
    if (area != "ALL") {
      if (fup == "54M") {
        dataSMI$TX_AREA = dataSMI$ARM
        dataSMI$TX_AREA[dataSMI$TX_AREA=="Intervention"] = "1"
        dataSMI$TX_AREA[dataSMI$TX_AREA=="Comparison"] = "0"
      }
      dataSMI = dataSMI[dataSMI$TX_AREA == area,]
    }
    
    dataSMI$MARITAL_FILTER = NA
    
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
    
    dataSMI = dataSMI[!is.na(dataSMI$WEIGHT_WOMAN),]
    
    if (country == "NIC") {
      dataSMI = dataSMI[substring(dataSMI$WTSEG,1,1) %in% as.character(0:9),]
      dataSMI$WEIGHT_WOMAN = as.numeric(dataSMI$WEIGHT_WOMAN)
    }
    
    # convertir para evitar error con haven_labelled
    dataSMI$FP_MONTH1_METHOD  <- as.numeric(dataSMI$FP_MONTH1_METHOD)
    dataSMI$FP_MONTH24_METHOD <- as.numeric(dataSMI$FP_MONTH24_METHOD)
    
    dataSMI = dataSMI %>%
      mutate(endMethod = case_when(
        FP_MONTH1_METHOD  < 1          ~ "99. No response",
        FP_MONTH1_METHOD %in% 1:4      ~ "01. No method",
        FP_MONTH1_METHOD %in% 5:8      ~ "02. Non-implant modern methods",
        FP_MONTH1_METHOD == 9          ~ "03. Implant",
        FP_MONTH24_METHOD == 18        ~ "02. Non-implant modern methods",
        FP_MONTH1_METHOD %in% 10:14    ~ "02. Non-implant modern methods",
        FP_MONTH1_METHOD %in% 15:17    ~ "04. Non-modern methods",
        FP_MONTH1_METHOD == 19         ~ "02. Non-implant modern methods",
        FP_MONTH1_METHOD == 20         ~ "04. Non-modern methods",
        T ~ as.character(FP_MONTH1_METHOD)
      )) %>%
      mutate(startMethod = case_when(
        FP_MONTH24_METHOD  < 1         ~ "99. No response",
        FP_MONTH24_METHOD %in% 1:4     ~ "01. No method",
        FP_MONTH24_METHOD %in% 5:8     ~ "02. Non-implant modern methods",
        FP_MONTH24_METHOD == 18        ~ "02. Non-implant modern methods",
        FP_MONTH24_METHOD == 9         ~ "03. Implant",
        FP_MONTH24_METHOD %in% 10:14   ~ "02. Non-implant modern methods",
        FP_MONTH24_METHOD %in% 15:17   ~ "04. Non-modern methods",
        FP_MONTH24_METHOD == 19        ~ "02. Non-implant modern methods",
        FP_MONTH24_METHOD == 20        ~ "04. Non-modern methods",
        T ~ as.character(FP_MONTH24_METHOD)
      ))
    
    design = svydesign(ids = ~WTSEG, weights = ~WEIGHT_WOMAN, data = dataSMI)
    svy_design <- as_survey(design)
    
    icStart <- svy_design %>%
      filter(!is.na(startMethod)) %>%
      group_by(startMethod) %>%
      summarize(proporcion = survey_mean(vartype = "ci", prop_method = "logit")) %>%
      mutate(
        ic_inferior = proporcion_low * 100,
        ic_superior = proporcion_upp * 100
      )
    
    icEnd <- svy_design %>%
      filter(!is.na(endMethod)) %>%
      group_by(endMethod) %>%
      summarize(proporcion = survey_mean(vartype = "ci", prop_method = "logit")) %>%
      mutate(
        ic_inferior = proporcion_low * 100,
        ic_superior = proporcion_upp * 100
      )
    
    startEndMethod = data.frame(
      country    = country,
      fup        = fup,
      method     = names(svytable(~ startMethod, design)),
      nStart     = table(dataSMI$startMethod, useNA = "no"),
      startValue = prop.table(svytable(~ startMethod, design)) * 100,
      startICL   = pmax(0,icStart$ic_inferior),
      startICH   = icStart$ic_superior,
      nEnd       = table(dataSMI$endMethod, useNA = "no"),
      endValue   = prop.table(svytable(~ endMethod, design)) * 100,
      endICL     = pmax(0,icEnd$ic_inferior),
      endICH     = icEnd$ic_superior
    )
    
    listaReturn[[fup]]$startEndMethod = startEndMethod
  }
  
  listaReturn
}
table1Data("HND", "ALL")
