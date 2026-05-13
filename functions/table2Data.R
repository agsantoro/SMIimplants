table2Data = function(country, area) {
  
  message(
    glue(
      "Preparing data for Table 2 ({country})"
    )
  )
  
  
  listaReturn = list()
  
  fups = if (country %in% c("HND","NIC")) {
    c("BASELINE","36M","54M")
  } else if (country %in% c("MEX","PAN","GTM")) {
    c("BASELINE","36M")
  }
  
  for (fup in fups) {
    
    load(glue("data/TABLE2_{country}_{fup}_HOUSEHOLD_MODULEA.rda"))
    
    # convertir haven_labelled
    dataSMI <- dataSMI %>%
      mutate(across(where(haven::is.labelled), as.numeric))
    
    # filtro por area
    
    # correccion 36M
    if (fup == "36M") {
      if ("SONNUM_LB_SPEC" %in% colnames(dataSMI)) dataSMI$SONNUM_LB = dataSMI$SONNUM_LB_SPEC
      if ("DAUNUM_LB_SPEC" %in% colnames(dataSMI)) dataSMI$DAUNUM_LB = dataSMI$DAUNUM_LB_SPEC
    }
    
    # correccion 54M
    if (fup == "54M") {
      if ("WOM_AGE_NUM" %in% colnames(dataSMI)) dataSMI$WOM_AGE = dataSMI$WOM_AGE_NUM
    }
    
    
    if (area != "ALL") {
      if (fup == "54M") {
        dataSMI$TX_AREA = dataSMI$ARM
        dataSMI$TX_AREA[dataSMI$TX_AREA=="Intervention"] = "1"
        dataSMI$TX_AREA[dataSMI$TX_AREA=="Comparison"] = "0"
      }
      dataSMI = dataSMI[dataSMI$TX_AREA == area,]
    }
    
    dataSMI$MARITAL_FILTER = NA
    
    if (fup == "36M") {
      if ("SONNUM_LB_SPEC" %in% colnames(dataSMI)) dataSMI$SONNUM_LB = dataSMI$SONNUM_LB_SPEC
      if ("DAUNUM_LB_SPEC" %in% colnames(dataSMI)) dataSMI$DAUNUM_LB = dataSMI$DAUNUM_LB_SPEC
    }
    
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
    
    # diseño muestral
    dataSMI = dataSMI[!is.na(dataSMI$WEIGHT_WOMAN),]
    
    if (country == "NIC") {
      dataSMI = dataSMI[substring(dataSMI$WTSEG,1,1) %in% as.character(0:9),]
      dataSMI$WEIGHT_WOMAN = as.numeric(dataSMI$WEIGHT_WOMAN)
    }
    
    # correccion 36M
    if (fup == "36M" & "SONNUM_LB_SPEC" %in% colnames(dataSMI)) {
      dataSMI$SONNUM_LB = dataSMI$SONNUM_LB_SPEC
    }
    
    listaReturn[[fup]]$dataSMI = dataSMI
  }
  
  listaReturn
}


table2Output = function(country) {
  
  tableGroups = data.frame()
  
  # set variables to define subgroups
  variablesSugroup = c(
    "SG_MARITAL","SG_AGE","SG_PAROUS","SG_SONNUM","SG_EDUC","SG_EDUC_SECONDARY","SG_LITERACY","SG_LITERACY_YES"
  )
  
  resultadosImplante = resultadosFinales[[country]]
  resultadosSubgroups = data.frame()
  for (i in names(resultadosImplante)) {
    if (i == "36M") {
      resultadosImplante[[i]]$dataSMI$SONNUM_LB = resultadosImplante[[i]]$dataSMI$SONNUM_LB_SPEC
      resultadosImplante[[i]]$dataSMI$DAUNUM_LB = resultadosImplante[[i]]$dataSMI$DAUNUM_LB_SPEC
    }
    
    variablesUseNow = "M5_IMP_USENOW"
    
    for (j in variablesUseNow) {
      if (length(resultadosImplante[[i]]$dataSMI[[j]])>0) {
        resultadosImplante[[i]]$dataSMI[[j]][is.na(resultadosImplante[[i]]$dataSMI[[j]])] = 0
        
        datosSubgrupos = resultadosImplante[[i]]$dataSMI
        
        datosSubgrupos = datosSubgrupos %>% mutate(
          SG_AGE = case_when(
            WOM_AGE < 0 ~ NA,
            WOM_AGE %in% 15:19 ~ "01. 15-19 years",
            WOM_AGE %in% 20:29 ~ "02. 20-29 years",
            WOM_AGE %in% 30:39 ~ "03. 30-39 years",
            WOM_AGE %in% 40:49 ~ "04. 40-49 years",
          ),
          SG_PAROUS = case_when(
            PAROUS == 1 ~ "1. YES",
            PAROUS == 0 ~ "2. NO",
            FALSE ~ NA 
          ),
          SG_SONNUM = case_when(
            SONNUM_LB + DAUNUM_LB < 0 ~ NA,
            SONNUM_LB + DAUNUM_LB == 0 ~ "1. 0",
            SONNUM_LB + DAUNUM_LB == 1 ~ "1. 1",
            SONNUM_LB + DAUNUM_LB == 2 ~ "2. 2",
            SONNUM_LB + DAUNUM_LB >= 3 ~ "3. 3 +",
          ),
          SG_SONNUM2 = case_when(
            SONNUM_LB + DAUNUM_LB < 0 ~ NA,
            SONNUM_LB + DAUNUM_LB == 0 ~ "1. 0",
            SONNUM_LB + DAUNUM_LB == 1 ~ "2. 1 OR 2",
            SONNUM_LB + DAUNUM_LB == 2 ~ "2. 1 OR 2",
            SONNUM_LB + DAUNUM_LB >= 3 ~ "3. 3 +",
          ),
          SG_EDUC = case_when(
            WOM_EDU_LEVEL < 0 ~ NA,
            WOM_EDU_LEVEL == 1 ~ "1. Primaria",
            WOM_EDU_LEVEL == 2 ~ "2. Secundaria",
            WOM_EDU_LEVEL %in% c(3,4) ~ "3. Media / Bachillerato / Universitario"
          ),
          SG_EDUC_SECONDARY = case_when(
            WOM_EDU_LEVEL < 0 ~ NA,
            WOM_EDU_LEVEL == 1 ~ "1. Primaria",
            WOM_EDU_LEVEL == 2 ~ "2. Secundaria o +",
            WOM_EDU_LEVEL %in% c(3,4) ~ "2. Secundaria o +"
          ),
          SG_LITERACY = case_when(
            WOM_LITERACY < 0 ~ NA,
            WOM_LITERACY == 1 ~ "1. NO",
            WOM_LITERACY == 2 ~ "2. PARTIAL",
            WOM_LITERACY == 3 ~ "3. FULL",
          ),
          SG_LITERACY_YES = case_when(
            WOM_LITERACY < 0 ~ NA,
            WOM_LITERACY == 1 ~ "1. NO",
            WOM_LITERACY == 2 ~ "2. YES",
            WOM_LITERACY == 3 ~ "2. YES",
          )
        )
        
        if (i == "54M") {
          datosSubgrupos = datosSubgrupos %>% mutate(
            SG_MARITAL = case_when(
              MARITAL_STATUS_PILOT == 2 ~ "1. MARRIED",
              MARITAL_STATUS_PILOT == 3 ~ "2. PARTNERED",
            ) 
          )
        } else {
          datosSubgrupos = datosSubgrupos %>% mutate(
            SG_MARITAL = case_when(
              MARITAL_STATUS == 2 ~ "1. MARRIED",
              MARITAL_STATUS == 3 ~ "2. PARTNERED",
            ) 
          )
        }
        
        diseno = svydesign(
          ids = ~ WTSEG,
          weights = ~ WEIGHT_WOMAN,
          data = datosSubgrupos
        )
        
        #if (j == "M6_OCP_USENOW") {browser()}
        
        for (k in variablesSugroup) {
          
          
          if (1 %in% unique(datosSubgrupos[[j]])) {
            
            table = data.frame(
              AREA = "ALL",
              FUP = i,
              VAR = j,
              GROUP = k,
              table(datosSubgrupos[[k]],datosSubgrupos[[j]]) %>% as.data.frame.matrix() %>% dplyr::select( `0`,`1`) %>% set_names(c("SAMPLE N","UNWEIGHTED_COUNT")),
              svyby(~do::exec(j),~factor(do::exec(k)), diseno, svymean) %>% set_names(c("CATEGORY","PROP","SE"))
            )
            rownames(table) = 1:nrow(table)
            
            ##### TABLE 3 #####
            datosSubgrupos[[j]][is.na(datosSubgrupos[[j]])] = 0
            datosSubgrupos[[j]][!datosSubgrupos[[j]] %in% c(0,1)] = 0
            
            
            diseno = svydesign(
              ids = ~ WTSEG,
              weights = ~ WEIGHT_WOMAN,
              data = datosSubgrupos
            )
            
            table_CI = 
              suppressWarnings(
                svyby(
                  eval(parse(text = paste0("~",j))) ,
                  eval(parse(text = paste0("~",k))) ,
                  diseno, 
                  FUN = svyciprop, 
                  method = "logit", 
                  vartype = "ci")
              )
              
            names(table_CI)[1] = "CATEGORY"
            names(table_CI)[3] = "L_CI"
            names(table_CI)[4] = "U_CI"
            table = table %>% left_join(table_CI, by = "CATEGORY")
            table$country = country
            table = table %>% dplyr::select(COUNTRY = country, FUP,AREA, VAR,GROUP,CATEGORY,SAMPLE.N,UNWEIGHTED_COUNT,PROP,SE,L_CI,U_CI)
            tableGroups = rbind(tableGroups,table)
            
            
          }
        }
      }
    }
  }
  
  
  tableGroups$PROP = format(round(tableGroups$PROP *100, 2), nsmall = 2, decimal.mark = ",", big.mark = ".")
  tableGroups$L_CI = format(round(tableGroups$L_CI *100, 2), nsmall = 2, decimal.mark = ",", big.mark = ".")
  tableGroups$U_CI = format(round(tableGroups$U_CI *100, 2), nsmall = 2, decimal.mark = ",", big.mark = ".")
  tableGroups$SE = NULL
  
  tableGroups
}
