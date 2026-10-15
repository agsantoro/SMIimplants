replacementData = function (country, dataSet) {
  areas = "ALL"
  
  # list to store data
  reemplazos = list()
  # loop in areas
  for (area in areas) {
    resultadosImplante = dataSet[[area]]
    metodos = data.frame()
    
    for (i in names(resultadosImplante)) {
      diseno = svydesign(
        ids = ~ WTSEG,
        weights = ~ WEIGHT_WOMAN,
        data = resultadosImplante[[i]]$dataSMI
      )
      table = prop.table(svytable(~ LEFT_METHOD,
                                  diseno))*100
      
      append = data.frame(
        FUP = i,
        METHOD = names(table),
        VALUE = as.vector(unname(table))
      )
      
      metodos = rbind(
        append,
        metodos
      )
    }
    
    # preparing data
    # Fixed labels from the source dictionary; no network translation required.
    methodLabels = c(`-2` = "Refused to answer", `-1` = "Does not know",
      `1` = "Birth", `2` = "Pregnancy", `3` = "Termination", `4` = "No method",
      `5` = "Female sterilization", `6` = "Male sterilization", `7` = "IUD",
      `8` = "Injectables", `9` = "Implant", `10` = "Pill", `11` = "Male condom",
      `12` = "Female condom", `13` = "Diaphragm", `14` = "Sponge, spermicide",
      `15` = "Lactational amenorrhea", `16` = "Rhythm method", `17` = "Withdrawal",
      `18` = "Emergency contraception", `19` = "Other modern method",
      `20` = "Other traditional method")
    labels = unname(methodLabels[as.character(metodos$METHOD)])
    if (anyNA(labels)) stop("Unmapped contraceptive method code")
    metodos$METHOD = labels
    metodos$VALUE = round(metodos$VALUE, 2)
    metodos = cbind(country = country, area = area, metodos)
    
    # appending
    reemplazos[[area]] = metodos
  }
  
  # preparing data
  reemplazos = do.call(rbind, reemplazos)
  reemplazos = reemplazos %>% arrange(-VALUE)
  reemplazos$FUP[reemplazos$FUP == "36M"] = "FUP 36M"
  reemplazos$FUP[reemplazos$FUP == "54M"] = "FUP 54M"
  reemplazos$area[reemplazos$area=="1"] = "Intervention"
  reemplazos$area[reemplazos$area=="0"] = "Control"
  reemplazos$area[reemplazos$area=="ALL"] = "All"
  rownames(reemplazos) = 1:nrow(reemplazos)
  return(reemplazos)
  
}