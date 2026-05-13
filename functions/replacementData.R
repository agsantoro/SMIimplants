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
    metodos$METHOD = labelled::copy_labels_from(as.numeric(metodos$METHOD), resultadosImplante[["36M"]]$dataSMI$FP_MONTH10_METHOD)
    metodos$METHOD = haven::as_factor(metodos$METHOD)
    metodos$METHOD = iconv(as.character(metodos$METHOD), from = "utf-8", to="ISO-8859-1")
    metodos$METHOD = as.character(polyglotr::google_translate(as.character(metodos$METHOD), "en","es"))
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