library(glue)
library(ggplot2)
library(dplyr)
library(survey)
library(srvyr)
library(purrr)
library(reactable)

# load functions
source("functions/table1Data.R")

# list to store data
tablaImplantLines = list()

# calculating indicators
tablaImplantLines[["HND"]] = table1Data("HND", "ALL")
tablaImplantLines[["MEX"]] = table1Data("MEX", "ALL")

HND = map(
  names(tablaImplantLines[["HND"]]), function(i) {
    tablaImplantLines[["HND"]][[i]]$startEndMethod
  }) %>% do.call(rbind, .) %>%
  dplyr::select(
    "country",
    "fup",
    "method",
    "nStart.Freq",
    "startValue.Freq",
    "startICL",
    "startICH",              
    "nEnd.Freq",
    "endValue.Freq",
    "endICL",
    "endICH" 
  )

MEX = map(
  names(tablaImplantLines[["MEX"]]), function(i) {
    tablaImplantLines[["MEX"]][[i]]$startEndMethod
  }) %>% do.call(rbind, .) %>%
  dplyr::select(
    "country",
    "fup",
    "method",
    "nStart.Freq",
    "startValue.Freq",
    "startICL",
    "startICH",              
    "nEnd.Freq",
    "endValue.Freq",
    "endICL",
    "endICH" 
  )

table1 = list(HND,MEX) %>% do.call(rbind, .)

# render table
htmlwidgets::saveWidget(reactable(table1), "table1.html", selfcontained = TRUE)
Sys.sleep(3)
browseURL("table1.html")
Sys.sleep(3)
unlink("table1.html")
