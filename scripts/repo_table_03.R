library(ggplot2)
library(survey)
library(dplyr)
library(purrr)
library(haven)

# load functions
source("functions/table3Data.R")

##### GRAFICO USO SUBGROUPS #####

table3 = table3Output()

# render table
htmlwidgets::saveWidget(reactable(table1), "table3.html", selfcontained = TRUE)
Sys.sleep(3)
browseURL("table3.html")
Sys.sleep(3)
unlink("table3.html")


