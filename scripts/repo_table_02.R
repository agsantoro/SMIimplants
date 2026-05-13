library(dplyr)
library(glue)
library(survey)
library(purrr)

# load functions
source("functions/table2Data.R")

# creating list to add data
resultadosFinales = list()
resultadosFinales[["HND"]] = table2Data("HND", "ALL")
resultadosFinales[["MEX"]] = table2Data("MEX", "ALL")


##### TABLE #####

table2 = 
  rbind(
    table2Output("HND"),
    table2Output("MEX")
  )

# render table
htmlwidgets::saveWidget(reactable(table1), "table2.html", selfcontained = TRUE)
Sys.sleep(3)
browseURL("table2.html")
Sys.sleep(3)
unlink("table2.html")
