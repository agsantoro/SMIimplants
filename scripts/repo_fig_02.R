library(glue)
library(ggplot2)
library(dplyr)
library(glue)
library(haven)
library(survey)

#setwd("Analysis/Question #1/Repositorio")

# load functions
source("functions/fig2Data.R")

# list to store data
tablaImplantLines = list()

# calculating indicators
tablaImplantLines[["HND"]] = fig2Data("HND", "ALL")
tablaImplantLines[["MEX"]] = fig2Data("MEX", "ALL")


# bindig rows to get dataset to plot
tablaImplantLines[["HND"]] = bind_rows(
  "BASELINE" = tablaImplantLines[["HND"]]$BASELINE$resultadosMes,
  "36M" = tablaImplantLines[["HND"]]$`36M`$resultadosMes,
  "54M" = tablaImplantLines[["HND"]]$`54M`$resultadosMes,
  .id = "fup") %>% mutate(
    country = "HND",
    area = "ALL")

tablaImplantLines[["MEX"]] = bind_rows(
  "BASELINE" = tablaImplantLines[["MEX"]]$BASELINE$resultadosMes,
  "36M" = tablaImplantLines[["MEX"]]$`36M`$resultadosMes,
  .id = "fup") %>% mutate(
    country = "MEX",
    area = "ALL")


##### FIGURE 1 #####

# data frame
tablaImplantLines = do.call(rbind,tablaImplantLines)
rownames(tablaImplantLines) = seq(1,nrow(tablaImplantLines),1)

# preparing data
data = tablaImplantLines %>% dplyr::mutate(
  fup =  case_when(
    fup == "BASELINE" ~ "Baseline",
    fup == "36M" ~ "2nd. follow-up",
    fup == "54M" ~ "3rd. follow-up"
  ),
  country = case_when(
    country == "HND" ~ "Honduras",
    country == "MEX" ~ "Mexico (Chiapas)"
  )
) %>% mutate(
  fup = factor(fup, levels = c("Baseline", "2nd. follow-up", "3rd. follow-up")),
  MONTH = rev(MONTH)
)

data$country <- as.factor(data$country)
data$area <- as.factor(data$area)
data$fup <- as.factor(data$fup)
data$MONTH = rev(data$MONTH)

# Create the faceted line plot
plot = ggplot(data, aes(x = MONTH, y = VALUE, group = fup, color = fup)) +
  geom_line() +
  facet_grid(. ~ country) +  
  labs(
    title = "Percentage of implant use as a contraceptive method in the 24 months prior to the survey, by month and country.",
    x = "Month",
    y = "Percentage of implant use",
    color = "Follow-up"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12), 
    axis.text.y = element_text(size = 5),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold"),
    panel.spacing.y = unit(1, "cm"),
    panel.spacing = unit(1, "lines")
  ) +
  scale_x_discrete(limits = as.character(1:24))

# print
windows()
print(plot)

