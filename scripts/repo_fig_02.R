library(glue)
library(ggplot2)
library(dplyr)
library(glue)
library(haven)
library(survey)

#setwd("Analysis/Question #1/Repositorio")

# load functions
source("functions/fig2Data.R")
source("functions/exportFigure.R")

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
  geom_line(linewidth = 0.65) +
  facet_grid(. ~ country, labeller = labeller(country = c("Honduras" = "Selected areas of\nHonduras", "Mexico (Chiapas)" = "Selected areas of\nChiapas, Mexico"))) +
  labs(
    title = NULL,
    x = "Month of retrospective reference period",
    y = "Percentage of implant use",
    color = "Follow-up"
  ) +
  theme_minimal(base_family = "Arial", base_size = 10) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    axis.text = element_text(size = 9, colour = "black"),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold"),
    panel.spacing.y = unit(1, "cm"),
    panel.spacing = unit(1, "lines")
  ) +
  scale_x_continuous(breaks = c(1, 6, 12, 18, 24)) +
  scale_color_manual(values = c("#333333", "#0072B2", "#D55E00")) +
  theme(legend.position = "bottom", strip.text = element_text(size = 10, face = "bold"))

# print
exportFigure("Fig2.tiff", function() print(plot), height = 4.5)

