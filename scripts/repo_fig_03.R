library(glue)
library(ggplot2)
library(dplyr)
library(stringr)
library(haven)
library(survey)

# load functions
source("functions/fig3Data.R")
source("functions/replacementData.R")

# list to store data
tablaImplantLines = list()

# calculating indicators
tablaImplantLines[["HND"]] = fig3Data("HND", "ALL")
tablaImplantLines[["MEX"]] = fig3Data("MEX", "ALL")

resultadosFinalesHND = list(
  "ALL" = tablaImplantLines$HND)

resultadosFinalesMEX = list(
  "ALL" = tablaImplantLines$MEX)


HND = replacementData("HND", resultadosFinalesHND)
MEX = replacementData("MEX", resultadosFinalesMEX)

# binding data
tablaReemplazos = union_all(
  HND,
  MEX
)


##### GRAFICO 3 #####

# preparing data 
tablaReemplazos$METHOD[tablaReemplazos$METHOD=="None of everything"] = "No method"
tablaReemplazos$METHOD[tablaReemplazos$METHOD=="I am all in the rhythm"] = "Rhythm method"
tablaReemplazos$METHOD[tablaReemplazos$METHOD=="Mã © all of breastfeeding amenorrhea (mela)"] = "Breastfeeding amenorrhea (mela)"
tablaReemplazos$METHOD[tablaReemplazos$METHOD=="Another, all traditional"] = "Other traditional method"

# Create the data frame
data = tablaReemplazos %>%
  dplyr::filter(area == "All") %>%
  group_by(country, FUP) %>%
  mutate(METHOD = factor(METHOD, levels = METHOD[order(VALUE)])) %>%
  ungroup() %>%
  # Recode the follow-up labels
  mutate(FUP = recode(FUP,
                      "FUP 36M" = "2nd follow-up",
                      "FUP 54M" = "3rd follow-up",
                      "BASELINE" = "Baseline"),
         country = recode(country,
                          "HND"="Honduras",
                          "MEX"="Chiapas"),
         # Set the order of follow-up periods
         FUP = factor(FUP, levels = c("Baseline", "2nd follow-up", "3rd follow-up")))

data$METHOD = str_replace_all(data$METHOD,"He doesn't know","She doesn't know")
data$METHOD = str_replace_all(data$METHOD,"don't know","She doesn't know")
data$METHOD = str_replace_all(data$METHOD,"He refused to answer","She refused to answer")
data$METHOD = str_replace_all(data$METHOD,"sponge, spermicide","Sponge, spermicide")

plot = ggplot(data, aes(x = VALUE, y = reorder(METHOD, VALUE))) +
  geom_bar(stat = "identity", fill = "#2E86C1") +
  geom_text(aes(label = sprintf("%.1f%%", VALUE), 
                x = VALUE, 
                hjust = ifelse(VALUE >= 0, -0.2, 1.2)), 
            size = 3) +
  facet_grid(country ~ FUP, scales = "free_y") +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 9),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold"),
    panel.spacing.y = unit(1,"cm")
  ) +
  labs(
    x = "Value (%)",
    y = "Method"
  ) +
  # Extend x-axis to make room for labels
  scale_x_continuous(expand = expansion(mult = c(0.1, 0.15)))

windows()
print(plot)

