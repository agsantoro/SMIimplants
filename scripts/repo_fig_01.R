library(glue)
library(dplyr)
library(haven)
library(grid)
source("functions/fig1Data.R")
source("functions/exportFigure.R")
dataFig1 = bind_rows(fig1Data("HND", "ALL"), fig1Data("MEX", "ALL")) %>%
  mutate(REG_NOTM = NTOT - REG_MARRIED,
         REG_NOTINNEED = REG_MARRIED - REG_INNEED)
# Five panels retain the original counts and eligibility branches.
drawFlowchart = function() {
  grid.newpage()
  for (i in seq_len(nrow(dataFig1))) {
    r = dataFig1[i, ]
    pushViewport(viewport(x = 0.5, y = 1 - (i - 0.5) / 5,
                          width = 0.96, height = 0.19))
    area = if (r$COUNTRY == "HND") "Selected areas of Honduras" else "Selected areas of Chiapas, Mexico"
    round = c(BASELINE = "Baseline", `36M` = "Second follow-up", `54M` = "Third follow-up")[[r$FUP]]
    grid.text(paste(area, round, sep = " | "), x = 0.01, y = 0.94,
              just = "left", gp = gpar(fontfamily = "Arial", fontsize = 10, fontface = "bold"))
    box = function(x, y, label, fill) {
      grid.rect(x, y, width = 0.29, height = 0.25,
                gp = gpar(fill = fill, col = "#555555", lwd = 0.7))
      grid.text(label, x, y, gp = gpar(fontfamily = "Arial", fontsize = 9))
    }
    arrowLine = function(x0,y0,x1,y1) grid.lines(c(x0,x1), c(y0,y1),
      arrow = arrow(length = unit(0.07,"inches")), gp = gpar(col = "#555555"))
    box(0.16,0.64,paste0("Women surveyed\nn = ",r$NTOT),"#eeeeee")
    box(0.50,0.64,paste0("Married or partnered\nn = ",r$REG_MARRIED),"#e5f5e0")
    box(0.84,0.64,paste0("In need of contraception\nn = ",r$REG_INNEED),"#e5f5e0")
    box(0.50,0.20,paste0("Not married or partnered\nn = ",r$REG_NOTM),"#fee0d2")
    box(0.84,0.20,paste0("Not in need of contraception\nn = ",r$REG_NOTINNEED),"#fee0d2")
    arrowLine(0.305,0.64,0.355,0.64)
    arrowLine(0.645,0.64,0.695,0.64)
    grid.lines(c(.16,.16,.355),c(.515,.20,.20),arrow=arrow(length=unit(.07,"inches")),gp=gpar(col="#555555"))
    grid.lines(c(.66,.66,.695),c(.64,.20,.20),arrow=arrow(length=unit(.07,"inches")),gp=gpar(col="#555555"))
    popViewport()
  }
}
exportFigure("Fig1.tiff", drawFlowchart, height = 8.5)
