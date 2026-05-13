library(DiagrammeR)
library(magrittr)
library(DiagrammeRsvg)
library(xml2)
library(glue)
library(dplyr)
library(haven)

# load functions
source("functions/fig1Data.R")

tablaImplantLines = list()

# get values
tablaImplantLines[["HND"]] = fig1Data("HND", "ALL")
tablaImplantLines[["MEX"]] = fig1Data("MEX", "ALL")

dataFig1 = do.call(rbind,tablaImplantLines) %>% 
  mutate(REG_NOTM = NTOT - REG_MARRIED,
         REG_NOTINNEED = REG_MARRIED - REG_INNEED)


# Defining flowchart
combined_graph_definition <- glue("
graph TD
    
    A[Women surveyed] --> B[Honduras]
    A --> C[Chiapas]
    
    B --> D[Baseline<br/>n = {dataFig1[[1,4]]}]
    B --> E[2nd. follow-up<br/>n = {dataFig1[[2,4]]}]
    B --> F[3rd. follow-up<br/>n = {dataFig1[[3,4]]}]

    C --> G[Baseline<br/>n = {dataFig1[[4,4]]}]
    C --> H[2nd. follow-up<br/>n = {dataFig1[[5,4]]}]
    
    D --> I[Married or partnered<br/>n = {dataFig1[[1,5]]}]
    D --> J[Not married or partnered<br/>n = {dataFig1[[1,7]]}]
    
    I --> K[In need of contraception<br/>n = {dataFig1[[1,6]]}]
    I --> L[Not in need of contraception<br/>n = {dataFig1[[1,8]]}]
    
    E --> M[Married or partnered<br/>n = {dataFig1[[2,5]]}]
    E --> N[Not married or partnered<br/>n = {dataFig1[[2,7]]}]
    
    M --> O[In need of contraception<br/>n = {dataFig1[[2,6]]}]
    M --> P[Not in need of contraception<br/>n = {dataFig1[[2,8]]}]
    
    F --> Q[Married or partnered<br/>n = {dataFig1[[3,5]]}]
    F --> R[Not married or partnered<br/>n = {dataFig1[[3,7]]}]
    
    Q --> S[In need of contraception<br/>n = {dataFig1[[3,6]]}]
    Q --> T[Not in need of contraception<br/>n = {dataFig1[[3,8]]}]
    
    
    G --> U[Married or partnered<br/>n = {dataFig1[[4,5]]}]
    G --> V[Not married or partnered<br/>n = {dataFig1[[4,7]]}]
    
    U --> W[In need of contraception<br/>n = {dataFig1[[4,6]]}]
    U --> X[Not in need of contraception<br/>n = {dataFig1[[4,8]]}]
    
    H --> Y[Married or partnered<br/>n = {dataFig1[[5,5]]}]
    H --> Z[Not married or partnered<br/>n = {dataFig1[[5,7]]}]
    
    Y -->  AA[In need of contraception<br/>n = {dataFig1[[5,6]]}]
    Y -->  AB[Not in need of contraception<br/>n = {dataFig1[[5,8]]}]
    
    style A fill:#f0f0f0,stroke:#636363
    style B fill:#f0f0f0,stroke:#636363
    style C fill:#f0f0f0,stroke:#636363
    
    style D fill:#ffffbf,stroke:#636363
    style E fill:#fc8d59,stroke:#636363
    style F fill:#91bfdb,stroke:#636363

    style G fill:#ffffbf,stroke:#636363
    style H fill:#fc8d59,stroke:#636363
    
    style I fill:#e5f5e0,stroke:#636363
    style K fill:#e5f5e0,stroke:#636363
    style M fill:#e5f5e0,stroke:#636363
    style O fill:#e5f5e0,stroke:#636363
    style Q fill:#e5f5e0,stroke:#636363
    style S fill:#e5f5e0,stroke:#636363
    style U fill:#e5f5e0,stroke:#636363
    style W fill:#e5f5e0,stroke:#636363
    style Y fill:#e5f5e0,stroke:#636363
    style AA fill:#e5f5e0,stroke:#636363
    
    style J fill:#fee0d2,stroke:#636363
    style L fill:#fee0d2,stroke:#636363
    style N fill:#fee0d2,stroke:#636363
    style P fill:#fee0d2,stroke:#636363
    style R fill:#fee0d2,stroke:#636363
    style T fill:#fee0d2,stroke:#636363
    style V fill:#fee0d2,stroke:#636363
    style X fill:#fee0d2,stroke:#636363
    style Z fill:#fee0d2,stroke:#636363
    style AB fill:#fee0d2,stroke:#636363
")

graph = mermaid(combined_graph_definition)

# render graph
htmlwidgets::saveWidget(graph, "flowchart.html", selfcontained = TRUE)
Sys.sleep(3)
browseURL("flowchart.html")
Sys.sleep(3)
unlink("flowchart.html")

