# Data and scripts from paper "Introduction of contraceptive implants as a family planning strategy in Honduras and Chiapas: Results from the Salud Mesoamerica Initiative (SMI)"

This repository contains the complete analytical pipeline, data, and R scripts required to reproduce the statistical findings, estimates, and visualizations presented in the associated research paper.

## Overview
The project is structured as a fully automated workflow. By executing the master script, users can transition from data inputs to the final tables and figures used in the manuscript. All estimations and analyses were implemented using the R Programming Language.

## Repository Structure
The directory is organized to maintain a strict separation between data inputs, processing logic, and outputs:

- [data/](https://github.com/agsantoro/SMIimplants/tree/main/data): source subsets of data required to perform each table or figure identified by this structure: {COUNTRY}_{FOLLOW-IP}_{SURVEY}_{MODULE}.rda

- [functions/](https://github.com/agsantoro/SMIimplants/tree/main/functions): R functions to prepare data.

- [scripts/](https://github.com/agsantoro/SMIimplants/tree/main/scripts): R scripts to display outputs.

- [RUN_PROCESS.R](https://github.com/agsantoro/SMIimplants/blob/main/RUN_PROCESS.R): the master execution script located in the root directory.


## Instructions for reproduction

To replicate the study results, follow the steps below:

1. Clone or Download this repository to your local environment.

2. Ensure that R is installed.

3. Install the required library dependencies.

4. Run this command in R console to visualize tables and figures in popup windows.

`
source("RUN_PROCESS.r")
`

Note: The RUN_PROCESS.r script manages the sequential execution of all modular scripts and prints it on IDE viewer. We recommend using RStudio as your IDE.

## Statistical methodology

The scripts in this repository allow you to view the following tables and figures, along with the procedures for generating the estimates they contain::

- [Figure 1](https://github.com/agsantoro/SMIimplants/blob/main/scripts/repo_fig_01.R): Flowchart of eligibility for “women in need of contraception”.

- [Figure 2](https://github.com/agsantoro/SMIimplants/blob/main/scripts/repo_fig_02.R): Percentage of implant use as a contraceptive method in the 24 months prior to the survey, by month and country.

- [Figure 3](https://github.com/agsantoro/SMIimplants/blob/main/scripts/repo_fig_03.R): Replacement of contraceptives by Implants at each follow-up, and by country.

- [Table 1](https://github.com/agsantoro/SMIimplants/blob/main/scripts/repo_table_01.R): Percentage of use of contraceptive methods by country and follow-up, at start and end of 24-month period prior to the survey.

- [Table 2](https://github.com/agsantoro/SMIimplants/blob/main/scripts/repo_table_02.R): Implant use by socioeconomic characteristics, follow-up and country

- [Table 3](https://github.com/agsantoro/SMIimplants/blob/main/scripts/repo_table_03.R): Implant use by socioeconomic characteristics, follow-up. Pooled data.

## Citation

If you utilize these materials in your research, please cite the original publication.

