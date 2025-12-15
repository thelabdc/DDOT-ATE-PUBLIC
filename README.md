# DDOT-ATE-PUBLIC

@authors: [Alyssa Huberts](https://thelabprojects.dc.gov/alyssa-huberts), [Nathan Dignazio](https://thelabprojects.dc.gov/nathan-dignazio), [Ryan T. Moore](https://thelabprojects.dc.gov/ryan-moore) 

## Introduction

This repo contains replication code for the project "Can Targeting Messages to High-Risk Drivers Prevent Crashes?" [(Project Page)](https://thelabprojects.dc.gov/high-risk-drivers)

The analysis was pre-registered on the Open Science Framework (OSF) 
[here](https://osf.io/5pufn/). 

You can find the final report (preprint) [here](https://static1.squarespace.com/static/5d2361aa11fed60001f7ab3a/t/683e00c440269358f4910164/1748893893685/ATE+Risky+Driver+Evaluation+Report+-+June+2025.pdf) and the published version in [Transportation Research Record](https://journals.sagepub.com/doi/10.1177/03611981251347294)


#### Requirements:

All relevant packages are listed at the beginning of the [00_master](https://github.com/thelabdc/DDOT-ATE-PUBLIC/blob/main/code/00_master.R) script. They are reproduced here:

```
library(tidyverse)
library(broom)
library(estimatr)
library(here)
library(janitor)
library(MASS)
library(stargazer)
library(texreg)
library(lubridate)
library(sandwich)
library(randomizr)

The data contains PII and we do not include these data in the repo. Instead, one can reproduce using the following directory structure for internal files: 
- `data`
  - `ate`: aggregated data for the number of risky and total citations for plates in the study, at 3 months and 12 months. 
  - `preanalysis_citation_summary_pretreat` Summary of pretreatment outcomes for study plates used for pre-analysis/multiple hypothesis calculations
  - `preanalysis_crash_data_pretreat` Pretreatment crash data from January 2016 - April 2022, used for pre-analysis/multiple hypothesis calculations
  - `pretreat_citation`: Pre-treatment citations outcomes, used in summary statistics
  - `pretreat_crash`: Pre-treatment crash outcomes, used in summary statistics
  - `ps-from-simulations-20000-sims-citations`: p values generated in 01_simulated-multiple-comparisons-correction-citations.r
  - `ps-from-simulations-20000-sims-crash`: p values generated in 01_simulated-multiple-comparisons-correction-crash.r


```

## Organization


- `code`
  - `[00_master.r](https://github.com/thelabdc/DDOT-ATE-PUBLIC/blob/main/code/00_master.R)`  
  - `01_preanalysis`
  - `02_analysis`

Additionally, this repository includes `README`, `.gitignore`, and `.Rproj` 
files, and the figures and tables from the paper. 

## `00_master.r`
- What it does:
    - Reads in data files and sets parameters like color for plots 

## `/code/01_pre-analysis`

This directory includes these files:
 `01_simulated-multiple-comparisons-correction-citations.r`
- Takes in:
    - Summary of pre-treatment citations data for the study sample of drivers
- What it does:
    - Uses simulation to identify the correct alpha to use as a threshold for significance at the 95% level when correcting for the pre-registered 12-test citation family. 
- Outputs:
    - 0.004704705, the simulated alpha with 20,000 simulations  (Calculated 2023-11-17)
 
   `02_simulated-multiple-comparisons-correction-crash.r`
- Takes in:
    - Summary of pre-treatment crash data for the study sample of drivers
- What it does:
    - Uses simulation to identify the correct alpha to use as a threshold for significance at the 95% level when correcting for the pre-registered 12-test citation family. 
- Outputs:
    - 0.0171, the simulated alpha with 20,000 simulations  (Calculated 2024-11-04)

## `/code/02_analysis`

This directory includes these files:
 `01_summary_statistics.r`
- Takes in: 
    - ate.csv (experiment data)
    - pretreat_citation (pretreatment data on citation outcome)
    - pretreat_crash (pretreatment data on crash outcome)
    
- What it does:
    - Makes summary of covariate balance summary of pre-treatment outcomes for paper.

- Outputs: 
  - tables/summary/balance_nomatch.csv
  - tables/summary/balance_match.csv
  - tables/summary/pretreat_outcome_balance_match.csv
  
`02_analysis_confirmatory.r`
- Takes in: 
  - ate.csv
  
- What it does:
    - Runs all confirmatory regressions for the paper.

- Outputs: 
    - Main coefficient plots for the paper's body: figs/confirmatory_mailer.png,  figs/confirmatory_matched.png
    - Main regression tables for the paper's appendix: tables/regression_results/confirmatory_wholesample.doc, tables/regression_results/confirmatory_matched_sample.docx
    
`03_analysis_exploratory.r`
- Takes in: 
    - ate.csv
    
- What it does:
    - Writes a function to generate output for main regressions on a subset of the data.
    - Creates tables with this output.
    - Creates box and whisker plots for the state level heterogenous effects and effects by risk tercile. 
    
Outputs: 
    - All tables in tables/het
    - figs/exploratory_risk_tercile_any.png, figs/exploratory_state_mailer.png, figs/exploratory_state_matched.png, figs/exploratory_state_any.png, figs/exploratory_risk_tercile_mailer.png, figs/exploratory_risk_tercile_match.png, figs/exploratory_risk_tercile_any.png
  
