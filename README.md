# DDOT-ATE-PUBLIC

@authors: [Alyssa Huberts](https://thelabprojects.dc.gov/alyssa-huberts), [Nathan Dignazio](https://thelabprojects.dc.gov/nathan-dignazio), [Ryan T. Moore](https://thelabprojects.dc.gov/ryan-moore)

## Introduction

This repo contains preliminary code for the project "Can Targeting Messages to High-Risk Drivers Prevent Crashes?" [(Project Page)](https://thelabprojects.dc.gov/high-risk-drivers)

The analysis was pre-registered on the Open Science Framework (OSF) 
[here](https://osf.io/5pufn/). 


#### Requirements:

For the simulation code, relevant packages are listed at the beginning of the script. They are reproduced here:

```
library(broom)
library(estimatr)
library(here)
library(randomizr)
library(tidyverse)
```

## Organization

Currently, this repo has one primary directory, with plans to include the following subdirectories:

- `code`
  - `01_pre-analysis`
  - `02_predictive model`
  - `03_randomization`
  - `04_evaluation`

Additionally, this repository includes `README`, `.gitignore`, and `.Rproj` 
files.

## `/code/pre-analysis`

This directory includes these files:
 `01_simulated-multiple-comparisons-correction.r`
- Takes in:
    - Summary of pre-treatment citations data for the study sample of drivers
- What it does:
    - Uses simulation to identify the correct alpha to use as a threshold for significance at the 95% level when correcting for the pre-registered 12-test citation family. 
- Outputs:
    - 0.004704705, the simulated alpha with 20,000 simulations  (Calculated 2023-11-17)

### Sensitive data

Several data sets are are _not_ included in this repository due to the sensitive nature of the data.  They include:

- **citation_summary**: This dataset contains summary information on our four main citation outcomes in the pre-treatment period:
  -  risky citations at 3 months (November 2022 - February 2023): red light citations and speeding > 15 mph above the speed limit 
  -  risky citations at 12 months (February 2022 - February 2023): red light citations and speeding > 15 mph above the speed limit
  -  total citations at 3 months (November 2022 - February 2023)
  -  total citations at 12 months (February 2022 - February 2023) 
