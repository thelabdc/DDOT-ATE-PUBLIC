# read in packages 
library(tidyverse)
library(broom)
library(estimatr)
library(boxr)
library(here)
library(janitor)
library(MASS)
library(stargazer)
library(texreg)
library(lubridate)
library(sandwich)

# Data sources for calculating p-values 
citation_summary_pretreat <- read_csv("data/citation_summary_pretreat.csv")
crash_data_pretreat <- read_csv("data/crash_data_pretreat.csv")

# Data sources for summary statistics and regression analysis
ate <- read_csv("data/ate.csv")

ate$mailer <- ifelse(ate$assignment == "Mailer",1,0)
ate$sms <- ifelse(ate$assignment == "Text",1,0)
ate$both <- ifelse(ate$assignment == "Both",1,0)

# Eliminate "make" categories of make that are too small to get rid of rank deficiency in interacted design matrix 
ate <- ate |> group_by(make_cleaned)|> mutate(make_count = n())|>
  mutate(make_cleaned = ifelse(make_count < 100, "Small Group", make_cleaned))
ate$make_count <- NULL


pretreat_citation <- read_csv("data/pretreat_citation.csv")
pretreat_crash <- read_csv("data/pretreat_crash.csv")

# colors for plots 
lab_blue     <- "#2B4888"
lab_red      <- "#CF3A4B"
lab_darkgreen      <- "#1e5430"

cb_red <- "#CC6677"
cb_purple <- "#332288"
cb_green <- "#009E73"