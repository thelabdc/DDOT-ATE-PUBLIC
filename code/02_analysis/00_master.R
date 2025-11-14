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

# read in data 
ate <- read_csv("data/ate.csv")

# colors for plots 
lab_blue     <- "#2B4888"
lab_red      <- "#CF3A4B"
lab_darkgreen      <- "#1e5430"

cb_red <- "#CC6677"
cb_purple <- "#332288"
cb_green <- "#009E73"