library(tidyverse)
library(lubridate)

## Loading sample data ####
denv_org <- read.csv("data/denv_sample_dat.csv")
denv_org <- denv_org %>%
  mutate(report_wk = as.Date(report_wk, "%m/%d/%Y"),
         onset_wk = as.Date(onset_wk, "%m/%d/%Y"))

## Compile the stan code to run NobBs####
NobBs_stan_model <- rstan::stan_model(file = "NobBS.stan")

## Running nowcast ####
nowcast_date = as.Date("2024-12-28")
  
## NobBS
#National nowcast:
source("NobBS_nation_nowcast.R")
# Serotype nowcast:
subpop_set = "Serotype"
source("NobBS_subpopulation_nowcast.R")
# Health region nowcast:
subpop_set = "Health Region"
source("NobBS_subpopulation_nowcast.R")
  
## Baseline
#Distribution of difference in cases model:
source("baseline_nation_nowcast.R")
#Serotype nowcast:
subpop_set = "Serotype"
source("baseline_subnation_nowcast.R")
#Health region nowcast:
subpop_set = "Health Region"
source("baseline_subnation_nowcast.R")
