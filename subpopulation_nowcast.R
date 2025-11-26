rm(list = setdiff(ls(), c("nowcast_date", "nowcast_list", "NobBs_stan_model","denv_org", "subpop_set")))
gc()

library(tidyverse)
library(lubridate)
library(readxl)

denv = denv_org

##Define the nowcast date####
#nowcast_date = max(denv$report_wk)
## pick historical data for nowcast if desired
denv <- filter(denv, report_wk <= nowcast_date)

#Subpopulation
if(subpop_set == "Serotype"){
  #Number of serotype:
  all_subpopulation = 1:4
  denv = rename(denv, subpopulation = serotype) %>% filter(subpopulation %in% all_subpopulation)
} else if(subpop_set == "Health Region"){
  #Number of health region:
  all_subpopulation = unique(denv$health_region);all_subpopulation = sort(all_subpopulation[!is.na(all_subpopulation)])
  denv = rename(denv, subpopulation = health_region)  %>% filter(subpopulation %in% all_subpopulation)
}

## Nowcasts
source("nowcast_functions.R")
nowcast_samples = 200

###Independent run####
test_now_all_subpopulation_l = c()
for(i in 1:length(all_subpopulation)){
  denv_subpopulation = filter(denv, subpopulation == all_subpopulation[i])
  
  params <- find_delay_params(
    df = denv_subpopulation,
    report_date = report_wk,
    delay_date = onset_wk,
    cutpoint = 0.95,
    delay_mult = 1,
    wind_mult = 1
  )
  
  ## Run nowcast
  test_now <- now_cast(
    data = denv_subpopulation,
    delay_date_name = "onset_wk",
    report_date_name = "report_wk",
    max_D = params$max_delay,
    moving_window = params$window,
    now = nowcast_date
  )
  test_now_all_subpopulation_l = c(test_now_all_subpopulation_l, list(test_now))
}

saveRDS(test_now_all_subpopulation_l, file = paste0("output/",subpop_set,"_ind_nowcast_", nowcast_date, ".rds"))

###Shared parameters model ####
params = c(quants = 0.95, max_delay = 0, window = 0)
for(i in 1:length(all_subpopulation)){
  denv_subpopulation = filter(denv, subpopulation == all_subpopulation[i])
  
  params_region <- find_delay_params(
    df = denv_subpopulation,
    report_date = report_wk,
    delay_date = onset_wk,
    cutpoint = 0.95,
    delay_mult = 1,
    wind_mult = 1
  )
  
  if(params["max_delay"] < params_region["max_delay"]) params["max_delay"] = params_region["max_delay"]
  if(params["window"] < params_region["window"]) params["window"] = params_region["window"]
}

## Run nowcast
test_now <- now_cast(
  data = denv,
  delay_date_name = "onset_wk",
  report_date_name = "report_wk",
  max_D = params$max_delay,
  moving_window = params$window,
  now = nowcast_date
)

saveRDS(test_now, file = paste0("output/",subpop_set,"_shared_nowcast_", nowcast_date, ".rds"))