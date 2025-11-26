library(tidyverse)
library(lubridate)
library(readxl)

## Loading PR data ####
source("collect_latest_data.R")
denv_org <- collect_latest_data(exclude_latest_incomplete_week = T)

## Compile the stan code to run NobBs####
NobBs_stan_model <- rstan::stan_model(file = "NobBS.stan")

## Different running time frame ####
nc_years = c(2009, 2010, 2013, 2024) #Years of nowcast
nc_dates_range = lapply(nc_years, function(x) ceiling_date(as.Date(paste0(x, c("-01-01", "-12-31"))), unit = "week", week_start = 7) - days(1))
nowcast_list = nc_dates_range %>% lapply(function(x) seq(x[1], x[2], 7)) %>% unlist %>% as.Date
nowcast_list = nowcast_list[(nowcast_list %>% year) %in% nc_years]

for(date in 1:length(nowcast_list)){
  nowcast_date = as.Date(nowcast_list[date])
  print(nowcast_date)
  #National nowcast:
  source("nation_nowcast.R")
  # # Serotype nowcast:
  # subpop_set = "Serotype"
  # source("subpopulation_nowcast.R")
  # #Health region nowcast:
  # subpop_set = "Health Region"
  # source("subpopulation_nowcast.R")
  
  # #Distribution of difference in cases model:
  # source("avgdiff_nation_nowcast.R")
  # Serotype nowcast:
  # subpop_set = "Serotype"
  # source("avgdiff_subnation_nowcast.R")
  # #Health region nowcast:
  # subpop_set = "Health Region"
  # source("avgdiff_subnation_nowcast.R")
}
