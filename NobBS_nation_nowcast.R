rm(list = setdiff(ls(), c("nowcast_date", "nowcast_list", "NobBs_stan_model","denv_org", "subpop_set")))
gc()

library(tidyverse)
library(lubridate)

denv <- denv_org

# Pick historical data for nowcast if desired
denv <- filter(denv, report_wk <= nowcast_date)

#Analyze on the national scale:
denv$subpopulation = "Nation"

## Nowcasts ####
source("nowcast_functions.R")
nowcast_samples = 200

## Values for dynamic parameter selection - USER MUST SET!
params <- find_delay_params(
  df = denv,
  report_date = report_wk,
  delay_date = onset_wk,
  cutpoint = 0.95,
  delay_mult = 1,
  wind_mult = 1
)

## Run nowcast
test_now <- now_cast(
  data = denv,
  delay_date_name = "onset_wk",
  report_date_name = "report_wk",
  max_D = round(params$max_delay),
  moving_window = round(params$window),
  now = nowcast_date
)

saveRDS(test_now, paste0("output/nation_default_nowcast_", nowcast_date, ".rds"))
