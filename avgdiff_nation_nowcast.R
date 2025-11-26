rm(list = setdiff(ls(), c("nowcast_date", "nowcast_list", "NobBs_stan_model", "AR1_poisson_stan_model","denv_org")))
gc()

library(tidyverse)

#method from https://www.pnas.org/doi/suppl/10.1073/pnas.2113561119
baseline_forecast <- function(observed, horizon = 4, num_samples = 100000, quantile_levels = c(0.05, 0.025, 0.5, 0.75, 0.975)) {
  if (length(observed) < 2) stop("Need at least two observations.")
  
  last_obs <- tail(observed, 1)
  diffs <- diff(observed)
  diffs_symm <- c(diffs, -diffs)
  
  # Create smooth piecewise linear approximation of empirical CDF (i.e., sampling weights)
  probs <- seq(0, 1, length.out = length(diffs_symm))
  sorted_diffs <- sort(diffs_symm)
  
  # Sample h steps ahead by summing h i.i.d. differences
  sampled_paths <- replicate(horizon, sample(sorted_diffs, num_samples, replace = TRUE))
  cum_diffs <- t(apply(sampled_paths, 1, cumsum))  # Each row is a trajectory of h steps
  
  forecasts <- sweep(cum_diffs, 1, last_obs, FUN = "+")
  
  # Truncate to non-negative
  forecasts[forecasts < 0] <- 0
  
  # Compute quantiles for each horizon
  quantiles_by_horizon <- lapply(1:horizon, function(h) {
    q <- quantile(forecasts[, h], probs = quantile_levels, names = TRUE)
    q[names(q) == "50%"] <- last_obs  # Force median to be last observed value
    return(data.frame(horizon = h, matrix(q, nrow = 1)))
  })
  
  baseline_df <- do.call(rbind, quantiles_by_horizon)
  colnames(baseline_df)[-1] = quantile_levels
  rownames(baseline_df) <- NULL
  return(baseline_df)
}

denv = denv_org
##Define the nowcast date####
#nowcast_date = max(denv$report_wk)
## pick historical data for nowcast if desired
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

##nowcast
current_year = year(nowcast_date)
denv_sel_year <-  denv %>% filter(report_yr %in% (current_year-1):current_year) 
case_counts <- denv_sel_year %>%
  group_by(report_wk) %>%
  summarise(cases = n())
# Create complete date sequence
full_dates <- tibble(report_wk = lapply((current_year - 1):current_year, function(x) {
  date_range = range(filter(denv_sel_year, report_yr == x)$report_wk)
  seq(date_range[1], date_range[2], 7)
}) %>% unlist %>% as.Date()) %>% tail(52 + params$window)

# Join with counts
case_counts_full <- full_dates %>%
  left_join(case_counts, by = "report_wk") %>%
  mutate(cases = replace_na(cases, 0))

case_counts_full_train = head(case_counts_full$cases, - params$window)

nowcast_baseline = baseline_forecast(observed = case_counts_full_train, horizon = params$window)

test_now = list(estimates = data.frame(
  estimate = nowcast_baseline$`0.5`,
  lower_95 = nowcast_baseline$`0.05`, upper_95 = nowcast_baseline$`0.975`,
  lower_50 = nowcast_baseline$`0.025`, upper_50 = nowcast_baseline$`0.75`,
  subpopulation = "Nation",
  delay_date = tail(case_counts_full$report_wk, params$window), n.reported = tail(case_counts_full$cases, params$window)
)) %>% 
  saveRDS(paste0("output/nation_baseline_nowcast_", nowcast_date, ".rds"))
