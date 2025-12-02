rm(list = setdiff(ls(), c("nowcast_date", "nowcast_list", "NobBs_stan_model", "denv_org", "subpop_set")))
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
  return(list(sum_post = baseline_df, nowcast_post = forecasts))
}

denv = denv_org

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


## Nowcasts ####
source("nowcast_functions.R")
nowcast_samples = 200

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
  #Samples of case data 52 weeks ago (Please change to your data accordingly)
  case_counts <- denv_subpopulation %>%
    group_by(onset_wk) %>%
    summarise(cases = n())
  # Create complete date sequence
  date_range <- range(denv_subpopulation$onset_wk, nowcast_date)
  full_dates <- data.frame(onset_wk = seq(date_range[1], date_range[2], 7))
  # Join with counts
  case_counts_full <- full_dates %>%
    left_join(case_counts, by = "onset_wk") %>%
    mutate(cases = replace_na(cases, 0))
  observed = rpois(52 - params$window, tail(case_counts_full$cases, params$window)[1])
  #Nowcast model run
  nowcast_baseline = baseline_forecast(observed = observed, horizon = params$window)
  
  test_now_all_subpopulation_l = c(test_now_all_subpopulation_l, 
    list(
    list(estimates = data.frame(
    estimate = nowcast_baseline$sum_post$`0.5`,
    lower_95 = nowcast_baseline$sum_post$`0.05`, upper_95 = nowcast_baseline$sum_post$`0.975`,
    lower_50 = nowcast_baseline$sum_post$`0.025`, upper_50 = nowcast_baseline$sum_post$`0.75`,
    subpopulation = all_subpopulation[i],
    delay_date = tail(case_counts_full$onset_wk, params$window), 
    n.reported = tail(case_counts_full$cases, params$window)
  ), nowcast.post.samps = list(nowcast = nowcast_baseline$nowcast_post)
  )
  )
  )
}
saveRDS(test_now_all_subpopulation_l, paste0("output/", subpop_set, "_baseline_nowcast_", nowcast_date, ".rds"))