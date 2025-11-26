require(rjags)
require(tidyverse)
require(rstan)
options(mc.cores = parallel::detectCores()) # checks number of cores without having later to specify the cores argument
rstan_options(auto_write = TRUE) # extended packages to use stan

### Find delays
find_delay_params <- function(df, report_date, delay_date, 
                              cutpoint, delay_mult, wind_mult, delay_min = 4, delay_max = 12) {
  
  parms <- df %>%
    mutate(
      delay = difftime({{ report_date }}, {{ delay_date }}, units = "week"), # Estimates difference between report and delay in weeks
      delay_wk = as.numeric(gsub("weeks", "", delay))
    ) %>%
    filter({{ report_date }} == max({{ report_date }})) %>%
    filter(delay_wk >= 0) %>%
    summarise(
      quants = cutpoint,
      delays = round(quantile(delay_wk, cutpoint))
    ) %>%
    mutate(
      delays = ifelse(delays < delay_min, delay_min, delays),
      delays = ifelse(delays > delay_max, delay_max, delays),
      max_delay = delays * delay_mult,
      offset = ifelse(wind_mult == 1, 1, 0),
      window = (max_delay * wind_mult) + offset
    ) %>%
    select(-delays, -offset)
  return(parms)
}

### Function for nowcast with dynamic params
now_cast <- function(data, delay_date_name, report_date_name, max_D, moving_window, now, n_nowcast_samples = 1e4) {
  
  data <- as.data.frame(data) %>%
    rename(delay_date = !!sym(delay_date_name), report_date = !!sym(report_date_name))
  
  if (is.na(moving_window)) moving_window <- NULL
  
  units <- "1 week"
  cutoff_D <- T
  proportion_reported <- 1
  
  specs <- list(
    dist = c("NB"),
    alpha1_mean_prior = 0,
    alpha1_prec_prior = 0.001,
    alphat_shape_prior = 0.001,
    alphat_rate_prior = 0.001,
    beta_priors = NULL,
    param_names = NULL,
    # conf=0.95,
    dispersion_prior = NULL
  )
  
  # Check that "now" is entered as a Date
  if (inherits(now, "Date") == FALSE) {
    stop("'Now' argument must be of datatype Date (as.Date)")
  }
  
  # create times variable for PR weeks
  times <- seq(sort(unique(data$delay_date))[1], now, by = units)
  
  # # Check that "now" is possible in the sequence of reporting data
  # if (dplyr::last(times) != now) {
  #   stop("The date `now` is not possible to estimate: the possible nowcast dates are seq(unique(data[,delay_date])[1],now,by=units).")
  # }
  
  # Print date
  message(paste("Computing a nowcast for", now))
  # Define "T", the length of dates between the first date of data and "now", making sure that "T" is unaffected by skipped-over dates in the time series
  # If the moving window is specified, "T" considers only the dates within the moving window; otherwise considers all historical data
  now.T <- ifelse(is.null(moving_window), length(times), moving_window)
  times <- times[(-now.T + 1):0 + length(times)]
  
  # Check the default arguments
  if (is.null(moving_window)) {
    moving_window <- now.T
  }
  if (is.null(max_D)) {
    max_D <- now.T - 1 # ifelse(is.null(moving_window),now.T-1,moving_window-1)
  }
  if (is.null(cutoff_D)) {
    cutoff_D <- TRUE
  }
  
  # Check that proportion_reported is between 0,1
  if (proportion_reported > 1 | proportion_reported <= 0) {
    stop("The proportion_reported must be a number between (0,1].")
  }
  
  # Manipulate the control arguments

  if (is.null(specs[["alpha1_mean_prior", exact = TRUE]])) {
    specs$alpha1_mean_prior <- 0
  }
  if (is.null(specs[["alpha1_prec_prior", exact = TRUE]])) {
    specs$alpha1_prec_prior <- 0.01
  }
  if (is.null(specs[["alphat_shape_prior", exact = TRUE]])) {
    specs$alphat_shape_prior <- 0.01
  }
  if (is.null(specs[["alphat_rate_prior", exact = TRUE]])) {
    specs$alphat_rate_prior <- 0.01
  }
  if (is.null(specs[["beta_priors", exact = TRUE]])) {
    specs$beta_priors <- rep(0.1, times = (max_D) + 1)
  }

  if (is.null(specs[["param_names", exact = TRUE]]) & (specs[["dist"]] == "NB")) {
    specs$param_names <- c("lambda", "alpha", "beta_logged", "tau2_alpha", "sum.n", "r")
  }

  if (is.null(specs[["dispersion_prior", exact = TRUE]]) & (specs[["dist"]] == "NB")) {
    specs$dispersion_prior <- c(0.01, 0.01)
  }
  
  if (max_D > (moving_window - 1)) {
    stop("Maximum delay cannot be greater than the length of the moving window minus 1 time unit")
  }
  
  # Prep the data: filter only to observable cases reported at or before "now"
  unit.num <- switch(units,
                     "1 day" = 1,
                     "1 week" = 7
  )
  w.days <- max(moving_window * unit.num, now.T * unit.num) # moving window converted to days
  
  realtime.data <- subset(data, (pull(data, delay_date) <= now) & 
                            (pull(data, delay_date) > (now - w.days)) & 
                            (pull(data, report_date) <= now) & 
                            (pull(data, report_date) > (now - w.days)))
  
  realtime.data$week.t <- match(realtime.data[, "delay_date"], times)
  realtime.data$delay <- match(realtime.data[, "report_date"], times) - realtime.data$week.t
  
  if (cutoff_D == FALSE) {
    realtime.data$delay <- ifelse(realtime.data$delay >= max_D, max_D, realtime.data$delay)
  }
  
  if (length(unique(realtime.data$week.t)) != now.T) {
    warning("Warning! The line list has zero case reports for one or more possible onset dates at one or more delays. Proceeding under the assumption that the true number of cases at the associated delay(s) and week(s) is zero.")
  }
  
  #Subpopulation
  l_subpopulation = unique(data$subpopulation) %>% sort
  n_subpopulation = length(l_subpopulation)
  
  # Build the reporting triangle, fill with 0 where unobservable
  reporting.triangle <- array(NA, dim = c(now.T, max_D + 1, n_subpopulation))
  for(h in 1:n_subpopulation){
    for (t in 1:now.T) {
      for (d in 0:max_D) {
        realtime.data.subpopulation = filter(realtime.data, subpopulation == l_subpopulation[h])
        reporting.triangle[t, (d + 1), h] <- 
          nrow(realtime.data.subpopulation[which(realtime.data.subpopulation$week.t == t & realtime.data.subpopulation$delay == d), ])
        if (now.T < (t + d)) {
          reporting.triangle[t, (d + 1), h] <- 0
        }
      }
    }
  }
  
  #Run stan model:
  dataList <- list(
    Today = now.T,
    D = max_D,
    n_subpopulation = n_subpopulation,
    n = reporting.triangle,
    alpha1_mean_prior = specs$alpha1_mean_prior,
    alpha1_prec_prior = specs$alpha1_prec_prior,
    alphat_rate_prior = specs$alphat_rate_prior,
    alphat_shape_prior = specs$alphat_shape_prior,
    beta_priors = specs$beta_priors,
    dispersion_prior_shape = specs$dispersion_prior[1],
    dispersion_prior_rate = specs$dispersion_prior[2],
    mu_beta = specs$beta_priors,
    var_beta = rep(1, max_D + 1))
    
  iter_m = 2000
  nowcastmodel <- sampling(
    object = NobBs_stan_model,
    data = dataList,
    chains = 4,
    iter = iter_m,
    thin = round((iter_m - iter_m/2)/1000*4)#,
    #control = list(adapt_delta = 0.99, max_treedepth = 12)
  )
  
  #generate nowcast:
  logged_lambda = extract(nowcastmodel, pars = c("logged_lambda"), permuted = T)$logged_lambda
  logged_lambda[logged_lambda > 10] = 10
  r = extract(nowcastmodel, pars = c("r"), permuted = T)$r
  r[r < 1e-99] = 1e-99
  n_samples = n_nowcast_samples
  sum_n = array(dim = c(n_samples, now.T, n_subpopulation))
  for (h in 1:n_subpopulation) {
    for (t in 1:now.T) {
      n_gen = matrix(nrow = n_samples, ncol = max_D + 1)
      for (d in 0:max_D) {
        if(d <= (max_D - t + 1)){
          n_gen[,d+1] = reporting.triangle[t, d+1, h]
        } else {
          if(any(is.na(rnbinom(n_samples, mu = exp(logged_lambda[, h, t, d+1]), size = r)))) stop()
          n_gen[,d+1] = rnbinom(n_samples, mu = exp(logged_lambda[, h, t, d+1]), size = r)
        }
      }
      sum_n[,t,h] = rowSums(n_gen);
    }
  }
  
  mymod.dat <- summary(nowcastmodel)$summary
  
  # Extract all hindcasts and 95% credible intervals
  t.extract <- (now.T - (now.T - 1)):(now.T) # nowcast all weeks up through the present
  
  estimates_ar <- array(NA, dim = c(now.T, n_subpopulation, 5) , dimnames = list(NULL, l_subpopulation, c("estimate", "lower_95", "upper_95", "lower_50", "upper_50")))
  for (h in 1:n_subpopulation){
    for (v in t.extract) {
      estimates_ar[v,h,] = sum_n[,v,h] %>% quantile(c(0.5, 0.025, 0.975, 0.25, 0.75)) %>% round
    }
  }
  estimates = apply(estimates_ar, 3, c)
  
  # Combine nowcast estimates with: dates, number of cases reported at each date
  reported <- group_by(realtime.data, subpopulation , delay_date) %>%
      summarize(n.reported = n()) %>%
      right_join(expand.grid(delay_date  = times, subpopulation = l_subpopulation)) %>%
      mutate(n.reported = case_when(is.na(n.reported) ~ 0, .default = as.numeric(n.reported))) %>%
      arrange(subpopulation, delay_date)
  
  # Join estimates with reported counts
  estimates <- bind_cols(estimates, reported)
  
  # Pull posteriors (trajectories and parameters)
  #delay distribution:
  delay_post = extract(nowcastmodel, pars = "beta")$beta
  #alpha distribution:
  alpha_post = extract(nowcastmodel, pars = "alpha")$alpha
  #tau2_alpha:
  tau2_alpha_post = extract(nowcastmodel, pars = "tau2_alpha")$tau2_alpha
  
  list(
    estimates = estimates,
    nowcast.post.samps = list(nowcast = sum_n, delay_post = delay_post, 
                              alpha_post = alpha_post, tau2_alpha_post = tau2_alpha_post),
    model_sum = mymod.dat
  )
}
