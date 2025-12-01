library(tidyverse)

nowcast_date = "2024-12-28"

#Eventually reported cases:
eventual_case_dat <- read.csv("data/denv_sample_dat_eventually_reported.csv")
eventual_case_dat$delay_date = as.Date(eventual_case_dat$delay_date, "%m/%d/%Y")
nation_ereported <- eventual_case_dat %>% group_by(delay_date) %>% summarise(cases = n())
region_ereported <- eventual_case_dat %>% group_by(delay_date, health_region) %>% summarise(cases = n()) %>% rename(subpopulation = health_region)
serotype_ereported <- eventual_case_dat %>% group_by(delay_date, serotype) %>% summarise(cases = n()) %>% rename(subpopulation = serotype)

##Loading output from the models
#Nation output:
l_output = c()
for(model in c("baseline", "default")){
  output = readRDS(paste0("output/nation_", model,"_nowcast_", nowcast_date,".rds"))
  output$estimates$model = case_when(model == "baseline" ~ "Baseline", 
                                    model == "default" ~ "NobBS")
  output$estimates = output$estimates %>% left_join(nation_ereported, by = "delay_date")
  l_output = c(l_output, list(output$estimates))
}
#Subpopulation output:
all_subpop = expand.grid(
  subpoptype = c("Health Region", "Serotype"),
  model = c("ind", "shared", "baseline")
)

for(i in 1:nrow(all_subpop)){
  subpoptype = all_subpop$subpoptype[i]
  model = all_subpop$model[i]
  output = readRDS(paste0("output/", subpoptype, "_", model, "_nowcast_", nowcast_date, ".rds"))
  if(model == "ind"){
    outputsubpop = lapply(output, function(x) x$estimates) %>% Reduce(rbind, .)
  } else if(model == "baseline"){
    outputsubpop = output %>% Reduce(rbind, .)
  } else {
    outputsubpop = output$estimates
  }
  outputsubpop$subpoptype = subpoptype
  outputsubpop$model = case_when(model == "baseline" ~ "Baseline", 
                                 model == "ind" ~ "Individual parameters",
                                 model == "shared" ~ "Shared parameters")
  if(subpoptype == "Health Region"){
    subpoptype_case_data = region_ereported
  } else if(subpoptype == "Serotype"){
    subpoptype_case_data = serotype_ereported
  }
  outputsubpop = outputsubpop %>% left_join(subpoptype_case_data, by = c("delay_date", "subpopulation"))
  
  l_output = c(l_output, list(outputsubpop))
}

##Helper functions
#50% and 95% Coverage:
cov5095_col_create <- function(data){
  data %>% mutate(cov50 = ifelse(cases <= upper_50 & cases >= lower_50, 1, 0),
                  cov95 = ifelse(cases <= upper_95 & cases >= lower_95, 1, 0)) 
}

#WIS function:
calculate_wis_components <- function(df, truth_name = "cases", median_name = "estimate", intervals = c(0.5, 0.05)) {
  # intervals: vector of alpha levels (e.g., c(0.5, 0.05) for 50% and 95% PIs)
  weights <- rep(1 / (length(intervals) + 0.5), length(intervals))  # equal weight per interval
  ae_weight <- 1 / (length(intervals) + 0.5) * 0.5
  
  total_dispersion <- rep(0, nrow(df))
  total_under <- rep(0, nrow(df))
  total_over <- rep(0, nrow(df))
  
  for (i in seq_along(intervals)) {
    alpha <- intervals[i]
    w <- weights[i]
    
    # Extract lower and upper columns by naming convention: lower_50, upper_50, etc.
    lower_col <- df[[paste0("lower_", as.character(100 * (1 - alpha)))]]  # e.g., lower_95
    upper_col <- df[[paste0("upper_", as.character(100 * (1 - alpha)))]]  # e.g., upper_95
    truth <- df[[truth_name]]
    
    # Components
    dispersion <- upper_col - lower_col
    underprediction <- ifelse(truth < lower_col, 2 / alpha * (lower_col - truth), 0)
    overprediction  <- ifelse(truth > upper_col, 2 / alpha * (truth - upper_col), 0)
    
    # Weighted sums
    total_dispersion <- total_dispersion + w * dispersion
    total_under <- total_under + w * underprediction
    total_over <- total_over + w * overprediction
  }
  
  # Absolute error from the median (optional)
  ae <- abs(df[[median_name]] - truth)
  
  # Final WIS (optional)
  wis <- ae_weight * ae + total_dispersion + total_under + total_over
  
  return(cbind(df, data.frame(
    WIS = wis,
    AE = ae_weight * ae,
    Dispersion = total_dispersion,
    Underprediction = total_under,
    Overprediction = total_over
  )))
}

l_output[[1]]$subpoptype = "Nation"; l_output[[1]]$model = "Baseline"
l_output[[2]]$subpoptype = "Nation"; l_output[[2]]$model = "NobBS"
 
#Summarise coverage of models:
lapply(l_output, cov5095_col_create) %>% Reduce(rbind, .) %>% group_by(subpoptype, model) %>%
  summarise(cov50 = mean(cov50), cov95 = mean(cov95))

#Summarise performance of models by WIS
lapply(l_output, calculate_wis_components) %>% Reduce(rbind, .) %>%
  group_by(subpoptype, model) %>%
  summarise(WIS = mean(WIS), AE = mean(AE),
          Dispersion = mean(Dispersion), Underprediction = mean(Underprediction), Overprediction = mean(Overprediction))
