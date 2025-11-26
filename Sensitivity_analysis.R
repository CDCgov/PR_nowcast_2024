library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(readxl)

###When 2024-11-23 reported cases is excluded from the final cases: the problem of batch reporting ####
denv_excluded_week47 = denv_org %>%
  filter(report_wk < "2024-11-23")
failed_nc_date_2024 <- c("2024-09-28", 
                         "2024-10-26", "2024-11-09", "2024-11-23")
nc_years_2024 = c(2024) #Years of nowcast
nc_dates_range = lapply(nc_years_2024, function(x) ceiling_date(as.Date(paste0(x, c("-01-01", "-12-31"))), unit = "week", week_start = 7) - days(1))
nowcast_list_2024 = nc_dates_range %>% lapply(function(x) seq(x[1], x[2], 7)) %>% unlist %>% as.Date
#nowcast_list_2024 = nowcast_list_2024[!(nowcast_list_2024 %in% failed_nc_date_2024)]
nowcast_list_2024 = nowcast_list_2024[year(nowcast_list_2024) == 2024]
#nowcast_list_2024 = nowcast_list_2024[nowcast_list_2024 <= "2024-11-23"]

nowcast_failed_2024_l <- c()
for(i in 1:length(nowcast_list_2024)){
  nowcast_date = as.Date(nowcast_list_2024[i])
  print(nowcast_date)
  test_now = readRDS(paste0("output/nation_default_nowcast_", nowcast_date, ".rds"))
  
  denv_i_final = filter(denv_org, onset >= as.Date("1986-01-01"),
                        onset_wk %in% test_now$estimates$delay_date)
  denv_case_final = table(denv_i_final$onset_wk %>% factor(levels = test_now$estimates$delay_date))
  
  denv_i_final_excluded_week47 = filter(denv_excluded_week47, onset >= as.Date("1986-01-01"),
                        onset_wk %in% test_now$estimates$delay_date)
  denv_case_final_excluded_week47 = table(denv_i_final_excluded_week47$onset_wk %>% factor(levels = test_now$estimates$delay_date))
  
  #if the nowcast is failed:
  nc_failed_v = failed_nowcast_indication(nowcast_case = test_now$nowcast.post.samps$nowcast[,,1] %>% t, 
                                          eventual_case = denv_case_final)
  
  nc_failed_v_excluded_week47 = failed_nowcast_indication(nowcast_case = test_now$nowcast.post.samps$nowcast[,,1] %>% t, 
                                          eventual_case = denv_case_final_excluded_week47)
  
  nowcast_failed_2024_l = test_now$estimates %>% cbind(cases = as.numeric(denv_case_final), cases_excluded_week47 = as.numeric(denv_case_final_excluded_week47), 
                                                       nc_failed_v = nc_failed_v, nc_failed_v_excluded_week47 = nc_failed_v_excluded_week47, nowcast_date = nowcast_date) %>% list %>%
    c(nowcast_failed_2024_l, .)
}

nowcast_failed_2024 = Reduce(rbind, nowcast_failed_2024_l)
(nowcast_failed_2024 %>%
    mutate(year = year(nowcast_date)) %>%
    filter(nowcast_date %in% failed_nc_date_2024) %>%
    ggplot() +
    geom_ribbon(aes(x = delay_date, ymin = lower_50, ymax = upper_50, group = nowcast_date, fill = "50% PI"), alpha = 0.25) +  # Added fill for legend
    geom_ribbon(aes(x = delay_date, ymin = lower_95, ymax = upper_95, group = nowcast_date, fill = "95% PI"), alpha = 0.15) +  # Added fill for legend
    geom_line(aes(x = delay_date, y = cases, color = "Eventual cases", group = nowcast_date),  size = 0.8) +  # Removed show.legend = FALSE
    geom_line(aes(x = delay_date, y = cases_excluded_week47, color = "Eventual cases (up until week 47)", group = nowcast_date),  size = 0.8) +  # Removed show.legend = FALSE
    geom_point(data = . %>% filter(nc_failed_v),
               aes(x = delay_date, y = cases, color = "Failed nowcast"),
               size = 3, shape = 16) +
    geom_point(data = . %>% filter(nc_failed_v_excluded_week47),
               aes(x = delay_date, y = cases_excluded_week47, color = "Failed nowcast"),
               size = 3, shape = 16) +
    geom_line(aes(x = delay_date, y = n.reported, color = "Reported cases (up until nowcast date)", group = nowcast_date), size = 0.8, linetype = 2) +  # Removed show.legend = FALSE
    theme_bw() +
    scale_color_manual(values = c(colors_legend, "Eventual cases (up until week 47)" = "purple")) +
    scale_fill_manual(values = nowcast_ribbon) +  # Set custom colors for ribbons
    scale_x_date(
      breaks = "1 week",#unique(nowcast_data$delay_date),  # Use the unique weeks in your dataset
      labels = scales::date_format("%U")  # Show the week number
    ) +
    scale_y_log10()+
    facet_wrap(.~ nowcast_date, scale = "free")+
    labs(x = "Week of Onset",
         y = "Case Count")+
    theme(axis.text.x = element_text(size = 12, angle = 90, hjust = 0.5, vjust = 0.5),
          axis.text.y = element_text(size = 12, hjust = 0.5, vjust = 0.5),
          strip.text = element_text(size = 12),
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          plot.title = element_text(size = 14, hjust = 0.5),
          legend.title = element_blank(),
          legend.position = "bottom",
          legend.text = element_text(size = 14))
)

nowcast_failed_2024 %>% horizon_col_create %>% cov5095_col_create %>% summarize_coverage(horizon)
nowcast_failed_2024 %>% select(-cases) %>% rename(cases = cases_excluded_week47) %>% horizon_col_create %>% cov5095_col_create %>% summarize_coverage(subpopulation)

nowcast_failed_2024 %>% mutate(cases = ifelse(nowcast_date < "2024-11-23", cases_excluded_week47, cases))  %>% horizon_col_create %>% cov5095_col_create %>% summarize_coverage(subpopulation)
