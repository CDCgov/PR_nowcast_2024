library(openxlsx)

#Function to format and collect data
collect_latest_data <- function(exclude_latest_incomplete_week = T) {
  past_data <- openxlsx::read.xlsx('//cdc.gov/locker/OID_DB_EPI_PII/Datasets/Nowcasting/historical data 1986_2024-06-13(report week_year).xlsx') %>%
    filter(CaseType %in% c('ConfirmedDengue', 'ProbableDengue')) %>%
    select(CASE_DATE, CASE_DATECREATED, SEROTYPE_DENV, GCODE) %>%
    mutate(
      diff = as.numeric(CASE_DATECREATED - CASE_DATE),
      CASE_DATE = as.Date(CASE_DATE, origin = "1899-12-30"),
      CASE_DATECREATED = as.Date(CASE_DATECREATED, origin = "1899-12-30"),
    ) %>%
    dplyr::filter(diff >= 0, diff < 60, CASE_DATECREATED < "2021-11-01")
  
  
  #Translate GCODE to health region:
  gcodes = read.csv("//cdc/project/NCEZID_Epidemic_Analytics/Data/geo_data/gcodes_puerto_rico.csv")
  eval(parse(text = 
               paste0("past_data = past_data %>% mutate(PRDH_REGION = case_when(",
                      paste0("as.numeric(GCODE) == ", gcodes$GCODE,"~'", gcodes$health_region,"',", collapse = "")
                      ,"TRUE ~ NA))"
               )
  )
  )
  
  recent_data <- openxlsx::read.xlsx("//cdc.gov/locker/OID_DB_EPI_PII/Datasets/ReportableSurveillance/GIS/Tableau PR Dengue Mapping/Dengue Case Mapping - new.xlsx") %>%
    filter(VIRGY_INTERPRETATION_DENV == 'P' | IGM_INTERPRETATION_DENV == 'P'| NS1Positive == 'P') %>%
    select(CASE_DATE, CASE_DATECREATED, SEROTYPE_DENV, PRDH_REGION) %>%
    mutate(
      CASE_DATE = as.Date(CASE_DATE, origin = "1899-12-30"),
      CASE_DATECREATED = as.Date(CASE_DATECREATED, origin = "1899-12-30"),
      PRDH_REGION = stringi::stri_trans_general(PRDH_REGION, "Latin-ASCII")
    ) %>%
    dplyr::filter(CASE_DATECREATED >= "2021-11-01")
  
  # join data
  denv <- bind_rows(past_data, recent_data) %>%
    rename(report = CASE_DATECREATED,
           onset = CASE_DATE,
           health_region = PRDH_REGION,
           serotype = SEROTYPE_DENV) %>%
    select(-GCODE) %>%
    as.data.frame()
  
  # add week based on MMMR definition
  denv <- mutate(denv,
                 diff = as.numeric(report - onset),
                 onset_wk_num = epiweek(onset),
                 report_wk_num = epiweek(report),
                 report_wk = ceiling_date(report, unit = "week", week_start = 7) - days(1),
                 onset_wk = ceiling_date(onset, unit = "week", week_start = 7) - days(1),
                 report_yr = year(report_wk),
                 onset_yr = year(onset_wk),
                 delay = difftime(report_wk, onset_wk, units = "week"),
                 delay_days = as.numeric(gsub(" weeks", "", difftime(report, onset, units = "day"))),
                 delay_wk = as.numeric(gsub(" weeks", "", delay))
                 )
  #checking the most recent report week
  most_recent_week <- as.Date(max(denv$report_wk))
  day_of_week <- weekdays(most_recent_week)
  
  # Check if the most recent week contains data reported on last day of epi week. This code will exclude the most recent week from the nowcast if it is deemed "incomplete"
  if (exclude_latest_incomplete_week == T) {
    if (!any(weekdays(filter(denv, report_wk == most_recent_week)$report) == day_of_week)) {
      denv <- filter(denv, report_wk != most_recent_week)
    }
  }
  
  denv <- filter(denv, onset_wk >= "1988-01-01", delay_wk >= 0)
  
  return(denv)
}

# #Preprocess test positive proportion data:
# collect_TPP_data <- function(exclude_latest_incomplete_week = T) {
#   test_data <- openxlsx::read.xlsx('//cdc.gov/project/NCEZID_Epidemic_Analytics/Projects/PR_nowcast/data/historical_data 1986 - 26JUN25.xlsx') %>%
#     filter(VIRGY_INTERPRETATION_DENV %in% c("P", "N")|IGM_INTERPRETATION_DENV %in% c("P", "N")) %>%
#     select(CASE_DATE, REPORT_DATE, SEROTYPE_DENV, GCODE, VIRGY_INTERPRETATION_DENV, IGM_INTERPRETATION_DENV) %>%
#     mutate(
#       CASE_DATE = as.Date(CASE_DATE, origin = "1899-12-30"),
#       REPORT_DATE = as.Date(REPORT_DATE, origin = "1899-12-30"),
#     )
#   
#   
#   #Translate GCODE to health region:
#   gcodes = read.csv("//cdc/project/NCEZID_Epidemic_Analytics/Data/geo_data/gcodes_puerto_rico.csv")
#   eval(parse(text = 
#                paste0("test_data = test_data %>% mutate(PRDH_REGION = case_when(",
#                       paste0("as.numeric(GCODE) == ", gcodes$GCODE,"~'", gcodes$health_region,"',", collapse = "")
#                       ,"TRUE ~ NA))"
#                )
#   )
#   )
#   
#   test_denv <- test_data %>%
#     rename(report = REPORT_DATE,
#            onset = CASE_DATE,
#            health_region = PRDH_REGION,
#            serotype = SEROTYPE_DENV,
#            virology_test = VIRGY_INTERPRETATION_DENV, 
#            IgM_test = IGM_INTERPRETATION_DENV) %>%
#     select(-GCODE) %>%
#     as.data.frame()
#   
#   # add week
#   test_denv <- mutate(test_denv,
#                  infected = onset - 6, 
#                  diff = as.numeric(report - infected),
#                  infected_wk_num = week(infected),
#                  report_wk_num = ifelse(week(report)== 53, 1, week(report)), # if cases are reported in week 53, they are relabeled as week 1 of the following year
#                  onset_wk_num = week(onset),
#                  infected_yr = year(infected),
#                  report_yr = ifelse(week(report)== 53, year(report) + 1, year(report)),
#                  onset_yr = ifelse(week(onset)== 53, year(onset) + 1, year(onset))
#   ) %>% # if cases are reported in week 53, they are relabeled as week 1 of the following year
#     mutate(infection_wk = as.Date(paste(year(infected), week(infected) * 7, sep='-'), format='%Y-%j'),
#            report_wk = as.Date(paste(report_yr, report_wk_num * 7, sep='-'), format='%Y-%j'), # aggregating dates into week clasification with Jan 1 of the given year marking the first day of the week
#            onset_wk = as.Date(paste(onset_yr, onset_wk_num * 7, sep='-'), format='%Y-%j'), 
#            delay = difftime(report_wk, onset_wk, units = "week"),
#            delay_days = as.numeric(gsub(" days", "", difftime(report, onset, units = "day"))),
#            delay_wk = as.numeric(gsub(" weeks", "", delay))
#     ) %>% filter(onset_wk_num != 53, diff >= 0, diff < 60) # removing cases with onset date during week 53 since the week is incomplete
#   
#   #checking the most recent report week
#   most_recent_week <- as.Date(max(test_denv$report_wk))
#   day_of_week <- weekdays(most_recent_week)
#   
#   # Check if the most recent week contains data reported on last day of epi week. This code will exclude the most recent week from the nowcast if it is deemed "incomplete"
#   if (exclude_latest_incomplete_week == T) {
#     if (!any(weekdays(filter(test_denv, report_wk == most_recent_week)$report) == day_of_week)) {
#       test_denv <- filter(test_denv, report_wk != most_recent_week)
#     }
#   }
#   
#   test_denv <- filter(test_denv, onset_wk >= "1988-01-01", delay_wk >= 0)
#   return(test_denv)
# }