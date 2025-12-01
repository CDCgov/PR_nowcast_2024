library(tidyverse)

#Colors:
colors_legend <-c("Nowcast estimates" = "purple",
                  "Reported cases (up until nowcast date)" = "black", 
                  "Eventually reported cases" = "black") 
nowcast_ribbon <- c("50% PI" = "purple", "95% PI" = "purple")
nowcast_date = "2024-12-28"

#Eventually reported cases:
eventual_case_dat <- read.csv("data/denv_sample_dat_eventually_reported.csv")
eventual_case_dat$delay_date = as.Date(eventual_case_dat$delay_date, "%m/%d/%Y")
nation_ereported <- eventual_case_dat %>% group_by(delay_date) %>% summarise(cases = n())
region_ereported <- eventual_case_dat %>% group_by(delay_date, health_region) %>% summarise(cases = n()) %>% rename(subpopulation = health_region)
serotype_ereported <- eventual_case_dat %>% group_by(delay_date, serotype) %>% summarise(cases = n()) %>% rename(subpopulation = serotype)

##National nowcast:
national_estimates <- lapply(
  c("baseline", "default"), 
  function(model){
    output = readRDS(paste0("output/nation_", model, "_nowcast_", nowcast_date, ".rds"))
    output$estimates$model = case_when(model == "baseline" ~ "Baseline", 
                                       model == "default" ~ "NobBS")
    output$estimates = output$estimates %>% left_join(nation_ereported, by = "delay_date")
    return(output$estimates)
  }
) %>% Reduce(rbind, .)

#National estimates plot:
national_estimates %>%
  ggplot() +
  geom_ribbon(aes(x = delay_date, ymin = lower_50, ymax = upper_50, group = model, fill = "50% PI"), alpha = 0.25) +  # Added fill for legend
  geom_ribbon(aes(x = delay_date, ymin = lower_95, ymax = upper_95, group = model, fill = "95% PI"), alpha = 0.15) +  # Added fill for legend
  geom_line(aes(x = delay_date, y = n.reported, color = "Reported cases (up until nowcast date)", group = model), size = 0.8, linetype = 2) +  # Removed show.legend = FALSE
  geom_line(aes(x = delay_date, y = cases, color = "Eventually reported cases", group = model), size = 0.8) + 
  theme_bw() +
  scale_color_manual(values = colors_legend) +
  scale_fill_manual(values = nowcast_ribbon) +  
  scale_y_continuous(limits = c(0, NA)) + 
  facet_wrap(.~ model)+
  scale_color_manual(
    values = colors_legend,
    breaks = names(colors_legend) # desired order
  )+ 
  labs(x = "Onset date",
       y = "Case Count")+
  theme(axis.text.x = element_text(size = 10, angle = 90, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(size = 10, hjust = 0.5, vjust = 0.5),
        strip.text = element_text(size = 10),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.title = element_blank(),
        legend.key.width = unit(1, "cm"),  
        legend.position = "bottom",
        legend.text = element_text(size = 14))

##Subpopulation nowcast:
all_subpop = expand.grid(
  subpoptype = c("Health Region", "Serotype"),
  model = c("ind", "shared", "baseline")
)

subnation_estimates <- lapply(
  1:nrow(all_subpop), 
  function(i){
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
    
    return(outputsubpop)
  }
) %>% Reduce(rbind, .)

subnation_estimates %>%
  ggplot() +
  geom_ribbon(aes(x = delay_date, ymin = lower_50, ymax = upper_50, group = model, fill = "50% PI"), alpha = 0.25) +  # Added fill for legend
  geom_ribbon(aes(x = delay_date, ymin = lower_95, ymax = upper_95, group = model, fill = "95% PI"), alpha = 0.15) +  # Added fill for legend
  geom_line(aes(x = delay_date, y = n.reported, color = "Reported cases (up until nowcast date)", group = model), size = 0.8, linetype = 2) +  # Removed show.legend = FALSE
  geom_line(aes(x = delay_date, y = cases, color = "Eventually reported cases", group = model), size = 0.8) + 
  theme_bw() +
  scale_color_manual(values = colors_legend) +
  scale_fill_manual(values = nowcast_ribbon) +  
  scale_y_continuous(limits = c(0, NA)) + 
  facet_grid(model ~ subpopulation + subpoptype, scale = "free")+
  scale_color_manual(
    values = colors_legend,
    breaks = names(colors_legend) # desired order
  )+ 
  labs(x = "Onset date",
       y = "Case Count")+
  theme(axis.text.x = element_text(size = 10, angle = 90, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(size = 10, hjust = 0.5, vjust = 0.5),
        strip.text = element_text(size = 10),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.title = element_blank(),
        legend.key.width = unit(1, "cm"),  
        legend.position = "bottom",
        legend.text = element_text(size = 14))
