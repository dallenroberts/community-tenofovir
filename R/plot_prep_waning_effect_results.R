################################################################################
## Allen Roberts
## Plot PrEP waning effect results
################################################################################

## Libraries
library(tidyverse)
library(ggplot2)
library(RColorBrewer)

## Suppress unhelpful warnings
options(dplyr.summarise.inform = FALSE) 

## Plot theme
theme_set(theme_classic())

## Run information
suite_name <-  "nyanza-prep100-sf03"
scenarios <- list.dirs(file.path("scenarios", suite_name), recursive = FALSE, full.names = FALSE)

## Boolean to analyze Report_Event_Recorder data
record_events <- FALSE

## Load results and combine
## Summary results
results_list <- lapply(scenarios, function(scenario_name) {
  
  load(file.path("scenarios", suite_name, scenario_name, "prep_we_comp_results.RData"))
  return(outputs)
})

results <- do.call(Map, c(f = rbind, results_list))

## PrEP duration analysis
if(record_events == TRUE) {
  
  duration_results_list <- lapply(scenarios, function(scenario_name) {
    
    load(file.path("scenarios", suite_name, scenario_name, "prep_duration_results.RData"))
    return(prep_duration_results)
  })
  
  duration_results <- do.call(rbind, duration_results_list)
  
  ## PrEP duration
  prep_duration_plot <- duration_results %>%
    ggplot(aes(x = duration)) +
    geom_histogram(aes(y = stat(count/sum(count))), binwidth = 30.42) +
    labs(x = "Days of PrEP use", y = "Proportion") +
    scale_x_continuous(limits = c(0, 390), breaks = seq(0, 360, by = 30)) +
    facet_wrap(~prep_method)
  
  
}

## PrEP coverage - beginning vs. end of time step
prep_timestep_plot <- results$prep_echo %>%
  mutate(prep_method = factor(prep_method, levels = c("Exp_90", "Const_90", "Const_100", "Exp_180", "Const_180"))) %>%
  filter(year == 2025) %>%
  ggplot(aes(x = prep_method, y = 100*median)) +
  geom_bar(aes(fill = prep_method), stat = "identity", position = position_dodge(), show.legend = FALSE) +
  geom_abline(intercept = 50, slope = 0, linetype = 'dashed') +
  scale_fill_brewer(palette = "Set1") +
  labs(x = "PrEP method", y = "PrEP Coverage, end of time step") +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  ggtitle("PrEP coverage, HIV-negative women ages 16-35")

## Incidence, by year, women ages 16-35
inc_echo_plot <- results$inc_echo %>%
  mutate(prep_method = factor(prep_method, levels = c("Exp_90", "Const_90", "Const_100", "Exp_180", "Const_180"))) %>%
  ggplot(aes(x = year, y = median, group = prep_method)) +
  geom_line(aes(color = prep_method)) +
  scale_color_brewer(palette = "Set1", name = "PrEP method") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  scale_x_continuous(limits = c(2020, 2030), breaks = seq(2020, 2030, by = 2)) +
  labs(x = "Year", y = "Incidence (per 100 PY)") +
  ggtitle("Incidence among women ages 16-35") 

## Incidence, by year, women ages 16-35, with uncertainty
## Note that uncertainty is large unless the scale factor is increased
inc_echo_ui_plot <- results$inc_echo %>%
  mutate(prep_method = factor(prep_method, levels = c("Exp_90", "Const_90", "Const_100", "Exp_180", "Const_180"))) %>%
  ggplot(aes(x = year, y = median, group = prep_method)) +
  geom_line(aes(color = prep_method)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = prep_method), alpha = 0.1, show.legend = FALSE) +
  scale_color_brewer(palette = "Set1", name = "PrEP method") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  scale_x_continuous(limits = c(2020, 2030), breaks = seq(2020, 2030, by = 2)) +
  labs(x = "Year", y = "Incidence (per 100 PY)") +
  ggtitle("Incidence among women ages 16-35") 

## Plots
pdf(file = file.path("scenarios", suite_name, "prep_we_comp_plots.pdf"), height = 6, width = 8)

if(record_events == TRUE) {
  print(prep_duration_plot)
}

print(prep_timestep_plot)
print(inc_echo_plot)
print(inc_echo_ui_plot)
dev.off()