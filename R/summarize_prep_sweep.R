################################################################################
## Allen Roberts
## PrEP sweep summary
################################################################################

## Libraries
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(lemon)
library(viridis)
library(viridisLite)
library(scales)

theme_set(theme_classic())

## Suppress unhelpful warnings
options(dplyr.summarise.inform = FALSE) 

## Run information
suite_name <- "nyanza-prep-sweep6"
scenarios <- list.dirs(file.path("scenarios", suite_name), recursive = FALSE, full.names = FALSE)

## Load results and combine
## Summary results
results_list <- lapply(scenarios, function(scenario_name) {
  
  load(file.path("scenarios", suite_name, scenario_name, "prep_sweep_results.RData"))
  return(outputs)
})

results <- do.call(Map, c(f = rbind, results_list))

## Results by run
results_by_run_list <- lapply(scenarios, function(scenario_name) {
  
  load(file.path("scenarios", suite_name, scenario_name, "prep_sweep_results_by_run.RData"))
  return(outputs)
})

results_by_run <- do.call(Map, c(f = rbind, results_by_run_list))

## PrEP coverage - beginning vs. end of time step
prep_timestep_plot <- results$prep_echo %>%
  filter(year == 2025) %>%
  ggplot(aes(x = prep_coverage, y = 100*median)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1, linetype = 'dashed') +
    labs(x = "PrEP coverage, end of time step", y = "PrEP Coverage, beginning of time step") +
    scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
    ggtitle("PrEP coverage, HIV-negative women ages 16-35")
    

## Incidence, by year, women ages 16-35
inc_echo_plot <- results$inc_echo %>%
  ggplot(aes(x = year, y = median, group = prep_coverage)) +
    geom_line(aes(color = prep_coverage)) +
    scale_color_viridis(direction = -1, name = "PrEP coverage", breaks = seq(0, 100, by = 20)) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
    scale_x_continuous(limits = c(2021, 2032), breaks = seq(2020, 2032, by = 2)) +
    labs(x = "Year", y = "Incidence (per 100 PY)") +
    ggtitle("Incidence among women ages 16-35") +
    guides(color = guide_legend(reverse = FALSE))

## Incidence, by year, women ages 16-35, with uncertainty
inc_echo_ui_plot <- results$inc_echo %>%
  ggplot(aes(x = year, y = median, group = prep_coverage)) +
  geom_line(aes(color = prep_coverage), show.legend = FALSE) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = prep_coverage), alpha = 0.1) +
  scale_color_viridis(direction = -1, name = "PrEP coverage") +
  scale_fill_viridis(direction = -1, name = "PrEP coverage", breaks = seq(0, 100, by = 20)) +
  scale_y_continuous(limits = c(0, 1.5), breaks = seq(0, 1.5, by = 0.25)) +
  scale_x_continuous(limits = c(2021, 2032), breaks = seq(2020, 2032, by = 2)) +
  labs(x = "Year", y = "Incidence (per 100 PY)") +
  ggtitle("Incidence among women ages 16-35") +
  guides(fill = guide_legend(reverse = FALSE, override.aes = list(alpha = 1)))

## Relative risk (with respect to base case), by year, women ages 16-35, 
rr_echo_plot <- results_by_run$inc_echo %>%
  filter(year >= 2021) %>%
  group_by(run_num, year) %>%
  mutate(value_base = value[prep_coverage == 0]) %>%
  filter(prep_coverage > 0) %>%
  mutate(rr = value/value_base) %>%
  group_by(year, gender, age_lower, age_upper, county, measure, prep_coverage) %>%
  summarise(median = median(rr),
            lower = quantile(rr, probs = 0.025),
            upper = quantile(rr, probs = 0.975)) %>%
  ggplot(aes(x = year, y = median, group = prep_coverage)) +
  geom_line(aes(color = prep_coverage)) +
  scale_color_viridis(direction = -1, name = "PrEP coverage", breaks = seq(0, 100, by = 20)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  scale_x_continuous(limits = c(2021, 2032), breaks = seq(2020, 2032, by = 2)) +
  labs(x = "Year", y = "Relative risk") +
  ggtitle("Relative risk of HIV infection, women ages 16-35") +
  guides(color = guide_legend(reverse = FALSE))


## Relative risk (with respect to base case), by year, women ages 16-35, with uncertainty
rr_echo_ui_plot <- results_by_run$inc_echo %>%
  filter(year >= 2021) %>%
  group_by(run_num, year) %>%
  mutate(value_base = value[prep_coverage == 0]) %>%
  filter(prep_coverage > 0) %>%
  mutate(rr = value/value_base) %>%
  group_by(year, gender, age_lower, age_upper, county, measure, prep_coverage) %>%
  summarise(median = median(rr),
            lower = quantile(rr, probs = 0.025),
            upper = quantile(rr, probs = 0.975)) %>%
  ggplot(aes(x = year, y = median, group = prep_coverage)) +
  geom_line(aes(color = prep_coverage), show.legend = FALSE) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = prep_coverage), alpha = 0.1) +
  scale_color_viridis(direction = -1, name = "PrEP coverage") +
  scale_fill_viridis(direction = -1, name = "PrEP coverage", breaks = seq(0, 100, by = 20)) +
  scale_y_continuous(limits = c(0, 1.1), breaks = seq(0, 1, by = 0.2)) +
  scale_x_continuous(limits = c(2021, 2032), breaks = seq(2020, 2032, by = 2)) +
  labs(x = "Year", y = "Relative risk") +
  ggtitle("Relative risk of HIV infection, women ages 16-35") +
  guides(fill = guide_legend(reverse = FALSE, override.aes = list(alpha = 1)))

## Relative risk (with respect to base case), women ages 16-35, with uncertainty
rr_echo_all_years_ui_plot <- results_by_run$inc_echo_2022_2032 %>%
  group_by(run_num) %>%
  mutate(value_base = value[prep_coverage == 0]) %>%
  filter(prep_coverage > 0) %>%
  mutate(rr = value/value_base) %>%
  group_by(year, gender, age_lower, age_upper, county, measure, prep_coverage) %>%
  summarise(median = median(rr),
            lower = quantile(rr, probs = 0.025),
            upper = quantile(rr, probs = 0.975)) %>%
  ggplot(aes(x = prep_coverage, y = median)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  scale_x_continuous(limits = c(0, 105), breaks = seq(0, 100, by = 20)) +
  labs(x = "PrEP Coverage (%)", y = "Relative risk") +
  ggtitle("Relative risk of HIV infection, 2022-2032, women ages 16-35") 

## Number of new infections, by year, entire province
new_inf_plot <- results$new_inf %>%
  group_by(year) %>%
  ggplot(aes(x = year, y = median, group = prep_coverage)) +
  geom_line(aes(color = prep_coverage)) +
  scale_color_viridis(direction = -1, name = "PrEP coverage", breaks = seq(0, 100, by = 20)) +
  scale_y_continuous(limits = c(0, 20000), breaks = seq(0, 20000, by = 5000), labels = comma) +
  scale_x_continuous(limits = c(2021, 2032), breaks = seq(2022, 2032, by = 2)) +
  labs(x = "Year", y = "New infections") +
  ggtitle("Number of new infections, entire province") +
  guides(color = guide_legend(reverse = FALSE))

## Number of new infections, by year, entire province, with uncertainty
new_inf_ui_plot <- results$new_inf %>%
  group_by(year) %>%
  ggplot(aes(x = year, y = median, group = prep_coverage)) +
  geom_line(aes(color = prep_coverage), show.legend = FALSE) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = prep_coverage), alpha = 0.1) +
  scale_color_viridis(direction = -1, name = "PrEP coverage") +
  scale_fill_viridis(direction = -1, name = "PrEP coverage", breaks = seq(0, 100, by = 20)) +
  scale_y_continuous(limits = c(0, 25000), breaks = seq(0, 25000, by = 5000), labels = comma) +
  scale_x_continuous(limits = c(2021, 2032), breaks = seq(2022, 2032, by = 2)) +
  labs(x = "Year", y = "New infections") +
  ggtitle("Number of new infections, entire province") +
  guides(fill = guide_legend(reverse = FALSE, override.aes = list(alpha = 1)))

## Number of new infections, by year, women 16-35
new_inf_echo_plot <- results$new_inf_echo %>%
  group_by(year) %>%
  ggplot(aes(x = year, y = median, group = prep_coverage)) +
  geom_line(aes(color = prep_coverage)) +
  scale_color_viridis(direction = -1, name = "PrEP coverage", breaks = seq(0, 100, by = 20)) +
  scale_y_continuous(limits = c(0, 10000), breaks = seq(0, 10000, by = 2000), labels = comma) +
  scale_x_continuous(limits = c(2021, 2032), breaks = seq(2020, 2032, by = 2)) +
  labs(x = "Year", y = "New infections") +
  ggtitle("Number of new infections, women 16-35") +
  guides(color = guide_legend(reverse = FALSE))

## Number of new infections, by year, women 16-35, with uncertainty
new_inf_echo_ui_plot <- results$new_inf_echo %>%
  group_by(year) %>%
  ggplot(aes(x = year, y = median, group = prep_coverage)) +
  geom_line(aes(color = prep_coverage), show.legend = FALSE) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = prep_coverage), alpha = 0.1) +
  scale_color_viridis(direction = -1, name = "PrEP coverage") +
  scale_fill_viridis(direction = -1, name = "PrEP coverage", breaks = seq(0, 100, by = 20)) +
  scale_y_continuous(limits = c(0, 15000), breaks = seq(0, 15000, by = 2000), labels = comma) +
  scale_x_continuous(limits = c(2021, 2032), breaks = seq(2022, 2032, by = 2)) +
  labs(x = "Year", y = "New infections") +
  ggtitle("Number of new infections, women 16-35") +
  guides(fill = guide_legend(reverse = FALSE, override.aes = list(alpha = 1)))

## Infections averted, by year, entire province
inf_averted_plot <- results_by_run$new_inf %>%
  group_by(run_num, year) %>%
  mutate(value_base = value[prep_coverage == 0]) %>%
  filter(prep_coverage > 0) %>%
  mutate(value = -(value - value_base)) %>%
  group_by(year, prep_coverage) %>%
  summarise(median = median(value),
            lower = quantile(value, probs = 0.025),
            upper = quantile(value, probs = 0.975)) %>%
  filter(year >= 2021) %>%
  ggplot(aes(x = year, y = median, group = prep_coverage)) +
  geom_line(aes(color = prep_coverage)) +
  scale_color_viridis(direction = -1, name = "PrEP coverage", breaks = seq(0, 100, by = 20)) +
  scale_y_continuous(limits = c(0, 8000), breaks = seq(0, 8000, by = 1000), labels = comma) +
  scale_x_continuous(limits = c(2021, 2032), breaks = seq(2020, 2032, by = 2)) +
  labs(x = "Year", y = "Infections averted") +
  ggtitle("Infections averted, entire province") +
  guides(color = guide_legend(reverse = TRUE))

## Number of new infections, by year, entire province, with uncertainty
inf_averted_ui_plot <- results_by_run$new_inf %>%
  group_by(run_num, year) %>%
  mutate(value_base = value[prep_coverage == 0]) %>%
  filter(prep_coverage > 0) %>%
  mutate(value = -(value - value_base)) %>%
  group_by(year, prep_coverage) %>%
  summarise(median = median(value),
            lower = quantile(value, probs = 0.025),
            upper = quantile(value, probs = 0.975)) %>%
  filter(year >= 2021) %>%
  ggplot(aes(x = year, y = median, group = prep_coverage)) +
  geom_line(aes(color = prep_coverage), show.legend = FALSE) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = prep_coverage), alpha = 0.1) +
  scale_color_viridis(direction = -1, name = "PrEP coverage") +
  scale_fill_viridis(direction = -1, name = "PrEP coverage", breaks = seq(0, 100, by = 20)) +
  scale_y_continuous(limits = c(0, 12000), breaks = seq(0, 12000, by = 2000), labels = comma) +
  scale_x_continuous(limits = c(2021, 2032), breaks = seq(2020, 2032, by = 2)) +
  labs(x = "Year", y = "Infections averted") +
  ggtitle("Infections averted, entire province") +
  guides(fill = guide_legend(reverse = TRUE, override.aes = list(alpha = 1)))

## Infections averted, by year, women 16-35
inf_averted_echo_plot <- results_by_run$new_inf_echo %>%
  group_by(run_num, year) %>%
  mutate(value_base = value[prep_coverage == 0]) %>%
  filter(prep_coverage > 0) %>%
  mutate(value = -(value - value_base)) %>%
  group_by(year, prep_coverage) %>%
  summarise(median = median(value),
            lower = quantile(value, probs = 0.025),
            upper = quantile(value, probs = 0.975)) %>%
  filter(year >= 2021) %>%
  group_by(year) %>%
  ggplot(aes(x = year, y = median, group = prep_coverage)) +
  geom_line(aes(color = prep_coverage)) +
  scale_color_viridis(direction = -1, name = "PrEP coverage", breaks = seq(0, 100, by = 20)) +
  scale_y_continuous(limits = c(0, 7000), breaks = seq(0, 7000, by = 1000), labels = comma) +
  scale_x_continuous(limits = c(2021, 2032), breaks = seq(2020, 2032, by = 2)) +
  labs(x = "Year", y = "Infections averted") +
  ggtitle("Infections averted, women 16-35") +
  guides(color = guide_legend(reverse = TRUE))

## Infections averted, by year, women 16-35, with uncertainty
inf_averted_echo_ui_plot <- results_by_run$new_inf_echo %>%
  group_by(run_num, year) %>%
  mutate(value_base = value[prep_coverage == 0]) %>%
  filter(prep_coverage > 0) %>%
  mutate(value = -(value - value_base)) %>%
  group_by(year, prep_coverage) %>%
  summarise(median = median(value),
            lower = quantile(value, probs = 0.025),
            upper = quantile(value, probs = 0.975)) %>%
  filter(year >= 2021) %>%
  group_by(year) %>%
  ggplot(aes(x = year, y = median, group = prep_coverage)) +
  geom_line(aes(color = prep_coverage), show.legend = FALSE) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = prep_coverage), alpha = 0.1) +
  scale_color_viridis(direction = -1, name = "PrEP coverage") +
  scale_fill_viridis(direction = -1, name = "PrEP coverage", breaks = seq(0, 100, by = 20)) +
  scale_y_continuous(limits = c(0, 10000), breaks = seq(0, 10000, by = 2000), labels = comma) +
  scale_x_continuous(limits = c(2021, 2032), breaks = seq(2020, 2032, by = 2)) +
  labs(x = "Year", y = "Infections averted") +
  ggtitle("Infections averted, women 16-35") +
  guides(fill = guide_legend(reverse = TRUE, override.aes = list(alpha = 1)))

## Infections averted, women 16-35, with uncertainty
inf_averted_all_years_echo_ui_plot <- results_by_run$new_inf_echo %>%
  filter(year >= 2022 & year <= 2032) %>%
  group_by(run_num, prep_coverage) %>%
  summarise(value = sum(value)) %>%
  mutate(value_base = value[prep_coverage == 0]) %>%
  filter(prep_coverage > 0) %>%
  mutate(value = -(value - value_base)) %>%
  group_by(prep_coverage) %>%
  summarise(median = median(value),
            lower = quantile(value, probs = 0.025),
            upper = quantile(value, probs = 0.975)) %>%
  ggplot(aes(x = prep_coverage, y = median)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper)) +
  scale_y_continuous(limits = c(0, 100000), breaks = seq(0, 100000, by = 10000), labels = comma) +
  scale_x_continuous(limits = c(0, 105), breaks = seq(0, 100, by = 20)) +
  labs(x = "PrEP coverage (%)", y = "Infections averted") +
  ggtitle("Infections averted among women 16-35, 2022-2032")

## Infections averted, entire province, with uncertainty
inf_averted_all_years_ui_plot <- results_by_run$new_inf %>%
  filter(year >= 2022 & year <= 2032) %>%
  group_by(run_num, prep_coverage) %>%
  summarise(value = sum(value)) %>%
  mutate(value_base = value[prep_coverage == 0]) %>%
  filter(prep_coverage > 0) %>%
  mutate(value = -(value - value_base)) %>%
  group_by(prep_coverage) %>%
  summarise(median = median(value),
            lower = quantile(value, probs = 0.025),
            upper = quantile(value, probs = 0.975)) %>%
  ggplot(aes(x = prep_coverage, y = median)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper)) +
  scale_y_continuous(limits = c(-500, 120000), breaks = seq(0, 120000, by = 10000), labels = comma) +
  scale_x_continuous(limits = c(0, 105), breaks = seq(0, 100, by = 20)) +
  labs(x = "PrEP coverage (%)", y = "Infections averted") +
  ggtitle("Infections averted, entire province, 2022-2032")

## Plots
pdf(file = file.path("scenarios", suite_name, "prep_sweep_plots.pdf"), height = 6, width = 8)
print(prep_timestep_plot)
print(inc_echo_plot)
print(inc_echo_ui_plot)
print(rr_echo_plot)
print(rr_echo_ui_plot)
print(rr_echo_all_years_ui_plot)
print(new_inf_echo_plot)
print(new_inf_echo_ui_plot)
print(inf_averted_echo_plot)
print(inf_averted_echo_ui_plot)
print(inf_averted_all_years_echo_ui_plot)
print(new_inf_plot)
print(new_inf_ui_plot)
print(inf_averted_plot)
print(inf_averted_ui_plot)
print(inf_averted_all_years_ui_plot)
dev.off()

## Individual plots for presentation
pdf(file = file.path("scenarios", suite_name, "inc_echo_ui_plot.pdf"), height = 4, width = 5)
print(inc_echo_ui_plot)
dev.off()

pdf(file = file.path("scenarios", suite_name, "rr_echo_all_years_ui_plot.pdf"), height = 4, width = 5)
print(rr_echo_all_years_ui_plot +
        theme(plot.title = element_text(size=11))
        )
dev.off()

pdf(file = file.path("scenarios", suite_name, "inf_averted_echo_ui_plot.pdf"), height = 4, width = 5)
print(inf_averted_echo_ui_plot)
dev.off()

pdf(file = file.path("scenarios", suite_name, "inf_averted_all_years_echo_ui_plot.pdf"), height = 4, width = 5)
print(inf_averted_all_years_echo_ui_plot)
dev.off()

pdf(file = file.path("scenarios", suite_name, "inf_averted_all_years_ui_plot.pdf"), height = 4, width = 5)
print(inf_averted_all_years_ui_plot)
dev.off()
