################################################################################
## Allen Roberts
## Select calibration runs that closely match observed data
################################################################################

## Libraries
library(tidyverse)
library(ggplot2)
library(RColorBrewer)

## Plot settings
theme_set(theme_classic())

## Suppress unhelpful warnings
options(dplyr.summarise.inform = FALSE) 

## Run information
suite_name <- "nyanza-circ-AB-BI-sweep2"
scenario_name <- "Baseline-campaign_western_Kenya_ART_RT_Circ-BI-0009"
existing_params_filename <- "Selected_Trajectory_Params_Clean_Unique_Formatted_ConstBI-0013.csv"
existing_params <- read.csv(file.path("calibration", "parameter_sets", existing_params_filename), stringsAsFactors = FALSE)
parameters_to_add_filename <- "Nyanza_30_40_50_iter174_fixed_constBI-0009.csv"
params_to_add <- read.csv(file.path("calibration", "parameter_sets", parameters_to_add_filename), stringsAsFactors = FALSE)
num_runs_to_select <- 250-179

## Load calibration results
load(file.path("scenarios", suite_name, scenario_name, "calibration_results_by_run.RData"))

## Load calibration data
load(file.path("calibration", "data", "calibration_data.RData"))

## Calculate sum of squared errors for gender-specific HIV prevalence
best_runs <- prev_data %>%
  filter(county == "All" & gender != "Both" & age_lower == 15 & age_upper == 50) %>%
  select(-c(measure, lower, upper)) %>%
  rename("observed_value" = "value") %>%
  left_join(run_results$prev_gender, by = c("year", "gender", "county", "age_lower", "age_upper")) %>%
  rename("model_value" = "value") %>%
  group_by(run_num) %>%
  summarise(sse = sum((model_value - observed_value)^2)) %>%
  slice_min(sse, n = num_runs_to_select)

## Plot runs against HIV prevalence data for sanity check
run_results$prev_gender %>%
  filter(run_num %in% unique(best_runs$run_num)) %>%
  filter(year <= 2020) %>%
  ggplot(aes(x = year, y = value, color = gender, fill = gender)) +
  geom_line(aes(group = run_num), alpha = 0.2) +
  geom_point(data = prev_data %>% filter(county == "All" & gender != "Both" & age_lower == 15 & age_upper == 50), aes(x = year, y = value), show.legend =  FALSE) +
  geom_errorbar(data = prev_data %>% filter(county == "All" & gender != "Both" & age_lower == 15 & age_upper == 50), aes(y = value, ymin = lower, ymax = upper)) +
  scale_color_brewer(palette = "Set1", name = "Gender") +
  scale_fill_brewer(palette = "Set1") +
  labs(x = "Year", y = "HIV Prevalence") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  facet_wrap(~gender) +
  ggtitle("HIV prevalence, ages 15-49")
  

## Create new parameter file, giving the new parameters new run numbers
new_params <- params_to_add %>%
  filter(parameterization_id %in% best_runs$run_num) %>%
  mutate(parameterization_id = 251:(251+num_runs_to_select - 1)) %>%
  bind_rows(existing_params) %>%
  arrange(parameterization_id) %>%
  rename("SeedYr HIGH" = "SeedYr.HIGH") ## Fix period that was added instead of space during reading of CSV

## Save
write.csv(new_params, file.path("calibration", "parameter_sets", "calibration_parameters_combined.csv"), row.names = FALSE)
