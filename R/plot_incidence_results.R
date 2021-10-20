################################################################################
## Allen Roberts
## Plot incidence results
################################################################################

library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(lemon)

## Suppress unhelpful warnings
options(dplyr.summarise.inform = FALSE) 

## Plot settings
theme_set(theme_classic())

## Load calibration results
suite_name <- "nyanza-noprep-sf03-arttest2-inc"
scenario_name <- "Baseline-campaign_western_Kenya_noPrEP_ARTtest-NoPrEP"
load(file.path("scenarios", suite_name, scenario_name, "calibration_results.RData"))

## ECHO data
echo_inc <- data.frame(year = 2016.5, value = 1.21, lower = 0.678, upper = 1.90, source = "ECHO")

## Incidence among women ages 16-35, stratified by risk
inc_risk_year_echo_ages_plot <- outputs$inc_echo_year_risk %>%
          filter(year >= 2015) %>%
          mutate(risk = factor(risk, levels = c("HIGH", "MEDIUM", "LOW"), labels = c("High", "Medium", "Low"))) %>%
          ggplot(aes(x = year, y = median, color = risk, fill = risk)) +
          geom_line() +
          geom_ribbon(aes(ymin = lower, ymax = upper, fill = risk),  alpha = 0.2, show.legend = FALSE) +
          scale_color_brewer(palette = "Set1", name = "Risk") +
          scale_fill_brewer(palette = "Set1") +
          labs(x = "Year", y = "HIV Incidence (per 100 PY)") +
          scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
                scale_x_continuous(limits = c(2015, 2020), breaks = seq(2015, 2020, by = 1)) +
          ggtitle("HIV incidence by risk and year, women ages 16-35")

## Incidence among women ages 16-35
inc_year_echo_ages_plot <- outputs$inc_echo_year %>%
        filter(year >= 2015) %>%
        ggplot(aes(x = year, y = median)) +
        geom_line() +
        geom_ribbon(aes(ymin = lower, ymax = upper),  alpha = 0.2, show.legend = FALSE) +
        geom_point(data = echo_inc, aes(x = year, y = value, shape = source)) +
        geom_errorbar(data = echo_inc, aes(x = year, y = value, ymin = lower, ymax = upper)) +
        labs(x = "Year", y = "HIV Incidence (per 100 PY)") +
        scale_y_continuous(limits = c(0, 3), breaks = seq(0, 3, by = 0.5)) +
        scale_x_continuous(limits = c(2015, 2020), breaks = seq(2015, 2020, by = 1)) +
        ggtitle("HIV incidence by year, women ages 16-35")

## Direct comparison by year
echo_model_results <- bind_rows(outputs$inc_echo, outputs$inc_echo_low_med)

inc_echo_ages_plot <- echo_model_results %>%
  ungroup() %>%
  mutate(source = paste("Model:", risk, "risk")) %>%
  rename("value" = "median") %>%
  select(year, source, value, lower, upper) %>%
  bind_rows(echo_inc) %>%
  ggplot(aes(x = source, y= value)) +
  geom_bar(aes(fill = source), stat = "identity") +
  geom_errorbar(aes(ymin = lower, ymax = upper)) +
  scale_fill_brewer(palette = "Set1", name = "Source") +
  labs(x = "Estimate", y = "Incidence (per 100 PY)") +
  scale_y_continuous(limits = c(0, 3), breaks = seq(0, 3, by = 0.5)) +
  ggtitle("Incidence among women 16-35: 2016-2017") +
  theme(plot.title = element_text(size=12))

pdf(file = file.path("scenarios", suite_name, scenario_name, "inc_echo_model_comparison.pdf"), height = 6, width = 8)
print(inc_risk_year_echo_ages_plot)
print(inc_year_echo_ages_plot)
print(inc_echo_ages_plot)
dev.off()

## Validation
outputs$inc_echo

## Plotting datasets
# inc <- bind_rows(outputs$inc_echo, outputs$inc_mpya)
# inc$group <- paste0(inc$age_lower ,"-", inc$age_upper)
# 
# inc_risk <- bind_rows(outputs$inc_echo_risk, outputs$inc_mpya_risk)
# inc_risk$group <- paste0(inc_risk$age_lower ,"-", inc_risk$age_upper)

## Plots
## HIV incidence, facetted by study
# pdf(file = file.path("scenarios", suite_name, scenario_name, "inc.pdf"), height = 6, width = 8)
# print(inc %>%
#         ggplot(aes(x = year, y = median, color = source, fill = source)) +
#         geom_line(show.legend = FALSE) +
#         geom_ribbon(aes(ymin = lower, ymax = upper),  alpha = 0.2, show.legend = FALSE) +
#         scale_color_brewer(palette = "Set1", name = "Gender") +
#         scale_fill_brewer(palette = "Set1") +
#         labs(x = "Year", y = "HIV Incidence (per 100 PY)") +
#         scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
#         scale_x_continuous(limits = c(2010, 2020), breaks = seq(2010, 2020, by = 5)) +
#         facet_wrap(~group) +
#         ggtitle("HIV incidence, women")
#       )
# dev.off()
# 
# ## HIV incidence, facetted by study and risk
# pdf(file = file.path("scenarios", suite_name, scenario_name, "inc_risk.pdf"), height = 6, width = 8)
# print(inc_risk %>%
#         ggplot(aes(x = year, y = median, color = source, fill = source)) +
#         geom_line(show.legend = FALSE) +
#         geom_ribbon(aes(ymin = lower, ymax = upper),  alpha = 0.2, show.legend = FALSE) +
#         scale_color_brewer(palette = "Set1", name = "Gender") +
#         scale_fill_brewer(palette = "Set1") +
#         labs(x = "Year", y = "HIV Incidence (per 100 PY)") +
#         scale_y_continuous(limits = c(0, 20), breaks = seq(0, 20, by = 5)) +
#         scale_x_continuous(limits = c(2010, 2020), breaks = seq(2010, 2020, by = 5)) +
#         facet_wrap(~group + risk, nrow = 2) +
#         ggtitle("HIV incidence, women")
# )
# dev.off()

