################################################################################
## Allen Roberts
## Plot calibration results
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
suite_name <- "nyanza-noprep-sf03-arttest2"
scenario_name <- "Baseline-campaign_western_Kenya_noPrEP_ARTtest-NoPrEP"
load(file.path("scenarios", suite_name, scenario_name, "calibration_results.RData"))

## Run-specific results
save_runs <- FALSE
if(save_runs == TRUE) {
  
  load(file.path("scenarios", suite_name, scenario_name, "calibration_results_by_run.RData"))

}

## Load calibration data
load(file.path("calibration", "data", "calibration_data.RData"))

## Counties
counties <- unique(outputs$pop_county_age$county)

## Plots
## Population size over time, entire province
pdf(file = file.path("scenarios", suite_name, scenario_name, "pop.pdf"), height = 6, width = 8)

print(outputs$pop %>%
        filter(year <= 2020) %>%
        ggplot(aes(x = year, y = median)) +
        geom_line() +
        geom_ribbon(aes(ymin = lower, ymax = upper),  alpha = 0.2, show.legend = FALSE) +
        geom_point(data = pop_data %>% filter(county == "All" & gender == "Both" & age_lower == 0 & age_upper == 100), aes(x = year, y = value)) +
        labs(x = "Year", y = "Population") + 
        scale_y_continuous(limits = c(0, 2e7)) +
        ggtitle("Nyanza Province")
      )

dev.off()

## Population size over time by age, entire province
pdf(file = file.path("scenarios", suite_name, scenario_name, "pop_age.pdf"), height = 6, width = 10)

pop_age_plot <- outputs$pop_age %>%
        filter(year <= 2020) %>%
        mutate(age = paste0(age_lower, "-", age_upper - 1),
               age = replace(age, age_lower == 0, "0-1"),
               age = fct_reorder(age, age_lower)) %>%
        ggplot(aes(x = year, y = median)) +
        geom_line() +
        geom_ribbon(aes(ymin = lower, ymax = upper),  alpha = 0.2, show.legend = FALSE) +
        geom_point(data = pop_data %>% 
                     filter(county == "All" & gender == "Both" & !(age_lower == 0 & age_upper == 100)) %>%
                     mutate(age = paste0(age_lower, "-", age_upper - 1),
                            age = replace(age, age_lower == 0, "0-1"),
                            age = fct_reorder(age, age_lower),
                            "Source" = "Census"), aes(x = year, y = value, shape = Source)) +
        labs(x = "Year", y = "Population") + 
        scale_x_continuous(limits = c(1980, 2020), breaks = seq(1980, 2020, by = 20)) +
        facet_rep_wrap(~age, nrow = 3, repeat.tick.labels = 'bottom') +
        ggtitle("Population")

print(pop_age_plot)

dev.off()

## Population size over time, by county
pdf(file = file.path("scenarios", suite_name, scenario_name, "pop_county.pdf"), height = 6, width = 8)

  print(outputs$pop_county %>%
          filter(year <= 2020) %>%
          ggplot(aes(x = year, y = median)) +
          geom_line() +
          geom_ribbon(aes(ymin = lower, ymax = upper),  alpha = 0.2, show.legend = FALSE) +
          geom_point(data = pop_data %>% filter(county != "All" & gender == "Both" & age_lower == 0 & age_upper == 100), aes(x = year, y = value)) +
          facet_wrap(~county) +
          labs(x = "Year", y = "Population"))
  
dev.off()


## Population size over time, by county and age
pdf(file = file.path("scenarios", suite_name, scenario_name, "pop_county_age.pdf"), height = 6, width = 8)
for(cc in counties) {
  
  print(outputs$pop_county_age %>%
    filter(county == cc & year <= 2020) %>%
    ggplot(aes(x = year, y = median)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper),  alpha = 0.2, show.legend = FALSE) +
    geom_point(data = pop_data %>% filter(county == cc & gender == "Both" & year != 2019), aes(x = year, y = value)) +
    labs(x = "Year", y = "Population") +
    facet_wrap(~age_lower) +
    ggtitle(cc))
  
}
dev.off()

## Population size, by age, just for 2009
pdf(file = file.path("scenarios", suite_name, scenario_name, "pop_age_2009.pdf"), height = 4, width = 5)

pop_age_2009_plot <- outputs$pop_age %>%
  filter(year == 2009) %>%
  mutate(age = paste0(age_lower, "-", age_upper - 1),
         age = replace(age, age_lower == 0, "0-1"),
         age = fct_reorder(age, age_lower),
         Source = "Model") %>%
  ggplot(aes(x = age, y = median, color = Source, shape = Source)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), show.legend = FALSE) +
  geom_point(data = pop_data %>% 
               filter(county == "All" & gender == "Both" & !(age_lower == 0 & age_upper == 100)) %>%
               mutate(age = paste0(age_lower, "-", age_upper - 1),
                      age = replace(age, age_lower == 0, "0-1"),
                      age = fct_reorder(age, age_lower),
                      Source = "Census"), aes(x = age, y = value)) +
  scale_color_brewer(palette = "Set1") +
  scale_shape_manual(values = c(17, 20)) +
  labs(x = "Age", y = "Population")  +
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5)) +
  ggtitle("Population (2009)")
  
print(pop_age_2009_plot)

dev.off()


## Number on ART
pdf(file = file.path("scenarios", suite_name, scenario_name, "art.pdf"), height = 6, width = 8)
print(outputs$art %>%
  filter(year <= 2020) %>%
  ggplot(aes(x = year, y = median)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper),  alpha = 0.2, show.legend = FALSE) +
  geom_point(data = art_data %>% filter(county == "All" & gender == "Both"), aes(x = year, y = value)) +
  labs(x = "Year", y = "# on ART")
)
dev.off()

## Number on ART, by gender and age group
pdf(file = file.path("scenarios", suite_name, scenario_name, "art_gender_age.pdf"), height = 6, width = 8)
print(outputs$art_gender_age %>%
        filter(year <= 2020) %>%
        ggplot(aes(x = year, y = median, color = gender, fill = gender)) +
        geom_line() +
        geom_ribbon(aes(ymin = lower, ymax = upper),  alpha = 0.2, show.legend = FALSE) +
        geom_point(data = art_data %>% filter(county == "All" & gender != "Both"), aes(x = year, y = value), show.legend = FALSE) +
        scale_color_brewer(palette = "Set1", name = "Gender") +
        scale_fill_brewer(palette = "Set1") +
        labs(x = "Year", y = "# on ART") +
        facet_wrap(~age_lower)
)
dev.off()

## Number on ART, adults by gender
pdf(file = file.path("scenarios", suite_name, scenario_name, "art_gender.pdf"), height = 6, width = 8)
art_gender_plot <- outputs$art_gender_age %>%
        filter(year <= 2020 & age_lower == 15) %>%
        ggplot(aes(x = year, y = median, color = gender)) +
        geom_line() +
        geom_ribbon(aes(ymin = lower, ymax = upper, fill = gender),  alpha = 0.2, show.legend = FALSE) +
        geom_point(data = art_data %>% 
                     filter(county == "All" & gender != "Both" & age_lower == 15)  %>%
                     mutate("Source" = "Kenya MOH"), aes(x = year, y = value, shape = Source)) +
        scale_color_brewer(palette = "Set1", name = "Gender") +
        scale_fill_brewer(palette = "Set1") +
        labs(x = "Year", y = "# on ART") +
        ggtitle("Number on ART, adults 15+")
print(art_gender_plot)
dev.off()

## Same plot, formatted for presentation
pdf(file = file.path("scenarios", suite_name, scenario_name, "art_gender_formatted.pdf"), height = 4, width = 5)
art_gender_formatted_plot <- outputs$art_gender_age %>%
  filter(year <= 2020 & age_lower == 15) %>%
  ggplot(aes(x = year, y = median, color = gender)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = gender),  alpha = 0.2, show.legend = FALSE) +
  geom_point(data = art_data %>% 
               filter(county == "All" & gender != "Both" & age_lower == 15)  %>%
               mutate("Source" = "Kenya MOH"), aes(x = year, y = value, shape = Source)) +
  scale_color_brewer(palette = "Set1", name = "Gender") +
  scale_fill_brewer(palette = "Set1") +
  labs(x = "Year", y = "# on ART") +
  ggtitle("Number on ART, adults 15+") +
  guides(color = guide_legend(order = 2), 
         shape = guide_legend(order = 1))
print(art_gender_formatted_plot)
dev.off()

## Number on ART, by gender, age group, and county
pdf(file = file.path("scenarios", suite_name, scenario_name, "art_county_gender_age.pdf"), height = 6, width = 8)
print(outputs$art_county_gender_age %>%
        filter(year <= 2020) %>%
        ggplot(aes(x = year, y = median, color = gender, fill = gender)) +
        geom_line() +
        geom_ribbon(aes(ymin = lower, ymax = upper),  alpha = 0.2, show.legend = FALSE) +
        geom_point(data = art_data %>% filter(county != "All" & gender != "Both"), aes(x = year, y = value)) +
        scale_color_brewer(palette = "Set1", name = "Gender") +
        scale_fill_brewer(palette = "Set1") +
        labs(x = "Year", y = "# on ART") +
        facet_wrap(~county + age_lower)
)
dev.off()

## Proportion of HIV+ on ART
pdf(file = file.path("scenarios", suite_name, scenario_name, "art_prev.pdf"), height = 6, width = 8)
art_prev_plot <- outputs$art_prev %>%
        filter(year <= 2020) %>%
        ggplot(aes(x = year, y = median)) +
        geom_line() +
        geom_ribbon(aes(ymin = lower, ymax = upper),  alpha = 0.2, show.legend = FALSE) +
        geom_point(data = art_prev_data %>% 
                     filter(gender == "Both") %>%
                     mutate("Source" = "UNAIDS Estimate"), aes(x = year, y = value, shape = Source)) +
        geom_errorbar(data = art_prev_data %>% filter(gender == "Both"), aes(y = value, ymin = lower, ymax = upper), show.legend = FALSE) +
        scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
        labs(x = "Year", y = "Proportion HIV+ on ART") +
        ggtitle("ART Prevalence")
print(art_prev_plot)

dev.off()

## Same plot, formatted for presentation
pdf(file = file.path("scenarios", suite_name, scenario_name, "art_prev_formatted.pdf"), height = 4, width = 5)
art_prev_formatted_plot <- outputs$art_prev %>%
  filter(year <= 2020) %>%
  ggplot(aes(x = year, y = median)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper),  alpha = 0.2, show.legend = FALSE) +
  geom_point(data = art_prev_data %>% 
               filter(gender == "Both") %>%
               mutate("Source" = "UNAIDS"), aes(x = year, y = value, shape = Source)) +
  geom_errorbar(data = art_prev_data %>% filter(gender == "Both"), aes(y = value, ymin = lower, ymax = upper), show.legend = FALSE) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  labs(x = "Year", y = "Proportion HIV+ on ART") +
  ggtitle("ART prevalence")
print(art_prev_formatted_plot)

dev.off()

## Proportion of HIV+ on ART, by gender
pdf(file = file.path("scenarios", suite_name, scenario_name, "art_prev_gender.pdf"), height = 6, width = 8)
art_prev_gender_plot <- outputs$art_prev_gender %>%
        filter(year <= 2020) %>%
        ggplot(aes(x = year, y = median, color = gender)) +
        geom_line() +
        geom_ribbon(aes(ymin = lower, ymax = upper, fill = gender),  alpha = 0.2, show.legend = FALSE) +
        geom_point(data = art_prev_data %>%
                     filter(gender != "Both") %>%
                     mutate("Source" = "PHIA"), aes(x = year, y = value, shape = Source)) +
      geom_errorbar(data = art_prev_data %>% filter(gender != "Both"), aes(y = value, ymin = lower, ymax = upper), show.legend = FALSE) +
        scale_color_brewer(palette = "Set1", name = "Gender") +
        scale_fill_brewer(palette = "Set1") +
        scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
        labs(x = "Year", y = "Proportion HIV+ on ART") +
        ggtitle("ART Prevalence, by Gender")
print(art_prev_gender_plot)
dev.off()

## Same plot, formatted for presentation
## Proportion of HIV+ on ART, by gender
pdf(file = file.path("scenarios", suite_name, scenario_name, "art_prev_gender_formatted.pdf"), height = 4, width = 5)
art_prev_gender_formatted_plot <- outputs$art_prev_gender %>%
  filter(year <= 2020) %>%
  ggplot(aes(x = year, y = median, color = gender)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = gender),  alpha = 0.2, show.legend = FALSE) +
  geom_point(data = art_prev_data %>%
               filter(gender != "Both") %>%
               mutate("Source" = "PHIA"), aes(x = year, y = value, shape = Source)) +
  geom_errorbar(data = art_prev_data %>% filter(gender != "Both"), aes(y = value, ymin = lower, ymax = upper), show.legend = FALSE) +
  scale_color_brewer(palette = "Set1", name = "Gender") +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  labs(x = "Year", y = "Proportion HIV+ on ART") +
  ggtitle("ART prevalence, by gender") +
  guides(color = guide_legend(order = 2), 
         shape = guide_legend(order = 1))

print(art_prev_gender_formatted_plot)
dev.off()


## Proportion of HIV+ on ART, by gender, no year restriction
pdf(file = file.path("scenarios", suite_name, scenario_name, "art_prev_gender_all_years.pdf"), height = 6, width = 8)
art_prev_gender_all_years_plot <- outputs$art_prev_gender %>%
  ggplot(aes(x = year, y = median, color = gender)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = gender),  alpha = 0.2, show.legend = FALSE) +
  geom_point(data = art_prev_data %>%
               filter(gender != "Both") %>%
               mutate("Source" = "PHIA"), aes(x = year, y = value, shape = Source)) +
  geom_errorbar(data = art_prev_data %>% filter(gender != "Both"), aes(y = value, ymin = lower, ymax = upper), show.legend = FALSE) +
  scale_color_brewer(palette = "Set1", name = "Gender") +
  scale_fill_brewer(palette = "Set1") +
  geom_hline(yintercept = 0.90*0.90, linetype = "dashed", color = "seagreen3") +
  geom_text(x = 2045, y = 0.90*0.90, label = "90-90", color = "seagreen3", vjust = 2) +
  geom_hline(yintercept = 0.95*0.95, linetype = "dashed", color = "seagreen4") +
  geom_text(x = 2045, y = 0.95*0.95, label = "95-95", color = "seagreen4", vjust = -1) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  scale_x_continuous(limits = c(1980, 2045), breaks = seq(1980, 2040, by = 10)) +
  labs(x = "Year", y = "Proportion HIV+ on ART") +
  ggtitle("ART Prevalence, by Gender")
print(art_prev_gender_all_years_plot)
dev.off()


## Proportion of HIV+ adults on ART, by gender, no year restriction
pdf(file = file.path("scenarios", suite_name, scenario_name, "art_prev_gender_adults_all_years.pdf"), height = 6, width = 8)
art_prev_gender_adults_all_years_plot <- outputs$art_prev_gender_adults %>%
  ggplot(aes(x = year, y = median, color = gender)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = gender),  alpha = 0.2, show.legend = FALSE) +
  geom_point(data = art_prev_data %>%
               filter(gender != "Both") %>%
               mutate("Source" = "PHIA"), aes(x = year, y = value, shape = Source)) +
  geom_errorbar(data = art_prev_data %>% filter(gender != "Both"), aes(y = value, ymin = lower, ymax = upper), show.legend = FALSE) +
  scale_color_brewer(palette = "Set1", name = "Gender") +
  scale_fill_brewer(palette = "Set1") +
  geom_hline(yintercept = 0.90*0.90, linetype = "dashed", color = "seagreen3") +
  geom_text(x = 2045, y = 0.90*0.90, label = "90-90", color = "seagreen3", vjust = 2) +
  geom_hline(yintercept = 0.95*0.95, linetype = "dashed", color = "seagreen4") +
  geom_text(x = 2045, y = 0.95*0.95, label = "95-95", color = "seagreen4", vjust = -1) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  scale_x_continuous(limits = c(1980, 2045), breaks = seq(1980, 2040, by = 10)) +
  labs(x = "Year", y = "Proportion HIV+ on ART") +
  ggtitle("ART Prevalence, ages 15+, by Gender")
print(art_prev_gender_adults_all_years_plot)
dev.off()


## Proportion of HIV+ on ART, by gender and county
pdf(file = file.path("scenarios", suite_name, scenario_name, "art_prev_gender_county.pdf"), height = 6, width = 8)
print(outputs$art_prev_gender_county %>%
        #        filter(year <= 2020) %>%
        ggplot(aes(x = year, y = median, color = gender, fill = gender)) +
        geom_line() +
        geom_ribbon(aes(ymin = lower, ymax = upper),  alpha = 0.2, show.legend = FALSE) +
        scale_color_brewer(palette = "Set1", name = "Gender") +
        scale_fill_brewer(palette = "Set1") +
        scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
        labs(x = "Year", y = "Proportion HIV+ on ART") +
        facet_wrap(~county)
)
dev.off()

## Proportion of HIV+ on ART who are virally suppressed, by gender
# pdf(file = file.path("scenarios", suite_name, scenario_name, "prop_suppressed_gender.pdf"), height = 6, width = 8)
# print(outputs$prop_suppressed_gender %>%
#         #        filter(year <= 2020) %>%
#         ggplot(aes(x = year, y = median, color = gender, fill = gender)) +
#         geom_line() +
#         geom_ribbon(aes(ymin = lower, ymax = upper),  alpha = 0.2, show.legend = FALSE) +
#         scale_color_brewer(palette = "Set1", name = "Gender") +
#         scale_fill_brewer(palette = "Set1") +
#         scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
#         labs(x = "Year", y = "Proportion suppressed of HIV+ on ART")
# )
# dev.off()

## HIV prevalence, by run
if(save_runs == TRUE) {
  
pdf(file = file.path("scenarios", suite_name, scenario_name, "hiv_prev_by_run.pdf"), height = 6, width = 8)
  print(run_results$prev %>%
          filter(year <= 2020) %>%
          mutate(sim = ifelse(run_num > 250, "AB", "KP")) %>%
          ggplot(aes(x = year, y = value)) +
          geom_line(aes(group = run_num, color = sim), alpha = 0.2) +
          geom_point(data = prev_data %>% filter(county == "All" & gender == "Both"), aes(x = year, y = value)) +
          geom_errorbar(data = prev_data %>% filter(county == "All" & gender == "Both"), aes(y = value, ymin = lower, ymax = upper)) +
          labs(x = "Year", y = "HIV Prevalence") +
          scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2))
  )
  
  print(run_results$prev_gender %>%
          filter(year <= 2020) %>%
          mutate(sim = ifelse(run_num > 250, "AB", "KP")) %>%
          ggplot(aes(x = year, y = value)) +
          geom_line(aes(group = run_num, color = sim), alpha = 0.2) +
          geom_point(data = prev_data %>% filter(county == "All" & gender != "Both" & age_lower == 15 & age_upper == 50), aes(x = year, y = value), show.legend =  FALSE) +
          geom_errorbar(data = prev_data %>% filter(county == "All" & gender != "Both" & age_lower == 15 & age_upper == 50), aes(y = value, ymin = lower, ymax = upper)) +
          labs(x = "Year", y = "HIV Prevalence") +
          scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2))
   +
    facet_wrap(~gender))

  dev.off()
}

## HIV prevalence
pdf(file = file.path("scenarios", suite_name, scenario_name, "hiv_prev.pdf"), height = 6, width = 8)
hiv_prev_plot <- outputs$prev %>%
  filter(year <= 2020) %>%
  ggplot(aes(x = year, y = median)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper),  alpha = 0.2, show.legend = FALSE) +
  geom_point(data = prev_data %>% 
               filter(county == "All" & gender == "Both") %>%
               mutate("Source" = case_when(
                 year %in% c(2003, 2008.5) ~ "DHS",
                 year %in% c(2007, 2012) ~ "AIS",
                 year == 2018 ~ "PHIA"
               )), aes(x = year, y = value, shape = Source)) +
  geom_errorbar(data = prev_data %>% filter(county == "All" & gender == "Both"), aes(y = value, ymin = lower, ymax = upper)) +
  labs(x = "Year", y = "HIV Prevalence") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  ggtitle("HIV Prevalence")

print(hiv_prev_plot)
dev.off()

## HIV prevalence by gender, same plot
# pdf(file = file.path("scenarios", suite_name, scenario_name, "hiv_prev_gender.pdf"), height = 6, width = 8)
# print(outputs$prev_gender %>%
#   filter(year <= 2020) %>%
#   ggplot(aes(x = year, y = median, color = gender, fill = gender)) +
#   geom_line() +
#   geom_ribbon(aes(ymin = lower, ymax = upper),  alpha = 0.2, show.legend = FALSE) +
#   geom_point(data = prev_data %>% filter(county == "All" & gender != "Both" & age_lower == 15 & age_upper == 50), aes(x = year, y = value), show.legend =  FALSE) +
#   geom_errorbar(data = prev_data %>% filter(county == "All" & gender != "Both" & age_lower == 15 & age_upper == 50), aes(y = value, ymin = lower, ymax = upper)) +
#   scale_color_brewer(palette = "Set1", name = "Gender") +
#   scale_fill_brewer(palette = "Set1") +
#   labs(x = "Year", y = "HIV Prevalence") +
#   scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
#   ggtitle("HIV prevalence, ages 15-49")
# )
# dev.off()

## HIV prevalence by gender, facetted
pdf(file = file.path("scenarios", suite_name, scenario_name, "hiv_prev_gender_facet.pdf"), height = 6, width = 8)
hiv_prev_gender_plot <- outputs$prev_gender %>%
        filter(year <= 2020) %>%
        ggplot(aes(x = year, y = median, color = gender)) +
        geom_line() +
        geom_ribbon(aes(ymin = lower, ymax = upper, fill = gender),  alpha = 0.2, show.legend = FALSE) +
        geom_point(data = prev_data %>% filter(county == "All" & gender != "Both" & age_lower == 15 & age_upper == 50) %>% 
                     mutate("Source" = case_when(
          year %in% c(2003, 2008.5) ~ "DHS",
          year %in% c(2007, 2012) ~ "AIS",
          year == 2018 ~ "PHIA"
        )), aes(x = year, y = value, shape = Source)) +
        geom_errorbar(data = prev_data %>% filter(county == "All" & gender != "Both" & age_lower == 15 & age_upper == 50), aes(y = value, ymin = lower, ymax = upper)) +
        scale_color_brewer(palette = "Set1", name = "Gender") +
        scale_fill_brewer(palette = "Set1") +
        labs(x = "Year", y = "HIV Prevalence") +
        scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
        facet_wrap(~gender) +
        ggtitle("HIV prevalence, ages 15-49")

print(hiv_prev_gender_plot)
dev.off()

## Same plot, but formatted for presentation
pdf(file = file.path("scenarios", suite_name, scenario_name, "hiv_prev_gender_facet_formatted.pdf"), height = 4, width = 5)
hiv_prev_gender_formatted_plot <- outputs$prev_gender %>%
  filter(year <= 2020) %>%
  ggplot(aes(x = year, y = median, color = gender)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = gender),  alpha = 0.2, show.legend = FALSE) +
  geom_point(data = prev_data %>% filter(county == "All" & gender != "Both" & age_lower == 15 & age_upper == 50) %>% 
               mutate("Source" = case_when(
                 year %in% c(2003, 2008.5) ~ "DHS",
                 year %in% c(2007, 2012) ~ "AIS",
                 year == 2018 ~ "PHIA"
               )), aes(x = year, y = value, shape = Source)) +
  geom_errorbar(data = prev_data %>% filter(county == "All" & gender != "Both" & age_lower == 15 & age_upper == 50), aes(y = value, ymin = lower, ymax = upper)) +
  scale_color_brewer(palette = "Set1", name = "Gender") +
  scale_fill_brewer(palette = "Set1") +
  labs(x = "Year", y = "HIV Prevalence") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  facet_wrap(~gender) +
  theme(panel.spacing = unit(1.5, "lines")) +
  ggtitle("HIV prevalence, 15-49")

print(hiv_prev_gender_formatted_plot)
dev.off()

## HIV prevalence by gender and age
pdf(file = file.path("scenarios", suite_name, scenario_name, "hiv_prev_gender_age.pdf"), height = 6, width = 8)
hiv_prev_gender_age_plot <- outputs$prev_gender_age %>%
        filter(year <= 2020 & age_upper <= 50) %>%
        mutate(age = paste0(age_lower, "-", age_upper - 1),
              age = replace(age, age_lower == 0, "0-1"),
              age = fct_reorder(age, age_lower)) %>%
        ggplot(aes(x = year, y = median, color = gender)) +
        geom_line() +
        geom_ribbon(aes(ymin = lower, ymax = upper, fill = gender),  alpha = 0.2, show.legend = FALSE) +
        geom_point(data = prev_data %>% 
                     filter(county == "All" & gender != "Both" & !(age_lower == 15 & age_upper == 50) & age_upper <= 50) %>% 
                     mutate("Source" = case_when(
                       year %in% c(2003, 2008.5) ~ "DHS",
                       year %in% c(2007, 2012) ~ "AIS",
                       year == 2018 ~ "PHIA"
                     ),
                     age = paste0(age_lower, "-", age_upper - 1),
                     age = replace(age, age_lower == 0, "0-1"),
                     age = fct_reorder(age, age_lower)), aes(x = year, y = value, shape = Source)) +
        geom_errorbar(data = prev_data %>% 
                        filter(county == "All" & gender != "Both" & !(age_lower == 15 & age_upper == 50) & age_upper <= 50) %>%
                      mutate(age = paste0(age_lower, "-", age_upper - 1),
                      age = replace(age, age_lower == 0, "0-1"),
                      age = fct_reorder(age, age_lower)), aes(y = value, ymin = lower, ymax = upper)) +
        scale_color_brewer(palette = "Set1", name = "Gender") +
        scale_fill_brewer(palette = "Set1") +
        labs(x = "Year", y = "HIV Prevalence") +
        scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
        facet_wrap(~gender + age) +
        ggtitle("HIV Prevalence by Gender and Age")
print(hiv_prev_gender_age_plot)
dev.off()

## HIV prevalence by gender and county
pdf(file = file.path("scenarios", suite_name, scenario_name, "hiv_prev_gender_county.pdf"), height = 6, width = 8)
print(outputs$prev_county_gender %>%
        filter(year <= 2020) %>%
        ggplot(aes(x = year, y = median, color = gender, fill = gender)) +
        geom_line() +
        geom_ribbon(aes(ymin = lower, ymax = upper),  alpha = 0.2, show.legend = FALSE) +
        geom_point(data = prev_data %>% filter(county != "All" & gender != "Both" & age_lower == 15 & age_upper == 50), aes(x = year, y = value), show.legend =  FALSE) +
        geom_errorbar(data = prev_data %>% filter(county != "All" & gender != "Both" & age_lower == 15 & age_upper == 50), aes(y = value, ymin = lower, ymax = upper)) +
        scale_color_brewer(palette = "Set1", name = "Gender") +
        scale_fill_brewer(palette = "Set1") +
        labs(x = "Year", y = "HIV Prevalence") +
        scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
        facet_wrap(~gender + county, nrow = 3) +
        ggtitle("HIV prevalence, ages 15-49")
)
dev.off()

## Circumcision, men ages 15-49
pdf(file = file.path("scenarios", suite_name, scenario_name, "circ_prev.pdf"), height = 6, width = 8)
circ_plot <- outputs$circ %>%
        filter(year <= 2020) %>%
        ggplot(aes(x = year, y = median)) +
        geom_line() +
        geom_ribbon(aes(ymin = lower, ymax = upper),  alpha = 0.2, show.legend = FALSE) +
        geom_point(data = circ_data %>% 
                     filter(county == "All" & year != 2003) %>%
                     mutate(Source = "DHS"), aes(x = year, y = value, shape = Source)) +
        geom_errorbar(data = circ_data %>% filter(county == "All" & year != 2003), aes(y = value, ymin = lower, ymax = upper)) +
        labs(x = "Year", y = "Circumcision Prevalence") +
        scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
        ggtitle("Circumcision prevalence, men ages 15-49")
print(circ_plot)
dev.off()

## Same plot, formatted for presentation
pdf(file = file.path("scenarios", suite_name, scenario_name, "circ_prev_formatted.pdf"), height = 4, width = 5)
circ_plot_formatted <- outputs$circ %>%
  filter(year >= 2000 & year <= 2020) %>%
  ggplot(aes(x = year, y = median)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper),  alpha = 0.2, show.legend = FALSE) +
  geom_point(data = circ_data %>% 
               filter(county == "All" & year != 2003) %>%
               mutate(Source = "DHS"), aes(x = year, y = value, shape = Source)) +
  geom_errorbar(data = circ_data %>% filter(county == "All" & year != 2003), aes(y = value, ymin = lower, ymax = upper), width = 1) +
  labs(x = "Year", y = "Proportion circumcised") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  ggtitle("Circumcision prevalence, men 15-49")
print(circ_plot_formatted)
dev.off()

pdf(file = file.path("scenarios", suite_name, scenario_name, "circ_prev_all_years.pdf"), height = 6, width = 8)
## Circumcision, men ages 15-49
circ_plot_all_years <- outputs$circ %>%
  ggplot(aes(x = year, y = median)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper),  alpha = 0.2, show.legend = FALSE) +
  geom_point(data = circ_data %>% 
               filter(county == "All" & year != 2003) %>%
               mutate(Source = "DHS"), aes(x = year, y = value, shape = Source)) +
  geom_errorbar(data = circ_data %>% filter(county == "All" & year != 2003), aes(y = value, ymin = lower, ymax = upper)) +
  labs(x = "Year", y = "Circumcision Prevalence") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  scale_x_continuous(breaks = seq(1980, 2040, by = 10)) +
  ggtitle("Circumcision prevalence, men ages 15-49")
print(circ_plot_all_years)
dev.off()

pdf(file = file.path("scenarios", suite_name, scenario_name, "circ_prev_15_24_all_years.pdf"), height = 6, width = 8)

## Circumcision, men ages 15-49
circ_plot_15_24_all_years <- outputs$circ_age %>%
  filter(age_lower >= 15 & age_upper <= 25) %>%
  ggplot(aes(x = year, y = median)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper),  alpha = 0.2, show.legend = FALSE) +
  # geom_hline(yintercept = 0.9, linetype = 'dashed') +
  labs(x = "Year", y = "Circumcision Prevalence") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  scale_x_continuous(limits = c(1980, 2045), breaks = seq(1980, 2040, by = 10)) +
  ggtitle("Circumcision prevalence, men ages 15-24")
print(circ_plot_15_24_all_years)
dev.off()

## Circumcision, men ages 15-49, by county
pdf(file = file.path("scenarios", suite_name, scenario_name, "circ_prev_county.pdf"), height = 6, width = 8)
print(outputs$circ_county %>%
        ggplot(aes(x = year, y = median)) +
        geom_line() +
        geom_ribbon(aes(ymin = lower, ymax = upper),  alpha = 0.2, show.legend = FALSE) +
        geom_point(data = circ_data %>% filter(county != "All"), aes(x = year, y = value)) +
        geom_errorbar(data = circ_data %>% filter(county != "All"), aes(y = value, ymin = lower, ymax = upper)) +
        labs(x = "Year", y = "Circumcision Prevalence") +
        scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
        facet_wrap(~county) +
        ggtitle("Circumcision prevalence, men ages 15-49")
)
dev.off()

## Circumcision, by age
pdf(file = file.path("scenarios", suite_name, scenario_name, "circ_prev_age.pdf"), height = 6, width = 8)
print(outputs$circ_age %>%
        ggplot(aes(x = year, y = median)) +
        geom_line() +
        geom_ribbon(aes(ymin = lower, ymax = upper),  alpha = 0.2, show.legend = FALSE) +
        labs(x = "Year", y = "Circumcision Prevalence") +
        scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
        facet_wrap(~age_lower) +
        ggtitle("Circumcision prevalence, men ages 15-49")
)
dev.off()

## All calibration plots
pdf(file = file.path("scenarios", suite_name, scenario_name, "calibration_plots.pdf"), height = 8, width = 10)
print(pop_age_plot)
print(hiv_prev_plot)
print(hiv_prev_gender_plot)
print(hiv_prev_gender_age_plot)
print(art_gender_plot)
print(art_prev_gender_plot)
print(circ_plot)
dev.off()

## Validation
outputs$prev_val

prev_data %>% filter(year == 2018 & county == "All")
