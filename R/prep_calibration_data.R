################################################################################
## Allen Roberts
## Format calibration data
################################################################################

library(tidyverse)
library(binom)

## Suppress unhelpful warnings
options(dplyr.summarise.inform = FALSE) 

## Load calibration data - move all this formatting to a separate script
pop_data <- read.csv(file.path("calibration", "data", "population.csv"), stringsAsFactors = FALSE)
art_data <- read.csv(file.path("calibration", "data", "art.csv"), stringsAsFactors = FALSE)
prev_data <- read.csv(file.path("calibration", "data", "prevalence.csv"), stringsAsFactors = FALSE)
art_prev_data <- read.csv(file.path("calibration", "data", "art_prev.csv"), stringsAsFactors = FALSE)
circ_data <- read.csv(file.path("calibration", "data", "circumcision_prev.csv"), stringsAsFactors = FALSE)

## Format calibration data
pop_data <- pop_data %>%
  rename("year" = "Year",
         "county" = "County",
         "gender" = "Gender",
         "value" = "Population") %>%
  separate(AgeBin, into = c("age_lower", "age_upper"), sep = ":") %>%
  mutate(age_lower = as.integer(gsub("\\[", "", age_lower)),
         age_upper = as.integer(gsub("\\)", "", age_upper)),
         gender = ifelse(gender == "Male", "Men", "Women")) %>%
  select(-c(weight, two_sigma)) %>%
  mutate(measure = "Population")

## Add population data for both genders combined
pop_data_both_genders <- pop_data %>%
  group_by(year, county, age_lower, age_upper, measure) %>%
  summarise(value = sum(value)) %>%
  mutate(gender = "Both",
         measure = "Population")

## Add population data across all ages and genders
pop_data_both_genders_all_ages <- pop_data %>%
  filter(year != 2019) %>%
  group_by(year, county, measure) %>%
  summarise(value = sum(value)) %>%
  mutate(gender = "Both",
         age_lower = 0,
         age_upper = 100,
         measure = "Population")

## Add population data across all genders and counties, by age
pop_data_both_genders_all_counties <- pop_data %>%
  filter(year != 2019) %>%
  group_by(year, measure, age_lower, age_upper) %>%
  summarise(value = sum(value)) %>%
  mutate(gender = "Both",
         county = "All",
         measure = "Population")

## Add population data across all ages and genders and counties
pop_data_both_genders_all_ages_all_counties <- pop_data %>%
  group_by(year, measure) %>%
  summarise(value = sum(value)) %>%
  mutate(county = "All",
         gender = "Both",
         age_lower = 0,
         age_upper = 100,
         measure = "Population")

## Add in additionally created datasets
pop_data <- bind_rows(pop_data, 
                      pop_data_both_genders, 
                      pop_data_both_genders_all_ages,
                      pop_data_both_genders_all_counties,
                      pop_data_both_genders_all_ages_all_counties)

prev_data <- prev_data %>%
  rename("year" = "Year",
         "county" = "County",
         "gender" = "Gender",
         "value" = "Prevalence") %>%
  mutate(lower = binom.confint(value*effective_count, effective_count, methods = "exact")$lower,
         lower = ifelse(effective_count == 0, lb, lower),
         upper = binom.confint(value*effective_count, effective_count, methods = "exact")$upper,
         upper = ifelse(effective_count == 0, ub, upper)) %>%
  separate(AgeBin, into = c("age_lower", "age_upper"), sep = ":") %>%
  mutate(age_lower = as.integer(gsub("\\[", "", age_lower)),
         age_upper = as.integer(gsub("\\)", "", age_upper)),
         gender = case_when(gender == "Female" ~ "Women",
                            gender == "Male" ~ "Men",
                            TRUE ~ gender)) %>%
  select(-c(weight, effective_count, lb, ub)) %>%
  mutate(measure = "HIV prevalence")

## Add population-weighted estimate of HIV prevalence for the entire province in 2018
prev_nyanza_2018 <- pop_data %>%
  filter(year == 2019 & gender == "Both" & county != "All") %>%
  rename("pop" = "value") %>%
  select(county, pop) %>%
  left_join(prev_data %>% filter(year == 2018), by = c("county")) %>%
  mutate(num_infected = pop*value,
         num_infected_lower = pop*lower,
         num_infected_upper = pop*upper) %>%
  summarise(value = sum(num_infected)/sum(pop),
            lower = sum(num_infected_lower)/sum(pop),
            upper = sum(num_infected_upper)/sum(pop)) %>%
  mutate("year" = 2018,
          "county" = "All",
         "gender" = "Both",
         "age_lower" = 15,
         "age_upper" = 65,
         "measure" = "HIV prevalence")

prev_data <- bind_rows(prev_data, prev_nyanza_2018)


art_data <- art_data %>%
  rename("year" = "Year",
         "county" = "County",
         "gender" = "Gender",
         "value" = "OnART") %>%
  separate(AgeBin, into = c("age_lower", "age_upper"), sep = ":") %>%
  mutate(age_lower = as.integer(gsub("\\[", "", age_lower)),
         age_upper = as.integer(gsub("\\)", "", age_upper)),
         gender = case_when(gender == "Female" ~ "Women",
                            gender == "Male" ~ "Men",
                            TRUE ~ gender)) %>%
  select(-c(weight, two_sigma)) %>%
  mutate(measure = "ART")

## ART, entire province, by age group and gender
art_age_gender <- art_data %>%
  filter(county != "All") %>%
  group_by(year, gender, age_lower, age_upper, measure) %>%
  summarise(value = sum(value)) %>%
  mutate(county = "All")

art_data <- bind_rows(art_data, art_age_gender)

## ART prevalence
art_prev_data <- art_prev_data %>%
  rename("year" = "Year",
         "county" = "County",
         "gender" = "Gender",
         "value" = "art_prev",
         "lower" = "lb",
         "upper" = "ub") %>%
  separate(AgeBin, into = c("age_lower", "age_upper"), sep = ":") %>%
  mutate(age_lower = as.integer(gsub("\\[", "", age_lower)),
         age_upper = as.integer(gsub("\\)", "", age_upper)),
         gender = case_when(gender == "Female" ~ "Women",
                            gender == "Male" ~ "Men",
                            TRUE ~ gender),
         measure = "ART Proportion") 

## Circumcision
circ_data <- circ_data %>%
  rename("year" = "Year",
         "county" = "County",
         "gender" = "Gender",
         "value" = "circ_prev",
         "lower" = "lb",
         "upper" = "ub") %>%
  separate(AgeBin, into = c("age_lower", "age_upper"), sep = ":") %>%
  mutate(age_lower = as.integer(gsub("\\[", "", age_lower)),
         age_upper = as.integer(gsub("\\)", "", age_upper)),
         measure = "Circumcision Prevalence") 


## Save
save(art_data, pop_data, prev_data, art_prev_data, circ_data, file = file.path("calibration", "data", "calibration_data.RData"))


