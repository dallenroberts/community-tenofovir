################################################################################
## Allen Roberts
## Estimate sex-specific ART coverage from UNAIDS data
## Takes into account the relative HIV prevalence for men and women as well as 
## the differences in ART coverage observed in PHIA
################################################################################

library(tidyverse)

## Suppress unhelpful warnings
options(dplyr.summarise.inform = FALSE) 

## Load calibration data
load(file.path("calibration", "data", "calibration_data.RData"))

## Reference prevalence - use 2012 data for entire province, adults
prev_women <- prev_data$value[prev_data$county == "All" & prev_data$year == 2012 & prev_data$gender == "Women" & prev_data$age_lower == 15 & prev_data$age_upper == 50]
prev_men <- prev_data$value[prev_data$county == "All" & prev_data$year == 2012 & prev_data$gender == "Men" & prev_data$age_lower == 15 & prev_data$age_upper == 50]

prev_mult <- prev_women/prev_men

## ART coverage ratio
art_mult <- art_prev_data$value[art_prev_data$year == 2018 & art_prev_data$gender == "Women"]/art_prev_data$value[art_prev_data$year == 2018 & art_prev_data$gender == "Men"]

## Multiplier to get gender-specific ART coverage
gender_mult <- (prev_men + prev_women*art_mult)/(prev_men + prev_women)

## Gender-specific ART coverage
art_prev_gender_unaids <- art_prev_data %>% 
  filter(Source == "UNAIDS") %>%
  mutate(art_prev_men = value/gender_mult,
         art_prev_women = value*gender_mult) %>%
  select(year, art_prev_men, art_prev_women)

write.csv(art_prev_gender_unaids, file = file.path("calibration", "data", "art_prev_gender_unaids.csv"), row.names = FALSE)


