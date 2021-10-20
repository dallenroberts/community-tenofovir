################################################################################
## Allen Roberts
## PrEP use distribution
################################################################################

## Libraries
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(viridis)
library(viridisLite)

## Suppress unhelpful warnings
options(dplyr.summarise.inform = FALSE) 

## Run information
suite_name <- "nyanza-prep-test"
scenario_name <- "Baseline-campaign_western_Kenya_ART_RT_Circ_PrEP-50-PrEP-50"

prep_events <- read.csv(file.path("scenarios", suite_name, scenario_name, "ReportEventRecorder", "ReportEventRecorder.csv"), stringsAsFactors = FALSE)

## Test that Individual_ID is unique
prep_events %>%
  select(Node_ID, Individual_ID) %>%
  distinct() %>%
  summarise(n = n(),
            n_unique = length(unique(Individual_ID)))

## Plot distribution of PrEP use
prep_duration_plot <- prep_events %>%
  select(Event_Name, Individual_ID, Age) %>%
  arrange(Individual_ID, Age) %>%
  group_by(Individual_ID) %>%
  mutate(start = if_else(Event_Name == "PrEP_Received", Age, NA_real_),
         end = lead(Age)) %>%
  filter(!is.na(start) & !is.na(end)) %>%
  mutate(duration = end - start) %>%
  ggplot(aes(x = duration)) +
    geom_histogram(aes(y = stat(count/sum(count))), binwidth = 30.42) +
    labs(x = "Days of PrEP use", y = "Proportion") +
    scale_x_continuous(limits = c(0, 390), breaks = seq(0, 360, by = 30))
  
pdf(file = file.path("scenarios", suite_name, scenario_name, "PrEP_Duration_Distribution.pdf"), height = 6, width = 8)
print(prep_duration_plot)
dev.off()
