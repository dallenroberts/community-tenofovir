################################################################################
## Allen Roberts
## Compare parameter sets between Anna's calibration and Kathryn's calibration
################################################################################

library(tidyverse)
library(ggplot2)

## Suppress unhelpful warnings
options(dplyr.summarise.inform = FALSE) 

## Plot settings
theme_set(theme_classic())

## Load parameter sets
sets_ab <- read.csv(file.path("calibration", "parameter_sets", "Nyanza_30_40_50_iter174_fixed.csv"), stringsAsFactors = FALSE)
sets_kp <- read.csv(file.path("calibration", "parameter_sets", "Selected_Trajectory_Params_Clean_Unique_Formatted.csv"), stringsAsFactors = FALSE)

## Combine and reshape
sets_ab$source <- "AB"
sets_kp$source <- "KP"

sets <- rbind(sets_ab, sets_kp)

sets <- sets %>%
  select(-c(run_number, iteration_number, sim_id, likelihood, parameterization_id)) %>%
  pivot_longer(-source, names_to = "parameter")

pdf(file = file.path("calibration", "parameter_sets", "parameter_set_comparison.pdf"), height = 10, width = 12)
print(ggplot(data = sets, aes(x = source, y = value, color = source, fill = source)) +
  geom_boxplot(position = "dodge") +
  scale_color_brewer(palette = "Set1", name = "Source") +
  scale_fill_brewer(palette = "Set1") +
  facet_wrap(~parameter, scales = "free") +
  theme(strip.text.x = element_text(size = 6)))
dev.off()
