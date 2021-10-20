################################################################################
## Allen Roberts
## Assess HIV incidence
################################################################################

## Libraries
library(tidyverse)
library(parallel)
library(foreach)
library(doParallel)

## Suppress unhelpful warnings
options(dplyr.summarise.inform = FALSE) 

## Set up parallels
numCores <- detectCores()
registerDoParallel(numCores)

## Function to combine results after parallel processing
comb <- function(x, ...) {  
  mapply(rbind,x,...,SIMPLIFY=FALSE)
}

## Run information
suite_name <- "nyanza-incidence-noprep"
scenario_name <- "Baseline-campaign_western_Kenya-Baseline"
scale_factor <- 0.05
report_interval <- 1/12

## Load EMOD runs
file_path <- paste("scenarios", suite_name, scenario_name, "ReportHIVByAgeAndGender", sep = "/")
file_list <- list.files(file_path, pattern = ".csv")

## Load incidence data from ECHO
## load(file.path("calibration", "data", "calibration_data.RData"))

outputs <- foreach(i=1:length(file_list), .combine = 'comb', .multicombine = TRUE) %dopar% {
  
  ## Reload packages - sometimes necessary when parallel computing
  library(tidyverse)
  
  # print(i)
  
  ## Read data 
  df <- read.csv(file.path(file_path, file_list[[i]]), stringsAsFactors = FALSE)
  
  ## Scale up totals based on population scale factor used in the model
  df <- df %>%
    mutate_at(.vars = vars(Population:Newly.Tested.Negative), .funs = function(x) x/scale_factor)
  
  run_num <- run_num <- as.integer(gsub("[^0-9]", "", unlist(strsplit(file_list[[i]], "_"))[2]))
  df$run_num <- run_num
  
  ## Standardize some variable names
  df <- df %>%
    rename("age_lower" = "Age",
           "gender" = "Gender",
           "year" = "Year") %>%
    mutate(county = case_when(NodeId == 1 ~ "Homa_Bay",
                              NodeId == 2 ~ "Kisii",
                              NodeId == 3 ~ "Kisumu",
                              NodeId == 4 ~ "Migori",
                              NodeId == 5 ~ "Nyamira",
                              NodeId == 6 ~ "Siaya"),
           gender = ifelse(gender == 0, "Men", "Women"),
           age_upper = case_when(age_lower == 0 ~ 16,
                                 age_lower == 16 ~ 18,
                                 age_lower == 18 ~ 25,
                                 age_lower == 25 ~ 36,
                                 age_lower == 36 ~ 100,
                                 age_lower == 100 ~ 105),
           risk = case_when(IP_Key.Risk == "HIGH" ~ "High",
                            IP_Key.Risk == "MEDIUM" ~ "Medium",
                            IP_Key.Risk == "LOW" ~ "Low"),
           risk = factor(risk, levels = c("Low", "Medium", "High")))

  ## Incidence
  ## Start and end of time interval for incidence calculation
  df$year_start <- df$year - report_interval
  df$year_end <- df$year ## not needed, but just to make it explicit

  ## One-year intervals corresponding to calendar year during which new infections/person-time were accrued
  df$year_floor <- floor(df$year_start)

  ## Person-time at risk
  df$person_years_at_risk <- ifelse(df$HasHIV == 0, df$Population*report_interval, df$Newly.Infected*report_interval/2)
  
  ## Incidence among women ages 16-35 (ECHO), by risk group
  inc_echo_risk <- df %>%
    filter(gender == "Women" & age_lower >= 16 & age_upper < 36 & year_floor >= 2010) %>%
    group_by(run_num,  year_floor, gender, risk) %>%
    summarise(new_infections = sum(Newly.Infected),
              person_years = sum(person_years_at_risk)) %>%
    mutate(value = 100*new_infections/person_years,
           year = year_floor,
           measure = "Incidence",
           county = "All",
           age_lower = 16,
           age_upper = 35,
           source = "ECHO") %>%
    ungroup() %>%
    select(-year_floor)
  
  ## Incidence among women ages 16-35 (ECHO)
  inc_echo <- df %>%
    filter(gender == "Women" & age_lower >= 16 & age_upper < 36 & year_floor >= 2010) %>%
    group_by(run_num,  year_floor, gender) %>%
    summarise(new_infections = sum(Newly.Infected),
              person_years = sum(person_years_at_risk)) %>%
    mutate(value = 100*new_infections/person_years,
           year = year_floor,
           measure = "Incidence",
           county = "All",
           age_lower = 16,
           age_upper = 35,
           risk = "All",
           source = "ECHO") %>%
    ungroup() %>%
    select(-year_floor)
  
  ## Incidence among women ages 18-24 (MPYA), by risk group
  inc_mpya_risk <- df %>%
    filter(gender == "Women" & age_lower >= 18 & age_upper < 26 & year_floor >= 2010) %>%
    group_by(run_num,  year_floor, gender, risk) %>%
    summarise(new_infections = sum(Newly.Infected),
              person_years = sum(person_years_at_risk)) %>%
    mutate(value = 100*new_infections/person_years,
           year = year_floor,
           measure = "Incidence",
           county = "All",
           age_lower = 18,
           age_upper = 25,
           source = "MPYA") %>%
    ungroup() %>%
    select(-year_floor)
  
  ## Incidence among women ages 18-24 (MPYA)
  inc_mpya <- df %>%
    filter(gender == "Women" & age_lower >= 18 & age_upper < 26 & year_floor >= 2010) %>%
    group_by(run_num,  year_floor, gender) %>%
    summarise(new_infections = sum(Newly.Infected),
              person_years = sum(person_years_at_risk)) %>%
    mutate(value = 100*new_infections/person_years,
           year = year_floor,
           measure = "Incidence",
           county = "All",
           age_lower = 18,
           age_upper = 25,
           risk = "All",
           source = "MPYA") %>%
    ungroup() %>%
    select(-year_floor)
  
  return(list("inc_echo_risk" = inc_echo_risk,
              "inc_echo" = inc_echo,
              "inc_mpya_risk" = inc_mpya_risk,
              "inc_mpya" = inc_mpya
  ))
  
}

## Calculate summary statistics across runs
for(ii in 1:length(outputs)) {
  
  outputs[[ii]] <- outputs[[ii]] %>%
    group_by(year, county, gender, risk, age_lower, age_upper, measure, source) %>%
    summarise(mean = mean(value),
              median = median(value),
              lower = quantile(value, probs = 0.025),
              upper = quantile(value, probs = 0.975))
    
}

## Save output
save(outputs, 
     file_path, 
     suite_name, 
     scenario_name, 
     scale_factor,
     file = file.path("scenarios", suite_name, scenario_name, "incidence_results.RData"))
