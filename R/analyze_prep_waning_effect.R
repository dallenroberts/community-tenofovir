################################################################################
## Allen Roberts
## Analyze PrEP waning effect configurations
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
suite_name <- "nyanza-prep100-sf03"
scenarios <- list.files(file.path("scenarios", suite_name))
scale_factor <- 0.03
report_interval <- 1/12
pop_adjust <- 0.5 ## Additional scale factor for the population

## Boolean to save certain results at the run level, rather than just after aggregating across runs
save_runs <- TRUE

## Boolean to analyze Report_Event_Recorder data
record_events <- FALSE

# scenario_name <- scenarios[[1]]

for(scenario_name in scenarios) {
  
  # prep_coverage <- 50
  
  # if(grepl("6mo", scenario_name) ) {
  #   if(grepl("CE", scenario_name)) {
  #     prep_method <- "Const_180"
  #   } else {
  #     prep_method <- "Exp_180"
  #   }
  # } else if(grepl("100", scenario_name)) {
  #   prep_method <- "Const_100"
  # } else {
  #   if(grepl("CE", scenario_name)) {
  #     prep_method <- "Const_90"
  #   } else {
  #     prep_method <- "Exp_90"
  #   }
  # }
  
  prep_coverage <- 100
  if(grepl("6mo", scenario_name)) {
    prep_method <- "Const_180"
  } else {
    prep_method <- "Const_90"
  }
    
  ## Load files
  if(record_events == TRUE) {
    event_recorder <- read.csv(file.path("scenarios", suite_name, scenario_name, "ReportEventRecorder.csv"), stringsAsFactors = FALSE)
  }
  
  file_path <- paste("scenarios", suite_name, scenario_name, "ReportHIVByAgeAndGender", sep = "/")
  file_list <- list.files(file_path, pattern = ".csv")
  
  ## Report HIV By Age and Gender analysis
  outputs <- foreach(i=1:length(file_list), .combine = 'comb', .multicombine = TRUE) %dopar% {
    
    ## Reload packages - sometimes necessary when parallel computing
    library(tidyverse)
    
    # print(i)
    
    ## Read data 
    df <- read.csv(file.path(file_path, file_list[[i]]), stringsAsFactors = FALSE)
    
    ## Scale up totals based on population scale factor used in the model
    df <- df %>%
      mutate_at(.vars = vars(Population:last_col()), .funs = function(x) x*pop_adjust/(scale_factor))
    
    run_num <- run_num <- as.integer(gsub("[^0-9]", "", unlist(strsplit(file_list[[i]], "_"))[2]))
    df$run_num <- run_num
    
    ## Standardize some variable names
    df <- df %>%
      rename("age_lower" = "Age",
             "gender" = "Gender",
             "hiv" = "HasHIV",
             "year" = "Year") %>%
      mutate(county = case_when(NodeId == 1 ~ "Homa_Bay",
                                NodeId == 2 ~ "Kisii",
                                NodeId == 3 ~ "Kisumu",
                                NodeId == 4 ~ "Migori",
                                NodeId == 5 ~ "Nyamira",
                                NodeId == 6 ~ "Siaya"),
             gender = ifelse(gender == 0, "Men", "Women"),
             age_upper = case_when(age_lower == 0 ~ 16,
                                   age_lower == 16 ~ 36,
                                   age_lower == 36 ~ 100,
                                   TRUE ~ age_lower + 5))
    # age_upper = case_when(age_lower == 0 ~ 1,
    #                       age_lower == 1 ~ 5,
    #                       age_lower == 80 ~ 100,
    #                       TRUE ~ age_lower + 5))
    
    ## Incidence
    ## Start and end of time interval for incidence calculation
    df$year_start <- df$year - report_interval
    df$year_end <- df$year ## not needed, but just to make it explicit
    
    ## One-year intervals corresponding to calendar year during which new infections/person-time were accrued
    df$year_floor <- floor(df$year_start)
    
    ## Person-time at risk
    df$person_years_at_risk <- ifelse(df$hiv == 0, df$Population*report_interval, df$Newly.Infected*report_interval/2)
    
    ## Incidence among women ages 16-35 (ECHO)
    inc_echo <- df %>%
      filter(gender == "Women" & age_lower >= 16 & age_upper <= 36 & year_floor >= 2020 & year_floor <= 2030) %>%
      group_by(run_num, year_floor, gender) %>%
      summarise(new_infections = sum(Newly.Infected),
                person_years = sum(person_years_at_risk)) %>%
      mutate(value = 100*new_infections/person_years,
             year = year_floor,
             measure = "Incidence",
             county = "All",
             age_lower = 16,
             age_upper = 35,
             risk = "All",
             prep_coverage = prep_coverage,
             prep_method = prep_method
      ) %>%
      ungroup() %>%
      select(-year_floor)
    
    ## PrEP coverage, women ages 16-35 (ECHO)
    prep_echo <- df %>%
      mutate(year_floor = floor(year)) %>%
      filter(gender == "Women" & age_lower >= 16 & age_upper <= 36 & year_floor >= 2020 & year_floor <= 2030) %>%
      group_by(year_floor, run_num) %>%
      summarise(value = sum(HasIntervention.PrEP.)/(sum(Population) - sum(Infected))) %>%
      mutate(year = year_floor,
             measure = "PrEP Coverage",
             age_lower = 16,
             age_upper = 35,
             gender = "Women",
             county = "All",
             risk = "All",
             prep_coverage = prep_coverage,
             prep_method = prep_method) %>%
      ungroup() %>%
      select(-year_floor)
    
    return(list(
                "inc_echo" = inc_echo,
                "prep_echo" = prep_echo
    ))
    
    
  }
  
  ## Save run-specific information
  if(save_runs == TRUE) {
    
    save(outputs, 
         file_path, 
         suite_name, 
         scenario_name, 
         scale_factor,
         file = file.path("scenarios", suite_name, scenario_name, "prep_we_comp_results_by_run.RData"))
    
  }
  
  ## Calculate summary statistics across runs
  for(ii in 1:length(outputs)) {
    
    outputs[[ii]] <- outputs[[ii]] %>%
      group_by(year, county, gender, age_lower, age_upper, measure, prep_coverage, prep_method) %>%
      summarise(mean = mean(value, na.rm = TRUE),
                median = median(value, na.rm = TRUE),
                lower = quantile(value, probs = 0.025, na.rm = TRUE),
                upper = quantile(value, probs = 0.975, na.rm = TRUE))
    
  }
  
  ## Save output
  save(outputs, 
       file_path, 
       suite_name, 
       scenario_name, 
       scale_factor,
       file = file.path("scenarios", suite_name, scenario_name, "prep_we_comp_results.RData"))
  
  ## Report_Event_Recorder analysis
  if(record_events == TRUE) {
    prep_duration_results <- event_recorder %>%
      select(Event_Name, Individual_ID, Age) %>%
      arrange(Individual_ID, Age) %>%
      group_by(Individual_ID) %>%
      mutate(start = if_else(Event_Name == "PrEP_Received", Age, NA_real_),
             end = lead(Age)) %>%
      filter(!is.na(start) & !is.na(end)) %>%
      mutate(duration = end - start,
             prep_method = prep_method) %>%
      select(Individual_ID, prep_method, duration)
    
    save(prep_duration_results,
         file_path, 
         suite_name, 
         scenario_name, 
         scale_factor,
         file = file.path("scenarios", suite_name, scenario_name, "prep_duration_results.RData"))
  }
  
  
}

