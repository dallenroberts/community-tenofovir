################################################################################
## Allen Roberts
## Analyze Nyanza PrEP sweeps
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
suite_name <- "nyanza-prep-sweep6"
scenarios <- list.dirs(file.path("scenarios", suite_name), full.names = FALSE, recursive = FALSE)
scale_factor <- 0.03
report_interval <- 1/12
pop_adjust <- 0.5 ## Additional scale factor for the population

## Boolean to save certain results at the run level, rather than just after aggregating across runs
save_runs <- TRUE

# scenario_name <- scenarios[[1]]

for(scenario_name in scenarios) {
  ## Load EMOD runs
  prep_coverage <- as.integer(last(str_split(scenario_name, "-")[[1]]))
  
  file_path <- paste("scenarios", suite_name, scenario_name, "ReportHIVByAgeAndGender", sep = "/")
  file_list <- list.files(file_path, pattern = ".csv")
  
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
    #df$year_start <- round(df$year - report_interval, 2)
    #df$year_end <- df$year ## not needed, but just to make it explicit
    df$year_start <- df$year
    df$year_end <- round(df$year + report_interval, 2)
    
    ## One-year intervals corresponding to calendar year during which new infections/person-time were accrued
    df$year_floor <- floor(df$year_start)
      
    ## Person-time at risk
    # df$person_years_at_risk <- ifelse(df$hiv == 0, df$Population*report_interval, df$Newly.Infected*report_interval/2)

    df <- df %>% 
      arrange(NodeId, gender, hiv, age_lower, year) %>%
      group_by(NodeId, gender, hiv, age_lower) %>%
      mutate(mid_interval_pop = (lag(Population) + Population)/2,
             person_years_at_risk = ifelse(hiv == 0, 
                                           mid_interval_pop*report_interval, 
                                           Newly.Infected*report_interval/2)) %>%
      filter(!is.na(person_years_at_risk)) %>%
      ungroup()
    
    ## Incidence, entire population
    inc <- (df %>%
              group_by(year_floor, run_num) %>%
              summarise(new_infections = sum(Newly.Infected),
                        person_years = sum(person_years_at_risk)) %>%
              mutate(value = 100*new_infections/person_years,
                     year = year_floor,
                     measure = "Incidence",
                     age_lower = 0,
                     age_upper = 105,
                     gender = "All",
                     county = "All",
                     risk = "All",
                     prep_coverage = prep_coverage) %>%
              ungroup() %>%
              select(-year_floor)
      )
    
    ## Incidence, entire population, years 2022-2032
    inc_2022_2032 <- (df %>%
              filter(year_floor >= 2022 & year_floor <= 2032) %>%
              group_by(run_num) %>%
              summarise(new_infections = sum(Newly.Infected),
                        person_years = sum(person_years_at_risk)) %>%
              mutate(value = 100*new_infections/person_years,
                     year = "2022-2032",
                     measure = "Incidence",
                     age_lower = 0,
                     age_upper = 105,
                     gender = "All",
                     county = "All",
                     risk = "All",
                     prep_coverage = prep_coverage) %>%
              ungroup()
    )
    
    ## Incidence, among women
    inc_women <- (df %>%
              filter(gender == "Women") %>%
              group_by(year_floor, run_num, gender) %>%
              summarise(new_infections = sum(Newly.Infected),
                        person_years = sum(person_years_at_risk)) %>%
              mutate(value = 100*new_infections/person_years,
                     year = year_floor,
                     measure = "Incidence",
                     age_lower = 0,
                     age_upper = 105,
                     county = "All",
                     risk = "All",
                     prep_coverage = prep_coverage) %>%
              ungroup() %>%
              select(-year_floor)
    )
    
    ## Incidence among women ages 16-35 (ECHO)
    inc_echo <- df %>%
      filter(gender == "Women" & age_lower >= 16 & age_upper <= 36) %>%
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
             prep_coverage = prep_coverage
             ) %>%
      ungroup() %>%
        select(-year_floor)
    
    ## Incidence among women ages 16-35 (ECHO), 2022-2032
    inc_echo_2022_2032 <- df %>%
      filter(gender == "Women" & age_lower >= 16 & age_upper <= 36 & year_floor >= 2022 & year_floor <= 2032) %>%
      group_by(run_num, gender) %>%
      summarise(new_infections = sum(Newly.Infected),
                person_years = sum(person_years_at_risk)) %>%
      mutate(value = 100*new_infections/person_years,
             year = "2022-2032",
             measure = "Incidence",
             county = "All",
             age_lower = 16,
             age_upper = 35,
             risk = "All",
             prep_coverage = prep_coverage
      ) %>%
      ungroup()
    
    ## New infections
    ## Entire population
    new_inf <- df %>%
              group_by(year_floor, run_num) %>%
              summarise(value = sum(Newly.Infected)) %>%
              mutate(year = year_floor,
                     measure = "New Infections",
                     age_lower = 0,
                     age_upper = 105,
                     gender = "All",
                     county = "All",
                     risk = "All",
                     prep_coverage = prep_coverage) %>%
              ungroup() %>%
              select(-year_floor)

    
    ## New infections, among women
    new_inf_women <- df %>%
                         filter(gender == "Women") %>%
                         group_by(year_floor, run_num) %>%
                         summarise(value = sum(Newly.Infected)) %>%
                         mutate(year = year_floor,
                                measure = "New Infections",
                                age_lower = 0,
                                age_upper = 105,
                                gender = "Women",
                                county = "All",
                                risk = "All",
                                prep_coverage = prep_coverage) %>%
                         ungroup() %>%
                         select(-year_floor)
    
    ## New infections among women ages 16-35 (ECHO)
    new_inf_echo <- df %>%
                        filter(gender == "Women" & age_lower >= 16 & age_upper <= 36) %>%
                        group_by(year_floor, run_num) %>%
                        summarise(value = sum(Newly.Infected)) %>%
                        mutate(year = year_floor,
                               measure = "New Infections",
                               age_lower = 16,
                               age_upper = 35,
                               gender = "Women",
                               county = "All",
                               risk = "All",
                               prep_coverage = prep_coverage) %>%
                        ungroup() %>%
                        select(-year_floor)
  
      ## PrEP coverage, women ages 16-35 (ECHO)
      prep_echo <- df %>%
                      mutate(year_floor = floor(year)) %>%
                      filter(gender == "Women" & age_lower >= 16 & age_upper <= 36) %>%
                      group_by(year_floor, run_num) %>%
                      summarise(value = sum(HasIntervention.PrEP.)/(sum(Population) - sum(Infected))) %>%
                      mutate(year = year_floor,
                             measure = "PrEP Coverage",
                             age_lower = 16,
                             age_upper = 35,
                             gender = "Women",
                             county = "All",
                             risk = "All",
                             prep_coverage = prep_coverage) %>%
                      ungroup() %>%
                      select(-year_floor)
      
      # ## PrEP coverage - age and gender
      # prep_age_gender <- (df %>% 
      #                    group_by(gender, age_group, year, run_num) %>%
      #                    summarise(prep_cov = 100 * sum(HasIntervention.PrEP.)/(sum(Population) - sum(Infected)))
      # )
      # 
      # ## PrEP coverage - age, gender, and risk
      # prep_age_gender_risk <- (df %>% 
      #                         group_by(gender, age_group, risk_group, year, run_num) %>%
      #                         summarise(prep_cov = 100 * sum(HasIntervention.PrEP.)/(sum(Population) - sum(Infected)))
      # )
      # 
      # ## PrEP coverage, 15-54, by gender and risk
      # prep_adults_gender_risk <- (df %>%
      #                            filter(Age >= 15 & Age <= 54) %>%
      #                            group_by(gender, risk_group, year, run_num) %>%
      #                            summarise(prep_cov = 100 * sum(HasIntervention.PrEP.)/(sum(Population) - sum(Infected)))
      # )
      
      ## Person-time on PrEP by age, gender, risk
      ## Note that this is approximate. It will be most accurate when the report_interval is set to the model time step (one month). I suspect it will overestimate PrEP person-time when the report_interval is substantially longer than the model time step. This error will increase when PrEP duration is shorter.
      # prep_persontime_gender_risk <- (df %>%
      #                                   group_by(gender, risk_group, run_num)) %>%
      #   summarise(prep_py = sum(HasIntervention.PrEP.*report_interval +
      #                             PrEP_Expired*report_interval/2 +
      #                             PrEP_Aborted*report_interval/2)) 
      # 
      # ## Total number of new infections, by age, gender, and risk
      # new_infections_gender_risk <- (df %>%
      #                                  group_by(gender, risk_group, run_num)) %>%
      #   summarise(new_infections = sum(Newly.Infected))
      
    return(list("inc" = inc,
                "inc_2022_2032" = inc_2022_2032,
                "inc_women" = inc_women,
                "inc_echo" = inc_echo,
                "inc_echo_2022_2032" = inc_echo_2022_2032,
                "new_inf" = new_inf,
                "new_inf_women" = new_inf_women,
                "new_inf_echo" = new_inf_echo,
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
         file = file.path("scenarios", suite_name, scenario_name, "prep_sweep_results_by_run.RData"))
    
  }
  
  ## Calculate summary statistics across runs
    for(ii in 1:length(outputs)) {
      
      outputs[[ii]] <- outputs[[ii]] %>%
        group_by(year, county, gender, age_lower, age_upper, measure, prep_coverage) %>%
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
       file = file.path("scenarios", suite_name, scenario_name, "prep_sweep_results.RData"))
}
