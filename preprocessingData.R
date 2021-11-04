library(tidyverse)
require(shinydashboard)
require(ggplot2)
require(dplyr)
require(highcharter) #to plot amazing time series plots
library(readxl)
require(tidyr)
library(tidymodels)
library(scales)
library(janitor)
library(gridExtra)
library(glue)
library(ggcorrplot)
library(vip)
hr <- read_csv("HR-Employee-Attrition.csv") %>% clean_names()

#Data preprocessing

hr <-
  hr %>%
  mutate(across(c(attrition,over18,over_time),
                ~ if_else(. == "Yes",1,0))) %>%
  mutate(across(c(attrition,over18,over_time),
                ~ as.factor(.))) %>%
  mutate(attrition = fct_relevel(attrition,c("1","0"))) %>%
  # Binary categorical
  mutate(across(c(department, education_field,
                  job_role, marital_status),~ as.factor(.))) %>%
  # Nominal categorical
  mutate(across(c(environment_satisfaction, job_satisfaction,
                  relationship_satisfaction,
                  work_life_balance,business_travel, education ,
                  job_involvement,job_level, stock_option_level,
                  performance_rating),
                ~as.ordered(.))) %>%
  # Ordinal categorical
  mutate(business_travel = factor(business_travel, ordered = TRUE,
                                  levels = c("Non-Travel",
                                             "Travel_Rarely","Travel_Frequently"))) %>%
  # Reordering
  select(-employee_count,-standard_hours,-over18)
# Removing non pertinant variables


# Dividing features into vectors to faciltate plotting
numerical <- c("age", "distance_from_home","hourly_rate",
               "daily_rate", "monthly_rate","monthly_income",
               "percent_salary_hike","years_at_company",
               "years_in_current_role","years_since_last_promotion",
               "years_with_curr_manager","total_working_years",
               "num_companies_worked","training_times_last_year")

categorical <- c("gender","over_time","department",
                 "education_field", "job_role", "marital_status")

ordinal <- c("environment_satisfaction", "job_satisfaction",
             "relationship_satisfaction","work_life_balance",
             "job_involvement","performance_rating",
             "business_travel", "education","job_level",
             "stock_option_level")

write.csv(hr,"dataPreproc.csv", row.names = FALSE)
