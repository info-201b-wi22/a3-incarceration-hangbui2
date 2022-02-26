incarceration_data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

# What is the average value of my variable across all the counties (in the current year)?
library(dplyr)

summary_info <- list()

summary_info$total_f_prison_2018 <- incarceration_data %>%
  filter(year == max(year)) %>%
  summarize(recent_total_female_jail_pop = sum(female_jail_pop, na.rm = TRUE)) %>%
  pull(recent_total_female_jail_pop)

summary_info$total_m_prison_2018 <- incarceration_data %>%
  filter(year == max(year)) %>%
  summarize(recent_total_male_jail_pop = sum(male_jail_pop, na.rm = TRUE)) %>%
  pull(recent_total_male_jail_pop)
  
# Where is my variable the highest / lowest?
sum_female_year <- incarceration_data %>%
  group_by(year) %>%
  summarize(sum_female = sum(female_jail_pop, na.rm = TRUE))

sum_male_year <- incarceration_data %>%
  group_by(year) %>%
  summarize(sum_male = sum(male_jail_pop, na.rm = TRUE))

summary_info$high_f_year <- sum_female_year %>%
  filter(sum_female == max(sum_female)) %>%
  pull(year)

summary_info$high_f_value <- sum_female_year %>%
  filter(sum_female == max(sum_female)) %>%
  pull(sum_female)
  
summary_info$high_m_year <- sum_male_year %>%
  filter(sum_male == max(sum_male)) %>%
  pull(year)
  
summary_info$high_m_value <- sum_male_year %>%
  filter(sum_male == max(sum_male)) %>%
  pull(sum_male)
  
# How much has my variable change over the last N years?

summary_info$f_change <- sum_female_year %>%
  mutate(change = (sum_female_year$sum_female - lag(sum_female_year$sum_female))) %>%
  summarise(avg_change = sum(change, na.rm = TRUE)/(nrow(sum_female_year)-1)) %>%
  pull(avg_change)

summary_info$m_change <- sum_male_year %>%
  mutate(change = (sum_male_year$sum_male - lag(sum_male_year$sum_male))) %>%
  summarise(avg_change = sum(change, na.rm = TRUE)/(nrow(sum_male_year)-1)) %>%
  pull(avg_change)
