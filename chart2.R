library(dplyr)
library(ggplot2)

incarceration_data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")
 
# Chart 2: Scatterplot comparing Total Pop 15 to 64 to Total Pop in Jail 

f_totalpop <- incarceration_data %>%
  filter(year == max(year)) %>%
  mutate(percentage_pop_jail_female = (female_jail_pop/(female_jail_pop + male_jail_pop))*100) %>%
  mutate(percentage_female_pop = (female_pop_15to64/total_pop_15to64)*100) %>%
  select(county_name, percentage_pop_jail_female, percentage_female_pop)

m_totalpop <- incarceration_data %>%
  filter(year == max(year)) %>%
  mutate(percentage_pop_jail_male = (male_jail_pop/(male_jail_pop + female_jail_pop))*100) %>%
  mutate(percentage_male_pop = (male_pop_15to64/total_pop_15to64)*100) %>%
  select(county_name, percentage_pop_jail_male, percentage_male_pop)

library(ggplot2)

ggplot(f_totalpop) +
  geom_point(aes(x=percentage_pop_jail_female, y = percentage_female_pop), color="darkred") +
  ylim(0,100) +
  ggtitle("Proportion of Female Population in County and in Jail") +
  ylab("Percentage of Female Population in County, Ages of 15-64") +
  xlab("Percentage of Female Population in Jail")

ggplot(m_totalpop) +
  geom_point(aes(x=percentage_pop_jail_male, y = percentage_male_pop),  color = "steelblue") +
  ylim(0,100) +
  ggtitle("Proportion of Male Population in County and in Jail") +
  ylab("Percentage of Male Population in County, Ages of 15-64") +
  xlab("Percentage of Male Population in Jail")

