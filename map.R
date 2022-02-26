library(dplyr)
library(ggplot2)
library(mapproj)
library(maps)
library(scales)

incarceration_data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

# Female

state_abbrevs <- data.frame(state.abb, state.name)
state_abbrevs <- state_abbrevs %>% 
  add_row(state.abb = "DC", state.name = "District of Columbia")
state_shape <- map_data('state')

state_female_jail_pop <- incarceration_data %>%
  group_by(state) %>%
  filter(year == max(year)) %>%
  summarize(total_female = sum(female_jail_pop, na.rm=T)) 

state_female_jail_pop <- left_join(state_female_jail_pop, state_abbrevs, by = c('state' = 'state.abb'))

state_female_jail_pop <- state_female_jail_pop %>%
  mutate(region = tolower(state.name))

state_shape_female <-left_join(state_shape, state_female_jail_pop, by = 'region')


ggplot(state_shape_female) +
  geom_polygon(aes(x=long, y=lat, group=group, fill=total_female)) +
  coord_map() +
  labs(title='Total Number of Female in Jail by State', x='Longitude', y='Latitude', fill='Number of Female') +
  scale_fill_continuous(low = 'white', high ='darkred', labels = scales::label_number_si())

# Male
state_male_jail_pop <- incarceration_data %>%
  group_by(state) %>%
  filter(year == max(year)) %>%
  summarize(total_male = sum(male_jail_pop, na.rm=T)) 

state_male_jail_pop <- left_join(state_male_jail_pop, state_abbrevs, by = c('state' = 'state.abb'))

state_male_jail_pop <- state_male_jail_pop %>%
  mutate(region = tolower(state.name))

state_shape_male <-left_join(state_shape, state_male_jail_pop, by = 'region')


ggplot(state_shape_male) +
  geom_polygon(aes(x=long, y=lat, group=group, fill=total_male)) +
  coord_map() +
  labs(title='Total Number of Male in Jail by State', x='Longitude', y='Latitude', fill='Number of Male') +
  scale_fill_continuous(low = 'white', high ='steelblue', labels = scales::label_number_si())
