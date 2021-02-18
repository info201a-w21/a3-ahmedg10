#Loading incarceration data from GitHub
library(readr)
incarceration <- read_csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

#Load the possible necessary packages 
library(tidyverse)
library(dplyr)
library(lintr)
library(styler)

#Create a Col named location that has the location of the area in "COUNTY, STATE 
#Format 
incarceration <- incarceration %>%
  mutate("location" = paste(county_name, ", ", state, sep = ""))


#According to the most recent datte 
#What counties has the highest black jail population?
location_highest_blk_jail_pop <- incarceration %>%
  group_by(state)%>%
  filter (year == max(year))%>%
  filter (black_jail_pop == max(black_jail_pop))%>%
  pull(location)

#According to the most recent date 
#What county has the highest black population 

county_highest_blk_pop <- incarceration %>%
  filter(year == max(year))%>%
  filter(black_pop_15to64 == max(black_pop_15to64))%>%
  pull(location)

#What county has the highest ratio of black people jailed compared to all black
#people in their county? 
county_ratio_black_jailed <- incarceration %>%
  filter(year == max(year)) %>%
  mutate(ratio = black_jail_pop / black_pop_15to64) %>%
  filter (ratio == max(ratio, na.rm = TRUE)) %>%
  pull (location)





  