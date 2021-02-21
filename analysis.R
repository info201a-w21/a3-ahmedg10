


# Set Up-------------------------------------------------------------------



# Loading incarceration data from GitHub
library(readr)
incarceration <- read_csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")



# Splitting Data by race
white_data <- incarceration %>%
  select(
    year, fips, state, county_name, white_pop_15to64, white_jail_pop,
    white_prison_pop, white_jail_pop_rate, white_prison_pop_rate
  ) %>%
  mutate("location" = paste(county_name, ", ", state, sep = ""))

black_data <- incarceration %>%
  select(year, fips, state, county_name, contains("black")) %>%
  mutate("location" = paste(county_name, ", ", state, sep = ""))

# DPLYR Exploration -------------------------------------------------------


# Create a Col named location that has the location of the area in "COUNTY,
# STATE) Format
incarceration <- incarceration %>%
  mutate("location" = paste(county_name, ", ", state, sep = ""))


# According to the most recent datte
# What counties has the highest black jail population?
location_highest_blk_jail_pop <- incarceration %>%
  group_by(state) %>%
  filter(year == max(year)) %>%
  filter(black_jail_pop == max(black_jail_pop)) %>%
  pull(location)

# According to the most recent date
# What county has the highest black population

county_highest_blk_pop <- incarceration %>%
  filter(year == max(year)) %>%
  filter(black_pop_15to64 == max(black_pop_15to64)) %>%
  pull(location)

# What county has the highest ratio of black people jailed compared to all black
# people in their county?
highest_ratio_blk_jailed <- incarceration %>%
  filter(year == max(year)) %>%
  mutate(ratio = black_jail_pop / black_pop_15to64) %>%
  filter(ratio == max(ratio, na.rm = TRUE)) %>%
  pull(location)

# What county had the highest difference between black people jailed and
# white people in jailed. In the most recent year?

county_highest_blk_white_diff <- incarceration %>%
  filter(year == max(year)) %>%
  mutate(blk_white_diff = black_jail_pop - white_jail_pop) %>%
  filter(blk_white_diff == max(blk_white_diff, na.rm = TRUE)) %>%
  pull(location)


# Find the top 10 locations with highest black jail population rate
top_10_locations <- black_data %>%
  filter(year == max(year)) %>%
  top_n(10, wt = black_jail_pop_rate) %>%
  arrange(-black_jail_pop_rate) %>%
  select(location, black_jail_pop_rate)






# Trends Over Time Chart --------------------------------------------------
jail_pop_data <- incarceration %>%
  select(year, county_name, black_jail_pop) %>%
  group_by(year) %>%
  summarise(mean = mean(black_jail_pop, na.rm = TRUE))

jail_pop_data_2 <- incarceration %>%
  filter(county_name == "New York County") %>%
  select(
    year, county_name, black_jail_pop, aapi_jail_pop, white_jail_pop,
    other_race_jail_pop, native_jail_pop, latinx_jail_pop
  )



# Creating a Chart

jail_long <-
  pivot_longer(
    data = jail_pop_data_2,
    cols = c(
      "black_jail_pop", "aapi_jail_pop", "white_jail_pop",
      "other_race_jail_pop", "native_jail_pop", "latinx_jail_pop"
    ),
    names_to = "race",
    values_to = "value"
  )



# Creating Graph
race_growth_yearly <- ggplot(jail_long) +
  geom_point(aes(x = year, y = value, group = race, color = race)) +
  labs(
    title = "Jail Population By Race in New York County",
    x = "year",
    y = "jail population"
  )






# Variable Comparison Chart -----------------------------------------------


# Filter data for New York County

new_york_county_blk_data <- black_data %>%
  filter(county_name == "New York County") %>%
  filter(year > "2000")

compare_black_jail_pop <- ggplot(new_york_county_blk_data) +
  geom_point(aes(x = black_pop_15to64, y = black_jail_pop)) +
  labs(
    title = "2000-2018 NY County Jail Population Comparison",
    x = "Black Population",
    y = "Black Jail Population"
  )



# Creating Map ------------------------------------------------------------

# Setting Data Up
black_jail_pop_filter <- black_data %>%
  filter(year == max(year))

# Use map data function to join black_data "counties" dataset with the map data
county_shapes <- map_data("county") %>%
  unite(polyname, region, subregion, sep = ",") %>%
  left_join(county.fips, by = "polyname")


# Merge maps data and black_jail_pop_filter
map_data <- county_shapes %>%
  left_join(black_jail_pop_filter, by = "fips") %>%
  filter(county_name != "Unknown")





# Set Up Blank Theme
blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  )

# Create Map:
black_jail_pop_rate_map <- ggplot(map_data) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = black_jail_pop_rate),
    color = "gray", size = .3
  ) +
  coord_map() +
  scale_fill_continuous(
    limits = c(0, max(map_data$black_jail_pop_rate)),
    na.value = "white", low = "yellow", high = "red"
  ) +
  blank_theme




# Create a specific map:
cali_map_data <- county_shapes %>%
  left_join(black_jail_pop_filter, by = "fips") %>%
  filter(state == "CA", county_name != "Unknown")

black_jail_pop_rate_map_cali <- ggplot(cali_map_data) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = black_jail_pop_rate),
    color = "gray", size = .3
  ) +
  coord_map() +
  scale_fill_continuous(
    limits = c(0, max(map_data$black_jail_pop_rate)),
    na.value = "white", low = "yellow", high = "red"
  ) +
  blank_theme +
  ggtitle("Black Jail Population Rate in California")
