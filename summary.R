library("tidyverse")
library("dplyr")

# load data into variable
jail_country_data <- read.csv("https://github.com/melaniewalsh/Neat-Datasets/blob/main/us-jail-pop.csv?raw=true")

# creating a column with the proportion of black people from the total jail population
jail_country_data_prop <- mutate(jail_country_data, b_proportion = black_jail_pop/total_jail_pop)

# finding which county/state has the highest proportion of black population in their jail
black_jail_prop <- jail_country_data_prop %>%
  filter(year == 2018) %>%
  filter(b_proportion < 1)

max_black_prop <- black_jail_prop %>%
  filter(b_proportion == max(b_proportion, na.rm = TRUE))

columns_max_b <- max_black_prop %>%
  select(b_proportion, county_name, state)

print(columns_max_b)

# finding which county/state has the lowest proportion of black population in their jail
min_black_prop <- black_jail_prop %>%
  filter(b_proportion > 0) %>%
  filter(b_proportion == min(b_proportion, na.rm = TRUE))

columns_min_b <- min_black_prop %>%
  select(b_proportion, county_name, state)

print(columns_min_b)

# creating a column with the proportion of black people from the total jail population
jail_country_data_props <- mutate(jail_country_data_prop, w_proportion = white_jail_pop/total_jail_pop)

# finding which county/state has the highest proportion of white population in their jail
white_jail_prop <- jail_country_data_props %>%
  filter(year == 2018) %>%
  filter(w_proportion < 1)

max_white_prop <- white_jail_prop %>%
  filter(w_proportion == max(w_proportion, na.rm = TRUE))

columns_max_w <- max_white_prop %>%
  select(w_proportion, county_name, state)

print(columns_max_w)

# finding which county/state has the lowest proportion of white population in their jail
min_white_prop <- white_jail_prop %>%
  filter(w_proportion > 0) %>%
  filter(w_proportion == min(w_proportion, na.rm = TRUE))

columns_min_w <- min_white_prop %>%
  select(w_proportion, county_name, state)

print(columns_min_w)

# finding which WA state county has the highest proportion of black population in their jail
black_jail_prop_wa <- jail_country_data_prop %>%
  filter(
    state == "WA",
    year == 2018,
    b_proportion < 1)

max_black_prop_wa <- black_jail_prop_wa %>%
  filter(b_proportion == max(b_proportion, na.rm = TRUE))

columns_max_b_wa <- max_black_prop_wa %>%
  select(b_proportion, county_name)

print(columns_max_b_wa)
