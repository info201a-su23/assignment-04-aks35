# creating a graph of the percentage of black people in jails in the top 5
# most populous states (California, Texas, Florida, New York, and Pennsylvania)
# by averaging county data from each state versus year from 2000-2018

library(dplyr)
library(ggplot2)

# load data into variable
jail_country_data <- read.csv("https://github.com/melaniewalsh/Neat-Datasets/blob/main/us-jail-pop.csv?raw=true")

# creating a column with the percentage of black people from the total jail population
jail_country_data_prop <- mutate(jail_country_data, b_proportion = (black_jail_pop/total_jail_pop) * 100)

# creating a data frame with jail data from the years of 2000-2018 for the 5
# specified states
five_states_data <- jail_country_data_prop %>%
  filter(
    state == "CA" |
      state == "TX" |
      state == "FL" |
      state == "NY" |
      state == "PA",
    year >= 2000 & year <= 2018,
    !is.na(b_proportion)
  )

# calculating the average percentage of black population in jails for each state per year
avg_black_pop <- five_states_data %>%
  group_by(state, year) %>%
  summarise(avg_black_pop = mean(b_proportion))

# creating the line graph with the data above
first_chart <- ggplot(avg_black_pop, aes(x = year, y = avg_black_pop, color = state)) +
  geom_line(size = 1) +
  labs(
    title = "Average Black Population in Jail Per State from 2000 to 2018",
    x = "Year",
    y = "Average Black Population in Jail Percentage",
    color = "State"
  )