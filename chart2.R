# creating a graph of the population of black people in WA state jails versus
# the total jail population in WA state throughout the years of 1970-2018
# by averaging WA county data

library(dplyr)
library(ggplot2)

# load data into variable
jail_country_data <- read.csv("https://github.com/melaniewalsh/Neat-Datasets/blob/main/us-jail-pop.csv?raw=true")

# creating a data frame with just WA state data
wa_state_data <- jail_country_data %>%
  filter(state == "WA" & !is.na(total_jail_pop) & !is.na(black_jail_pop))

# summarizing county data per year for total jail pop and black jail pop
summed_data <- wa_state_data %>%
  group_by(year) %>%
  summarise(total_jail_pop = sum(total_jail_pop),
            black_jail_pop = sum(black_jail_pop))

# graphing the data above in a line graph
second_chart <- ggplot(
  summed_data,
  aes(x = total_jail_pop)) +
  geom_line(aes(y = black_jail_pop), color = "purple") +
  labs(
    title = "Black Population in Jail in WA State vs Total Jail Population in WA Overtime (1970-2018)",
    x = "Total Jail Population",
    y = "Black Population in Jail"
  )
