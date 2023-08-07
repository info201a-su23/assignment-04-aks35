# creating a map of the US where each state is labeled by color
# based off the percentage of black people in their jails in 2018 by
# summing county data

library(dplyr)
library(ggplot2)
library(maps)

# load data into variable
jail_country_data <- read.csv("https://github.com/melaniewalsh/Neat-Datasets/blob/main/us-jail-pop.csv?raw=true")

# loading US map
us_map <- map_data("state")

state_mapping <- data.frame(
  state_abbr = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY"),
  state_full_name = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming")
)

# retrieving and calculating percentages of black populations in jail
# in 2018 of each state
perc_black_pop <- jail_country_data %>%
  filter(year == 2018 & !is.na(black_jail_pop) & !is.na(total_jail_pop)) %>%
  group_by(state) %>%
  summarize(black_jail_pop = sum(black_jail_pop),
            total_jail_pop = sum(total_jail_pop)) %>%
  mutate(black_jail_percentage = (black_jail_pop / total_jail_pop) * 100)

# making sure that the state variables are the same so that the data can merge
# making sure the state names are all lower case and that abbreviations are switched to full names
perc_black_pop <- perc_black_pop %>%
  left_join(state_mapping, by = c("state" = "state_abbr")) %>%
  rename(region = state_full_name) %>%
  mutate(region = tolower(region))

# merging the data
merged_data <- left_join(us_map, perc_black_pop, by = "region")

# creating the map and using scale fill gradient to create color gradient within states
map_chart <- ggplot(merged_data, aes(x = long, y = lat, group = group, fill = black_jail_percentage)) +
  geom_polygon() +
  coord_fixed(ratio = 1.3) +
  scale_fill_gradientn(colors = c("blue", "red"), name = "Black Population (%)", limits = c(0, 100)) +
  labs(
    title = "Percentage of Black Population in Jail in 2018",
    fill = "Black Population (%)"
  ) +
  theme_void()
