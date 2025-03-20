# Avery Eastman, 2/24/2025
# The purpose of this assignment is to gain experience with joins and pivots

# attaching the tidyverse package; saving the NY-Times URL as a value called "url"; and I am reading that URL into an object called `covid`
library(tidyverse)
url = 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties-recent.csv'
covid = read_csv(url)

library(tidyr)
library(dplyr)
library(ggplot2)

state_info <- data.frame(abbr = state.abb,
  state = state.name,
  region = state.region
)

inner_join(state_info, covid, by = "state") |>
  group_by(region, date) |>
  summarise(cases = sum(cases),
            deaths = sum(deaths)) |>
  pivot_longer(cols = c(cases, deaths),
               names_to = "type",
               values_to = "count") |>
  ggplot(aes(x = date, y = count)) +
  geom_line() +
  facet_grid(type~region, scales = "free_y") +
  theme_bw()
