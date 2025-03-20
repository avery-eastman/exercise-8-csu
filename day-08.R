# Avery Eastman, 2/24/2025
# The purpose of this assignment is to gain experience with joins and pivots

# attaching the tidyverse package; saving the NY-Times URL as a value called "url"; and I am reading that URL into an object called `covid`
library(tidyverse)
url <- 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties-recent.csv'
covid <- read_csv(url)

library(tidyr)
library(dplyr)
library(ggplot2)

state_info <- data.frame(abbr = state.abb,
  state = state.name,
  region = state.region
)

state_info <- state_info |>
  inner_join(covid, by = "state") |>
  group_by(region, date) |>
  summarise(cases = sum(cases),
            deaths = sum(deaths)) |>
  pivot_longer(cols = c(cases, deaths),
               names_to = "type",
               values_to = "count")

faceted_plot <- ggplot(state_info, aes(x = date, y = count, color = region)) +
  geom_line() +
  facet_grid(type~region, scales = "free_y") +
  theme_bw() +
  labs(
    title = "Cummulative Cases and Deaths: Region",
    subtitle = "COVID-19 Data: NY-Times",
    caption = "Daily Exercise 8",
    x = "Date",
    y = "Daily Cummalative Count"
  )

ggsave("images/faceted_plot.png")
faceted_plot
