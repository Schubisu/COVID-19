library(tidyverse)

confirmed <- read.csv("../time_series_19-covid-Confirmed.csv")
deaths <- read.csv("../time_series_19-covid-Deaths.csv")
recovered <- read.csv("../time_series_19-covid-Recovered.csv")
population <- read.csv(
  "population.csv", 
  header = FALSE, 
  col.names = c("Country.Region", "population"))

long_data <- confirmed %>% 
  pivot_longer(
    c(-`Province.State`, -`Country.Region`, -Long, -Lat), 
    names_to="date", 
    values_to="confirmed") %>%
  left_join(
    deaths %>%
      pivot_longer(
        c(-`Province.State`, -`Country.Region`, -Long, -Lat),
        names_to="date",
        values_to="deaths")) %>%
  left_join(
    recovered %>%
      pivot_longer(
        c(-`Province.State`, -`Country.Region`, -Long, -Lat),
        names_to="date",
        values_to="recovered")) %>%
  mutate(date = as.Date(date, format="X%m.%d.%y")) %>%
  pivot_longer(
    c(confirmed, deaths, recovered),
    names_to="case",
    values_to="value"
  ) %>%
  group_by(Country.Region, date, case) %>%
  summarize_if(is.numeric, sum) %>%
  ungroup() %>%
  left_join(population)
  
