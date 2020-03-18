library(tidyverse)

confirmed <- read.csv("time_series_19-covid-Confirmed.csv")
deaths <- read.csv("time_series_19-covid-Deaths.csv")
recovered <- read.csv("time_series_19-covid-Recovered.csv")

long_data <- confirmed %>% 
  pivot_longer(
    c(-`Province.State`, -`Country.Region`), 
    names_to="date", 
    values_to="confirmed") %>%
  left_join(
    deaths %>%
      pivot_longer(
        c(-`Province.State`, -`Country.Region`),
        names_to="date",
        values_to="deaths")) %>%
  left_join(
    recovered %>%
      pivot_longer(
        c(-`Province.State`, -`Country.Region`),
        names_to="date",
        values_to="recovered")) %>%
  mutate(date = as.Date(date, format="X%m.%d.%y"))
