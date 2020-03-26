library(tidyverse)

# confirmed <- read.csv("../time_series_19-covid-Confirmed.csv")
# deaths <- read.csv("../time_series_19-covid-Deaths.csv")
# recovered <- read.csv("../time_series_19-covid-Recovered.csv")
confirmed <- read.csv("../time_series_covid19_confirmed_global.csv")
deaths <- read.csv("../time_series_covid19_deaths_global.csv")
recovered <- read.csv("../time_series_covid19_recovered_global.csv")
population <- read.csv(
  "population.csv", 
  header = FALSE, 
  col.names = c("Country.Region", "population"))

long_data <- confirmed %>% 
  select(-Lat, -Long) %>%
  pivot_longer(
    c(-`Province.State`, -`Country.Region`), 
    names_to="date", 
    values_to="confirmed") %>%
  mutate(date = as.Date(date, format="X%m.%d.%y")) %>%
  left_join(
    deaths %>%
    select(-Lat, -Long) %>%
      pivot_longer(
        c(-`Province.State`, -`Country.Region`),
        names_to="date",
        values_to="deaths") %>%
      mutate(date = as.Date(date, format="X%m.%d.%y"))) %>%
  left_join(
    recovered %>%
    select(-Lat, -Long) %>%
      pivot_longer(
        c(-`Province.State`, -`Country.Region`),
        names_to="date",
        values_to="recovered") %>%
      mutate(date = as.Date(date, format="X%m.%d.%y"))) %>%
  group_by(Country.Region, date) %>%
  summarize_if(is.numeric, sum) %>%
  ungroup() %>%
  left_join(population) %>%
  mutate(fatality = deaths / (confirmed + recovered))
  
