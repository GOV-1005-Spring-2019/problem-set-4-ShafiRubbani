library(tidyverse)
library(dplyr)
library(generics)
library(gt)
library(lubridate)

polling_data <- read_csv(file = "ps_4_elections-poll-nc09-3.csv",
                 col_types =  cols(
                   .default = col_character(),
                   turnout_scale = col_double(),
                   turnout_score = col_double(),
                   w_LV = col_double(),
                   w_RV = col_double(),
                   final_weight = col_double(),
                   timestamp = col_datetime(format = "")))

glimpse(polling_data)
summary(polling_data)

# Problem 1
dem_supporters <- polling_data %>% filter(response == "Dem") %>% count()

rep_vs_und <- polling_data %>%
  filter(response %in% c("Rep", "Und")) %>%
  group_by(response) %>% 
  summarize(party_count = n()) %>% 
  summarize(diff = party_count[1] - party_count[2])

gender_labels <- polling_data %>% filter(gender != gender_combined) %>% count()

white <- polling_data %>% filter(race_eth == "White", file_race_black != "White") %>% count()

first_response_diff <- polling_data %>%
  filter(response %in% c("Rep", "Dem")) %>%
  group_by(response) %>% 
  summarize(first_response = min(timestamp)) %>%
  summarize(time_diff = round(first_response[1] - first_response[2], digits = 0))

# Problem 2
response_chart <- polling_data %>% 
  filter(race_eth != "[DO NOT READ] Don't know/Refused") %>% 
  mutate(race_eth = fct_relevel(race_eth, c("White", "Black", "Hispanic", "Asian", "Other"))) %>%
  select(response, race_eth, final_weight) %>% 
  group_by(race_eth, response) %>% 
  summarize(total = sum(final_weight)) %>%
  spread(key =  response, value = total)

response_chart[is.na(response_chart)] <- 0

response_chart <- response_chart %>% 
  mutate(all = Dem + Rep + Und + `3`) %>% 
  mutate(Dem = Dem / all) %>% 
  mutate(Rep = Rep / all) %>% 
  mutate(Und = Und / all) %>% 
  select(-c(`3`, all)) %>% 
  ungroup()

response_chart[response_chart == 0] <- NA

response_chart %>%
  gt() %>% 
  tab_header(title = "Polling Results by Race in North Carolina 9th Congressional District") %>% 
  cols_label(
    race_eth = "",
    Dem = "Democrat",
    Rep = "Republican",
    Und = "Undecided"
  ) %>%
  fmt_percent(columns = vars(Dem, Rep, Und),
              decimals = 0)

# Problem 3
