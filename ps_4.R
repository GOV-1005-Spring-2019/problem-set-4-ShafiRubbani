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
  ungroup() %>%
  na_if(0)
             
#response_chart[response_chart == 0] <- NA

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
              decimals = 0) %>% 
  fmt_missing(columns = vars(Und), rows = 4)

# Problem 3
polling_data %>% 
  filter(educ != "[DO NOT READ] Refused") %>%
  ggplot(aes(x = fct_rev(fct_relevel(educ, c("Graduate or Professional Degree", "Bachelors' degree", "Some college or trade school", "High school", "Grade school"))), y = final_weight)) + 
  coord_flip() +
  labs(title = "More Educated Matter Less in North Carolina 9th",
       subtitle = "Poll gives more weight to people who are less likely to participate in polls",
       x = "",
       y = "Weight Given to Respondents in Calculating Poll Results",
       caption = "New York Times Upshot/Siena College 2018 live polls") +
  # Replace beeswarm geometry with a violin geometry with kernel width of 2.5
  geom_violin() +
  # add individual points on top of violins
  geom_jitter(alpha = 0.3, size = 0.7, width = 0.2)

# Problem 4
education_by_gender <- polling_data %>%
  select(gender, educ) %>% 
  filter(educ != "[DO NOT READ] Refused") %>%
  group_by(gender, educ = fct_rev(fct_relevel(educ, c("Graduate or Professional Degree", "Bachelors' degree", "Some college or trade school", "High school", "Grade school")))) %>% 
  summarize(count = n())

education_by_gender %>% 
  ggplot(aes(x = gender, y = count, fill = educ)) +
  geom_col(position = 'fill')
