## how often does a faceoff win correlate with next action?
library(tidyverse)

## find pxp data
play_by_play_data <- read_csv(here::here("Big-Data-Cup-2021-main/Big-Data-Cup-2021-main",'pxp_womens_oly_2022_v2.csv')) %>% 
  ## change dates to easy format for filtering
  mutate(game_date = parse_date(game_date, format = "%d/%m/%Y"))

play_by_play_data %>% 
  glimpse()

play_by_play_data %>% 
  filter(str_detect(event, "Faceoff")) %>% 
  count(event_successful, event, event_type )

## does the next event after faceoff always correspond to winning team?
play_by_play_data %>% 
  mutate(last_event_team = lag(team_name), 
         last_event_faceoff = lag(event)=="Faceoff Win") %>% 
  filter(last_event_faceoff) %>% 
  summarise(m = mean(last_event_team == team_name),
            n = n())

## is it the same with nwhl data?
## find pxp data
nwhl_play_by_play_data <- read_csv(here::here("Big-Data-Cup-2021-main/Big-Data-Cup-2021-main/hackathon_nwhl.csv")) %>% 
  janitor::clean_names() 

nwhl_play_by_play_data %>% 
  filter(str_detect(event, "Faceoff")) %>% 
  count(event, detail_1)

## does the next event after faceoff always correspond to winning team?
nwhl_play_by_play_data %>% 
  mutate(last_event_team = lag(team), 
         last_event_faceoff = lag(event)=="Faceoff Win") %>% 
  filter(last_event_faceoff) %>% 
  summarise(m = mean(last_event_team == team),
            n = n())

## what are the common next events?
nwhl_play_by_play_data %>% 
  mutate(last_event_team = lag(team), 
         last_event_faceoff = lag(event)=="Faceoff Win") %>% 
  filter(last_event_faceoff) %>% 
  count(event)


