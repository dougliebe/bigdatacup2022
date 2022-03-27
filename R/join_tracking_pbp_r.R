library(tidyverse)

## read in pp info
power_play_info <- read_csv(here::here("Big-Data-Cup-2021-main/Big-Data-Cup-2021-main/TrackingData",'pp_info.csv'))
power_play_info %>% 
  head()

## find pxp data
play_by_play_data <- read_csv(here::here("Big-Data-Cup-2021-main/Big-Data-Cup-2021-main",'pxp_womens_oly_2022_v2.csv')) %>% 
  ## change dates to easy format for filtering
  mutate(game_date = parse_date(game_date, format = "%d/%m/%Y"))

play_by_play_data %>% 
  glimpse()

## example
## select for 2022-08-02 game, Canada v USA
game_events <-
  play_by_play_data %>% 
  ## filter date 
  filter(game_date == "2022-02-08") %>% 
  ## team name is canada or us
  filter(str_detect(team_name, "(Canada|United States)")) 

game_events %>% 
  glimpse()

## get period 1 events only
period_events <-
  game_events %>% 
  filter(period == 1)

period_events %>% 
  glimpse

## here I changed the order slightly. Instead of filtering out the time of the PP
## I first looked for the time of the PP in the data, 
## assuming all sequential data points labelled as situation type != 5v5 would be PP
first_pp <- 
  period_events %>% 
  group_by(period, team_name) %>% 
  arrange(desc(clock_seconds)) %>% 
  mutate(game_state_num = cumsum(situation_type != lag(situation_type, default = "5 on 5"))) %>% 
  group_by(team_name) %>% 
  filter(situation_type != "5 on 5") %>% 
  filter(game_state_num == min(game_state_num))

## show that the times are correct
pp_range <- range(first_pp$clock_seconds)
pp_range
## we only include PP events
first_pp %>% 
  filter(situation_type == "5 on 5")

## compare to manual method
period_events %>% 
  filter(between(clock_seconds, 350, 386)) %>% 
  filter(situation_type == "5 on 5")
## the example gives us the penalty + the faceoff after the goal,
## neither of which are part of the power play

## get power play tracking data
pp_tracking <- read_csv(here::here("Big-Data-Cup-2021-main/Big-Data-Cup-2021-main/TrackingData/2022-02-08 Canada at USA/2022-02-08 Canada at USA P1 PP1.csv"))
pp_tracking %>% 
  glimpse()

## we can line this data up with the video shot information
video_shot_info <- read_csv(here::here("Big-Data-Cup-2021-main/Big-Data-Cup-2021-main/TrackingData/2022-02-08 Canada at USA/videoShotsInfo_2022-02-08 Canada at USA P1 PP1.csv")) %>% 
  ## clean names to help with times
  janitor::clean_names()

video_shot_info

## we want to convert our pp_tracking frames into seconds
pp_tracking_clean <-
  pp_tracking %>% 
    ## get a column for secs, 30 frames per second
    mutate(seconds_elapsed = frame_id/30) %>% 
    ## we know the PP started at 386 on the game_clock
    mutate(clock_time = pp_range[2]-seconds_elapsed)

## check to match clock times
range(pp_tracking_clean$clock_time)




































