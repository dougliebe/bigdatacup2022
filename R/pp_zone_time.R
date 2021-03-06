library(tidyverse)

## how much time is spent in the zone on a powerplay?

## ## find pxp data
hack_play_by_play_data <- read_csv(here::here("Big-Data-Cup-2021-main/hackathon_nwhl.csv")) %>% 
  bind_rows(read_csv(here::here("Big-Data-Cup-2021-main/hackathon_womens.csv"))) %>% 
  janitor::clean_names() %>% 
  mutate(clock = as.character(clock)) %>% 
  separate(clock, sep = ":", into = c("minutes_left", "seconds_left"), remove = F) %>% 
  mutate(clock = parse_number(minutes_left) * 60 + parse_number(seconds_left),
         rev_period = case_when(period == 1 ~ 3,
                                period == 2 ~ 2, 
                                period == 3 ~ 1,
                                TRUE ~ 1),
         full_clock = clock+ (rev_period-1)*1200,
         game_id = paste0(as.numeric(game_date), home_team, away_team))


## separate out each powerplay
## find all faceoffs, filter only 5v4, in the adv team's attacking zone
hack_play_by_play_data %>% 
  mutate(
    ## is there a 5v4 situation
    is_5v4 = ((home_team_skaters == 5 &
                 away_team_skaters == 4) |
                (home_team_skaters == 4 &
                   away_team_skaters == 5)),
    ## which team is in attacking zone?
    whos_attacking = case_when(
      x_coordinate < 125 & x_coordinate > 75 ~ "NZ",
      x_coordinate >= 125 & team == away_team ~ away_team,
      x_coordinate >= 125 & team == home_team ~ home_team,
      x_coordinate <= 75 & team == away_team ~ home_team,
      x_coordinate <= 75 & team == home_team ~ away_team
    ),
    ## which team is at the adv?
    adv_team = case_when(
      home_team_skaters > away_team_skaters ~ home_team,
      away_team_skaters > home_team_skaters ~ away_team,
      TRUE ~ NA_character_
    ),
    ## is the adv team in their attacking zone?
    is_in_att_zone = adv_team == whos_attacking,
    ## is the event a FO?
    is_fo = event == "Faceoff Win",
    ## man adv label
    skater_adv = paste0(home_team_skaters,"on",away_team_skaters),
    ## 
    is_5on5 = (home_team_skaters == 5 &
                 away_team_skaters == 5)
  ) %>% 
  group_by(game_date, home_team, away_team) %>% 
  ## first group plays by man advantage
  mutate(skater_adv_segment = cumsum(skater_adv != lag(skater_adv, default = "5on5"))) %>% 
  group_by(skater_adv_segment, .add = T) %>% 
  ## we only care up to the next stoppage of play so new group at every fo
  mutate(play_segment = cumsum(is_fo)) %>% 
  ## lets focus on only pp 5v4
  filter(is_5v4) ->
  pp_time_only


off_zone_fos_only <- 
  pp_time_only %>% 
  mutate(x_coordinate_adv = if_else(team==adv_team,x_coordinate ,flip_values(x_coordinate)),
         y_coordinate_adv = if_else(team==adv_team,y_coordinate ,flip_values(y_coordinate, .x = F))) %>% 
  mutate(x_coordinate_adv_2 = if_else(team==adv_team,x_coordinate_2 ,flip_values(x_coordinate_2)),
         y_coordinate_adv_2 = if_else(team==adv_team,y_coordinate_2 ,flip_values(y_coordinate_2, .x = F))) %>% 
  group_by(.add=T, play_segment) %>% 
  arrange(desc(clock)) %>% 
  mutate(event_no = 1:n(),
         starts_with_oz_fo = any(is_fo & is_in_att_zone)) %>%
  ## wont leave zone on this play
  mutate(wont_leave_zone = !cumany(!lead(is_in_att_zone, default = T))) %>% 
  ## must start with an oz FO
  filter(starts_with_oz_fo)

