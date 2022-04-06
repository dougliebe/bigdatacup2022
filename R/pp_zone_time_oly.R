library(tidyverse)

## how much time is spent in the zone on a powerplay?


## ## find pxp data
oly_play_by_play_data <- read_csv(here::here("Big-Data-Cup-2021-main/pxp_womens_oly_2022_v2.csv")) %>% 
  janitor::clean_names() %>% 
  ## change date format 
  rename(team= team_name) %>% 
  mutate(game_date = as.Date(game_date, "%d/%m/%Y")) %>% 
  mutate(home_team= ifelse(venue == "home", team, opp_team_name),
         away_team = ifelse(venue == "home", opp_team_name, team)) %>% 
  # mutate(clock = as.character(clock)) %>% 
  # separate(clock, sep = ":", into = c("minutes_left", "seconds_left"), remove = F) %>% 
  mutate(clock = clock_seconds,
         rev_period = case_when(period == 1 ~ 3,
                                period == 2 ~ 2, 
                                period == 3 ~ 1,
                                TRUE ~ 1),
         full_clock = clock+ (rev_period-1)*1200,
         game_id = paste0(as.numeric(game_date), home_team, away_team))

## next to do the extra step to get home/away skaters to align with nwhl data
oly_play_by_play_data %>% 
  ## sep skaters in situation type
  separate(situation_type, remove = F,
           into = c("off_team","def_team"), sep = " on ",
           convert = T) %>% 
  ## standardize all coords
  rename(x_coordinate = x_coord,
         x_coordinate_2 = x_coord_2,
         y_coordinate = y_coord,
         y_coordinate_2 = y_coord_2,
         detail_4 = event_detail_3,
         detail_1 = event_type,
         detail_2 = event_detail_1,
         detail_3 = event_detail_2,
         player = player_name,
         player_2 = player_name_2) %>% 
  ## 
  mutate(home_team_skaters = ifelse(team == home_team, off_team, def_team),
         away_team_skaters = ifelse(team != home_team, off_team, def_team)) ->
  oly_pxp_corrected

## separate out each powerplay
## find all faceoffs, filter only 5v4, in the adv team's attacking zone
oly_pxp_corrected %>% 
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
  pp_time_only_oly


off_zone_fos_only_oly <- 
  pp_time_only_oly %>% 
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




