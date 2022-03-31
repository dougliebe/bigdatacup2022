library(tidyverse)

## how much time is spent in the zone on a powerplay?

## ## find pxp data
nwhl_play_by_play_data <- read_csv(here::here("Big-Data-Cup-2021-main/Big-Data-Cup-2021-main/hackathon_nwhl.csv")) %>% 
  janitor::clean_names() %>% 
  separate(clock, sep = ":", into = c("minutes_left", "seconds_left"), remove = F) %>% 
  mutate(clock = parse_number(minutes_left) * 60 + parse_number(seconds_left),
         rev_period = case_when(period == 1 ~ 3,
                                period == 2 ~ 2, 
                                period == 3 ~ 1),
         full_clock = clock+ (rev_period-1)*1200)

## separate out each powerplay
## a powerplay is from penalty at 5on5 to next non 5on4 event
nwhl_play_by_play_data %>% 
  mutate(is_5on5_penalty = (home_team_skaters == 5 &
                              away_team_skaters == 5 &
                              event == "Penalty Taken"),
         is_not_same = ((home_team_skaters == 5 &
                          away_team_skaters == 4) |
                          (home_team_skaters == 4 &
                             away_team_skaters == 5)),
         man_adv = paste0(home_team_skaters,"on",away_team_skaters),
         is_5on5 = (home_team_skaters == 5 &
                      away_team_skaters == 5)) %>% 
  group_by(game_date, home_team, away_team) %>% 
  ## we only care up to the next stoppage of play
  mutate(man_adv_segment = cumsum(man_adv != lag(man_adv, default = "5on5")),
         play_segment = cumsum(event == "Faceoff Win")) %>% 
  filter(man_adv != "5on5", ) ->
  pp_time_only

## first event on each change of man adv should be FO
pp_time_only %>% 
  group_by(new_segment,play_segment, .add = T) %>% 
  mutate(man_adv_switch = paste0(pmax(home_team_skaters, away_team_skaters), "on", 
                                 pmin(home_team_skaters, away_team_skaters)),
         event_no = 1:n(), 
         team_on_pp = ifelse(home_team_skaters>away_team_skaters, home_team, away_team)) %>%
  filter(any(event_no == 1 & event == "Faceoff Win")) %>% 
  ungroup() ->
  man_adv_starts_with_fo



## look at an example play
man_adv_starts_with_fo %>% 
  filter(game_date == unique(game_date)[1],
         home_team == unique(home_team)[1],
         new_segment == unique(new_segment)[2]) %>% 
  mutate(x_coordinate = if_else(team==team_on_pp,x_coordinate ,flip_values(x_coordinate)),
         y_coordinate = if_else(team==team_on_pp,y_coordinate ,flip_values(y_coordinate, .x = F))) %>% 
  arrange(desc(clock)) %>% 
  # view
  slice(1:20) %>%
  ggplot()  %>% 
  plot_rink()+
  geom_path(aes(x = x_coordinate , y = y_coordinate), size = 1) +
  geom_label(aes(x = x_coordinate , y = y_coordinate ,label = event_no, color = team))

## need to convert locations relative to pp team
## if team doing action == pp team, x, y,
## if not -> x flip over 100, y flip over 42.5
flip_values <- function(x,.x = T) {
  if(.x) {
    ifelse(x > 100, 100 - (x - 100), 100 - x)
  } else {
    ifelse(x > 42.5, 42.5 - (x - 42.5), 42.5 - x)
  }
}
flip_values(c(1,2,180))
  
man_adv_starts_with_fo %>% 
  group_by(game_date, home_team, away_team, new_segment) %>% 
  mutate(time = max(full_clock)-min(full_clock)) %>% 
  filter(time > 120) %>% view
  # ggplot()+
  # geom_histogram(aes(time))

man_adv_starts_with_fo %>%   
  mutate(last_event_team = lag(team), 
         last_event_faceoff = lag(event)=="Faceoff Win",
         fo_location_x = lag(x_coordinate),
         fo_location_y = lag(y_coordinate)) %>% 
  filter(last_event_faceoff) %>% 
  ggplot() %>% 
  plot_rink() +
  geom_point(aes(x = x_coordinate , y = y_coordinate ), size = 1)+
  geom_segment(aes(xend = x_coordinate, yend = y_coordinate, 
                   x = fo_location_x, y = fo_location_y))
