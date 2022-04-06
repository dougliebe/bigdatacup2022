


## need to convert locations relative to pp team
## if team doing action == pp team, x, y,
## if not -> x flip over 100, y flip over 42.5
flip_values <- function(x,.x = T) {
  if(.x) {
    200-x
    # x
  } else {
    85-x
    # x
  }
}


## collect and clean the data from last year + olympics
source(here::here("R/pp_zone_time.R"))
source(here::here("R/pp_zone_time_oly.R"))

## now we have all data that:
## starts with an OZ FO
## is from a 5 v 4 powerplay

off_zone_fos_only %>% 
  bind_rows(
    off_zone_fos_only_oly
  ) %>%
  select(game_id, game_date, home_team, away_team, 
         period, clock, full_clock, team,
         player, event, x_coordinate, y_coordinate, x_coordinate_adv, y_coordinate_adv,
         player_2, x_coordinate_2, y_coordinate_2, x_coordinate_adv_2, y_coordinate_adv_2,
         starts_with('detail'), home_team, away_team, rev_period, home_team_skaters, away_team_skaters,
         is_5v4, is_fo, is_5on5, is_in_att_zone, whos_attacking, adv_team, skater_adv_segment, play_segment,
         event_no, starts_with_oz_fo, wont_leave_zone) %>% 
  group_by(game_id, skater_adv_segment) %>% 
  mutate(skater_adv_segment= cur_group_id()) %>% 
  ungroup() %>%  
  write_csv(here::here("new-data/combined_pp_time_oz_only.csv"))

full_data <- read_csv(here::here("new-data/combined_pp_time_oz_only.csv"))

full_data %>% 
  group_by(game_id, skater_adv_segment, play_segment) %>% 
  ## filter out up to the play that puts the puck out of the zone
  filter(lag(wont_leave_zone, default = T, n = 2)) ->
  from_fo_to_leaving_zone


from_fo_to_leaving_zone %>% 
  filter(skater_adv_segment == 1) ->
  ex_data






# pp_time_only %>%
#   group_by(game_date, home_team, away_team,man_adv_segment, play_segment) %>%
#   filter(hasnt_left_zone) %>%
#   mutate(time = max(full_clock)-min(full_clock)) %>%
#   filter(time > 100) %>% view
# # ggplot()+
# #   geom_histogram(aes(time))


source(here::here("Big-Data-Cup-2021-main/OTTHAC_Tutorial/Code/plot_rink.R"))

## title info
date <- ex_data$game_date[1]
adv_team = ex_data$adv_team[1]
opp_team = ex_data$team[ex_data$team != adv_team][1]
time = paste0(ex_data$clock[1], " to ", min(ex_data$clock), " left in period ", ex_data$period[1])


ex_data %>% 
  # view
  # slice(1:20) %>%
  ggplot()  %>% 
  plot_rink()+
  labs(title = paste0(date, " - ",adv_team, " vs ", opp_team),
       subtitle = time) +
  geom_path(aes(x = x_coordinate_adv , y = y_coordinate_adv), size = 0.7, alpha = 0.8) +
  geom_segment(aes(x_coordinate_adv_2, y = y_coordinate_adv_2,
                   xend = x_coordinate_adv, yend = y_coordinate_adv), lty= 2,
               arrow = arrow(ends = "first",length = unit(0.04, "npc"),type = "closed"))+
  geom_label(aes(x = x_coordinate_adv , y = y_coordinate_adv ,label = event_no, color = team))







