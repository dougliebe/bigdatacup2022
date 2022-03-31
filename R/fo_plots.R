# Source in function to create the rink plot in ggplot
source("Big-Data-Cup-2021-main/Big-Data-Cup-2021-main/OTTHAC_Tutorial/Code/plot_rink.R")

## ## find pxp data
nwhl_play_by_play_data <- read_csv(here::here("Big-Data-Cup-2021-main/Big-Data-Cup-2021-main/hackathon_nwhl.csv")) %>% 
  janitor::clean_names() 

## what are the common next events?
nwhl_play_by_play_data %>% 
  mutate(last_event_team = lag(team), 
         last_event_faceoff = lag(event)=="Faceoff Win",
         fo_location_x = lag(x_coordinate),
         fo_location_y = lag(y_coordinate)) %>% 
  filter(last_event_faceoff) ->
  after_fo_win
ggplot(after_fo_win) %>% 
plot_rink() +
  geom_point(aes(x = x_coordinate , y = y_coordinate ), size = 1)+
  geom_segment(aes(xend = x_coordinate, yend = y_coordinate, 
                   x = fo_location_x, y = fo_location_y))

nwhl_play_by_play_data %>% 
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
