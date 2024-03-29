library(tidyverse)
library(ggplot2)

games_lost_by_faceoffs_2023 = function() {
  df = read_csv("EH_pbp_query_20222023_2023-05-11.csv")
  df_new = df %>%
    select(game_id, game_date, game_period, home_team, away_team, game_strength_state, game_score_state, game_seconds, event_description, event_team, event_type, event_player_1, event_player_2, event_player_3) %>%
    filter(event_type == 'FAC' | event_type == 'GOAL') %>%
    filter(game_period >= 3) %>%
    mutate(last_faceoff_time = lag(game_seconds)) %>%
    mutate(time_since_faceoff = game_seconds - last_faceoff_time) %>%
    filter(event_type == 'GOAL') %>%
    arrange(time_since_faceoff, desc(game_seconds))
  
  leafs_bolts = df_new %>%
    filter(home_team == 'TOR' | away_team == 'TOR')
  
  within_10_secs = df_new %>% filter(time_since_faceoff <= 10)
  within_10_secs_grouped = within_10_secs %>% select(event_team) %>% group_by(event_team) %>% mutate(count = n()) %>% distinct(event_team, .keep_all = TRUE) %>% arrange(desc(count))

  df_new_all = df %>%
    select(game_id, game_date, game_period, home_team, away_team, game_strength_state, game_score_state, game_seconds, event_description, event_team, event_type, event_player_1, event_player_2, event_player_3) %>%
    filter(event_type == 'FAC' | event_type == 'GOAL') %>%
    mutate(last_faceoff_time = lag(game_seconds)) %>%
    mutate(time_since_faceoff = game_seconds - last_faceoff_time) %>%
    filter(event_type == 'GOAL') %>%
    arrange(time_since_faceoff, desc(game_seconds))
  
  leafs_bolts = df_new %>%
    filter(home_team == 'TOR' | away_team == 'TOR')
  
  within_10_secs_all = df_new_all %>% filter(time_since_faceoff <= 10)
  within_10_secs_grouped_all = within_10_secs_all %>% select(event_team) %>% group_by(event_team) %>% mutate(count = n()) %>% distinct(event_team, .keep_all = TRUE) %>% arrange(desc(count))

  df_new_same_zone = df %>%
    select(game_id, game_date, game_period, home_team, away_team, game_strength_state, game_score_state, game_seconds, event_description, event_team, event_type, event_zone, event_player_1, event_player_2, event_player_3) %>%
    filter(event_type == 'FAC' | event_type == 'GOAL') %>%
    mutate(last_faceoff_time = lag(game_seconds)) %>%
    mutate(last_faceoff_same_zone = ifelse((lag(event_zone) == event_zone & lag(event_team) == event_team) | (lag(event_zone) != event_zone & lag(event_team) != event_team), TRUE, FALSE)) %>%
    mutate(time_since_faceoff = game_seconds - last_faceoff_time) %>%
    filter(event_type == 'GOAL') %>%
    arrange(time_since_faceoff, desc(game_seconds))

  within_10_secs_all = df_new_all %>% filter(time_since_faceoff <= 10)
  within_10_secs_grouped_all = within_10_secs_all %>% select(event_team) %>% group_by(event_team) %>% mutate(count = n()) %>% distinct(event_team, .keep_all = TRUE) %>% arrange(desc(count))

  tor_fla = df_new %>% filter((home_team == 'TOR' & away_team == 'FLA') | (home_team == 'FLA' & away_team == 'TOR'))
}





off_off = read_csv("training_data_all_offensive_offensive.csv")
def_def = read_csv("training_data_all_defensive_defensive.csv")

summary(off_off$net_xg)
summary(def_def$net_xg)
ggplot(off_off, aes(x = TOI_Win_All, y = net_xg, group = GAR_Win_All)) + geom_density(kernel = 'gaussian')

geom_smooth(method = 'lm')
ggplot(off_off, aes(x = net_xg)) + geom_histogram(bins = 100)
ggplot(off_off, aes(x = GAR_Win_All, y = net_xg)) + geom_density(kernel = 'gaussian')


temp2 = pbp %>% select(game_id, event_type) %>% filter(event_type == 'FAC') %>% group_by(game_id) %>% mutate(event_type = n()) %>% distinct(.keep_all = TRUE) %>% arrange(desc(event_type))
temp = pbp %>% select(event_type) %>% filter(event_type == 'FAC')
temp = pbp %>% select(event_type, event_zone) %>% filter(event_type == 'FAC')
temp = pbp %>% select(event_type, event_zone) %>% filter(event_type == 'FAC') %>% group_by(event_zone) %>% mutate(event_type = n()) %>% distinct(.keep_all = TRUE)
View(temp)
(98686+98823) / (98686+98823+91670)
off / tot * 0.023 + def / tot * 0.020