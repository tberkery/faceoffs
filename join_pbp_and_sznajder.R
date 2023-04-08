library(tidymodels)
library(tidyverse)
library(finetune)
library(xgboost)
library(data.table)
library(zoo)
library(anytime) 

years = c(2017, 2018, 2020, 2021)

condition = function(big_join, years) {
  full_combined = read_csv(paste0("Full", years[1], "_new.csv")) %>%
    mutate(game_date = anydate(game_date))
  years_subset = years[-1]
  for (y in years_subset) {
    full_year = read_csv(paste0("Full", y, "_new.csv")) %>%
      mutate(game_date = anydate(game_date))
    full_combined = rbind(full_combined, full_year)
  }
  faceoffs_data = big_join %>%
    mutate(zone_change_time = 
             ifelse(event_type == 'ZONE_EXIT' |
                      event_type == 'ZONE_ENTRY' |
                      event_type == 'STOP', 
                    game_seconds, NA),
           #Taking out entries that occur as the faceoff happens
           zone_change_time = ifelse(
             (event_type == 'ZONE_ENTRY' | event_type == 'ZONE_EXIT') &
               lag(event_type) == 'FAC' &
               game_seconds == lag(game_seconds),
             NA,
             zone_change_time),
           end_faceoff_attribution =
             na.locf(zone_change_time, fromLast = TRUE, na.rm = F),
           zone_time = end_faceoff_attribution - game_seconds) %>%
    filter((event_type == 'FAC' & event_zone != 'Neu') & 
             #Looked like reasonable cutoff from density plot
             zone_time <= 250) %>%
    mutate(game_date = substr(game_date, 1, 10))
  
  same_games = full_combined %>%
    mutate(game_date = substr(game_date, 1, 10)) %>%
    inner_join(faceoffs_data,
              by = c('home_team', 'away_team', 'game_date', 'season_x' = 'season')) %>%
    select(game_id_x) %>%
    rename(game_id = game_id_x) %>%
    distinct(game_id)
  
  faceoffs_data = faceoffs_data %>%
    arrange(game_date, game_id, game_seconds)
  
  faceoffs_data_subset = faceoffs_data %>%
    select(season, game_id, game_seconds, zone_change_time, end_faceoff_attribution, zone_time)
  faceoffs_with_xg = full_combined %>%
    left_join(faceoffs_data_subset, by = c('season_x' = 'season', 'game_id_x' = 'game_id', 'game_seconds')) %>%
    inner_join(same_games, by = c('game_id_x' = 'game_id'))
  
  faceoffs_with_player_roles = faceoffs_with_xg %>%
    select(game_id_x, season_x, game_seconds, event_type,  
           Win_F1_Name, Win_F2_Name, Win_F3_Name, Win_D1_Name, Win_D2_Name, Win_G1_Name,
           Lose_F1_Name, Lose_F2_Name, Lose_F3_Name, Lose_D1_Name, Lose_D2_Name, Lose_G1_Name,
           Win_F1, Win_F2, Win_F3, Win_D1, Win_D2, Win_G1, Lose_F1, Lose_F2, Lose_F3, Lose_D1, Lose_D2, Lose_G1)
  
  faceoff_zone_info_subset = faceoffs_data %>%
    select(game_id, game_seconds, event_type, event_zone, zone_change_time, end_faceoff_attribution, zone_time)
  
  faceoffs_with_xg = faceoffs_with_xg %>%
    filter(event_zone != 'Neu')
  
  xg_info = big_join %>%
    #select(season, game_id, game_seconds, event_type, pred_goal) %>%
    left_join(faceoff_zone_info_subset, by = c('game_id', 'game_seconds', 'event_type')) %>%
    filter(event_type == "SHOT" | event_type == "MISS" | event_type == "GOAL" | event_type == "FAC") %>%
    mutate(last_faceoff_time_temp = ifelse(event_type == "FAC", game_seconds, NA),
           last_faceoff_team_temp = ifelse(event_type == "FAC", event_team, NA)) %>%
    mutate(last_faceoff_time = na.locf(last_faceoff_time_temp, na.rm = F),
           last_faceoff_winner = na.locf(last_faceoff_team_temp, na.rm = F)) %>%
    mutate(end_faceoff_attribution = na.locf(end_faceoff_attribution, fromLast = T, na.rm = F)) %>%
    mutate(winner_attributable_xg = ifelse(event_team == last_faceoff_winner & pred_goal > 0 & game_seconds > last_faceoff_time & game_seconds < end_faceoff_attribution, pred_goal, 0),
           loser_attributable_xg = ifelse(event_team != last_faceoff_winner & pred_goal > 0 & game_seconds > last_faceoff_time & game_seconds < end_faceoff_attribution, pred_goal, 0)) %>%
    mutate(winner_xg = lag(winner_attributable_xg),
           loser_xg = lag(loser_attributable_xg))
  
  faceoffs_full_new = faceoffs_with_player_roles %>%
    left_join(xg_info, by = c('game_id_x' = 'game_id', 'season_x' = 'season', 'game_seconds', 'event_type'))
  return(faceoffs_full)
}

get_role_encoded_stats = function(pbp_with_role, mega_dict) {
  mega_dict = mega_dict %>%
    mutate(season = paste0("20", substr(Season, 1, 2), "20", substr(Season, 4, 5)))
  pbp_with_role_and_stats = pbp_with_role %>%
    mutate(season_x = paste(season_x)) %>%
    left_join(mega_dict, by = c('Win_F1_Name' = 'EH_ID', 'season_x' = 'season')) %>%
    left_join(mega_dict, by = c('Win_F2_Name' = 'EH_ID', 'season_x' = 'season'), suffix = c('_Win_F1', '_Win_F2')) %>%
    left_join(mega_dict, by = c('Win_F3_Name' = 'EH_ID', 'season_x' = 'season'), suffix = c('', '_Win_F3')) %>%
    left_join(mega_dict, by = c('Win_D1_Name' = 'EH_ID', 'season_x' = 'season'), suffix = c('', '_Win_D1')) %>%
    left_join(mega_dict, by = c('Win_D2_Name' = 'EH_ID', 'season_x' = 'season'), suffix = c('', '_Win_D2')) %>%
    left_join(mega_dict, by = c('Lose_F1_Name' = 'EH_ID', 'season_x' = 'season'), suffix = c('', '_Lose_F1')) %>%
    left_join(mega_dict, by = c('Lose_F2_Name' = 'EH_ID', 'season_x' = 'season'), suffix = c('', '_Lose_F2')) %>%
    left_join(mega_dict, by = c('Lose_F3_Name' = 'EH_ID', 'season_x' = 'season'), suffix = c('', '_Lose_F3')) %>%
    left_join(mega_dict, by = c('Lose_D1_Name' = 'EH_ID', 'season_x' = 'season'), suffix = c('', '_Lose_D1')) %>%
    left_join(mega_dict, by = c('Lose_D2_Name' = 'EH_ID', 'season_x' = 'season'), suffix = c('', '_Lose_D2'))
  return(pbp_with_role_and_stats)
}