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
  xg_info_accumulated = xg_info %>%
    mutate(winner_attributable_xg_updated = accumulate(lag(winner_attributable_xg), 
                                           ~ ifelse(lag(event_type) == "FAC", 0, .x + .y), 
                                           .init = 0)) %>%
    mutate(loser_attributable_xg_updated = accumulate(lag(loser_attributable_xg), 
                                                       ~ ifelse(lag(event_type) == "FAC", 0, .x + .y), 
                                                       .init = 0))
  
  faceoffs_full_new = faceoffs_with_player_roles %>%
    left_join(xg_info, by = c('game_id_x' = 'game_id', 'season_x' = 'season', 'game_seconds', 'event_type'))
  return(faceoffs_full_new) # previously returned faceoffs_full
}

get_role_encoded_stats = function(pbp_with_role, mega_dict) {
  print("this function was called")
  mega_dict = mega_dict %>%
    mutate(season = paste0("20", substr(Season, 1, 2), "20", substr(Season, 4, 5)))
  check_leivo(pbp_with_role)
  pbp_with_role_and_stats = pbp_with_role %>%
    mutate(season_x = paste(season_x)) %>%
    left_join(mega_dict, by = c('Win_F1_Name' = 'EH_ID', 'season_x' = 'season')) 
  check_leivo(pbp_with_role_and_stats)
  pbp_with_role_and_stats = pbp_with_role_and_stats %>%
    left_join(mega_dict, by = c('Win_F2_Name' = 'EH_ID', 'season_x' = 'season'), suffix = c('_Win_F1', '_Win_F2'))
  check_leivo(pbp_with_role_and_stats)
  pbp_with_role_and_stats = pbp_with_role_and_stats %>%
    left_join(mega_dict, by = c('Win_F3_Name' = 'EH_ID', 'season_x' = 'season'))
  check_leivo(pbp_with_role_and_stats)
  pbp_with_role_and_stats = pbp_with_role_and_stats %>%
    left_join(mega_dict, by = c('Win_D1_Name' = 'EH_ID', 'season_x' = 'season'), suffix = c('_Win_F3', '_Win_D1'))
  check_leivo(pbp_with_role_and_stats)
  pbp_with_role_and_stats = pbp_with_role_and_stats %>%
    left_join(mega_dict, by = c('Win_D2_Name' = 'EH_ID', 'season_x' = 'season'))
  check_leivo(pbp_with_role_and_stats)
  pbp_with_role_and_stats = pbp_with_role_and_stats %>%
    left_join(mega_dict, by = c('Lose_F1_Name' = 'EH_ID', 'season_x' = 'season'), suffix = c('_Win_D2', '_Lose_F1'))
  check_leivo(pbp_with_role_and_stats)
  pbp_with_role_and_stats = pbp_with_role_and_stats %>%
    left_join(mega_dict, by = c('Lose_F2_Name' = 'EH_ID', 'season_x' = 'season'))
  check_leivo(pbp_with_role_and_stats)
  pbp_with_role_and_stats = pbp_with_role_and_stats %>%
    left_join(mega_dict, by = c('Lose_F3_Name' = 'EH_ID', 'season_x' = 'season'), suffix = c('_Lose_F2', '_Lose_F3'))
  check_leivo(pbp_with_role_and_stats)
  pbp_with_role_and_stats = pbp_with_role_and_stats %>%
    left_join(mega_dict, by = c('Lose_D1_Name' = 'EH_ID', 'season_x' = 'season'))
  check_leivo(pbp_with_role_and_stats)
  pbp_with_role_and_stats = pbp_with_role_and_stats %>%
    left_join(mega_dict, by = c('Lose_D2_Name' = 'EH_ID', 'season_x' = 'season'), suffix = c('_Lose_D1', '_Lose_D2'))
  check_leivo(pbp_with_role_and_stats)
  return(pbp_with_role_and_stats)
}

subset_relevant_cols = function(pbp_with_role_and_stats) {
  remove_cols = c('session', 'game_date', 'event_description', 'event_detail', 'event_player_3', 'event_length', 'num_on', 'num_off', 'players_on', 'players_off', 
                  'home_on_1', 'home_on_2', 'home_on_3', 'home_on_4', 'home_on_5', 'home_on_6','home_on_7', 'away_on_1', 'away_on_2', 'away_on_3', 'away_on_4', 
                  'away_on_5', 'away_on_6','away_on_7', 'home_goalie', 'away_goalie', 'pbp_distance', 'pred_goal', 'is_pp', 'event_zone.y', 'zone_change_time', 'zone_time')
  data = pbp_with_role_and_stats %>%
    select(-all_of(remove_cols)) %>%
    mutate(game_id = game_id_x, season = season_x, event_zone = event_zone.x) %>%
    filter(game_strength_state == '5v5')
  return(data)
}

impute_missing_values = function(data) {
  
}

condition_updated = function(big_join, dataset_imputed) {
  game_date_dict = big_join %>%
    select(season, game_id, game_date) %>%
    distinct(.keep_all = TRUE) %>%
    drop_na()
  dataset_imputed_with_game_date = dataset_imputed %>%
    inner_join(game_date_dict, by = c('season', 'game_id'))
  faceoffs_data = big_join %>%
    filter(!(event_type == 'ZONE_ENTRY' & str_detect(event_description, "FAC entry") == TRUE)) %>% # ignore faceoff driven zone entries (as tracked by Sznajder)
    mutate(zone_change_time = 
             ifelse(event_type == 'ZONE_EXIT' |
                      event_type == 'ZONE_ENTRY' |
                      event_type == 'STOP', 
                    game_seconds, NA),
           # #Taking out entries that occur as the faceoff happens
           # zone_change_time = ifelse(
           #   (event_type == 'ZONE_ENTRY' | event_type == 'ZONE_EXIT') &
           #     lag(event_type) == 'FAC' &
           #     game_seconds == lag(game_seconds),
           #   NA,
           #   zone_change_time),
           end_faceoff_attribution =
             na.locf(zone_change_time, fromLast = TRUE, na.rm = F),
           end_faceoff_attribution = ifelse(end_faceoff_attribution > lag(end_faceoff_attribution) & event_type != 'FAC', lag(end_faceoff_attribution), end_faceoff_attribution),
           zone_time = end_faceoff_attribution - game_seconds) %>%
    filter((event_type == 'FAC' & event_zone != 'Neu') & 
             #Looked like reasonable cutoff from density plot
             zone_time <= 250) %>%
    mutate(game_date = substr(game_date, 1, 10))
  
  same_games = dataset_imputed_with_game_date %>%
    select(game_date, season, home_team, away_team) %>%
    mutate(game_date = substr(game_date, 1, 10)) %>%
    distinct(game_date, season, home_team, away_team, .keep_all = TRUE) %>% # ADDED THIS!
    inner_join(faceoffs_data,
               by = c('home_team', 'away_team', 'game_date', 'season' = 'season')) %>%
    select(game_id) %>%
    distinct(game_id)
  
  faceoffs_data = faceoffs_data %>%
    arrange(game_date, game_id, game_seconds)
  
  faceoffs_data_subset = faceoffs_data %>%
    select(season, game_id, game_seconds, zone_change_time, end_faceoff_attribution, zone_time)
  
  faceoffs_with_xg = dataset_imputed %>%
    select(-event_index, -game_period, -clock_time, -event_type, -event_zone, -home_team, -away_team, -home_score, -away_score, -game_score_state, -game_strength_state, face_index, -pen_index, -shift_index, -pbp, -teams) %>%
    left_join(faceoffs_data, by = c('season' = 'season', 'game_id' = 'game_id', 'game_seconds'))
  #inner_join(same_games, by = c('game_id' = 'game_id'))
  
  faceoffs_with_player_roles = faceoffs_with_xg %>%
    select(game_id, season, game_seconds, event_type,
           Win_F1_Name, Win_F2_Name, Win_F3_Name, Win_D1_Name, Win_D2_Name, Win_G_Name,
           Lose_F1_Name, Lose_F2_Name, Lose_F3_Name, Lose_D1_Name, Lose_D2_Name,
           Win_F1, Win_F2, Win_F3, Win_D1, Win_D2, Lose_F1, Lose_F2, Lose_F3, Lose_D1, Lose_D2)
  
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
    #mutate(end_faceoff_attribution = ifelse(event_type != "FAC", NA, end_faceoff_attribution)) %>%
    mutate(end_faceoff_attribution = na.locf(end_faceoff_attribution, na.rm = F)) %>%
    mutate(winner_attributable_xg = ifelse(event_team == last_faceoff_winner & pred_goal > 0 & game_seconds > last_faceoff_time & game_seconds < end_faceoff_attribution, pred_goal, 0),
           loser_attributable_xg = ifelse(event_team != last_faceoff_winner & pred_goal > 0 & game_seconds > last_faceoff_time & game_seconds < end_faceoff_attribution, pred_goal, 0)) %>%
    #loser_attributable_xg = ifelse(event_team != last_faceoff_winner & pred_goal > 0 & game_seconds > last_faceoff_time & game_seconds < end_faceoff_attribution, pred_goal, 0)) %>%
    mutate(winner_xg = lag(winner_attributable_xg),
           loser_xg = lag(loser_attributable_xg))
  
  faceoffs_id = xg_info %>%
    select(season, game_id, game_seconds, event_type) %>%
    filter(event_type == 'FAC') %>%
    group_by(season, game_id, game_seconds, event_type) %>%
    mutate(ID = cur_group_id())
  
  xg_info_id = xg_info %>%
    left_join(faceoffs_id, by = c('season', 'game_id', 'game_seconds', 'event_type')) %>%
    filter(event_zone.x != 'Neu') %>%
    mutate(ID = na.locf(ID, fromLast = T, na.rm = F)) %>%
    group_by(ID) %>%
    mutate(winner_xg = sum(winner_attributable_xg, na.rm = TRUE),
           loser_xg = sum(loser_attributable_xg, na.rm = TRUE)) %>%
    ungroup()
  
  xg_info_id = xg_info_id %>%
    mutate(end_possession_attributable = ifelse(((event_type == 'SHOT' | event_type == 'MISS' | event_type == 'GOAL') & event_team != last_faceoff_winner) | event_type == "FAC", game_seconds, NA)) %>%
    #mutate(end_possession_attributable = ifelse(event_type == 'FAC', NA, end_possession_attributable)) %>%
    #mutate(end_possession_attributable = ifelse(lag(end_possession_attributable) < end_possession_attributable, NA, end_possession_attributable)) %>%
    #mutate(end_possession_attributable = ifelse(event_type == 'FAC', game_seconds, end_possession_attributable)) %>%
    mutate(end_possession_attributable = na.locf(end_possession_attributable, fromLast = T, na.rm = F)) %>%
    mutate(end_possession_attributable = lead(end_possession_attributable)) %>%
    mutate(end_possession_attributable = ifelse(event_type != 'FAC', NA, end_possession_attributable)) %>%
    mutate(end_possession_attributable = na.locf(end_possession_attributable, na.rm = F))
  
  xg_info_id = xg_info_id %>%
    mutate(possession_time_change = ifelse(((event_team == last_faceoff_winner & event_zone.x == 'Off') | (event_team != last_faceoff_winner & event_zone.x == 'Def')) & game_seconds < end_possession_attributable, game_seconds - lag(game_seconds), 0)) %>%
    mutate(ifelse(possession_time_change < 0, 0, possession_time_change))
  
  xg_info_id = xg_info_id %>%
    group_by(ID) %>%
    mutate(attributable_possession = sum(possession_time_change, na.rm = TRUE)) %>%
    ungroup()
  
  temp = xg_info_id %>%
    ungroup() %>%
    select(season, game_id, ID, game_date, game_period, game_seconds, event_type, event_team, last_faceoff_winner, winner_xg, loser_xg, pred_goal, attributable_possession, end_possession_attributable, event_player_1, event_player_2, event_description)
  faceoffs_full_new = xg_info_id %>%
    left_join(faceoffs_with_player_roles, by = c('game_id' = 'game_id', 'season' = 'season', 'game_seconds', 'event_type')) %>%
    filter(event_type == 'FAC') %>%
    mutate(winner_xg = lead(winner_xg),
           loser_xg = lead(loser_xg))
  
  temp2 = faceoffs_full_new %>% 
    select(game_id, ID, game_seconds, event_type, event_team, event_zone.x, last_faceoff_winner, event_description, winner_xg, loser_xg)
  
  faceoffs_objective_summary = faceoffs_full_new %>%
    select(game_id, season, game_seconds, event_type, winner_xg, loser_xg, attributable_possession)
  data_with_objective = dataset_imputed_with_game_date %>%
    left_join(faceoffs_objective_summary, by = c('game_id', 'season', 'game_seconds', 'event_type')) %>%
    filter(event_zone != 'Neu') %>%
    drop_na()

  temp = data_with_objective %>%
    select(season, game_id, game_date, game_period, game_seconds, event_type, event_team, winner_xg, loser_xg, event_player_1, event_player_2)
  return(data_with_objective)
}