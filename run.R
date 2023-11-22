library(tidyverse)

source("./load_sznajder/load_data.R")
source("./load_sznajder/load_data_20-22.R")
source("Join_Entries.R")
source("join_pbp_and_sznajder.R")

source("parse_roles.R")
source("impute_percentile.R")
source("grouped_role.R")
source("condense_lines.R")

# -------- RUN WITH CONTEXT BY ROLE (F1, F2, F3, D1, D2, D3, G1) -------

curate_sznajder = function() { # function designed to do 2017-2022
  load_season(2017, 2019) # note that this does the 2017-2018 and 2018-2019 seasons (i.e. boundaries are inclusive, exclusive)
  # following csvs are intermediary files updated by load_season
  zone_entries_17_18_19 = read_csv("zone_entries_intermediate.csv")
  zone_exits_17_18_19 = read_csv("zone_exits_intermediate.csv")
  
  load_season(2020, 2022) # same note... this is 2020-2021 and 2021-2022.
  zone_entries_20_21_22 = read_csv("zone_entries_intermediate.csv")
  zone_exits_20_21_22 = read_csv("zone_exits_intermediate.csv")
  all_zone_entries = rbind(zone_entries_17_18_19, zone_entries_20_21_22)
  all_zone_exits = rbind(zone_exits_17_18_19, zone_exits_20_21_22)
  
  return_list = vector(mode = "list", length = 2)
  return_list[[1]] = all_zone_entries
  return_list[[2]] = all_zone_exits
  return(return_list)
}

connect_pbp_and_sznajder = function(return_list,
                                    start_year = 2017,
                                    end_year = 2022) {
  all_zone_entries = return_list[[1]]
  all_zone_exits = return_list[[2]]
  
  tryCatch({
    big_join = read_csv("updated_big_join_new.csv")
  }, error = function(e) {
    big_join = join_entries(start_year, end_year, all_zone_entries, all_zone_exits)
    big_join %>% write_csv("updated_big_join_new.csv")
  })  
  
  return(big_join)
}

identify_roles_and_add_stats = function(big_join, years = c(2017, 2018, 2020, 2021)) {
  pbp_with_role = condition(big_join, years)
  check_leivo(pbp_with_role)
  temp = pbp_with_role %>%
    filter(event_player_1 == 'NAZEM.KADRI')
  print(nrow(temp))
  source("driver.R")
  mega_dict = connect_skaters_and_goaltending_to_team_performance
  dataset = get_role_encoded_stats(pbp_with_role, mega_dict) #ERROR SEEMS TO BE HERE
  dataset %>% write_csv("new_dataset_updated_2.csv")
  dataset = subset_relevant_cols(dataset)
  check_leivo(dataset)
  source("impute_data.R")
  dataset = impute_data(dataset)
  check_leivo(dataset)
  source("broaden_role.R")
  data_broadened_role = broaden_role(dataset)
  check_leivo(data_broadened_role)
  data_with_microstats = data_broadened_role %>%
    mutate(year = as.numeric(str_sub(season_x, 1, 4))) %>%
    left_join(microstats, by = c('Player_Win_F1' = 'Player', 'year')) %>%
    left_join(microstats, by = c('Player_Win_F2' = 'Player', 'year'), suffix = c('_Win_F1', '_Win_F2')) %>%
    left_join(microstats, by = c('Player_Win_F3' = 'Player', 'year')) %>%
    left_join(microstats, by = c('Player_Win_D1' = 'Player', 'year'), suffix = c('_Win_F3', '_Win_D1')) %>%
    left_join(microstats, by = c('Player_Win_D2' = 'Player', 'year')) %>%
    left_join(microstats, by = c('Player_Lose_F1' = 'Player', 'year'), suffix = c('_Win_D2', '_Lose_F1')) %>%
    left_join(microstats, by = c('Player_Lose_F2' = 'Player', 'year')) %>%
    left_join(microstats, by = c('Player_Lose_F3' = 'Player', 'year'), suffix = c('_Lose_F2', '_Lose_F3')) %>%
    left_join(microstats, by = c('Player_Lose_D1' = 'Player', 'year')) %>%
    left_join(microstats, by = c('Player_Lose_D2' = 'Player', 'year'), suffix = c('_Lose_D1', '_Lose_D2'))
  check_leivo(data_with_microstats)
  data_with_microstats = data_with_microstats %>%
    mutate(net_xg = winner_xg + loser_xg) %>%
    mutate(faceoff_type = case_when(
      event_zone != 'Neu' &  ((last_faceoff_winner == home_team & home_zone == 'Off') | (last_faceoff_winner != home_team & home_zone == 'Def')) ~ 'Off zone won by Off team',
      event_zone != 'Neu' &  ((last_faceoff_winner != home_team & home_zone == 'Off') | (last_faceoff_winner == home_team & home_zone == 'Def')) ~ 'Def zone won by Def team',
      event_zone != 'Neu' &  ((last_faceoff_winner == home_team & home_zone == 'Def') | (last_faceoff_winner != home_team & home_zone == 'Off')) ~ 'Def zone won by Def team',
      event_zone != 'Neu' &  ((last_faceoff_winner != home_team & home_zone == 'Def') | (last_faceoff_winner == home_team & home_zone == 'Off')) ~ 'Off zone won by Off team',
      TRUE ~ 'other'
    )) %>%
    select(faceoff_type, event_zone, last_faceoff_winner, event_team, home_team, away_team, home_zone, where(is.numeric)) %>%
    drop_na()
  check_leivo(data_with_microstats)
  model_name = ''
  return(data_with_microstats)
}

run = function() {
  zone_entries_and_exits_list = curate_sznajder()
  all_zone_entries = zone_entries_and_exits_list[[1]]
  all_zone_exits = zone_entries_and_exits_list[[2]]
  sznajder_and_pbp = connect_pbp_and_sznajder(zone_entries_and_exits_list,
                                              2017, 2022)
  data_contextualized = identify_roles_and_add_stats(sznajder_and_pbp, c(2017, 2018, 2020, 2021))
  return(data_contextualized)
}

load_sznajder_2022 = function() {
  
  load_season(2022, 2023) # same note... this is 2020-2021 and 2021-2022.
  all_zone_entries = read_csv("zone_entries_intermediate_fresh.csv")
  all_zone_exits = read_csv("zone_exits_intermediate_fresh.csv")
  source("Join_Entries.R")
  big_join = join_entries(2022, 2022, all_zone_entries, all_zone_exits)
  big_join = read_csv("big_join_after_fixes.csv")
  source("join_pbp_and_sznajder.R")
  pbp_with_role = condition(big_join, c(2022))
  source("driver.R")
  mega_dict = connect_skaters_and_goaltending_to_team_performance
  dataset = get_role_encoded_stats(pbp_with_role, mega_dict)
  dataset %>% write_csv("new_dataset_updated_2022.csv")
  dataset = subset_relevant_cols(dataset)
}

# ------- RUN WITH CONTEXT BY POSITION (F, D, G) -------

get_sznajder_and_pbp = function() {
  tryCatch({
    big_join = read_csv("updated_big_join.csv")
    return(big_join)
    #big_join = read_csv("big_join_combined.csv")
  }, error = function(e) {
    zone_entries_and_exits_list = curate_sznajder()
    all_zone_entries = zone_entries_and_exits_list[[1]]
    all_zone_exits = zone_entries_and_exits_list[[2]]
    big_join = connect_pbp_and_sznajder(zone_entries_and_exits_list,
                                                2017, 2022)
    return(big_join)
  })
}

contextualize_with_EH_stats = function(big_join) {
  print(nrow(big_join %>% filter(game_id == 2017020001, event_type == 'FAC', event_zone != 'Neu')))

  mega_dict = connect_skaters_and_goaltending_to_team_performance()
  
  faceoffs = identify_roles(big_join, mega_dict)
  print(nrow(faceoffs %>% filter(game_id == 2017020001, event_type == 'FAC', event_zone != 'Neu')))
  faceoffs = identify_faceoff_winners(faceoffs)
  print(nrow(faceoffs %>% filter(game_id == 2017020001, event_type == 'FAC', event_zone != 'Neu')))
  faceoffs = encode_team_faceoff_status(faceoffs)
  print(nrow(faceoffs %>% filter(game_id == 2017020001, event_type == 'FAC', event_zone != 'Neu')))
  check_leivo(faceoffs)
  dataset = get_role_encoded_stats_updated(faceoffs, mega_dict)
  remove_cols = c('session', 'game_date', 'event_description', 'event_length', 'num_on', 'num_off', 'players_on', 'players_off', 
                  'home_on_1', 'home_on_2', 'home_on_3', 'home_on_4', 'home_on_5', 'home_on_6','home_on_7', 'away_on_1', 'away_on_2', 'away_on_3', 'away_on_4', 
                  'away_on_5', 'away_on_6','away_on_7', 'home_goalie', 'away_goalie', 'pbp_distance', 'pred_goal', 'is_pp', 'event_zone.y', 'zone_change_time', 'zone_time')
  print(nrow(dataset %>% filter(game_id == 2017020001, event_type == 'FAC', event_zone != 'Neu')))
  dataset = dataset %>%
    select(-any_of(remove_cols)) %>%
    filter(game_strength_state == '5v5')
  print(nrow(dataset %>% filter(game_id == 2017020001, event_type == 'FAC', event_zone != 'Neu')))
  return(dataset)
}

contextualize_with_microstats = function(dataset) {
  microstats = read_csv("microstats.csv")
  data_with_microstats = dataset %>%
    mutate(year = as.numeric(str_sub(season, 1, 4))) %>%
    left_join(microstats, by = c('Player_Win_F1' = 'Player', 'year')) %>%
    left_join(microstats, by = c('Player_Win_F2' = 'Player', 'year'), suffix = c('_Win_F1', '_Win_F2')) %>%
    left_join(microstats, by = c('Player_Win_F3' = 'Player', 'year')) %>%
    left_join(microstats, by = c('Player_Win_D1' = 'Player', 'year'), suffix = c('_Win_F3', '_Win_D1')) %>%
    left_join(microstats, by = c('Player_Win_D2' = 'Player', 'year')) %>%
    left_join(microstats, by = c('Player_Win_F1' = 'Player', 'year'), suffix = c('_Win_D2', '_Lose_F1')) %>%
    left_join(microstats, by = c('Player_Win_F2' = 'Player', 'year')) %>%
    left_join(microstats, by = c('Player_Win_F3' = 'Player', 'year'), suffix = c('_Lose_F2', '_Lose_F3')) %>%
    left_join(microstats, by = c('Player_Win_D1' = 'Player', 'year')) %>%
    left_join(microstats, by = c('Player_Win_D2' = 'Player', 'year'), suffix = c('_Lose_D1', '_Lose_D2'))
  return(data_with_microstats)
}

impute_missing = function(data_with_microstats) {

  replacement_thresholds = determine_thresholds_via_percentile(data_with_microstats, 0.2)
  dataset_imputed = impute_by_percentile_threshold(data_with_microstats, replacement_thresholds)
  print(nrow(dataset_imputed %>% filter(game_id == 2017020001, event_type == 'FAC', event_zone != 'Neu')))
  
  dataset_imputed = dataset_imputed %>%
    mutate(Win_F1 = GAR_Win_F1,
           Win_F2 = GAR_Win_F2,
           Win_F3 = GAR_Win_F3,
           Win_D1 = GAR_Win_D1,
           Win_D2 = GAR_Win_D2,
           Lose_F1 = GAR_Lose_F1,
           Lose_F2 = GAR_Lose_F2,
           Lose_F3 = GAR_Lose_F3,
           Lose_D1 = GAR_Lose_D1,
           Lose_D2 = GAR_Lose_D2)
  
  # microstats_colnames = c()
  # statuses = c("Win", "Lose")
  # positions = c("F1", "F2", "F3", "D1", "D2")
  # for (status in statuses) {
  #   for (pos in positions) {
  #     for (col_type in colnames(microstats)) {
  #       if (col_type != 'Player') {
  #         new_col_name = paste0(col_type, "_", status, "_", pos)
  #         microstats_colnames = c(microstats_colnames, new_col_name)
  #       }
  #     }
  #   }
  # }
  
  # added_stats = dataset_imputed %>%
  #   ungroup() %>%
  #   select(game_id, season, event_type, game_seconds, any_of(microstats_colnames))
  return(dataset_imputed)
}

condition = function(big_join, dataset_imputed) {

  dataset_with_objective = condition_updated(big_join, dataset_imputed)
  
  # dataset_with_objective_and_microstats = dataset_with_objective %>%
  #   left_join(added_stats, by = c("game_id", "season", "event_type", "game_seconds"))
  return(dataset_with_objective)
}

broaden_by_role = function(dataset_with_objective) {

  dataset_broadened = group_roles(dataset_with_objective)
  data = dataset_broadened
  data = data %>%
    mutate(net_xg = winner_xg - loser_xg) %>%
    mutate(last_faceoff_winner = event_team) %>%
    mutate(faceoff_type = case_when(
      event_zone != 'Neu' &  ((last_faceoff_winner == home_team & home_zone == 'Off') | (last_faceoff_winner != home_team & home_zone == 'Def')) ~ 'Off zone won by Off team',
      event_zone != 'Neu' &  ((last_faceoff_winner != home_team & home_zone == 'Off') | (last_faceoff_winner == home_team & home_zone == 'Def')) ~ 'Def zone won by Def team',
      event_zone != 'Neu' &  ((last_faceoff_winner == home_team & home_zone == 'Def') | (last_faceoff_winner != home_team & home_zone == 'Off')) ~ 'Def zone won by Def team',
      event_zone != 'Neu' &  ((last_faceoff_winner != home_team & home_zone == 'Def') | (last_faceoff_winner == home_team & home_zone == 'Off')) ~ 'Off zone won by Off team',
      TRUE ~ 'other'
    )) %>%
    select(faceoff_type, event_zone, last_faceoff_winner, event_team, home_team, away_team, home_zone, ends_with('_Name'), where(is.numeric)) %>%
    drop_na()
  source("condense_lines.R")
  data_line_matchups = data %>%
    condense_to_line_matchups()
  model_name = ''
  return(data_line_matchups)
}

prep_all_model = function(data) {
  model_name = 'all'
  
  data_off_off = data %>%
    ungroup() %>%
    filter(faceoff_type == 'Off zone won by Off team') %>%
    select(-faceoff_type)
  
  data_def_def = data %>%
    ungroup() %>%
    filter(faceoff_type == 'Def zone won by Def team') %>%
    select(-faceoff_type)
  
  print(colnames(data_off_off))
  print(colnames(data_def_def))
  
  cols = colnames(data_off_off %>% select(where(is.numeric)) %>% select(-net_xg, -attributable_possession, -season))
  data_off_off <- data_off_off %>%
    mutate(across(all_of(cols), ~scale(.))) %>%
    mutate(across(all_of(cols), ~as.numeric(.)))
  
  cols = colnames(data_def_def %>% select(where(is.numeric)) %>% select(-net_xg, -attributable_possession, -season))
  data_def_def <- data_def_def %>%
    mutate(across(all_of(cols), ~scale(.))) %>%
    mutate(across(all_of(cols), ~as.numeric(.)))
  
  print(nrow(data_off_off))
  print(nrow(data_def_def))
  
  data_off_off = data_off_off %>% select_if(~ !any(is.na(.)))
  data_def_def = data_def_def %>% select_if(~ !any(is.na(.)))
  
  data_off_off %>% select(-winner_xg, -loser_xg, -contains("FOW"), -contains("FOL")) %>% select(-ends_with("_All")) %>% write_csv(paste0('training_data_', model_name, '_offensive_offensive.csv'))
  data_def_def %>% select(-winner_xg, -loser_xg, -contains("FOW"), -contains("FOL")) %>% select(-ends_with("_All")) %>% write_csv(paste0('training_data_', model_name, '_defensive_defensive.csv'))
}

check_leivo = function(temp) {
  temp = temp %>%
    filter(Win_F1_Name == 'JOSH.LEIVO')
  print(nrow(temp))
}

projections = function() {
  source("./load_sznajder/load_data_22-23.R")
  load_season(2022, 2023)
  all_zone_entries = read_csv("zone_entries_intermediate.csv")
  all_zone_exits = read_csv("zone_exits_intermediate.csv")
  source("Join_Entries.R")
  big_join = join_entries(2022, 2022, all_zone_entries, all_zone_exits)
  #big_join_2022 = read_csv("big_join_after_fixes.csv")
  source("join_pbp_and_sznajder.R")
  source("driver.R")
  mega_dict = connect_skaters_and_goaltending_to_team_performance
  source("parse_roles.R")
  faceoffs = identify_roles(big_join, mega_dict)
  print(nrow(faceoffs %>% filter(game_id == 2017020001, event_type == 'FAC', event_zone != 'Neu')))
  faceoffs = identify_faceoff_winners(faceoffs)
  print(nrow(faceoffs %>% filter(game_id == 2017020001, event_type == 'FAC', event_zone != 'Neu')))
  faceoffs = encode_team_faceoff_status(faceoffs)
  print(nrow(faceoffs %>% filter(game_id == 2017020001, event_type == 'FAC', event_zone != 'Neu')))
  check_leivo(faceoffs)
  dataset = get_role_encoded_stats_updated(faceoffs, mega_dict)
  remove_cols = c('session', 'game_date', 'event_description', 'event_length', 'num_on', 'num_off', 'players_on', 'players_off', 
                  'home_on_1', 'home_on_2', 'home_on_3', 'home_on_4', 'home_on_5', 'home_on_6','home_on_7', 'away_on_1', 'away_on_2', 'away_on_3', 'away_on_4', 
                  'away_on_5', 'away_on_6','away_on_7', 'home_goalie', 'away_goalie', 'pbp_distance', 'pred_goal', 'is_pp', 'event_zone.y', 'zone_change_time', 'zone_time')
  print(nrow(dataset %>% filter(game_id == 2017020001, event_type == 'FAC', event_zone != 'Neu')))
  dataset = dataset %>%
    select(-any_of(remove_cols)) %>%
    filter(game_strength_state == '5v5')
  print(nrow(dataset %>% filter(game_id == 2017020001, event_type == 'FAC', event_zone != 'Neu')))
  
  
  microstats = read_csv("microstats.csv")
  data_with_microstats = dataset %>%
    mutate(year = as.numeric(str_sub(season, 1, 4))) %>%
    left_join(microstats, by = c('Player_Win_F1' = 'Player', 'year')) %>%
    left_join(microstats, by = c('Player_Win_F2' = 'Player', 'year'), suffix = c('_Win_F1', '_Win_F2')) %>%
    left_join(microstats, by = c('Player_Win_F3' = 'Player', 'year')) %>%
    left_join(microstats, by = c('Player_Win_D1' = 'Player', 'year'), suffix = c('_Win_F3', '_Win_D1')) %>%
    left_join(microstats, by = c('Player_Win_D2' = 'Player', 'year')) %>%
    left_join(microstats, by = c('Player_Win_F1' = 'Player', 'year'), suffix = c('_Win_D2', '_Lose_F1')) %>%
    left_join(microstats, by = c('Player_Win_F2' = 'Player', 'year')) %>%
    left_join(microstats, by = c('Player_Win_F3' = 'Player', 'year'), suffix = c('_Lose_F2', '_Lose_F3')) %>%
    left_join(microstats, by = c('Player_Win_D1' = 'Player', 'year')) %>%
    left_join(microstats, by = c('Player_Win_D2' = 'Player', 'year'), suffix = c('_Lose_D1', '_Lose_D2'))
  
  source("impute_percentile.R")
  replacement_thresholds = determine_thresholds_via_percentile(data_with_microstats, 0.2)
  dataset_imputed = impute_by_percentile_threshold(data_with_microstats, replacement_thresholds)
  print(nrow(dataset_imputed %>% filter(game_id == 2017020001, event_type == 'FAC', event_zone != 'Neu')))
  
  dataset_imputed = dataset_imputed %>%
    mutate(Win_F1 = GAR_Win_F1,
           Win_F2 = GAR_Win_F2,
           Win_F3 = GAR_Win_F3,
           Win_D1 = GAR_Win_D1,
           Win_D2 = GAR_Win_D2,
           Lose_F1 = GAR_Lose_F1,
           Lose_F2 = GAR_Lose_F2,
           Lose_F3 = GAR_Lose_F3,
           Lose_D1 = GAR_Lose_D1,
           Lose_D2 = GAR_Lose_D2)
  
  # microstats_colnames = c()
  # statuses = c("Win", "Lose")
  # positions = c("F1", "F2", "F3", "D1", "D2")
  # for (status in statuses) {
  #   for (pos in positions) {
  #     for (col_type in colnames(microstats)) {
  #       if (col_type != 'Player') {
  #         new_col_name = paste0(col_type, "_", status, "_", pos)
  #         microstats_colnames = c(microstats_colnames, new_col_name)
  #       }
  #     }
  #   }
  # }
  
  # added_stats = dataset_imputed %>%
  #   ungroup() %>%
  #   select(game_id, season, event_type, game_seconds, any_of(microstats_colnames))
  
  source("join_pbp_and_sznajder.R")
  dataset_with_objective = condition_updated(big_join, dataset_imputed)
  
  # dataset_with_objective_and_microstats = dataset_with_objective %>%
  #   left_join(added_stats, by = c("game_id", "season", "event_type", "game_seconds"))
  
  source("grouped_role.R")
  dataset_broadened = group_roles(dataset_with_objective)
  data = dataset_broadened
  data = data %>%
    mutate(net_xg = winner_xg - loser_xg) %>%
    mutate(last_faceoff_winner = event_team) %>%
    mutate(faceoff_type = case_when(
      event_zone != 'Neu' &  ((last_faceoff_winner == home_team & home_zone == 'Off') | (last_faceoff_winner != home_team & home_zone == 'Def')) ~ 'Off zone won by Off team',
      event_zone != 'Neu' &  ((last_faceoff_winner != home_team & home_zone == 'Off') | (last_faceoff_winner == home_team & home_zone == 'Def')) ~ 'Def zone won by Def team',
      event_zone != 'Neu' &  ((last_faceoff_winner == home_team & home_zone == 'Def') | (last_faceoff_winner != home_team & home_zone == 'Off')) ~ 'Def zone won by Def team',
      event_zone != 'Neu' &  ((last_faceoff_winner != home_team & home_zone == 'Def') | (last_faceoff_winner == home_team & home_zone == 'Off')) ~ 'Off zone won by Off team',
      TRUE ~ 'other'
    )) %>%
    select(faceoff_type, event_zone, last_faceoff_winner, event_team, home_team, away_team, home_zone, ends_with('_Name'), where(is.numeric)) %>%
    drop_na()
  source("condense_lines.R")
  data_line_matchups = data %>%
    condense_to_line_matchups()
  model_name = ''
  prep_all_model(data_line_matchups)
  
}