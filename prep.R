library(tidyverse)

data = read_csv("training.csv")
microstats = read_csv("microstats.csv")
data_with_microstats = data %>%
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
  
  
data = read_csv("all_df_updated.csv")

data = read_csv("recoded_roles_updated.csv")
data = data %>%
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

model_name = ''

prep_box_score_model = function(data) {
  model_name = 'box_score'
  data = data %>%
    mutate(net_xg = winner_xg + loser_xg) %>%
    mutate(faceoff_type = case_when(
      event_zone != 'Neu' &  ((last_faceoff_winner == home_team & home_zone == 'Off') | (last_faceoff_winner != home_team & home_zone == 'Def')) ~ 'Off zone won by Off team',
      event_zone != 'Neu' &  ((last_faceoff_winner != home_team & home_zone == 'Off') | (last_faceoff_winner == home_team & home_zone == 'Def')) ~ 'Def zone won by Def team',
      event_zone != 'Neu' &  ((last_faceoff_winner == home_team & home_zone == 'Def') | (last_faceoff_winner != home_team & home_zone == 'Off')) ~ 'Def zone won by Def team',
      event_zone != 'Neu' &  ((last_faceoff_winner != home_team & home_zone == 'Def') | (last_faceoff_winner == home_team & home_zone == 'Off')) ~ 'Off zone won by Off team',
      TRUE ~ 'other'
    ))
  
  data_off_off = data %>%
    filter(faceoff_type == 'Off zone won by Off team') %>%
    select(-faceoff_type)

  data_def_def = data %>%
    filter(faceoff_type == 'Def zone won by Def team') %>%
    select(-faceoff_type)
  
  data_off_off = data_off_off %>%
    select(net_xg, starts_with('G_') & !contains('_Plus_Minus'), starts_with('A1_'), starts_with('A2_'))
  
  data_def_def = data_def_def %>%
    select(net_xg,starts_with('G_') & !contains('_Plus_Minus'), starts_with('A1_'), starts_with('A2_'))
  
  print(colnames(data_off_off))
  print(colnames(data_def_def))
  
  cols = colnames(data_off_off %>% select(-net_xg))
  data_off_off <- data_off_off %>%
    mutate(across(all_of(cols), ~scale(.))) %>%
    mutate(across(all_of(cols), ~as.numeric(.)))
  
  cols = colnames(data_def_def %>% select(-net_xg))
  data_def_def <- data_def_def %>%
    mutate(across(all_of(cols), ~scale(.))) %>%
    mutate(across(all_of(cols), ~as.numeric(.)))
  
  print(nrow(data_off_off))
  print(nrow(data_def_def))
  
  data_off_off %>% write_csv(paste0('training_data_', model_name, '_offensive_offensive.csv'))
  data_def_def %>% write_csv(paste0('training_data_', model_name, '_defensive_defensive.csv'))
}

prep_expected_goals_model = function(data) {
  model_name = 'expected_goals'
  data = data %>%
    mutate(net_xg = winner_xg + loser_xg) %>%
    mutate(faceoff_type = case_when(
      event_zone != 'Neu' &  ((last_faceoff_winner == home_team & home_zone == 'Off') | (last_faceoff_winner != home_team & home_zone == 'Def')) ~ 'Off zone won by Off team',
      event_zone != 'Neu' &  ((last_faceoff_winner != home_team & home_zone == 'Off') | (last_faceoff_winner == home_team & home_zone == 'Def')) ~ 'Def zone won by Def team',
      event_zone != 'Neu' &  ((last_faceoff_winner == home_team & home_zone == 'Def') | (last_faceoff_winner != home_team & home_zone == 'Off')) ~ 'Def zone won by Def team',
      event_zone != 'Neu' &  ((last_faceoff_winner != home_team & home_zone == 'Def') | (last_faceoff_winner == home_team & home_zone == 'Off')) ~ 'Off zone won by Off team',
      TRUE ~ 'other'
    ))
  
  data_off_off = data %>%
    filter(faceoff_type == 'Off zone won by Off team') %>%
    select(-faceoff_type)
  
  data_def_def = data %>%
    filter(faceoff_type == 'Def zone won by Def team') %>%
    select(-faceoff_type)
  
  data_off_off = data_off_off %>%
    select(net_xg, contains('xG') & !contains('team') & !contains('Plus_Minus') & !contains('RelTM') & !contains('winner_attributable_xg') & !contains('loser_attributable_xg') & !contains('winner_xg') & !contains('loser_xg') & !contains('xGF') & !contains('xGA'))
  
  data_def_def = data_def_def %>%
    select(net_xg, contains('xG') & !contains('team') & !contains('Plus_Minus') & !contains('RelTM') & !contains('winner_attributable_xg') & !contains('loser_attributable_xg') & !contains('winner_xg') & !contains('loser_xg') & !contains('xGF') & !contains('xGA'))
  
  print(colnames(data_off_off))
  print(colnames(data_def_def))
  
  cols = colnames(data_off_off %>% select(-net_xg))
  data_off_off <- data_off_off %>%
    mutate(across(all_of(cols), ~scale(.))) %>%
    mutate(across(all_of(cols), ~as.numeric(.)))
  
  cols = colnames(data_def_def %>% select(-net_xg))
  data_def_def <- data_def_def %>%
    mutate(across(all_of(cols), ~scale(.))) %>%
    mutate(across(all_of(cols), ~as.numeric(.)))
  
  print(nrow(data_off_off))
  print(nrow(data_def_def))
  
  data_off_off %>% write_csv(paste0('training_data_', model_name, '_offensive_offensive.csv'))
  data_def_def %>% write_csv(paste0('training_data_', model_name, '_defensive_defensive.csv'))
}

prep_relative_to_teammates_model = function(data) {
  model_name = 'RelTM'
  data = data %>%
    mutate(net_xg = winner_xg + loser_xg) %>%
    mutate(faceoff_type = case_when(
      event_zone != 'Neu' &  ((last_faceoff_winner == home_team & home_zone == 'Off') | (last_faceoff_winner != home_team & home_zone == 'Def')) ~ 'Off zone won by Off team',
      event_zone != 'Neu' &  ((last_faceoff_winner != home_team & home_zone == 'Off') | (last_faceoff_winner == home_team & home_zone == 'Def')) ~ 'Def zone won by Def team',
      event_zone != 'Neu' &  ((last_faceoff_winner == home_team & home_zone == 'Def') | (last_faceoff_winner != home_team & home_zone == 'Off')) ~ 'Def zone won by Def team',
      event_zone != 'Neu' &  ((last_faceoff_winner != home_team & home_zone == 'Def') | (last_faceoff_winner == home_team & home_zone == 'Off')) ~ 'Off zone won by Off team',
      TRUE ~ 'other'
    ))
  
  data_off_off = data %>%
    filter(faceoff_type == 'Off zone won by Off team') %>%
    select(-faceoff_type)
  
  data_def_def = data %>%
    filter(faceoff_type == 'Def zone won by Def team') %>%
    select(-faceoff_type)
  
  data_off_off = data_off_off %>%
    select(net_xg, contains('RelTM'))
  
  data_def_def = data_def_def %>%
    select(net_xg, contains('RelTM'))
  
  print(colnames(data_off_off))
  print(colnames(data_def_def))
  
  cols = colnames(data_off_off %>% select(-net_xg))
  data_off_off <- data_off_off %>%
    mutate(across(all_of(cols), ~scale(.))) %>%
    mutate(across(all_of(cols), ~as.numeric(.)))
  
  cols = colnames(data_def_def %>% select(-net_xg))
  data_def_def <- data_def_def %>%
    mutate(across(all_of(cols), ~scale(.))) %>%
    mutate(across(all_of(cols), ~as.numeric(.)))
  
  print(nrow(data_off_off))
  print(nrow(data_def_def))
  
  data_off_off %>% write_csv(paste0('training_data_', model_name, '_offensive_offensive.csv'))
  data_def_def %>% write_csv(paste0('training_data_', model_name, '_defensive_defensive.csv'))
}

prep_zone_starts_model = function(data) {
  model_name = 'starts'
  data = data %>%
    mutate(net_xg = winner_xg + loser_xg) %>%
    mutate(faceoff_type = case_when(
      event_zone != 'Neu' &  ((last_faceoff_winner == home_team & home_zone == 'Off') | (last_faceoff_winner != home_team & home_zone == 'Def')) ~ 'Off zone won by Off team',
      event_zone != 'Neu' &  ((last_faceoff_winner != home_team & home_zone == 'Off') | (last_faceoff_winner == home_team & home_zone == 'Def')) ~ 'Def zone won by Def team',
      event_zone != 'Neu' &  ((last_faceoff_winner == home_team & home_zone == 'Def') | (last_faceoff_winner != home_team & home_zone == 'Off')) ~ 'Def zone won by Def team',
      event_zone != 'Neu' &  ((last_faceoff_winner != home_team & home_zone == 'Def') | (last_faceoff_winner == home_team & home_zone == 'Off')) ~ 'Off zone won by Off team',
      TRUE ~ 'other'
    ))
  
  data_off_off = data %>%
    filter(faceoff_type == 'Off zone won by Off team') %>%
    select(-faceoff_type)
  
  data_def_def = data %>%
    filter(faceoff_type == 'Def zone won by Def team') %>%
    select(-faceoff_type)
  
  data_off_off = data_off_off %>%
    select(net_xg, contains('OZS'), contains('NZS'), contains('DZS'))
  
  data_def_def = data_def_def %>%
    select(net_xg, contains('OZS'), contains('NZS'), contains('DZS'))
  
  print(colnames(data_off_off))
  print(colnames(data_def_def))
  
  cols = colnames(data_off_off %>% select(-net_xg))
  data_off_off <- data_off_off %>%
    mutate(across(all_of(cols), ~scale(.))) %>%
    mutate(across(all_of(cols), ~as.numeric(.)))
  
  cols = colnames(data_def_def %>% select(-net_xg))
  data_def_def <- data_def_def %>%
    mutate(across(all_of(cols), ~scale(.))) %>%
    mutate(across(all_of(cols), ~as.numeric(.)))
  
  print(nrow(data_off_off))
  print(nrow(data_def_def))
  
  data_off_off %>% write_csv(paste0('training_data_', model_name, '_offensive_offensive.csv'))
  data_def_def %>% write_csv(paste0('training_data_', model_name, '_defensive_defensive.csv'))
}

prep_all_model = function(data) {
  model_name = 'all'
  data = data %>%
    mutate(net_xg = winner_xg + loser_xg) %>%
    mutate(faceoff_type = case_when(
      event_zone != 'Neu' &  ((last_faceoff_winner == home_team & home_zone == 'Off') | (last_faceoff_winner != home_team & home_zone == 'Def')) ~ 'Off zone won by Off team',
      event_zone != 'Neu' &  ((last_faceoff_winner != home_team & home_zone == 'Off') | (last_faceoff_winner == home_team & home_zone == 'Def')) ~ 'Def zone won by Def team',
      event_zone != 'Neu' &  ((last_faceoff_winner == home_team & home_zone == 'Def') | (last_faceoff_winner != home_team & home_zone == 'Off')) ~ 'Def zone won by Def team',
      event_zone != 'Neu' &  ((last_faceoff_winner != home_team & home_zone == 'Def') | (last_faceoff_winner == home_team & home_zone == 'Off')) ~ 'Off zone won by Off team',
      TRUE ~ 'other'
    ))
  
  data_off_off = data %>%
    filter(faceoff_type == 'Off zone won by Off team') %>%
    select(-faceoff_type)
  
  data_def_def = data %>%
    filter(faceoff_type == 'Def zone won by Def team') %>%
    select(-faceoff_type)
  
  print(colnames(data_off_off))
  print(colnames(data_def_def))
  
  cols = colnames(data_off_off %>% select(where(is.numeric)) %>% select(-net_xg))
  data_off_off <- data_off_off %>%
    mutate(across(all_of(cols), ~scale(.))) %>%
    mutate(across(all_of(cols), ~as.numeric(.)))
  
  cols = colnames(data_def_def %>% select(where(is.numeric)) %>% select(-net_xg))
  data_def_def <- data_def_def %>%
    mutate(across(all_of(cols), ~scale(.))) %>%
    mutate(across(all_of(cols), ~as.numeric(.)))
  
  print(nrow(data_off_off))
  print(nrow(data_def_def))
  
  data_off_off = data_off_off %>% select_if(~ !any(is.na(.)))
  data_def_def = data_def_def %>% select_if(~ !any(is.na(.)))
  
  data_off_off %>% write_csv(paste0('training_data_', model_name, '_offensive_offensive.csv'))
  data_def_def %>% write_csv(paste0('training_data_', model_name, '_defensive_defensive.csv'))
}

prep_box_score_model(data)
prep_expected_goals_model(data)
prep_relative_to_teammates_model(data)
prep_zone_starts_model(data)
prep_all_model(data)

vol_summary <- function(df, summary_name, col) {
  mutate(df, "{summary_name}" := sum({{col}}))
}
vol_summary <- function(df, summary_name, col1, col2, col3) {
  mutate(df, "{summary_name}" := sum({{col}}))
}
rate_summary <- function(df, summary_name, col) {
  mutate(df, "{summary_name}" := mean({{col}}))
}

get_line_summary = function(data) {
  num_cols = length(colnames(data))
  start_col_pattern = 41 # Win_F1
  cols_per_role = 183 - 41
  all_cols = colnames(data)
  total_cols = length(all_cols)
  all_stat_cols = colnames(data[,start_col_pattern:total_cols])
  rate_cols = colnames(data %>% select(contains('Percent') | (contains('Per') & contains('60'))))
  vol_cols = setdiff(all_stat_cols, rate_cols)
  data_summarized = data
  rate_cols_Win_F1 = 
  for (stat in rate_cols) {
    data_summarized = data_summarized %>%
      rate_summary(paste0())
  }
}
