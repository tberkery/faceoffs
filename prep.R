library(tidyverse)

data = read_csv("training.csv")
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

prep_box_score_model(data)
prep_expected_goals_model(data)
prep_relative_to_teammates_model(data)
prep_zone_starts_model(data)
