source("driver.R")

nhl_player_seasons = read_csv("nhl_player_seasons.csv")
pbp = Full2017
cols_of_interest = c('game_date', 'season_x', 'Season', 'home_team', 'away_team', 'game_id_x', 'game_period', 'game_seconds', 'clock_time', 'event_type', 'event_team', 'event_zone', 'event_player_1', 'event_player_2', 'faceoff_winning_team_xG_since_faceoff',
                     'faceoff_losing_team_xG_since_faceoff', 'faceoff_losing_team_xG_since_faceoff', 'faceoff_winner', 'Win_F1_Name', 'Win_F2_Name', 'Win_F3_Name',
                     'Win_D1_Name', 'Win_D2_Name', 'Lose_F1_Name', 'Lose_F2_Name', 'Lose_F3_Name', 'Lose_D1_Name', 'Lose_D2_Name', 'Win_F1', 'Win_F2', 'Win_F3',
                     'Win_D1', 'Win_D2', 'Lose_F1', 'Lose_F2', 'Lose_F3', 'Lose_D1', 'Lose_D2')
pbp_sub = pbp %>%
  select(cols_of_interest)

training_all = pbp %>% inner_join(nhl_player_seasons, by = c('Season', 'Win_F1_Name' = 'Player'), suffix = c('', '_Win_F1'))
