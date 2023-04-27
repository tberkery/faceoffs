library(tidyverse)
assemble_stats = function() {
  source("driver.R")
  mega_dict = connect_skaters_and_goaltending_to_team_performance()
}

load_sznajder = function() {
  source("load_data.R")
  load_season(2017, 2019) # note that this does the 2017-2018 and 2018-2019 seasons (i.e. boundaries are inclusive, exclusive)
  zone_entries_17_18_19 = read_csv("zone_entries_intermediate.csv")
  zone_exits_17_18_19 = read_csv("zone_exits_intermediate.csv")
  source("load_data_20-22.R")
  load_season(2020, 2022) # same note... this is 2020-2021 and 2021-2022.
  zone_entries_20_21_22 = read_csv("zone_entries_intermediate.csv")
  zone_exits_20_21_22 = read_csv("zone_exits_intermediate.csv")
  all_zone_entries = rbind(zone_entries_17_18_19, zone_entries_20_21_22)
  all_zone_exits = rbind(zone_exits_17_18_19, zone_exits_20_21_22)
  source("Join_Entries.R")
  big_join = join_entries(2017, 2022, all_zone_entries, all_zone_exits)
  big_join %>% write_csv("updated_big_join.csv")
  big_join = read_csv("big_join_updated")
  source("join_pbp_and_sznajder.R")
  pbp_with_role = condition(big_join, c(2017, 2018, 2020, 2021))
  mega_dict = assemble_stats()
  dataset = get_role_encoded_stats(pbp_with_role, mega_dict)
  dataset %>% write_csv("new_dataset_updated.csv")
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
}

load_sznajder_2022 = function() {
  source("load_data_22-23.R")
  load_season(2022, 2023) # same note... this is 2020-2021 and 2021-2022.
  all_zone_entries = read_csv("zone_entries_intermediate.csv")
  all_zone_exits = read_csv("zone_exits_intermediate.csv")
  source("Join_Entries.R")
  big_join = join_entries(2022, 2023, all_zone_entries, all_zone_exits)
  big_join = read_csv("big_join_after_fixes.csv")
  source("join_pbp_and_sznajder.R")
  pbp_with_role = condition(big_join, c(2022))
  mega_dict = assemble_stats()
  dataset = get_role_encoded_stats(pbp_with_role, mega_dict)
  dataset %>% write_csv("new_dataset_updated_2022.csv")
  dataset = subset_relevant_cols(dataset)
}

check_leivo = function(temp) {
  temp = temp %>%
    filter(Win_F1_Name == 'JOSH.LEIVO')
  print(nrow(temp))
}