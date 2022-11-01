source("driver.R")
library(dplyr)
library(tidyr)
library(stringr)
year = 2012

load_play_by_play = function(year) {
  pbp = read_csv(paste0("faceoff_analytics_", year, ".csv"))
  return(pbp)
}

get_goalies_list = function(eh_goalies) {
  goalie_eh_ids = unique(eh_goalies$EH_ID)
  return(goalie_eh_ids)
}

get_forwards_list = function(skaters) {
  forwards = skaters %>% filter(Position == 'L' | Position == 'C' | Position == 'R')
  forward_eh_ids = unique(forwards$EH_ID)
  return(forward_eh_ids)
}

get_defensemen_list = function(skaters) {
  defensemen = skaters %>% filter(Position == 'D')
  defensemen_eh_ids = unique(defensemen$EH_ID)
  return(defensemen_eh_ids)
}

condition_pbp_for_faceoffs = function(pbp) {
  pbp_subset = pbp %>%
    filter(event_type != 'CHANGE') %>%
    filter(event_type == 'FAC' | lead(event_type, 1) == 'FAC') %>%
    select(game_date, season_x, home_team, away_team,  event_type, event_team, event_zone, event_player_1, event_player_2, starts_with('home_on'), home_goalie, home_skaters, starts_with('away_on'), faceoff_winning_team_xG_since_faceoff, faceoff_losing_team_xG_since_faceoff) %>%
    mutate(faceoff_winner = event_team,
           home_team_FA_xG = ifelse(event_team == home_team, lead(faceoff_winning_team_xG_since_faceoff, 1), lead(faceoff_losing_team_xG_since_faceoff, 1)),
           away_team_FA_xG = ifelse(event_team == away_team, lead(faceoff_winning_team_xG_since_faceoff, 1), lead(faceoff_losing_team_xG_since_faceoff, 1))
    ) %>%
    filter(event_type == 'FAC') %>%
    mutate(winning_team_on_1 = ifelse(event_team == home_team, home_on_1, away_on_1)) %>%
    mutate(winning_team_on_2 = ifelse(event_team == home_team, home_on_2, away_on_2)) %>%
    mutate(winning_team_on_3 = ifelse(event_team == home_team, home_on_3, away_on_3)) %>%
    mutate(winning_team_on_4 = ifelse(event_team == home_team, home_on_4, away_on_4)) %>%
    mutate(winning_team_on_5 = ifelse(event_team == home_team, home_on_5, away_on_5)) %>%
    mutate(winning_team_on_6 = ifelse(event_team == home_team, home_on_6, away_on_6)) %>%
    mutate(winning_team_on_7 = ifelse(event_team == home_team, home_on_7, away_on_7)) %>%
    mutate(losing_team_on_1 = ifelse(event_team == home_team, home_on_1, away_on_1)) %>%
    mutate(losing_team_on_2 = ifelse(event_team == home_team, home_on_2, away_on_2)) %>%
    mutate(losing_team_on_3 = ifelse(event_team == home_team, home_on_3, away_on_3)) %>%
    mutate(losing_team_on_4 = ifelse(event_team == home_team, home_on_4, away_on_4)) %>%
    mutate(losing_team_on_5 = ifelse(event_team == home_team, home_on_5, away_on_5)) %>%
    mutate(losing_team_on_6 = ifelse(event_team == home_team, home_on_6, away_on_6)) %>%
    mutate(losing_team_on_7 = ifelse(event_team == home_team, home_on_7, away_on_7)) %>%
    mutate(Season = paste0(substr(season_x, 3, 4), '-', substr(season_x, 7, 8)))
}

parse_out_positions = function(pbp_subset) {
  forwards = get_forwards_list(read_eh_box())
  defensemen = get_defensemen_list(read_eh_box())
  goalies = get_goalies_list(read_eh_goalies())
  pbp_subset_classified = pbp_subset %>%
    mutate(winning_team_on_1_pos = case_when(
      winning_team_on_1 %in% forwards ~ 'F',
      winning_team_on_1 %in% defensemen ~ 'D',
      winning_team_on_1 %in% goalies ~ 'G',
      TRUE ~ 'Unknown'
    )) %>%
    mutate(winning_team_on_2_pos = case_when(
      winning_team_on_2 %in% forwards ~ 'F',
      winning_team_on_2 %in% defensemen ~ 'D',
      winning_team_on_2 %in% goalies ~ 'G',
      TRUE ~ 'Unknown'
    )) %>%
    mutate(winning_team_on_3_pos = case_when(
      winning_team_on_3 %in% forwards ~ 'F',
      winning_team_on_3 %in% defensemen ~ 'D',
      winning_team_on_3 %in% goalies ~ 'G',
      TRUE ~ 'Unknown'
    )) %>%
    mutate(winning_team_on_4_pos = case_when(
      winning_team_on_4 %in% forwards ~ 'F',
      winning_team_on_4 %in% defensemen ~ 'D',
      winning_team_on_4 %in% goalies ~ 'G',
      TRUE ~ 'Unknown'
    )) %>%
    mutate(winning_team_on_5_pos = case_when(
      winning_team_on_5 %in% forwards ~ 'F',
      winning_team_on_5 %in% defensemen ~ 'D',
      winning_team_on_5 %in% goalies ~ 'G',
      TRUE ~ 'Unknown'
    )) %>%
    mutate(winning_team_on_6_pos = case_when(
      winning_team_on_6 %in% forwards ~ 'F',
      winning_team_on_6 %in% defensemen ~ 'D',
      winning_team_on_6 %in% goalies ~ 'G',
      TRUE ~ 'Unknown'
    )) %>%
    mutate(winning_team_on_7_pos = case_when(
      winning_team_on_7 %in% forwards ~ 'F',
      winning_team_on_7 %in% defensemen ~ 'D',
      winning_team_on_7 %in% goalies ~ 'G',
      TRUE ~ 'Unknown'
    )
    )
  
  gar_list = get_gar_list()
  
  pbp_subset_classified_with_gar = pbp_subset_classified %>%
    left_join(gar_list, by = c('winning_team_on_1' = 'EH_ID', 'Season'), suffix = c('', '_winning_team_on_1')) %>%
    rename(winning_team_on_1_GAR = GAR) %>%
    left_join(gar_list, by = c('winning_team_on_2' = 'EH_ID', 'Season'), suffix = c('', '_winning_team_on_2')) %>%
    left_join(gar_list, by = c('winning_team_on_3' = 'EH_ID', 'Season'), suffix = c('', '_winning_team_on_3')) %>%
    left_join(gar_list, by = c('winning_team_on_4' = 'EH_ID', 'Season'), suffix = c('', '_winning_team_on_4')) %>%
    left_join(gar_list, by = c('winning_team_on_5' = 'EH_ID', 'Season'), suffix = c('', '_winning_team_on_5')) %>%
    left_join(gar_list, by = c('winning_team_on_6' = 'EH_ID', 'Season'), suffix = c('', '_winning_team_on_6')) %>%
    left_join(gar_list, by = c('winning_team_on_7' = 'EH_ID', 'Season'), suffix = c('', '_winning_team_on_7')) %>%
    mutate(top_forward)
  
  for (row in 1:nrow(pbp_subset_classified_with_gar)) {
     w1_gar <- pbp_subset_classified_with_gar[row, "winning_team_on_1"]
     w2_gar <- pbp_subset_classified_with_gar[row, "winning_team_on_2"]
     w3_gar <- pbp_subset_classified_with_gar[row, "winning_team_on_3"]
     w4_gar <- pbp_subset_classified_with_gar[row, "winning_team_on_4"]
     w5_gar <- pbp_subset_classified_with_gar[row, "winning_team_on_5"]
     w6_gar <- pbp_subset_classified_with_gar[row, "winning_team_on_6"]
     w7_gar <- pbp_subset_classified_with_gar[row, "winning_team_on_7"]
  }
}


get_gar_list = function() {
  skater_gar = read_eh_gar_skaters()
  skater_gar = skater_gar %>%
    select(EH_ID, Season, GAR)
  goalie_gar = read_eh_gar_goalies()
  goalie_gar = goalie_gar %>%
    select(EH_ID, Season, GAR)
  all_gar = rbind(skater_gar, goalie_gar)
  return(all_gar)
}
