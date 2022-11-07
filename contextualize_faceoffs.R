source("driver.R")
library(dplyr)
library(tidyr)
library(stringr)
year = 2012
skaters = read_csv('EH_goalies_gar.csv')
eh_goalies  = read_csv('EH_skaters_gar.csv')

# Testing change

load_play_by_play = function(year) {
  pbp = read_csv(paste0("faceoff_analytics_", year, ".csv"))
  return(pbp)
}

pbp = load_play_by_play(2018)

get_goalies_list = function(eh_goalies) {
  goalie_eh_ids = unique(eh_goalies$EH_ID)
  return(goalie_eh_ids)
}

goalie_eh_ids = get_goalies_list(eh_goalies)

get_forwards_list = function(skaters) {
  forwards = skaters %>% filter(Position == 'L' | Position == 'C' | Position == 'R')
  forward_eh_ids = unique(forwards$EH_ID)
  return(forward_eh_ids)
}

forward_eh_ids = get_forwards_list(skaters)

get_defensemen_list = function(skaters) {
  defensemen = skaters %>% filter(Position == 'D')
  defensemen_eh_ids = unique(defensemen$EH_ID)
  return(defensemen_eh_ids)
}

defensemen_eh_ids = get_defensemen_list(skaters)

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

pbp_subset = condition_pbp_for_faceoffs(pbp)

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
    rename(winning_team_on_2_GAR = GAR) %>%
    left_join(gar_list, by = c('winning_team_on_3' = 'EH_ID', 'Season'), suffix = c('', '_winning_team_on_3')) %>%
    rename(winning_team_on_3_GAR = GAR) %>%
    left_join(gar_list, by = c('winning_team_on_4' = 'EH_ID', 'Season'), suffix = c('', '_winning_team_on_4')) %>%
    rename(winning_team_on_4_GAR = GAR) %>%
    left_join(gar_list, by = c('winning_team_on_5' = 'EH_ID', 'Season'), suffix = c('', '_winning_team_on_5')) %>%
    rename(winning_team_on_5_GAR = GAR) %>%
    left_join(gar_list, by = c('winning_team_on_6' = 'EH_ID', 'Season'), suffix = c('', '_winning_team_on_6')) %>%
    rename(winning_team_on_6_GAR = GAR) %>%
    left_join(gar_list, by = c('winning_team_on_7' = 'EH_ID', 'Season'), suffix = c('', '_winning_team_on_7')) %>%
    rename(winning_team_on_7_GAR = GAR)
    
  
  for (row in 1:nrow(pbp_subset_classified_with_gar)) {
    print(row)
    
    pbp_subset_classified_with_gar[row,] -> Test
    
    Test$Forward1 = ifelse(Test$winning_team_on_1_pos == 'F', Test$winning_team_on_1_GAR, NA)
    Test$Forward2 = ifelse(Test$winning_team_on_2_pos == 'F', Test$winning_team_on_2_GAR, NA)
    Test$Forward3 = ifelse(Test$winning_team_on_3_pos == 'F', Test$winning_team_on_3_GAR, NA)
    Test$Forward4 = ifelse(Test$winning_team_on_4_pos == 'F', Test$winning_team_on_4_GAR, NA)
    Test$Forward5 = ifelse(Test$winning_team_on_5_pos == 'F', Test$winning_team_on_5_GAR, NA)
    Test$Forward6 = ifelse(Test$winning_team_on_6_pos == 'F', Test$winning_team_on_6_GAR, NA)
    Test$Forward7 = ifelse(Test$winning_team_on_7_pos == 'F', Test$winning_team_on_7_GAR, NA)
    
    Test$Defenseman1 = ifelse(Test$winning_team_on_1_pos == 'D', Test$winning_team_on_1_GAR, NA)
    Test$Defenseman2 = ifelse(Test$winning_team_on_2_pos == 'D', Test$winning_team_on_2_GAR, NA)
    Test$Defenseman3 = ifelse(Test$winning_team_on_3_pos == 'D', Test$winning_team_on_3_GAR, NA)
    Test$Defenseman4 = ifelse(Test$winning_team_on_4_pos == 'D', Test$winning_team_on_4_GAR, NA)
    Test$Defenseman5 = ifelse(Test$winning_team_on_5_pos == 'D', Test$winning_team_on_5_GAR, NA)
    Test$Defenseman6 = ifelse(Test$winning_team_on_6_pos == 'D', Test$winning_team_on_6_GAR, NA)
    Test$Defenseman7 = ifelse(Test$winning_team_on_7_pos == 'D', Test$winning_team_on_7_GAR, NA)
    
    Test$F1 = as.numeric(Test[60:66][order(-Test[60:66])][1])
    Test$F2 = as.numeric(Test[60:66][order(-Test[60:66])][2])
    Test$F3 = as.numeric(Test[60:66][order(-Test[60:66])][3])
    Test$F4 = as.numeric(Test[60:66][order(-Test[60:66])][4])
    Test$F5 = as.numeric(Test[60:66][order(-Test[60:66])][5])
    
    Test$D1 = as.numeric(Test[67:73][order(-Test[67:73])][1])
    Test$D2 = as.numeric(Test[67:73][order(-Test[67:73])][2])
    Test$D3 = as.numeric(Test[67:73][order(-Test[67:73])][3])
    Test$D4 = as.numeric(Test[67:73][order(-Test[67:73])][4])
    Test$D5 = as.numeric(Test[67:73][order(-Test[67:73])][5])
    
    if(row == 1){
      Play = Test
    } 
    else {
      Play = Play %>% bind_rows(Test)
    }
    
    # w1_gar <- pbp_subset_classified_with_gar[row, "winning_team_on_1"]
    # w2_gar <- pbp_subset_classified_with_gar[row, "winning_team_on_2"]
    # w3_gar <- pbp_subset_classified_with_gar[row, "winning_team_on_3"]
    # w4_gar <- pbp_subset_classified_with_gar[row, "winning_team_on_4"]
    # w5_gar <- pbp_subset_classified_with_gar[row, "winning_team_on_5"]
    # w6_gar <- pbp_subset_classified_with_gar[row, "winning_team_on_6"]
    # w7_gar <- pbp_subset_classified_with_gar[row, "winning_team_on_7"]
  }
  
  pbp_subset_classified_with_gar = Play %>%
    select(-Forward1, -Forward2, -Forward3, -Forward4, -Forward5, -Forward6, -Forward7,
           -Defenseman1, -Defenseman2, -Defenseman3, -Defenseman4, -Defenseman5, -Defenseman6,
           -Defenseman7)
  
  return(pbp_subset_classified_with_gar)
}

ps = parse_out_positions(pbp_subset)

