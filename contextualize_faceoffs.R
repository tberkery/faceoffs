setwd("~/Desktop/7th Semester/Faceoffs_Project")
source("driver.R")
library(dplyr)
library(tidyr)
library(stringr)
skaters = read_csv('EH_goalies_gar.csv')
eh_goalies  = read_csv('EH_skaters_gar.csv')

# Testing change

load_play_by_play = function(year) {
  pbp = read_csv(paste0("faceoff_analytics_", year, ".csv"))
  return(pbp)
}

pbp = load_play_by_play(2015)

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
    select(game_date, season_x, home_team, away_team, 
           event_type, event_team, event_zone, event_player_1, event_player_2,
           starts_with('home_on'), home_goalie, home_skaters, starts_with('away_on'),
           faceoff_winning_team_xG_since_faceoff, faceoff_losing_team_xG_since_faceoff) %>%
    mutate(faceoff_winner = event_team,
           home_team_FA_xG = ifelse(event_team == home_team,
                                    lead(faceoff_winning_team_xG_since_faceoff, 1), 
                                    lead(faceoff_losing_team_xG_since_faceoff, 1)),
           away_team_FA_xG = ifelse(event_team == away_team,
                                    lead(faceoff_winning_team_xG_since_faceoff, 1),
                                    lead(faceoff_losing_team_xG_since_faceoff, 1))
    ) %>%
    filter(event_type == 'FAC') %>%
    mutate(winning_team_on_1 = ifelse(event_team == home_team, home_on_1, away_on_1)) %>%
    mutate(winning_team_on_2 = ifelse(event_team == home_team, home_on_2, away_on_2)) %>%
    mutate(winning_team_on_3 = ifelse(event_team == home_team, home_on_3, away_on_3)) %>%
    mutate(winning_team_on_4 = ifelse(event_team == home_team, home_on_4, away_on_4)) %>%
    mutate(winning_team_on_5 = ifelse(event_team == home_team, home_on_5, away_on_5)) %>%
    mutate(winning_team_on_6 = ifelse(event_team == home_team, home_on_6, away_on_6)) %>%
    mutate(winning_team_on_7 = ifelse(event_team == home_team, home_on_7, away_on_7)) %>%
    mutate(losing_team_on_1 = ifelse(event_team != home_team, home_on_1, away_on_1)) %>%
    mutate(losing_team_on_2 = ifelse(event_team != home_team, home_on_2, away_on_2)) %>%
    mutate(losing_team_on_3 = ifelse(event_team != home_team, home_on_3, away_on_3)) %>%
    mutate(losing_team_on_4 = ifelse(event_team != home_team, home_on_4, away_on_4)) %>%
    mutate(losing_team_on_5 = ifelse(event_team != home_team, home_on_5, away_on_5)) %>%
    mutate(losing_team_on_6 = ifelse(event_team != home_team, home_on_6, away_on_6)) %>%
    mutate(losing_team_on_7 = ifelse(event_team != home_team, home_on_7, away_on_7)) %>%
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
    )) %>% 
      mutate(losing_team_on_1_pos = case_when(
        losing_team_on_1 %in% forwards ~ 'F',
        losing_team_on_1 %in% defensemen ~ 'D',
        losing_team_on_1 %in% goalies ~ 'G',
        TRUE ~ 'Unknown'
      )) %>%
      mutate(losing_team_on_2_pos = case_when(
        losing_team_on_2 %in% forwards ~ 'F',
        losing_team_on_2 %in% defensemen ~ 'D',
        losing_team_on_2 %in% goalies ~ 'G',
        TRUE ~ 'Unknown'
      )) %>%
      mutate(losing_team_on_3_pos = case_when(
        losing_team_on_3 %in% forwards ~ 'F',
        losing_team_on_3 %in% defensemen ~ 'D',
        losing_team_on_3 %in% goalies ~ 'G',
        TRUE ~ 'Unknown'
      )) %>%
      mutate(losing_team_on_4_pos = case_when(
        losing_team_on_4 %in% forwards ~ 'F',
        losing_team_on_4 %in% defensemen ~ 'D',
        losing_team_on_4 %in% goalies ~ 'G',
        TRUE ~ 'Unknown'
      )) %>%
      mutate(losing_team_on_5_pos = case_when(
        losing_team_on_5 %in% forwards ~ 'F',
        losing_team_on_5 %in% defensemen ~ 'D',
        losing_team_on_5 %in% goalies ~ 'G',
        TRUE ~ 'Unknown'
      )) %>%
      mutate(losing_team_on_6_pos = case_when(
        losing_team_on_6 %in% forwards ~ 'F',
        losing_team_on_6 %in% defensemen ~ 'D',
        losing_team_on_6 %in% goalies ~ 'G',
        TRUE ~ 'Unknown'
      )) %>%
      mutate(losing_team_on_7_pos = case_when(
        losing_team_on_7 %in% forwards ~ 'F',
        losing_team_on_7 %in% defensemen ~ 'D',
        losing_team_on_7 %in% goalies ~ 'G',
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
    rename(winning_team_on_7_GAR = GAR) %>%
    left_join(gar_list, by = c('losing_team_on_1' = 'EH_ID', 'Season'), suffix = c('', '_losing_team_on_1')) %>%
    rename(losing_team_on_1_GAR = GAR) %>%
    left_join(gar_list, by = c('losing_team_on_2' = 'EH_ID', 'Season'), suffix = c('', '_losing_team_on_2')) %>%
    rename(losing_team_on_2_GAR = GAR) %>%
    left_join(gar_list, by = c('losing_team_on_3' = 'EH_ID', 'Season'), suffix = c('', '_losing_team_on_3')) %>%
    rename(losing_team_on_3_GAR = GAR) %>%
    left_join(gar_list, by = c('losing_team_on_4' = 'EH_ID', 'Season'), suffix = c('', '_losing_team_on_4')) %>%
    rename(losing_team_on_4_GAR = GAR) %>%
    left_join(gar_list, by = c('losing_team_on_5' = 'EH_ID', 'Season'), suffix = c('', '_losing_team_on_5')) %>%
    rename(losing_team_on_5_GAR = GAR) %>%
    left_join(gar_list, by = c('losing_team_on_6' = 'EH_ID', 'Season'), suffix = c('', '_losing_team_on_6')) %>%
    rename(losing_team_on_6_GAR = GAR) %>%
    left_join(gar_list, by = c('losing_team_on_7' = 'EH_ID', 'Season'), suffix = c('', '_losing_team_on_7')) %>%
    rename(losing_team_on_7_GAR = GAR)
  
  pbp_subset_classified_with_gar$Win_Forward1 = ifelse(pbp_subset_classified_with_gar$winning_team_on_1_pos == 'F', pbp_subset_classified_with_gar$winning_team_on_1_GAR, NA)
  pbp_subset_classified_with_gar$Win_Forward2 = ifelse(pbp_subset_classified_with_gar$winning_team_on_2_pos == 'F', pbp_subset_classified_with_gar$winning_team_on_2_GAR, NA)
  pbp_subset_classified_with_gar$Win_Forward3 = ifelse(pbp_subset_classified_with_gar$winning_team_on_3_pos == 'F', pbp_subset_classified_with_gar$winning_team_on_3_GAR, NA)
  pbp_subset_classified_with_gar$Win_Forward4 = ifelse(pbp_subset_classified_with_gar$winning_team_on_4_pos == 'F', pbp_subset_classified_with_gar$winning_team_on_4_GAR, NA)
  pbp_subset_classified_with_gar$Win_Forward5 = ifelse(pbp_subset_classified_with_gar$winning_team_on_5_pos == 'F', pbp_subset_classified_with_gar$winning_team_on_5_GAR, NA)
  pbp_subset_classified_with_gar$Win_Forward6 = ifelse(pbp_subset_classified_with_gar$winning_team_on_6_pos == 'F', pbp_subset_classified_with_gar$winning_team_on_6_GAR, NA)
  pbp_subset_classified_with_gar$Win_Forward7 = ifelse(pbp_subset_classified_with_gar$winning_team_on_7_pos == 'F', pbp_subset_classified_with_gar$winning_team_on_7_GAR, NA)
  
  
  pbp_subset_classified_with_gar$Win_Defenseman1 = ifelse(pbp_subset_classified_with_gar$winning_team_on_1_pos == 'D', pbp_subset_classified_with_gar$winning_team_on_1_GAR, NA)
  pbp_subset_classified_with_gar$Win_Defenseman2 = ifelse(pbp_subset_classified_with_gar$winning_team_on_2_pos == 'D', pbp_subset_classified_with_gar$winning_team_on_2_GAR, NA)
  pbp_subset_classified_with_gar$Win_Defenseman3 = ifelse(pbp_subset_classified_with_gar$winning_team_on_3_pos == 'D', pbp_subset_classified_with_gar$winning_team_on_3_GAR, NA)
  pbp_subset_classified_with_gar$Win_Defenseman4 = ifelse(pbp_subset_classified_with_gar$winning_team_on_4_pos == 'D', pbp_subset_classified_with_gar$winning_team_on_4_GAR, NA)
  pbp_subset_classified_with_gar$Win_Defenseman5 = ifelse(pbp_subset_classified_with_gar$winning_team_on_5_pos == 'D', pbp_subset_classified_with_gar$winning_team_on_5_GAR, NA)
  pbp_subset_classified_with_gar$Win_Defenseman6 = ifelse(pbp_subset_classified_with_gar$winning_team_on_6_pos == 'D', pbp_subset_classified_with_gar$winning_team_on_6_GAR, NA)
  pbp_subset_classified_with_gar$Win_Defenseman7 = ifelse(pbp_subset_classified_with_gar$winning_team_on_7_pos == 'D', pbp_subset_classified_with_gar$winning_team_on_7_GAR, NA)
  
  pbp_subset_classified_with_gar$Lose_Forward1 = ifelse(pbp_subset_classified_with_gar$losing_team_on_1_pos == 'F', pbp_subset_classified_with_gar$losing_team_on_1_GAR, NA)
  pbp_subset_classified_with_gar$Lose_Forward2 = ifelse(pbp_subset_classified_with_gar$losing_team_on_2_pos == 'F', pbp_subset_classified_with_gar$losing_team_on_2_GAR, NA)
  pbp_subset_classified_with_gar$Lose_Forward3 = ifelse(pbp_subset_classified_with_gar$losing_team_on_3_pos == 'F', pbp_subset_classified_with_gar$losing_team_on_3_GAR, NA)
  pbp_subset_classified_with_gar$Lose_Forward4 = ifelse(pbp_subset_classified_with_gar$losing_team_on_4_pos == 'F', pbp_subset_classified_with_gar$losing_team_on_4_GAR, NA)
  pbp_subset_classified_with_gar$Lose_Forward5 = ifelse(pbp_subset_classified_with_gar$losing_team_on_5_pos == 'F', pbp_subset_classified_with_gar$losing_team_on_5_GAR, NA)
  pbp_subset_classified_with_gar$Lose_Forward6 = ifelse(pbp_subset_classified_with_gar$losing_team_on_6_pos == 'F', pbp_subset_classified_with_gar$losing_team_on_6_GAR, NA)
  pbp_subset_classified_with_gar$Lose_Forward7 = ifelse(pbp_subset_classified_with_gar$losing_team_on_7_pos == 'F', pbp_subset_classified_with_gar$losing_team_on_7_GAR, NA)
  
  pbp_subset_classified_with_gar$Lose_Defenseman1 = ifelse(pbp_subset_classified_with_gar$losing_team_on_1_pos == 'D', pbp_subset_classified_with_gar$losing_team_on_1_GAR, NA)
  pbp_subset_classified_with_gar$Lose_Defenseman2 = ifelse(pbp_subset_classified_with_gar$losing_team_on_2_pos == 'D', pbp_subset_classified_with_gar$losing_team_on_2_GAR, NA)
  pbp_subset_classified_with_gar$Lose_Defenseman3 = ifelse(pbp_subset_classified_with_gar$losing_team_on_3_pos == 'D', pbp_subset_classified_with_gar$losing_team_on_3_GAR, NA)
  pbp_subset_classified_with_gar$Lose_Defenseman4 = ifelse(pbp_subset_classified_with_gar$losing_team_on_4_pos == 'D', pbp_subset_classified_with_gar$losing_team_on_4_GAR, NA)
  pbp_subset_classified_with_gar$Lose_Defenseman5 = ifelse(pbp_subset_classified_with_gar$losing_team_on_5_pos == 'D', pbp_subset_classified_with_gar$losing_team_on_5_GAR, NA)
  pbp_subset_classified_with_gar$Lose_Defenseman6 = ifelse(pbp_subset_classified_with_gar$losing_team_on_6_pos == 'D', pbp_subset_classified_with_gar$losing_team_on_6_GAR, NA)
  pbp_subset_classified_with_gar$Lose_Defenseman7 = ifelse(pbp_subset_classified_with_gar$losing_team_on_7_pos == 'D', pbp_subset_classified_with_gar$losing_team_on_7_GAR, NA)
  
  pbp_subset_classified_with_gar$Win_Forward1_Name = ifelse(pbp_subset_classified_with_gar$winning_team_on_1_pos == 'F', pbp_subset_classified_with_gar$winning_team_on_1, NA)
  pbp_subset_classified_with_gar$Win_Forward2_Name = ifelse(pbp_subset_classified_with_gar$winning_team_on_2_pos == 'F', pbp_subset_classified_with_gar$winning_team_on_2, NA)
  pbp_subset_classified_with_gar$Win_Forward3_Name = ifelse(pbp_subset_classified_with_gar$winning_team_on_3_pos == 'F', pbp_subset_classified_with_gar$winning_team_on_3, NA)
  pbp_subset_classified_with_gar$Win_Forward4_Name = ifelse(pbp_subset_classified_with_gar$winning_team_on_4_pos == 'F', pbp_subset_classified_with_gar$winning_team_on_4, NA)
  pbp_subset_classified_with_gar$Win_Forward5_Name = ifelse(pbp_subset_classified_with_gar$winning_team_on_5_pos == 'F', pbp_subset_classified_with_gar$winning_team_on_5, NA)
  pbp_subset_classified_with_gar$Win_Forward6_Name = ifelse(pbp_subset_classified_with_gar$winning_team_on_6_pos == 'F', pbp_subset_classified_with_gar$winning_team_on_6, NA)
  pbp_subset_classified_with_gar$Win_Forward7_Name = ifelse(pbp_subset_classified_with_gar$winning_team_on_7_pos == 'F', pbp_subset_classified_with_gar$winning_team_on_7, NA)
  
  pbp_subset_classified_with_gar$Win_Defenseman1_Name = ifelse(pbp_subset_classified_with_gar$winning_team_on_1_pos == 'D', pbp_subset_classified_with_gar$winning_team_on_1, NA)
  pbp_subset_classified_with_gar$Win_Defenseman2_Name = ifelse(pbp_subset_classified_with_gar$winning_team_on_2_pos == 'D', pbp_subset_classified_with_gar$winning_team_on_2, NA)
  pbp_subset_classified_with_gar$Win_Defenseman3_Name = ifelse(pbp_subset_classified_with_gar$winning_team_on_3_pos == 'D', pbp_subset_classified_with_gar$winning_team_on_3, NA)
  pbp_subset_classified_with_gar$Win_Defenseman4_Name = ifelse(pbp_subset_classified_with_gar$winning_team_on_4_pos == 'D', pbp_subset_classified_with_gar$winning_team_on_4, NA)
  pbp_subset_classified_with_gar$Win_Defenseman5_Name = ifelse(pbp_subset_classified_with_gar$winning_team_on_5_pos == 'D', pbp_subset_classified_with_gar$winning_team_on_5, NA)
  pbp_subset_classified_with_gar$Win_Defenseman6_Name = ifelse(pbp_subset_classified_with_gar$winning_team_on_6_pos == 'D', pbp_subset_classified_with_gar$winning_team_on_6, NA)
  pbp_subset_classified_with_gar$Win_Defenseman7_Name = ifelse(pbp_subset_classified_with_gar$winning_team_on_7_pos == 'D', pbp_subset_classified_with_gar$winning_team_on_7, NA)
  
  pbp_subset_classified_with_gar$Lose_Forward1_Name = ifelse(pbp_subset_classified_with_gar$losing_team_on_1_pos == 'F', pbp_subset_classified_with_gar$losing_team_on_1, NA)
  pbp_subset_classified_with_gar$Lose_Forward2_Name = ifelse(pbp_subset_classified_with_gar$losing_team_on_2_pos == 'F', pbp_subset_classified_with_gar$losing_team_on_2, NA)
  pbp_subset_classified_with_gar$Lose_Forward3_Name = ifelse(pbp_subset_classified_with_gar$losing_team_on_3_pos == 'F', pbp_subset_classified_with_gar$losing_team_on_3, NA)
  pbp_subset_classified_with_gar$Lose_Forward4_Name = ifelse(pbp_subset_classified_with_gar$losing_team_on_4_pos == 'F', pbp_subset_classified_with_gar$losing_team_on_4, NA)
  pbp_subset_classified_with_gar$Lose_Forward5_Name = ifelse(pbp_subset_classified_with_gar$losing_team_on_5_pos == 'F', pbp_subset_classified_with_gar$losing_team_on_5, NA)
  pbp_subset_classified_with_gar$Lose_Forward6_Name = ifelse(pbp_subset_classified_with_gar$losing_team_on_6_pos == 'F', pbp_subset_classified_with_gar$losing_team_on_6, NA)
  pbp_subset_classified_with_gar$Lose_Forward7_Name = ifelse(pbp_subset_classified_with_gar$losing_team_on_7_pos == 'F', pbp_subset_classified_with_gar$losing_team_on_7, NA)
  
  pbp_subset_classified_with_gar$Lose_Defenseman1_Name = ifelse(pbp_subset_classified_with_gar$losing_team_on_1_pos == 'D', pbp_subset_classified_with_gar$losing_team_on_1, NA)
  pbp_subset_classified_with_gar$Lose_Defenseman2_Name = ifelse(pbp_subset_classified_with_gar$losing_team_on_2_pos == 'D', pbp_subset_classified_with_gar$losing_team_on_2, NA)
  pbp_subset_classified_with_gar$Lose_Defenseman3_Name = ifelse(pbp_subset_classified_with_gar$losing_team_on_3_pos == 'D', pbp_subset_classified_with_gar$losing_team_on_3, NA)
  pbp_subset_classified_with_gar$Lose_Defenseman4_Name = ifelse(pbp_subset_classified_with_gar$losing_team_on_4_pos == 'D', pbp_subset_classified_with_gar$losing_team_on_4, NA)
  pbp_subset_classified_with_gar$Lose_Defenseman5_Name = ifelse(pbp_subset_classified_with_gar$losing_team_on_5_pos == 'D', pbp_subset_classified_with_gar$losing_team_on_5, NA)
  pbp_subset_classified_with_gar$Lose_Defenseman6_Name = ifelse(pbp_subset_classified_with_gar$losing_team_on_6_pos == 'D', pbp_subset_classified_with_gar$losing_team_on_6, NA)
  pbp_subset_classified_with_gar$Lose_Defenseman7_Name = ifelse(pbp_subset_classified_with_gar$losing_team_on_7_pos == 'D', pbp_subset_classified_with_gar$losing_team_on_7, NA)
  
  pbp_subset_classified_with_gar = pbp_subset_classified_with_gar %>%
    mutate(Win_F1 = NA,
           Win_F2 = NA,
           Win_F3 = NA,
           Win_F4 = NA,
           Win_F5 = NA,
           Win_F6 = NA,
           Win_F7 = NA,
           Win_F1_Name = NA,
           Win_F2_Name = NA,
           Win_F3_Name = NA,
           Win_F4_Name = NA,
           Win_F5_Name = NA,
           Win_F6_Name = NA,
           Win_F7_Name = NA,
           Win_D1 = NA,
           Win_D2 = NA,
           Win_D3 = NA,
           Win_D4 = NA,
           Win_D5 = NA,
           Win_D6 = NA,
           Win_D7 = NA,
           Win_D1_Name = NA,
           Win_D2_Name = NA,
           Win_D3_Name = NA,
           Win_D4_Name = NA,
           Win_D5_Name = NA,
           Win_D6_Name = NA,
           Win_D7_Name = NA,
           Lose_F1 = NA,
           Lose_F2 = NA,
           Lose_F3 = NA,
           Lose_F4 = NA,
           Lose_F5 = NA,
           Lose_F6 = NA,
           Lose_F7 = NA,
           Lose_F1_Name = NA,
           Lose_F2_Name = NA,
           Lose_F3_Name = NA,
           Lose_F4_Name = NA,
           Lose_F5_Name = NA,
           Lose_F6_Name = NA,
           Lose_F7_Name = NA,
           Lose_D1 = NA,
           Lose_D2 = NA,
           Lose_D3 = NA,
           Lose_D4 = NA,
           Lose_D5 = NA,
           Lose_D6 = NA,
           Lose_D7 = NA,
           Lose_D1_Name = NA,
           Lose_D2_Name = NA,
           Lose_D3_Name = NA,
           Lose_D4_Name = NA,
           Lose_D5_Name = NA,
           Lose_D6_Name = NA,
           Lose_D7_Name = NA
           )
  
  for (row in 1:nrow(pbp_subset_classified_with_gar)) {
    print(paste0(row, " ", round(row/nrow(pbp_subset_classified_with_gar)),2))
    
    pbp_subset_classified_with_gar[row,] -> Temp
    
    Temp[130:136] = as.list(as.numeric(Temp[74:80][order(-Temp[74:80])]))
    Temp[137:143] = as.list(as.character(Temp[102:108][order(-Temp[74:80])]))
    
    Temp[144:150] = as.list(as.numeric(Temp[81:87][order(-Temp[81:87])]))
    Temp[151:157] = as.list(as.numeric(Temp[109:115][order(-Temp[81:87])]))
    
    Temp[158:164] = as.list(as.numeric(Temp[88:94][order(-Temp[88:94])]))
    Temp[165:171] = as.list(as.character(Temp[116:122][order(-Temp[88:94])]))
    
    Temp[172:178] = as.list(as.numeric(Temp[95:101][order(-Temp[95:101])]))
    Temp[179:185] = as.list(as.character(Temp[123:129][order(-Temp[95:101])]))
    
    
    if(row == 1){
      Play = Temp
    } 
    else {
      Play = Play %>% bind_rows(Temp)
    }
  }
  
  pbp_subset_classified_with_gar = Play %>%
    select(-Win_Forward1_Name, -Win_Forward2_Name, -Win_Forward3_Name, -Win_Forward4_Name, -Win_Forward5_Name, -Win_Forward6_Name, -Win_Forward7_Name,
           -Win_Defenseman1_Name, -Win_Defenseman2_Name, -Win_Defenseman3_Name, -Win_Defenseman4_Name, -Win_Defenseman5_Name, -Win_Defenseman6_Name,
           -Win_Defenseman7_Name, -Lose_Forward1_Name, -Lose_Forward2_Name, -Lose_Forward3_Name, -Lose_Forward4_Name, -Lose_Forward5_Name, -Lose_Forward6_Name, -Lose_Forward7_Name,
           -Lose_Defenseman1_Name, -Lose_Defenseman2_Name, -Lose_Defenseman3_Name, -Lose_Defenseman4_Name, -Lose_Defenseman5_Name, -Lose_Defenseman6_Name,
           -Lose_Defenseman7_Name,-Win_Forward1, -Win_Forward2, -Win_Forward3, -Win_Forward4, -Win_Forward5, -Win_Forward6, -Win_Forward7,
           -Win_Defenseman1, -Win_Defenseman2, -Win_Defenseman3, -Win_Defenseman4, -Win_Defenseman5, -Win_Defenseman6,
           -Win_Defenseman7, -Lose_Forward1, -Lose_Forward2, -Lose_Forward3, -Lose_Forward4, -Lose_Forward5, -Lose_Forward6, -Lose_Forward7,
           -Lose_Defenseman1, -Lose_Defenseman2, -Lose_Defenseman3, -Lose_Defenseman4, -Lose_Defenseman5, -Lose_Defenseman6,
           -Lose_Defenseman7)
  
  return(pbp_subset_classified_with_gar)
}

ps = parse_out_positions(pbp_subset)
