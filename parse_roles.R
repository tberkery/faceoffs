library(tidyverse)

identify_roles = function(big_join, mega_dict) {
  player_season_positions = mega_dict %>%
    select(EH_ID, Season, Position) %>%
    mutate(Pos = case_when(
      Position == 'D' ~ 'Defenseman',
      Position == 'D/R' ~ 'Defenseman',
      Position == 'D/L' ~ 'Defenseman',
      TRUE ~ 'Forward'
    )) %>%
    mutate(season = as.numeric(paste0("20", substr(Season, 1, 2), "20", substr(Season, 4, 5)))) %>%
    select(-Position, -Season) %>%
    group_by(EH_ID, season) %>%
    distinct(.keep_all = TRUE)
  faceoffs = big_join %>%
    filter(event_type == 'FAC') %>% 
    left_join(player_season_positions, by = c('home_on_1' = 'EH_ID', 'season')) %>%
    left_join(player_season_positions, by = c('home_on_2' = 'EH_ID', 'season'), suffix = c('_home_on_1', '_home_on_2')) %>%
    left_join(player_season_positions, by = c('home_on_3' = 'EH_ID', 'season')) %>%
    left_join(player_season_positions, by = c('home_on_4' = 'EH_ID', 'season'), suffix = c('_home_on_3', '_home_on_4')) %>%
    left_join(player_season_positions, by = c('home_on_5' = 'EH_ID', 'season')) %>%
    left_join(player_season_positions, by = c('away_on_1' = 'EH_ID', 'season'), suffix = c('_home_on_5', '_away_on_1')) %>%
    left_join(player_season_positions, by = c('away_on_2' = 'EH_ID', 'season')) %>%
    left_join(player_season_positions, by = c('away_on_3' = 'EH_ID', 'season'), suffix = c('_away_on_2', '_away_on_3')) %>%
    left_join(player_season_positions, by = c('away_on_4' = 'EH_ID', 'season')) %>%
    left_join(player_season_positions, by = c('away_on_5' = 'EH_ID', 'season'), suffix = c('_away_on_4', '_away_on_5'))
  
  statuses = c("home", "away")
  for_cols = c("_on_1", "_on_2", "_on_3", "_on_1", "_on_2", "_on_3")
  def_cols = c("_on_4", "_on_5", "_on_4", "_on_5")
  for (status in statuses) {
    for (for_col in for_cols) {
      for (def_col in def_cols) {
        print(paste0("working on ", status, for_col, "/", status, def_col, " swap"))
        criteria = (!is.na(faceoffs[[paste0("Pos_", status, for_col)]]) &
                      !is.na(faceoffs[[paste0("Pos_", status, def_col)]]) &
                      faceoffs[[paste0("Pos_", status, for_col)]] == 'Defenseman' & 
                      faceoffs[[paste0("Pos_", status, def_col)]] == 'Forward')
        faceoffs[criteria, c(paste0(status, for_col), paste0(status,def_col))] = faceoffs[criteria, c(paste0(status, def_col), paste0(status, for_col))]
        faceoffs[criteria, c(paste0("Pos_", status, for_col), paste0("Pos_", status, def_col))] = faceoffs[criteria, c(paste0("Pos_", status, def_col), paste0("Pos_", status, for_col))]
      }
    }
  }
 return(faceoffs) 
}

identify_faceoff_winners = function(faceoffs) {
  faceoffs_updated = faceoffs %>%
    mutate(team_abbrev_faceoff_winner = str_trim(substr(event_description, 1, 3))) %>%
    mutate(all_needle_positions = str_locate_all(event_description, team_abbrev_faceoff_winner)) %>%
    mutate(index_of_second_needle = sapply(all_needle_positions, function(x) x[2])) %>%
    mutate(winning_player_name_start_needle = index_of_second_needle + 3 + 1 + 3) %>% # first +3 is for team abbrev, +1 is for space, next +3 is for number (includes a #). If number is only 1 digit, will be okay because all that will be eliminated is the otherwise trimmed extra space.
    mutate(entire_rest_of_string_needle = str_trim(substr(event_description, winning_player_name_start_needle, length(event_description))))
  faceoffs_updated$entire_rest_of_string_needle <- str_trim(str_extract(faceoffs_updated$entire_rest_of_string_needle, "^[^\\s]+"))
  faceoffs_updated = faceoffs_updated %>%
    rename(last_name_faceoff_winner = entire_rest_of_string_needle) %>%
    select(-contains("needle")) 

  faceoffs_updated = faceoffs_updated %>%
    mutate(home_names = paste(home_on_1, home_on_2, home_on_3, home_on_4, home_on_5)) %>%
    mutate(away_names = paste(away_on_1, away_on_2, away_on_3, away_on_4, away_on_5)) %>%
    rowwise() %>% # WILL NOT WORK WITHOUT THIS
    mutate(home_faceoff_win = grepl(last_name_faceoff_winner, home_names, fixed = TRUE)) %>%
    mutate(across(c("home_on_1", "home_on_2", "home_on_3", "home_on_4", "home_on_5",
                    "away_on_1", "away_on_2", "away_on_3", "away_on_4", "away_on_5", "last_name_faceoff_winner"), ~paste0(.)))
  
  print(faceoffs_updated$home_faceoff_win)

  faceoffs_updated = faceoffs_updated %>%
    mutate(faceoff_winning_player = case_when(
      event_player_1 %in% home_names & home_faceoff_win ~ event_player_1,
      event_player_1 %in% home_names & !home_faceoff_win ~ event_player_2,
      event_player_2 %in% home_names & home_faceoff_win ~ event_player_2,
      event_player_2 %in% home_names & !home_faceoff_win ~ event_player_1,
      TRUE ~ NA
    ))
  
  temp = faceoffs_updated %>%
    select(event_team, team_abbrev_faceoff_winner, last_name_faceoff_winner, event_zone, home_faceoff_win, starts_with("home_on_"), starts_with("away_on_"))
  return(faceoffs_updated)
}