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
    select(-Position, -Season, -season) %>%
    group_by(EH_ID) %>%
    distinct(EH_ID, .keep_all = TRUE)
  faceoffs = big_join %>%
    filter(event_type == 'FAC') %>% 
    left_join(player_season_positions, by = c('home_on_1' = 'EH_ID')) %>%
    left_join(player_season_positions, by = c('home_on_2' = 'EH_ID'), suffix = c('_home_on_1', '_home_on_2')) %>%
    left_join(player_season_positions, by = c('home_on_3' = 'EH_ID')) %>%
    left_join(player_season_positions, by = c('home_on_4' = 'EH_ID'), suffix = c('_home_on_3', '_home_on_4')) %>%
    left_join(player_season_positions, by = c('home_on_5' = 'EH_ID')) %>%
    left_join(player_season_positions, by = c('home_on_6' = 'EH_ID'), suffix = c('_home_on_5', '_home_on_6')) %>%
    left_join(player_season_positions, by = c('away_on_1' = 'EH_ID')) %>%
    left_join(player_season_positions, by = c('away_on_2' = 'EH_ID'), suffix = c('_away_on_1', '_away_on_2')) %>%
    left_join(player_season_positions, by = c('away_on_3' = 'EH_ID')) %>%
    left_join(player_season_positions, by = c('away_on_4' = 'EH_ID'), suffix = c('_away_on_3', '_away_on_4')) %>%
    left_join(player_season_positions, by = c('away_on_5' = 'EH_ID')) %>%
    left_join(player_season_positions, by = c('away_on_4' = 'EH_ID'), suffix = c('_away_on_5', '_away_on_6'))
  
  faceoffs = faceoffs %>%
    filter_faceoffs()
  
  # This code swaps (if needed) the goalie to the _on_6 column (for home and away). 
  # Only guaranteed and designed to work for 5v5 situations
  statuses = c("home", "away")
  all_cols = c("_on_1", "_on_2", "_on_3", "_on_4", "_on_5")
  for (status in statuses) {
    for (col in all_cols) {
      print(paste0("working on ", status, col))
      goalie_type = paste0(status, "_goalie")
      criteria = (!is.na(faceoffs[[paste0(status, col)]]) &
                    !is.na(faceoffs[[paste0(goalie_type)]]) &
                    faceoffs[[paste0(status, col)]] == faceoffs[[paste0(goalie_type)]])
      faceoffs[criteria, c(paste0(status, col), paste0(status, "_on_6"))] = faceoffs[criteria, c(paste0(status, "_on_6"), paste0(status, col))]
      faceoffs[criteria, c(paste0("Pos_", status, col), paste0("Pos_", status, "_on_6"))] = faceoffs[criteria, c(paste0("Pos_", status, "_on_6"), paste0("Pos_", status, col))]
        
    }
  }
  
  # The following line seems redundant to me, but encoding will not work properly without it. Re-joining is essential!
  faceoffs = faceoffs %>%
    filter(event_type == 'FAC') %>% 
    select(-starts_with("Pos_")) %>%
    left_join(player_season_positions, by = c('home_on_1' = 'EH_ID')) %>%
    left_join(player_season_positions, by = c('home_on_2' = 'EH_ID'), suffix = c('_home_on_1', '_home_on_2')) %>%
    left_join(player_season_positions, by = c('home_on_3' = 'EH_ID')) %>%
    left_join(player_season_positions, by = c('home_on_4' = 'EH_ID'), suffix = c('_home_on_3', '_home_on_4')) %>%
    left_join(player_season_positions, by = c('home_on_5' = 'EH_ID')) %>%
    left_join(player_season_positions, by = c('home_on_6' = 'EH_ID'), suffix = c('_home_on_5', '_home_on_6')) %>%
    left_join(player_season_positions, by = c('away_on_1' = 'EH_ID')) %>%
    left_join(player_season_positions, by = c('away_on_2' = 'EH_ID'), suffix = c('_away_on_1', '_away_on_2')) %>%
    left_join(player_season_positions, by = c('away_on_3' = 'EH_ID')) %>%
    left_join(player_season_positions, by = c('away_on_4' = 'EH_ID'), suffix = c('_away_on_3', '_away_on_4')) %>%
    left_join(player_season_positions, by = c('away_on_5' = 'EH_ID')) %>%
    left_join(player_season_positions, by = c('away_on_4' = 'EH_ID'), suffix = c('_away_on_5', '_away_on_6'))
  
  # This code swaps (if needed) the defensemen to the _on_4 and _on_5 columns and the forwards to the _on_1, _on_2, and _on_3 positions (for home and away)
  # Only guaranteed and designed to work for 5v5 situations
  statuses = c("home", "away")
  for_cols = c("_on_1", "_on_2", "_on_3")
  def_cols = c("_on_4", "_on_5")
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
  temp = faceoffs %>% select(contains('_on_') | contains('Pos')) %>% head(1000)
  temp2 = faceoffs %>% select(contains('_on_') | contains('Pos')) %>% filter(away_on_3 == "Defensemen")
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

  temp = faceoffs_updated %>%
    select(event_team, team_abbrev_faceoff_winner, last_name_faceoff_winner, event_zone, home_faceoff_win, starts_with("home_on_"), starts_with("away_on_"))
  return(faceoffs_updated)
}

filter_faceoffs = function(data) {
  data %>%
    filter(home_skaters == 5 & away_skaters == 5) %>%
    select(-home_skaters, -away_skaters, -contains('7'), -pbp_distance)
}

encode_team_faceoff_status = function(faceoffs_updated) {
  roles = c("Win", "Lose")
  positions = c("F1", "F2", "F3", "D1", "D2")
  faceoffs_encoded = faceoffs_updated
  for (role in roles) {
    for (pos in positions) {
      new_name = paste0(role, "_", pos, "_Name")
      status = ifelse(role == "Win", "home", "away") # for now, temporarily assuming winning team is home team, losing team is away
      num = ifelse(pos == "F1", 1, ifelse(pos == "F2", 2, ifelse(pos == "F3", 3, ifelse(pos == "D1", 4, ifelse(pos == "D2", 5, NA)))))
      old_name = paste0(status, "_on_", num)
      faceoffs_encoded = faceoffs_encoded %>%
        rename(!!{new_name} := !!{old_name})
    }
  }
  
  roles = c("Win", "Loss")
  pos_cols = c("F1", "F2", "F3", "D1", "D2")
  for (pos_col in pos_cols) {
    criteria = (!is.na(faceoffs_encoded[["home_faceoff_win"]]) &
                  faceoffs_encoded[["home_faceoff_win"]] == FALSE)
    faceoffs_encoded[criteria, c(paste0("Win_", pos_col, "_Name"), paste0("Lose_", pos_col, "_Name"))] = faceoffs_encoded[criteria, c(paste0("Lose_", pos_col, "_Name"), paste0("Win_", pos_col, "_Name"))]
    faceoffs_encoded[criteria, c("home_on_6", "away_on_6")] = faceoffs_encoded[criteria, c("away_on_6", "home_on_6")] # handles goalies
  }
  faceoffs_encoded = faceoffs_encoded %>%
    rename(Win_G_Name = home_on_6,
           Lose_G_Name = away_on_6)
  
  # Note previous line (marked "handled goalies") puts goalie of (faceoff) winning team in home_on_6 and goalie of losing team in away_on_6
  return(faceoffs_encoded)
  
  
}