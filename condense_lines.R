library(tidyverse)
condense_to_line_matchups = function(data_all) {
  mut_cols = colnames(data_all %>% select(74:829) %>% select(where(is.numeric)))
  line_matchups = data_all %>%
    distinct(.keep_all = TRUE) %>%
    rowwise() %>%
    mutate(Win_Players = str_c(sort(c(Win_F1_Name, Win_F2_Name, Win_F3_Name, Win_D1_Name, Win_D2_Name, Win_G_Name)), collapse = ", ")) %>%
    mutate(Lose_Players = str_c(sort(c(Lose_F1_Name, Lose_F2_Name, Lose_F3_Name, Lose_D1_Name, Lose_D2_Name, Lose_G_Name)), collapse = ", ")) %>%
    ungroup() %>%
    select(-ends_with("_Name")) %>%
    select(-c(last_faceoff_winner, event_team, event_zone, home_team, away_team, home_zone,
              game_id, event_index, game_period, game_seconds, home_score, away_score, face_index, pen_index, shift_index, pbp)) %>%
    group_by(season, faceoff_type, Win_Players, Lose_Players) %>%
    mutate(count = n(),
           winner_xg = mean(winner_xg, na.rm = TRUE), 
           loser_xg = mean(loser_xg, na.rm = TRUE),
           net_xg = mean(net_xg, na.rm = TRUE)) %>%
    distinct(.keep_all = TRUE) %>%
    arrange(desc(count))
  
  line_matchups = line_matchups %>% select(season, faceoff_type, Win_Players, Lose_Players, net_xg, count, all_of(setdiff(colnames(line_matchups), c('season', 'faceoff_type', 'Win_Players', 'Lose_Players', 'net_xg', 'count'))))
  return(line_matchups)
}