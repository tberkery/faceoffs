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
    inner_join(player_season_positions, by = c('home_on_1' = 'EH_ID', 'season')) %>%
    inner_join(player_season_positions, by = c('home_on_2' = 'EH_ID', 'season'), suffix = c('_home_on_1', '_home_on_2')) %>%
    inner_join(player_season_positions, by = c('home_on_3' = 'EH_ID', 'season')) %>%
    inner_join(player_season_positions, by = c('home_on_4' = 'EH_ID', 'season'), suffix = c('_home_on_3', '_home_on_4')) %>%
    inner_join(player_season_positions, by = c('home_on_5' = 'EH_ID', 'season')) %>%
    inner_join(player_season_positions, by = c('away_on_1' = 'EH_ID', 'season'), suffix = c('_home_on_5', '_away_on_1')) %>%
    inner_join(player_season_positions, by = c('away_on_2' = 'EH_ID', 'season')) %>%
    inner_join(player_season_positions, by = c('away_on_3' = 'EH_ID', 'season'), suffix = c('_away_on_2', '_away_on_3')) %>%
    inner_join(player_season_positions, by = c('away_on_4' = 'EH_ID', 'season')) %>%
    inner_join(player_season_positions, by = c('away_on_5' = 'EH_ID', 'season'), suffix = c('_away_on_4', '_away_on_5'))
  
  for_cols = c("home_on_1", "home_on_2", "home_on_3", "away_on_1", "away_on_2", "away_on_3")
  def_cols = c("home_on_4", "home_on_5", "away_on_4", "away_on_5")
  for (for_col in for_cols) {
    for (def_col in def_cols) {
      print(paste0("working on ", for_col, "/", def_col, " swap"))
      criteria = (faceoffs[[paste0("Pos_", for_col)]] == 'Defenseman' & faceoffs[[paste0("Pos_", def_col)]] == 'Forward')
      faceoffs[criteria, c(for_col, def_col)] = faceoffs[criteria, c(def_col, for_col)]
      faceoffs[criteria, c(paste0("Pos_", for_col), paste0("Pos_", def_col))] = faceoffs[criteria, c(paste0("Pos_", def_col), paste0("Pos_", for_col))]
    }
  }
}