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
  
  statuses = c("home", "away")
  for_cols = c("_on_1", "_on_2", "_on_3", "_on_1", "_on_2", "_on_3")
  def_cols = c("_on_4", "_on_5", "_on_4", "_on_5")
  for (status in statuses) {
    for (for_col in for_cols) {
      for (def_col in def_cols) {
        print(paste0("working on ", status, for_col, "/", status, def_col, " swap"))
        criteria = (faceoffs[[paste0("Pos_", status, for_col)]] == 'Defenseman' & faceoffs[[paste0("Pos_", status, def_col)]] == 'Forward')
        faceoffs[criteria, c(paste0(status, for_col), paste0(status,def_col))] = faceoffs[criteria, c(paste0(status, def_col), paste0(status, for_col))]
        faceoffs[criteria, c(paste0("Pos_", status, for_col), paste0("Pos_", status, def_col))] = faceoffs[criteria, c(paste0("Pos_", status, def_col), paste0("Pos_", status, for_col))]
      }
    }
  }
}