
read_EH_master = function() {
  EH_master = read_csv("nhl_player_seasons.csv")
  EH_master = EH_master %>%
    mutate(season = as.numeric(paste0("20", substr(Season, 1, 2))))
  return(EH_master)
}

read_conditioned_pbp = function(start_year, end_year) {
  pbp_conditioned = read_csv(paste0("Full", start_year, ".csv"))
  for (year in (start_year + 1):end_year) {
    yearly_pbp = read_csv(paste0("Full", year, ".csv"))
    rbind(pbp_conditioned, yearly_pbp)
  }
  pbp_conditioned = pbp_conditioned %>%
    mutate(season = as.numeric(substr(season_x, 1, 4)),
           prior_season = season - 1)
  return(pbp_conditioned)
}

join_by_role = function() {
  EH_master = read_EH_master()
  pbp_conditioned = read_conditioned_pbp(2018, 2019)
  pbp_with_context = pbp_conditioned %>%
    left_join(EH_master, by = c('Win_F1_Name' = 'EH_ID', 'prior_season' = 'season'), suffix = c('', '_Win_F1'))
  pbp_with_context = pbp_with_context %>%
    left_join(EH_master, by = c('Win_F2_Name' = 'EH_ID', 'prior_season' = 'season'), suffix = c('', '_Win_F2'))
  pbp_with_context = pbp_with_context %>%
    left_join(EH_master, by = c('Win_F3_Name' = 'EH_ID', 'prior_season' = 'season'), suffix = c('', '_Win_F3'))
  pbp_with_context = pbp_with_context %>%
    left_join(EH_master, by = c('Win_D1_Name' = 'EH_ID', 'prior_season' = 'season'), suffix = c('', '_Win_D1'))
  pbp_with_context = pbp_with_context %>%
    left_join(EH_master, by = c('Win_D2_Name' = 'EH_ID', 'prior_season' = 'season'), suffix = c('', '_Win_D2'))
}