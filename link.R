library(tidyverse)

link_sznajder = function(pbp) {
  seasons = unique(pbp$season)
  pbp_linked = pbp %>%
    mutate()
}

link_pbp_and_sznajder = function(pbp, games) {
  pbp_boosted = pbp %>%
    mutate(minute_mark_pbp = substr(clock_time, 1, 2),
           second_mark_pbp = substr(clock_time, 4, 5)) %>%
    mutate(next_pbp_minute_mark = lead(minute_mark_pbp, 1),
           next_pbp_second_mark = lead(second_mark_pbp, 1))
  games_boosted = games %>%
    mutate(minute_mark_sznajder = substr(Time, 1, 2),
           second_mark_sznajder = substr(Time, 4, 5))
  pbp_with_sznajder = pbp_boosted %>%
    left_join(games_boosted, by = c('minute_mark_pbp' = 'minute_mark_sznajder', 
                            'game_period' = 'Period',
                            'home_team' = 'abbreviation_home',
                            'away_team' = 'abbreviation_away')) %>%
    group_by(minute_mark_pbp, game_period, home_team, away_team) %>%
    slice(which.min(abs(as.numeric(second_mark_pbp) - as.numeric(second_mark_sznajder))))
  pbp_with_sznajder = pbp_with_sznajder %>%
    filter(second_mark_sznajder <= second_mark_pbp)
  return(pbp_with_sznajder)
}

lookup_team = function(pbp) {
  team_lookup = read_csv('team_lookup.csv')
  eh_teams = pbp %>%
    select(home_team) %>%
    distinct(home_team) %>%
    rename(eh_team_abbrev = home_team)
  team_lookup = team_lookup %>%
    left_join(eh_teams, by = c('abbreviation' = 'eh_team_abbrev'))
  return(team_lookup)
}

