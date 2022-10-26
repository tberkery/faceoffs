source("evolving_hockey.R")

get_data = function() {
  eh_rel = read_eh_relative()
  eh_box = read_eh_box()
  eh_on_ice = read_eh_on_ice()
  eh_zones = read_eh_zones()
  eh_gar_skaters = read_eh_gar_skaters()
  eh_gar_goalies = read_eh_gar_goalies()
  eh_goalies = read_eh_goalies()
  eh_teams = read_eh_team()
  eh_team_standings = read_eh_team_standings()
}

get_player_data = function() {
  eh_box = read_eh_box()
  eh_rel = read_eh_relative() %>% select(-c(Player, `API ID`, Team, Position, Shoots, Birthday, Age, `Draft Yr`, `Draft Rd`, `Draft Ov`, GP, TOI))
  
  skaters = eh_box %>% inner_join(eh_rel, by = c('EH_ID', 'Season'))
  
  eh_on_ice = read_eh_on_ice() %>% select(-c(Player, `API ID`, Team, Position, Shoots, Birthday, Age, `Draft Yr`, `Draft Rd`, `Draft Ov`, GP, TOI))
  
  skaters = skaters %>% inner_join(eh_on_ice, by = c('EH_ID', 'Season'))
  
  eh_zones = read_eh_zones() %>% select(-c(Player, `API ID`, Team, Position, Shoots, Birthday, Age, `Draft Yr`, `Draft Rd`, `Draft Ov`, GP, TOI))
  
  skaters = skaters %>% inner_join(eh_zones, by = c('EH_ID', 'Season'))
}
