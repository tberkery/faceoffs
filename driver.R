source("evolving_hockey.R")

get_data = function() {
  eh_rel = read_eh_relative()
  eh_box = eh_box()
  eh_on_ice = read_eh_on_ice()
  eh_zones = read_eh_zones()
  eh_gar_skaters = read_eh_gar_skaters()
  eh_gar_goalies = read_eh_gar_goalies()
  eh_goalies = read_eh_goalies()
  eh_teams = read_eh_team()
  eh_team_standings = read_eh_team_standings()
}