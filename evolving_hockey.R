library(tidyverse)

read_eh_relative = function() {
  eh_rel = read_csv("EH_skaters_relative_5v5_adjusted.csv")
  return(eh_rel)
}

read_eh_box = function() {
  eh_box = read_csv("EH_skaters_box.csv")
  return(eh_box)
}

read_eh_on_ice = function() {
  eh_on_ice = read_csv("EH_skaters_on_ice_5v5_adjusted.csv")
  return(eh_on_ice)
}

read_eh_zones = function() {
  eh_zones = read_csv("EH_skaters_zones_5v5.csv")
  return(eh_zones)
}

read_eh_gar_skaters = function() {
  eh_gar_skaters = read_csv("EH_skaters_gar.csv")
  return(eh_gar_skaters)
}

read_eh_gar_goalies = function() {
  eh_gar_goalies = read_csv("EH_goalies_gar.csv")
  return(eh_gar_goalies)
}

read_eh_goalies = function() {
  eh_goalies = read_csv("EH_goalies.csv")
  return(eh_goalies)
}

read_eh_team = function() {
  eh_teams = read_csv("EH_teams_adjusted.csv")
  return(eh_teams)
}

read_eh_team_standings = function() {
  eh_standings = read_csv("EH_teams_standings.csv")
  return(eh_standings)
}