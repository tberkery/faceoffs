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
  return(skaters)
}

compute_goaltending_by_team_season = function() {
  eh_goalies = read_eh_goalies()
  team_season_goaltending = eh_goalies %>%
    select(-c(Player, EH_ID, `API ID`, Position, Catches, Birthday, starts_with('Draft'), GP, TOI)) %>%
    group_by(Team, Season) %>%
    mutate(across(c(Age, `Sv%`, `FSv%`, `xFSv%`, `dFSv%`), ~weighted.mean(., na.rm = TRUE))) %>%
    mutate(across(c(GA, SA, FA, xGA, GSAA, GSAx), ~sum(., na.rm = TRUE))) %>%
    distinct(Team, Season, .keep_all = TRUE)
  
  eh_goalies_gar = read_eh_gar_goalies()
  team_season_goalie_gar = eh_goalies_gar %>%
    select(-c(Player, EH_ID, `API ID`, Position, Catches, Birthday, Age, starts_with("Draft"), GP)) %>%
    group_by(Team, Season) %>%
    mutate(across(c(TOI_EV, TOI_SH, FA_EV, FA_SH, EVD_GAR, SHD_GAR, Take_GAR, Draw_GAR, GAR, WAR, SPAR), ~sum(., na.rm = TRUE))) %>%
    distinct(Team, Season, .keep_all = TRUE)
  
  team_season_goaltending = team_season_goaltending %>%
    inner_join(team_season_goalie_gar, by = c('Team', 'Season'))
  return(team_season_goaltending)
}

connnect_skaters_to_team_goaltending = function() {
  skaters = get_player_data()
  team_season_goaltending = compute_goaltending_by_team_season()
  skaters_with_goaltending = skaters %>%
    inner_join(team_season_goaltending, by = c('Team', 'Season'))
}
