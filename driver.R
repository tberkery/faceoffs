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
  
  skaters = eh_box %>% inner_join(eh_rel, by = c('EH_ID', 'Season'), suffix = c('', '_relative'))
  
  eh_on_ice = read_eh_on_ice() %>% select(-c(Player, `API ID`, Team, Position, Shoots, Birthday, Age, `Draft Yr`, `Draft Rd`, `Draft Ov`, GP, TOI))
  
  skaters = skaters %>% inner_join(eh_on_ice, by = c('EH_ID', 'Season'), suffix = c('', '_on_ice'))
  
  eh_zones = read_eh_zones() %>% select(-c(Player, `API ID`, Team, Position, Shoots, Birthday, Age, `Draft Yr`, `Draft Rd`, `Draft Ov`, GP, TOI))
  
  skaters = skaters %>% inner_join(eh_zones, by = c('EH_ID', 'Season'), suffix = c('', '_zones'))
  
  eh_gar_skaters = read_eh_gar_skaters() %>% select(-c(Player, `API ID`, Team, Position, Shoots, Birthday, Age, `Draft Yr`, `Draft Rd`, `Draft Ov`, GP))
  
  skaters = skaters %>% inner_join(eh_gar_skaters, by = c('EH_ID', 'Season'), suffix = c('', '_gar'))
  
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
    inner_join(team_season_goalie_gar, by = c('Team', 'Season'), suffix = c('', '_gar'))
  return(team_season_goaltending)
}

connect_skaters_to_team_goaltending = function() {
  skaters = get_player_data()
  team_season_goaltending = compute_goaltending_by_team_season()
  skaters_with_goaltending = skaters %>%
    inner_join(team_season_goaltending, by = c('Team', 'Season'), suffix = c("", "_goaltending"))
  return(skaters_with_goaltending)
}

compute_team_performance_by_season = function() {
  eh_teams = read_eh_team()
  eh_teams_conditioned = eh_teams %>%
    select(-c(Name, GP, TOI))
  
  eh_team_standings = read_eh_team_standings()
  eh_team_standings_conditioned = eh_team_standings %>%
    select(-c(Name, GP, TOI, ROW))
  
  eh_team_summary = eh_teams_conditioned %>%
    inner_join(eh_team_standings_conditioned, by = c('Team', 'Season'), suffix = c('_adjusted', '')) 
  # eh_teams contains adjusted data, eh_team_standings contains unadjusted data. Therefore, if same field in both but has different values in each, it's due to adjustment.
  
  return(eh_team_summary)
}

connect_skaters_and_goaltending_to_team_performance = function() {
  skaters_with_goaltending = connect_skaters_to_team_goaltending()
  eh_team_summary = compute_team_performance_by_season()
  skaters_with_goaltending_and_teams = skaters_with_goaltending %>%
    inner_join(eh_team_summary, by = c('Team', 'Season'), suffix = c('', '_team'))
  skaters_with_goaltending_and_teams = skaters_with_goaltending_and_teams %>%
    select(-contains("Birthday"), -contains("FOA±"))
  skaters_with_goaltending_and_teams = skaters_with_goaltending_and_teams %>%
    rename_cols()
  skaters_with_goaltending_and_teams = skaters_with_goaltending_and_teams %>%
    mutate(Draft_Yr = replace_na(Draft_Yr, -1)) %>%
    mutate(Draft_Rd = replace_na(Draft_Rd, -1)) %>%
    mutate(Draft_Ov = replace_na(Draft_Ov, -1))
  skaters_with_goaltending_and_teams = skaters_with_goaltending_and_teams %>%
    scale_2020_volume_stats
  skaters_with_goaltending_and_teams %>% na.omit() %>% write_csv("nhl_player_seasons.csv")
  return(skaters_with_goaltending_and_teams)
}

rename_cols = function(skaters_with_goaltending_and_teams) {
  skaters_with_goaltending_and_teams = skaters_with_goaltending_and_teams %>%
    rename(Draft_Yr = `Draft Yr`,
           Draft_Rd = `Draft Rd`,
           Draft_Ov = `Draft Ov`,
           FSh_Percent = `FSh%`,
           xFSh_Percent = `xFSh%`,
           iPEN_Plus_Minus = `iPEN±`,
           RelTM_G_Plus_Minus_per_60 = `RelTM G±/60`,
           RelTM_S_Plus_Minus_per_60 = `RelTM S±/60`,
           RelTM_F_Plus_Minus_per_60 = `RelTM F±/60`,
           RelTM_C_Plus_Minus_per_60 = `RelTM C±/60`,
           RelTM_xG_Plus_Minus_per_60 = `RelTM xG±/60`,
           RelTM_GF_per_60 = `RelTM GF/60`,
           RelTM_GA_per_60 = `RelTM GA/60`,
           RelTM_SF_per_60 = `RelTM SF/60`,
           RelTM_SA_per_60 = `RelTM SA/60`,
           RelTM_FF_per_60 = `RelTM FF/60`,
           RelTM_FA_per_60 = `RelTM FA/60`,
           RelTM_CF_per_60 = `RelTM CF/60`,
           RelTM_CA_per_60 = `RelTM CA/60`,
           RelTM_xGF_per_60 = `RelTM xGF/60`,
           RelTM_xGA_per_60 = `RelTM xGA/60`,
           GF_Percent_on_ice = `GF%`,
           SF_Percent_on_ice = `SF%`,
           FF_Percent_on_ice = `FF%`,
           CF_Percent_on_ice = `CF%`,
           xGF_Percent_on_ice = `xGF%`,
           GF_per_60_on_ice = `GF/60`,
           GA_per_60_on_ice = `GA/60`,
           SF_per_60_on_ice = `SF/60`,
           SA_per_60_on_ice = `SA/60`,
           FF_per_60_on_ice = `FF/60`,
           FA_per_60_on_ice = `FA/60`,
           CF_per_60_on_ice = `CF/60`,
           CA_per_60_on_ice = `CA/60`,
           xGF_per_60_on_ice = `xGF/60`,
           xGA_per_60_on_ice = `xGA/60`,
           G_Plus_Minus_per_60_on_ice = `G±/60`,
           S_Plus_Minus_per_60_on_ice = `S±/60`,
           F_Plus_Minus_per_60_on_ice = `F±/60`,
           C_Plus_Minus_per_60_on_ice = `C±/60`,
           xG_Plus_Minus_per_60_on_ice = `xG±/60`,
           Sh_Percent_on_ice = `Sh%_on_ice`,
           Sv_Percent_on_ice = `Sv%`,
           TOI_per_GP = `TOI/GP`,
           TOI_Percent = `TOI%`,
           OZS_Percent = `OZS%`,
           NZS_Percent = `NZS%`,
           DZS_Percent = `DZS%`,
           OTF_Percent = `OTF%`,
           OZF_Percent = `OZF%`,
           NZF_Percent = `NZF%`,
           DZF_Percent = `DZF%`,
           Ice_Percent = `Ice%`,
           Sv_Percent_goaltending = `Sv%_goaltending`,
           FSv_Percent = `FSv%`,
           xFSv_Percent = `xFSv%`,
           dFSv_Percent = `dFSv%`,
           xGF_Percent_team = `xGF%_team`,
           GF_per_60_team = `GF/60_team`,
           GA_per_60_team = `GA/60_team`,
           SF_per_60_team = `SF/60_team`,
           SA_per_60_team = `SA/60_team`,
           FF_per_60_team = `FF/60_team`,
           FA_per_60_team = `FA/60_team`,
           CF_per_60_team = `CF/60_team`,
           CA_per_60_team = `CA/60_team`,
           xGF_per_60_team = `xGF/60_team`,
           xGA_per_60_team = `xGA/60_team`,
           G_Plus_Minus_per_60_team = `G±/60_team`,
           S_Plus_Minus_per_60_team = `S±/60_team`,
           F_Plus_Minus_per_60_team = `F±/60_team`,
           C_Plus_Minus_per_60_team = `C±/60_team`,
           xG_Plus_Minus_per_60_team = `xG±/60_team`,
           Sh_Percent_Adjusted_team = `Sh%_adjusted`,
           Sv_Percent_Adjusted_team = `Sv%_adjusted`,
           W_team = W,
           L_team = L,
           OL_team = OL,
           Points_Percent_team = `Points%`,
           GF_team = GF,
           G_Plus_Minus_team = `G±`,
           Sh_Percent_team = `Sh%_team`,
           Sv_Percent_team = `Sv%_team`
    )
  return(skaters_with_goaltending_and_teams)
}

scale_2020_volume_stats = function(skaters_with_goaltending_and_teams) {
  skaters_with_goaltending_and_teams = skaters_with_goaltending_and_teams %>%
    mutate(across(G:FOW, ~ifelse(Season == '20-21', .*82/56, .))) %>%
    mutate(across(Ice_F:SPAR, ~ifelse(Season == '20-21', .*82/56, .))) %>%
    mutate(across(GA:xGA, ~ifelse(Season == '20-21', .*82/56, .))) %>%
    mutate(across(GSAA:SPAR_goaltending, ~ifelse(Season == '20-21', .*82/56, .))) %>%
    mutate(across(W_team:Points_team, ~ifelse(Season == '20-21', .*82/56, .))) %>%
    mutate(across(GF_team:G_Plus_Minus_team, ~ifelse(Season == '20-21', .*82/56, .)))
  return(skaters_with_goaltending_and_teams)
}

