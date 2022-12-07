library(tidyverse)
source("join_by_role.R")
source("impute.R")
source("driver.R")

# eliminating some less necessary fields

condition = function() {
  initial_data = join_by_role()
  data = condition_types(initial_data) %>%
    select(-contains('API ID'), -(contains('_team') & !contains('faceoff')), -contains('_goaltending'),
           -contains('7'), -contains('_Name')) %>%
    rename(Sh_Percent_Win_F1 = `Sh%_Win_F1`,
           Sh_Percent_Win_F2 = `Sh%_Win_F2`,
           Sh_Percent_Win_F3 = `Sh%_Win_F3`,
           Sh_Percent_Win_D1 = `Sh%_Win_D1`,
           Sh_Percent_Win_D2 = `Sh%_Win_D1`,
           Sh_Percent_Win_G1 = `Sh%_Win_G1`,
           Sh_Percent_Lose_F1 = `Sh%_Lose_F1`,
           Sh_Percent_Lose_F2 = `Sh%_Lose_F2`,
           Sh_Percent_Lose_F3 = `Sh%_Lose_F3`,
           Sh_Percent_Lose_D1 = `Sh%_Lose_D1`,
           Sh_Percent_Lose_D2 = `Sh%_Lose_D1`,
           Sh_Percent_Lose_G1 = `Sh%_Lose_G1`)
  
  eh_team_summary = compute_team_performance_by_season()
  data_with_teams = data %>%
    inner_join(eh_team_summary, by = c('Season', 'faceoff_winner' = 'Team')) %>%
    inner_join(eh_team_summary, by = c('Season', 'faceoff_loser' = 'Team'), suffix = c('_winner', '_loser'))
  data = data %>% remove_cols() %>% condition_cols()
  data_imputed = impute_10th_percentile(data)
  data_imputed = data_imputed %>% drop_na()
  data_imputed = data_imputed %>% select(-c(season, prior_season))
  data_imputed = data_imputed[ , which(apply(data_imputed, 2, var) != 0)]
  non_numeric_data_cols = colnames(data_imputed %>% select(!where(is.numeric)))
  data_imputed = data_imputed %>% select(all_of(non_numeric_data_cols), all_of(as.vector(setdiff(colnames(data_imputed), non_numeric_data_cols)))) 
  return(data_imputed)

}

remove_cols = function(data) {
  data = data %>% select(-c(
    starts_with('Win_Goalie'),
    starts_with('Lose_Goalie'),
    starts_with('Win_F4'),
    starts_with('Win_F5'),
    starts_with('Win_F6'),
    starts_with('Win_D3'),
    starts_with('Win_D4'),
    starts_with('Win_D5'),
    starts_with('Win_D6'),
    starts_with('Win_G1'),
    starts_with('Win_G2'),
    starts_with('Win_G3'),
    starts_with('Win_G4'),
    starts_with('Win_G5'),
    starts_with('Win_G6'),
    starts_with('Lose_F4'),
    starts_with('Lose_F5'),
    starts_with('Lose_F6'),
    starts_with('Lose_D3'),
    starts_with('Lose_D4'),
    starts_with('Lose_D5'),
    starts_with('Lose_D6'),
    starts_with('Lose_G1'),
    starts_with('Lose_G2'),
    starts_with('Lose_G3'),
    starts_with('Lose_G4'),
    starts_with('Lose_G5'),
    starts_with('Lose_G6'),
    starts_with('API_ID_'),
    starts_with('Season_'),
    starts_with('Team_'),
    starts_with('Draft_Yr_'),
    starts_with('Draft_Rd_'),
    starts_with('GA_') & !contains('on_ice'),
    starts_with('SA_') & !contains('on_ice'),
    starts_with('FA_') & !contains('on_ice'),
    starts_with('xGA_') & !contains('on_ice'),
    starts_with('FSv_Percent_'),
    starts_with('xFSv_Percent_'),
    starts_with('dFSv_Percent_'),
    starts_with('GSAA_'),
    starts_with('GSAx_'),
    starts_with('Player'),
    starts_with('Position_'),
    starts_with('Shoots_'),
    contains('G1')
  ))
  return(data)
}

condition_cols = function(data) {
  data = data %>%
    rename(eh_season = Season)
  return(data)
}

condition_types = function(data) {
  data_conditioned = data %>%
    mutate(
      across(starts_with('Position'), ~as.factor(.))
    )
  data_conditioned = data_conditioned %>%
    mutate(
      across(starts_with('Shoots_'), ~as.factor(.))
    )
  return(data_conditioned)
}

# List of columns to omit model: starts_with('Player'), contains('API ID'), starts_with('Season_'), starts_with('Draft_Yr'), starts_with('Draft_Rd'), starts_with('WAR')
# starts_with('SPAR'), contains('GAR')

address_na = function(data) {
  data_conditioned = data %>%
    select(-starts_with('Player'), -contains('API ID'), -starts_with('Season_'), 
           -starts_with('Draft_Yr'), -starts_with('Draft_Rd'), -starts_with('WAR'),
           -starts_with('SPAR'), -contains('GAR')) %>%
    select(-c(game_date, home_team, away_team, event_type, event_team, event_player_1,
              event_player_2, starts_with('home_on'), home_goalie, starts_with('away_on'), 
              faceoff_winner, home_team_FA_xG, away_team_FA_xG,
              starts_with('winning_team_on'), starts_with('losing_team_on'),
              starts_with('Win_Goalie'), starts_with('Lose_Goalie'),
              contains('F4'), contains('F5'), contains('F6'), contains('F7'),
              contains('D3'), contains('D4'), contains('D5'), contains('D6'),
              contains('D7'), contains('G2'), contains('G3'), contains('G4'), 
              contains('G5'), contains('G6'), contains('G7'), contains('7'),
              starts_with('prior_season'), starts_with('Player'), starts_with('API_ID'),
              starts_with('API ID')))
  print(nrow(data_conditioned))
  data_filtered = data_conditioned %>%
    filter(!is.na(Team_Win_F1) & !is.na(Team_Win_F2) & !is.na(Team_Win_F3) & !is.na(Team_Win_D1)& !is.na(Team_Win_D2)& !is.na(Team_Win_G1)&
             !is.na(Team_Lose_F1) & !is.na(Team_Lose_F2)& !is.na(Team_Lose_F3)& !is.na(Team_Lose_D1)& !is.na(Team_Lose_D2)& !is.na(Team_Lose_G1))
  print(nrow(data_filtered))
  return(data_conditioned)
}

view_sources_of_NA = function(data) {
  subset = data %>% filter(season == year)
  na_players = unique(data$Win_F1_Name)
  na_players = c(na_players, unique(data$Win_F2_Name))
  na_players = c(na_players, unique(data$Win_F3_Name))
  na_players = c(na_players, unique(data$Win_D1_Name))
  na_players = c(na_players, unique(data$Win_D2_Name))
  na_players = unique(na_players)
  print(na_players)
  print(length(na_players))
}
