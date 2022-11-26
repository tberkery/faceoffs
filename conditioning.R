library(tidyverse)
source("join_by_role.R")
source("impute.R")

# eliminating some less necessary fields

condition = function() {
  initial_data = join_by_role()
  data = condition_types(initial_data) %>%
    select(-contains('API ID'), -contains('_team'), -contains('_goaltending'),
           -contains('7'), -contains('_Name')) %>%
    rename(Sh_Percent_Win_F1 = `Sh%_Win_F1`,
           Sh_Percent_Win_F2 = `Sh%_Win_F2`,
           Sh_Percent_Win_F3 = `Sh%_Win_F3`,
           Sh_Percent_Win_D1 = `Sh%_Win_D1`,
           Sh_Percent_Win_D2 = `Sh%_Win_D1`,
           Sh_Percent_Win_G1 = `Sh%_Win_G1`
           ,
           Sh_Percent_Lose_F1 = `Sh%_Lose_F1`,
           Sh_Percent_Lose_F2 = `Sh%_Lose_F2`,
           Sh_Percent_Lose_F3 = `Sh%_Lose_F3`,
           Sh_Percent_Lose_D1 = `Sh%_Lose_D1`,
           Sh_Percent_Lose_D2 = `Sh%_Lose_D1`,
           Sh_Percent_Lose_G1 = `Sh%_Lose_G1`)
  data = impute_mice(data)
  data = address_na(data)
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
