library(tidyverse)
source("join_by_role.R")

condition = function() {
  initial_data = join_by_role()
  data = condition_types(initial_data)
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
              starts_with('Win_Goalie'), starts_with('Lose_Goalie'), ends_with('_Name'),
              contains('F4'), contains('F5'), contains('F6'), contains('F7'),
              contains('D3'), contains('D4'), contains('D5'), contains('D6'),
              contains('D7'), contains('G2'), contains('G3'), contains('G4'), 
              contains('G5'), contains('G6'), contains('G7'), contains('7'),
              starts_with('prior_season'), starts_with('Player'), starts_with('API_ID'),
              starts_with('API ID')))
  print(nrow(data_conditioned))
  data_filtered = data_conditioned %>%
    filter(!is.na(Win_F1) & !is.na(Win_F2) & !is.na(Win_F3) & !is.na(Win_D1)& !is.na(Win_D2)& !is.na(Win_G1)&
             !is.na(Lose_F1) & !is.na(Lose_F2)& !is.na(Lose_F3)& !is.na(Lose_D1)& !is.na(Lose_D2)& !is.na(Lose_G1))
  print(nrow(data_filtered))
  return(data_conditioned)
}
