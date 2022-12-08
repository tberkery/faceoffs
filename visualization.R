source("models.R")
source("join_by_role.R")
get_player_names = function(model_projections) {
  player_data = condition()
  player_data_subset = player_data %>% select(c(event_player_1, event_player_2, faceoff_winner, faceoff_winning_team_xG_since_faceoff))
  model_projections_with_players = model_projections %>% cbind(player_data_subset)
}
proj_by_player_season = function(model_projections) {
  model_projections_by_player_season = model_projections_with_players %>%
    pivot_longer(cols = c('event_player_1', 'event_player_2'), names_to = c('center'), values_to = c('summary') )
}