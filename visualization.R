source("models.R")
source("join_by_role.R")
source("driver.R")
get_player_names = function(model_projections) {
  model_projections = read_csv('model_projections.csv')
  #player_data = condition()
  player_data = read_csv('data_imputed_12_to_18.csv')
  player_data_subset = player_data %>% select(c(event_player_1, event_player_2, faceoff_winner, faceoff_winning_team_xG_since_faceoff))
  model_projections_with_players = model_projections %>% cbind(player_data_subset)
}
proj_by_player_season = function(model_projections) {
  model_projections_with_players = get_player_names()
  model_projections_with_players = model_projections_with_players %>%
    pivot_longer(cols = c('event_player_1', 'event_player_2'), names_to = c('center'), values_to = c('summary') )
  players_info = get_player_data() %>%
    select(Player, EH_ID, Season, Team, Age)
  model_projections_with_players = model_projections_with_players %>%
    inner_join(players_info, by = c('summary' = 'EH_ID'))
  model_projections_with_players = model_projections_with_players %>%
    rename(perf = faceoff_winning_team_xG_since_faceoff) %>%
    group_by(summary, Season) %>%
    mutate(avg_proj = mean(proj, na.rm = TRUE),
           med_proj = median(proj, na.rm = TRUE),
           sd_proj = sd(proj, na.rm = TRUE),
           q1_proj = quantile(proj, 0.25, na.rm = TRUE),
           q3_proj = quantile(proj, 0.75, na.rm = TRUE),
           sum_proj = sum(proj, na.rm = TRUE),
           count_proj = n()
    ) %>%
    mutate(avg_perf = mean(perf, na.rm = TRUE),
           med_perf = median(perf, na.rm = TRUE),
           sd_perf = sd(perf, na.rm = TRUE),
           q1_perf = quantile(perf, 0.25, na.rm = TRUE),
           q3_perf = quantile(perf, 0.75, na.rm = TRUE),
           sum_perf = sum(perf, na.rm = TRUE),
           count_perf = n()
    )
    
}