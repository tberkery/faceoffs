source("models.R")
source("join_by_role.R")
source("driver.R")
get_player_names = function(model_projections) {
  model_projections = read_csv('model_projections.csv')
  #player_data = condition()
  player_data = read_csv('data_imputed_12_to_18.csv')
  player_data_subset = player_data %>% select(c(eh_season, event_player_1, event_player_2, faceoff_winner, faceoff_winning_team_xG_since_faceoff))
  model_projections_with_players = model_projections %>% cbind(player_data_subset)
}
proj_by_player_season = function(model_projections) {
  model_projections_with_players = get_player_names()
  model_projections_with_players = model_projections_with_players %>%
    pivot_longer(cols = c('event_player_1', 'event_player_2'), names_to = c('center'), values_to = c('summary') )
  players_info = get_player_data() %>%
    select(Player, EH_ID, Season, Team, Age) %>%
    distinct(Player, EH_ID, Season, .keep_all = TRUE)
  model_projections_with_players = model_projections_with_players %>%
    inner_join(players_info, by = c('summary' = 'EH_ID', 'eh_season' = 'Season'))
  model_projections_with_players_summary = model_projections_with_players %>%
    rename(perf = faceoff_winning_team_xG_since_faceoff) %>%
    group_by(summary, eh_season) %>%
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
    ) %>%
    distinct(summary, eh_season, .keep_all = TRUE)
  model_projections_with_players_summary %>% write_csv("player_projections_overall.csv")
  
  model_projections_with_players_by_zone = model_projections_with_players %>%
    rename(perf = faceoff_winning_team_xG_since_faceoff) %>%
    mutate(zone = ifelse(event_zone_Def == 1, 'Def', ifelse(event_zone_Off == 1, 'Off', 'Neu'))) %>%
    group_by(summary, eh_season, zone) %>%
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
    ) %>%
    distinct(summary, eh_season, zone, .keep_all = TRUE)
  model_projections_with_players_by_zone %>% write_csv("player_projections_by_zone.csv")
    
}