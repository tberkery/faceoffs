library(tidymodels)
library(tidyverse)
library(finetune)
library(xgboost)
library(data.table)
library(zoo)

Full2017 <- read_csv("Full2017_updated.csv")

condition = function(big_join) {
  faceoffs_data = big_join %>%
    mutate(zone_change_time = 
             ifelse(event_type == 'ZONE_EXIT' |
                      event_type == 'ZONE_ENTRY' |
                      event_type == 'STOP', 
                    game_seconds, NA),
           #Taking out entries that occur as the faceoff happens
           zone_change_time = ifelse(
             (event_type == 'ZONE_ENTRY' | event_type == 'ZONE_EXIT') &
               lag(event_type) == 'FAC' &
               game_seconds == lag(game_seconds),
             NA,
             zone_change_time),
           end_faceoff_attribution =
             na.locf(zone_change_time, fromLast = TRUE, na.rm = F),
           zone_time = end_faceoff_attribution - game_seconds) %>%
    filter((event_type == 'FAC' & event_zone != 'Neu') & 
             #Looked like reasonable cutoff from density plot
             zone_time <= 250)
  
  same_games = Full2017 %>%
    mutate(game_date = substr(game_date, 1, 10)) %>%
    left_join(faceoffs_data,
              by = c('home_team', 'away_team', 'game_date')) %>%
    select(game_id_x) %>%
    rename(game_id = game_id_x) %>%
    distinct(game_id)
  
  faceoffs_data = faceoffs_data %>%
    arrange(game_date, game_id, game_seconds)
  
  faceoffs_data_subset = faceoffs_data %>%
    select(season, game_id, game_seconds, zone_change_time, end_faceoff_attribution, zone_time)
  faceoffs_with_xg = Full2017 %>%
    left_join(faceoffs_data_subset, by = c('season_x' = 'season', 'game_id_x' = 'game_id', 'game_seconds')) %>%
    inner_join(same_games, by = c('game_id_x' = 'game_id'))
  
  faceoffs_with_player_roles = faceoffs_with_xg %>%
    select(game_id_x, season_x, game_seconds, event_type,  
           Win_F1_Name, Win_F2_Name, Win_F3_Name, Win_D1_Name, Win_D2_Name, Win_G1_Name,
           Lose_F1_Name, Lose_F2_Name, Lose_F3_Name, Lose_D1_Name, Lose_D2_Name, Lose_G1_Name,
           Win_F1, Win_F2, Win_F3, Win_D1, Win_D2, Win_G1, Lose_F1, Lose_F2, Lose_F3, Lose_D1, Lose_D2, Lose_G1)
  
  faceoff_zone_info_subset = faceoffs_data %>%
    select(game_id, game_seconds, event_type, event_zone, zone_change_time, end_faceoff_attribution, zone_time)
  
  faceoffs_with_xg = faceoffs_with_xg %>%
    filter(event_zone != 'Neu')
  
  xg_info = big_join %>%
    #select(season, game_id, game_seconds, event_type, pred_goal) %>%
    left_join(faceoff_zone_info_subset, by = c('game_id', 'game_seconds', 'event_type')) %>%
    filter(event_type == "SHOT" | event_type == "MISS" | event_type == "GOAL" | event_type == "FAC") %>%
    mutate(last_faceoff_time_temp = ifelse(event_type == "FAC", game_seconds, NA),
           last_faceoff_team_temp = ifelse(event_type == "FAC", event_team, NA)) %>%
    mutate(last_faceoff_time = na.locf(last_faceoff_time_temp, na.rm = F),
           last_faceoff_winner = na.locf(last_faceoff_team_temp, na.rm = F)) %>%
    mutate(end_faceoff_attribution = na.locf(end_faceoff_attribution, fromLast = T, na.rm = F)) %>%
    mutate(winner_attributable_xg = ifelse(event_team == last_faceoff_winner & pred_goal > 0 & game_seconds > last_faceoff_time & game_seconds < end_faceoff_attribution, pred_goal, 0),
           loser_attributable_xg = ifelse(event_team != last_faceoff_winner & pred_goal > 0 & game_seconds > last_faceoff_time & game_seconds < end_faceoff_attribution, pred_goal, 0)) %>%
    mutate(winner_xg = lag(winner_attributable_xg),
           loser_xg = lag(loser_attributable_xg))
  
  faceoffs_full_new = faceoffs_with_player_roles %>%
    inner_join(xg_info, by = c('game_id_x' = 'game_id', 'season_x' = 'season', 'game_seconds', 'event_type'))
  return(faceoffs_full)
}

xg_info_with_role_encoded = xg_info %>%
  left_join(faceoffs_with_player_roles, by = c('game_id' = 'game_id_x', 'season' = 'season_x', 'game_seconds', 'event_type'))
# now use pred_goal to get expected goals for both teams

# ggplot(faceoffs_data, aes(x = zone_time)) +
#   geom_density() +
#   theme_bw() +
#   xlab('Zone Time (Seconds)') +
#   ylab('Frequency') +
#   ggtitle('Post-Faceoff Zone Time')

table(faceoffs_data$event_type) %>%
  filter(event_type == "FAC" & event_zone != 'Neu') %>%
  select(-zone_change_time)

xgb_spec = 
  boost_tree(
    trees = tune(),
    min_n = tune(),
    mtry = tune(),
    learn_rate = tune(),
    tree_depth = tune(),
    loss_reduction = tune(),
  ) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

set.seed(123)
faceoffs_split_xg = faceoffs_data %>%
  initial_split(strata = Petal.Width)

faceoffs_train = training(faceoffs_split_xg)
faceoffs_test = testing(faceoffs_split_xg)

hockey_folds = vfold_cv(faceoffs_train, strata = Species)

hockey_recipe = 
  recipe(
    Petal.Width ~ 
      Sepal.Length + Sepal.Width + Petal.Length,
    data = faceoffs_train
  ) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE)

xgb_spec = 
  boost_tree(
    trees = tune(),
    min_n = tune(),
    mtry = tune(),
    learn_rate = tune(),
    tree_depth = tune(),
    loss_reduction = tune(),
  ) %>%
  set_engine("xgboost") %>%
  set_mode("regression")

xgb_wf = workflow(hockey_recipe, xgb_spec)

xgb_rs = tune_race_anova(
  xgb_wf,
  hockey_folds,
  grid = 15,
  #metrics = metric_set(roc_auc),
  control = control_race(verbose_elim = TRUE)
)

xgb_last = xgb_wf %>%
  finalize_workflow(select_best(xgb_rs)) %>%
  last_fit(faceoffs_split_xg)

xgb_metrics = xgb_last %>%
  collect_metrics()

#Gets tree specifications for model
tree_specs = select_best(xgb_rs)

fitted_wf = extract_workflow(xgb_last)

preds = (predict(fitted_wf, faceoffs_data)) %>%
  select(.pred) %>%
  bind_cols(faceoffs_data)
