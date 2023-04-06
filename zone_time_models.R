library(tidyverse)
library(tidymodels)
library(tidyverse)
library(finetune)
library(xgboost)
library(data.table)
library(zoo)

faceoffs = read_csv("updated.csv") #read_csv("faceoffs_with_zone_time_attribution.csv")
faceoffs_subset = faceoffs %>%
  select(home_team, away_team, event_team, event_zone, faceoff_winner, event_player_1, event_player_2,
         Win_F1, Win_F2, Win_F3, Win_D1, Win_D2, Win_G1, Lose_F1, Lose_F2, Lose_F3, Lose_D1, Lose_D2, Lose_G1, zone_time) %>%
  mutate(faceoff_winning_center = ifelse(faceoff_winner == home_team, event_player_2, event_player_1)) %>%
  mutate(faceoff_winner_zone = ifelse(faceoff_winner == event_team, event_zone, ifelse(event_zone == "Off", "Def", "Off"))) %>%
  filter(faceoff_winner_zone == 'Off') %>%
  select(-home_team, -away_team, -event_team, -event_zone, -faceoff_winner_zone, -event_player_1, -event_player_2, -faceoff_winner, -faceoff_winning_center)

faceoffs_subset <- replace(faceoffs_subset, is.na(faceoffs_subset), 0)

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
faceoffs_split_xg = faceoffs_subset %>%
  initial_split()

faceoffs_train = training(faceoffs_split_xg)
faceoffs_test = testing(faceoffs_split_xg)

hockey_folds = vfold_cv(faceoffs_train)

hockey_recipe = 
  recipe(
    zone_time ~ .,
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

preds = (predict(fitted_wf, faceoffs_subset)) %>%
  select(.pred) %>%
  bind_cols(faceoffs_subset)
