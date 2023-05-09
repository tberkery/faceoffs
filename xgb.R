library(tidymodels)
library(tidyverse)
library(finetune)
library(xgboost)
library(data.table)
library(zoo)
faceoffs_data = read_csv('data_training.csv')

faceoffs_data = data_off_off %>%
  na.omit() %>%
  filter(!is.na(net_xg)) %>%
  mutate(shot = as.factor(ifelse(net_xg > 0,1,0))) %>%
  distinct()

set.seed(123)
faceoffs_split_xg = faceoffs_data %>%
  initial_split(strata = shot)

faceoffs_train = training(faceoffs_split_xg) %>% select(-net_xg)
faceoffs_test = testing(faceoffs_split_xg)

hockey_folds = vfold_cv(faceoffs_train, strata = shot)

hockey_recipe = 
  recipe(
    shot ~ .,
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
  set_mode("classification")

xgb_wf = workflow(hockey_recipe, xgb_spec)

xgb_rs = tune_race_anova(
  xgb_wf,
  hockey_folds,
  grid = 2,
  #metrics = metric_set(roc_auc),
  control = control_race(verbose_elim = TRUE)
)

xgb_last = xgb_wf %>%
  finalize_workflow(select_best(xgb_rs)) %>%
  last_fit(faceoffs_split_xg)

xgb_metrics = xgb_last %>%
  collect_metrics()

#.73 percent accuracy
# roc_auc is .523

library(vip)
extract_workflow(xgb_last) %>%
  extract_fit_parsnip() %>%
  vip(geom = "point", num_features = 15)

#Gets tree specifications for model
tree_specs = select_best(xgb_rs)

fitted_wf = extract_workflow(xgb_last)

preds_first = (predict(fitted_wf, faceoffs_data)) %>%
  select(.pred_class) %>%
  bind_cols(faceoffs_data) 
preds_first$x_xg = preds_second$.pred
preds_all = preds_first %>%
  mutate(overall_pred = ifelse(.pred_class == 1, x_xg, 0))

#Part 2 starting right here

faceoffs_split_xg = faceoffs_data %>%
  filter(shot == 1) %>%
  select(-shot) %>%
  initial_split(strata = net_xg)

faceoffs_train = training(faceoffs_split_xg)
faceoffs_test = testing(faceoffs_split_xg)

hockey_folds = vfold_cv(faceoffs_train, strata = net_xg)

hockey_recipe = 
  recipe(
    net_xg ~ .,
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
  grid = 2,
  #metrics = metric_set(roc_auc),
  control = control_race(verbose_elim = TRUE)
)

xgb_last = xgb_wf %>%
  finalize_workflow(select_best(xgb_rs)) %>%
  last_fit(faceoffs_split_xg)

options(scipen = 999)
xgb_metrics = xgb_last %>%
  collect_metrics()
#rmse is .06
#rsq is .0000

library(vip)
extract_workflow(xgb_last) %>%
  extract_fit_parsnip() %>%
  vip(geom = "point", num_features = 15)

#Gets tree specifications for model
tree_specs = select_best(xgb_rs)

fitted_wf = extract_workflow(xgb_last)

preds_second = (predict(fitted_wf, faceoffs_data)) %>%
  select(.pred) %>%
  bind_cols(faceoffs_data)

