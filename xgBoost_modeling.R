library(tidymodels)
library(tidyverse)
library(finetune)
library(xgboost)

#This is just the framework of a model. We are not actually using iris data
faceoffs_data = iris

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
