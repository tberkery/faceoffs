library(tidymodels)
library(tidyverse)
library(finetune)
library(xgboost)
library(data.table)
library(zoo)

Full2017 <- read_csv("~/Downloads/Full2018.csv")

faceoffs_data = big_join %>%
  mutate(zone_change_time = 
           ifelse(event_type == 'ZONE_EXIT' |
                    event_type == 'ZONE_ENTRY' |
                    event_type == 'STOP', 
                  game_seconds, NA),
         #Taking out entries that occur as the faceoff happens
         zone_change_time = ifelse(
           event_type == 'ZONE_ENTRY' &
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
  left_join(faceoffs_data,
            by = c('home_team', 'away_team', 'game_date')) %>%
  select(game_id)



# ggplot(faceoffs_data, aes(x = zone_time)) +
#   geom_density() +
#   theme_bw() +
#   xlab('Zone Time (Seconds)') +
#   ylab('Frequency') +
#   ggtitle('Post-Faceoff Zone Time')

table(faceoffs_data$event_type)
%>%
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
