library(psych)
library(tidyverse)
library(tidymodels)
library(parsnip)

source("conditioning.R")

implement_pca = function(data_imputed) {
  data_imputed_subset = data_imputed %>% head(n = 10000)
  data_pca = psych::principal(r = data_imputed_subset %>% select(where(is.numeric)), nfactor = 10)
  summary(data_pca)
  return(data_pca)
}

rand_forest = function() {
  data_imputed = condition()
  
  set.seed(123)
  
  data_imputed_subset = data_imputed %>% select(-event_type, -eh_season) %>% head(10000)
  
  faceoffs_split <- initial_split(data_imputed_subset)
  faceoffs_train <- training(faceoffs_split)
  faceoffs_test <- testing(faceoffs_split)
  
  faceoffs_rec <- recipe(faceoff_winning_team_xG_since_faceoff ~ ., data = faceoffs_train) #%>%
    #step_dummy(all_nominal(), -all_outcomes(), one_hot = TRUE)
    
  faceoffs_prep <- prep(faceoffs_rec)
  juiced <- juice(faceoffs_prep)
  
  tune_spec <- parsnip::rand_forest(
    mode = "regression",
    mtry = tune(),
    trees = 1000,
    min_n = tune()
  ) %>%
    set_mode("regression") %>%
    set_engine("ranger")
  
  tune_wf <- workflow() %>%
    add_recipe(faceoffs_rec) %>%
    add_model(tune_spec)
  
  set.seed(234)
  faceoffs_folds <- vfold_cv(faceoffs_train)
  
  no_cores = detectCores()
  print("num cores is")
  print(no_cores)
  
  registerDoParallel(32) #makeCluster(no_cores))
  
  set.seed(345)
  tune_res <- tune_grid(
    tune_wf,
    resamples = faceoffs_folds,
    grid = 20
  )
  
  tune_res
}