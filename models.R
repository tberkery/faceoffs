library(psych)
library(tidyverse)
library(tidymodels)
library(parsnip)

source("conditioning.R")

implement_pca = function(data_imputed) {
  data_imputed_old = data_imputed
  data_imputed = data_imputed %>% select(-c(season, prior_season)) %>% select(1:50)
  non_numeric_data_cols = colnames(data_imputed %>% select(!where(is.numeric)))
  num_non_numeric_cols = length(non_numeric_data_cols)
  data_imputed_subset = data_imputed %>% select(all_of(non_numeric_data_cols), all_of(as.vector(setdiff(colnames(data_imputed), non_numeric_data_cols)))) %>% head(n = 10000)
  data_pca = psych::principal(r = data_imputed_subset[num_non_numeric_cols:length(colnames(data_imputed))] %>% select(where(is.numeric)), nfactor = 10)
  summary(data_pca)
  return(data_pca)
}

pca = function(data_imputed) {
  data_imputed_old = data_imputed
  data_imputed = data_imputed %>% select(-c(season, prior_season))
  data_imputed = data_imputed[ , which(apply(data_imputed, 2, var) != 0)]
  non_numeric_data_cols = colnames(data_imputed %>% select(!where(is.numeric)))
  num_non_numeric_cols = length(non_numeric_data_cols)
  data_imputed_subset = data_imputed %>% select(all_of(non_numeric_data_cols), all_of(as.vector(setdiff(colnames(data_imputed), non_numeric_data_cols)))) 
  first_col = num_non_numeric_cols + 1
  last_col = length(colnames(data_imputed))
  pca_res = prcomp(data_imputed_subset[first_col:last_col], center = TRUE, scale. = TRUE)
  data_pca = as.data.frame(pca_res$x)
  return(data_pca)
}

pca_modified = function(data_imputed) {
  data_imputed_old = data_imputed
  data_imputed = data_imputed %>% select(-c(season, prior_season))
  data_imputed = data_imputed[ , which(apply(data_imputed, 2, var) != 0)]
  non_numeric_data_cols = colnames(data_imputed %>% select(!where(is.numeric)))
  num_non_numeric_cols = length(non_numeric_data_cols)
  data_imputed_subset = data_imputed %>% select(all_of(non_numeric_data_cols), all_of(as.vector(setdiff(colnames(data_imputed), non_numeric_data_cols)))) 
  first_col = num_non_numeric_cols + 1
  last_col = length(colnames(data_imputed))
  pca_res = prcomp(data_imputed_subset[first_col:last_col])
  data_pca = as.data.frame(pca_res$x)
  return(data_pca)
}

pca_fixed_component_count = function(data_imputed) {
  data_imputed_old = data_imputed
  data_imputed = data_imputed %>% select(-c(season, prior_season))
  data_imputed = data_imputed[ , which(apply(data_imputed, 2, var) != 0)]
  non_numeric_data_cols = colnames(data_imputed %>% select(!where(is.numeric)))
  num_non_numeric_cols = length(non_numeric_data_cols)
  data_imputed_subset = data_imputed %>% select(all_of(non_numeric_data_cols), all_of(as.vector(setdiff(colnames(data_imputed), non_numeric_data_cols)))) 
  first_col = num_non_numeric_cols + 1
  last_col = length(colnames(data_imputed))
  pca_res = psych::principal(r = data_imputed_subset[first_col:last_col], nfactors = 100)
  data_pca = as.data.frame(pca_res$x)
  return(data_pca)
}

rand_forest = function() {
  data_imputed = condition()
  data_pca = pca(data_imputed)
  set.seed(123)
  data_imputed_old = data_imputed
  data_imputed = data_pca
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

rand_forest_pca = function(data_pca, data_imputed_old) {
  data_pca_with_objective = data_pca %>% mutate(faceoff_winning_team_xG_since_faceoff = 0)
  data_pca_with_objective$faceoff_winning_team_xG_since_faceoff = data_imputed_old$faceoff_winning_team_xG_since_faceoff
  set.seed(123)
  faceoffs_split <- initial_split(data_pca_with_objective)
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
  faceoffs_folds <- vfold_cv(faceoffs_train, v = 5)
  
  no_cores = detectCores()
  print("num cores is")
  print(no_cores)
  
  registerDoParallel(32) #makeCluster(no_cores))
  
  set.seed(345)
  tune_res <- tune_grid(
    tune_wf,
    resamples = faceoffs_folds,
    grid = 10
  )
  
  tune_res
}