library(tidyverse)

group_roles = function(dataset) {
  data = dataset
  general_cols = colnames(data %>% select(contains('Win_F1')) %>% select(-starts_with('Win_F1')) %>% select(-starts_with('Pos')) %>% select(where(is.numeric))) # use Win_F1 as example
  general_cols <- gsub('_Win_F1', '', general_cols) # strip _Win_F1 suffix to get all generalizable column names
  general_cols = general_cols[-1] # drop Win_F1
  general_cols = general_cols[-1] # drop API ID
  col = general_cols[[1]]
  dataset_updated = data
  general_cols = c('iCF', 'iFF')
  for (col in general_cols) {
    win_f1_col = paste0(col, "_Win_F1")
    win_f2_col = paste0(col, "_Win_F2")
    win_f3_col = paste0(col, "_Win_F3")
    win_d1_col = paste0(col, "_Win_D1")
    win_d2_col = paste0(col, "_Win_D2")
    lose_f1_col = paste0(col, "_Lose_F1")
    lose_f2_col = paste0(col, "_Lose_F2")
    lose_f3_col = paste0(col, "_Lose_F3")
    lose_d1_col = paste0(col, "_Lose_D1")
    lose_d2_col = paste0(col, "_Lose_D2")
    col_win_all = paste0(col, "_Win_All")
    col_win_f = paste0(col, "_Win_F")
    col_win_d = paste0(col, "_Win_D")
    col_lose_all = paste0(col, "_Lose_All")
    col_lose_f = paste0(col, "_Lose_F")
    col_lose_d = paste0(col, "_Lose_D")
    
    print(sapply(dataset_updated[c(win_f1_col, win_f2_col, win_f3_col, win_d1_col, win_d2_col)], class))
    cols = c(win_f1_col, win_f2_col, win_f3_col, win_d1_col, win_d2_col, lose_f1_col, lose_f2_col, lose_f3_col, lose_d1_col, lose_d2_col)
    
    dataset_temp = dataset_updated %>%
      mutate(across(all_of(cols), ~as.numeric(.))) %>%
      select(cols)
    
    dataset_temp[[col_win_all]] = dataset_temp[[win_f1_col]] + dataset_temp[[win_f2_col]] + dataset_temp[[win_f3_col]] + dataset_temp[[win_d1_col]] + dataset_temp[[win_d2_col]]
    dataset_temp[[col_win_f]] = dataset_temp[[win_f1_col]] + dataset_temp[[win_f2_col]] + dataset_temp[[win_f3_col]]
    dataset_temp[[col_win_d]] = dataset_temp[[win_d1_col]] + dataset_temp[[win_d2_col]]
    
    dataset_temp[[col_lose_all]] = dataset_temp[[lose_f1_col]] + dataset_temp[[lose_f2_col]] + dataset_temp[[lose_f3_col]] + dataset_temp[[lose_d1_col]] + dataset_temp[[lose_d2_col]]
    dataset_temp[[col_lose_f]] = dataset_temp[[lose_f1_col]] + dataset_temp[[lose_f2_col]] + dataset_temp[[lose_f3_col]]
    dataset_temp[[col_lose_d]] = dataset_temp[[lose_d1_col]] + dataset_temp[[lose_d2_col]]
    
    dataset_updated[[col_win_all]] = dataset_temp[[col_win_all]]
    dataset_updated[[col_win_f]] = dataset_temp[[col_win_f]]
    dataset_updated[[col_win_d]] = dataset_temp[[col_win_d]]
    dataset_updated[[col_lose_all]] = dataset_temp[[col_lose_all]]
    dataset_updated[[col_lose_f]] = dataset_temp[[col_lose_f]]
    dataset_updated[[col_lose_d]] = dataset_temp[[col_lose_d]]
    
    # if (grepl("%", col) | grepl("_per_", col) | grepl("_Per_", col) | grepl("_Percent_", col) | grepl("_percent_", col) | grepl("Draft", col)) {
    #   dataset_temp = dataset_temp %>%
    #     mutate(!!col_win_all := (!!win_f1_col + !!win_f2_col + !!win_f3_col + !!win_d1_col + !!win_d2_col) / 5) %>%
    #     mutate(!!col_win_f := (!!win_f1_col + !!win_f2_col + !!win_f3_col) / 3) %>%
    #     mutate(!!col_win_d := (!!win_d1_col + !!win_d2_col) / 2) %>%
    #     mutate(!!col_lose_all := (!!lose_f1_col + !!lose_f2_col + !!lose_f3_col + !!lose_d1_col + !!lose_d2_col) / 5) %>%
    #     mutate(!!col_lose_f := (!!lose_f1_col + !!lose_f2_col + !!lose_f3_col) / 3) %>%
    #     mutate(!!col_lose_d := (!!lose_d1_col + !!lose_d2_col) / 2)
    #   
    #   
    # } else {
    # 
    # }
  }
}