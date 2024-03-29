library(mice)
library(tidyverse)
library(doParallel)
library(foreach)
library(itertools)

impute_10th_percentile = function(data) {
  data_imputed = data %>%
    mutate(Draft_Ov_Win_F1 = replace_na(Draft_Ov_Win_F1, 32 * 7 + 1)) %>%
    mutate(Draft_Ov_Win_F2 = replace_na(Draft_Ov_Win_F2, 32 * 7 + 1)) %>%
    mutate(Draft_Ov_Win_F3 = replace_na(Draft_Ov_Win_F3, 32 * 7 + 1)) %>%
    mutate(Draft_Ov_Win_D1 = replace_na(Draft_Ov_Win_D1, 32 * 7 + 1)) %>%
    mutate(Draft_Ov_Win_D2 = replace_na(Draft_Ov_Win_D2, 32 * 7 + 1)) %>%
    mutate(Draft_Ov_Lose_F1 = replace_na(Draft_Ov_Lose_F1, 32 * 7 + 1)) %>%
    mutate(Draft_Ov_Lose_F2 = replace_na(Draft_Ov_Lose_F2, 32 * 7 + 1)) %>%
    mutate(Draft_Ov_Lose_F3 = replace_na(Draft_Ov_Lose_F3, 32 * 7 + 1)) %>%
    mutate(Draft_Ov_Lose_D1 = replace_na(Draft_Ov_Lose_D1, 32 * 7 + 1)) %>%
    mutate(Draft_Ov_Lose_D2 = replace_na(Draft_Ov_Lose_D2, 32 * 7 + 1)) %>%
    mutate(across(where(is.numeric), ~replace_na(., quantile(., 0.1, na.rm = TRUE) + rnorm(1, mean = mean(., na.rm = TRUE), sd = sd(., na.rm = TRUE)))))
  return(data_imputed)
}

# still tinkering with desired imputation strategy (if any)

impute_mice = function(data) {
  data_f1_win = data %>% select(where(is.numeric) & contains('Win_F1'))
  data_f2_win = data %>% select(where(is.numeric) & contains('Win_F2'))
  data_f3_win = data %>% select(where(is.numeric) & contains('Win_F3'))
  data_d1_win = data %>% select(where(is.numeric) & contains('Win_D1')) 
  data_d2_win = data %>% select(where(is.numeric) & contains('Win_D2'))
  data_g1_win = data %>% select(where(is.numeric) & contains('Win_G1'))
  data_f1_win_mice = mice(data_f1_win, method = "cart", m = 1, maxit = 1)
  data_f1_win_imputed = complete(data_f1_win_mice, 1)
  data_f1_win_imputed %>% write_csv("data_f1_win_imputed.csv")
  
  data_f2_win_mice = mice(data_f2_win, method = "cart", m = 1, maxit = 1)
  data_f2_win_imputed = complete(data_f2_win_mice, 1)
  data_f2_win_imputed %>% write_csv("data_f2_win_imputed.csv")
  
  data_f3_win_mice = mice(data_f3_win, method = "cart", m = 1, maxit = 1)
  data_f3_win_imputed = complete(data_f3_win_mice, 1)
  data_f3_win_imputed %>% write_csv("data_f3_win_imputed.csv")
  
  data_d1_win_mice = mice(data_d1_win, method = "cart", m = 1, maxit = 1)
  data_d1_win_imputed = complete(data_d1_win_mice, 1)
  data_d1_win_imputed %>% write_csv("data_d1_win_imputed.csv")
  
  data_d2_win_mice = mice(data_d2_win, method = "cart", m = 1, maxit = 1)
  data_d2_win_imputed = complete(data_d2_win_mice, 1)
  data_d2_win_imputed %>% write_csv("data_d2_win_imputed.csv")
  
  data_g1_win_mice = mice(data_g1_win, method = "cart", m = 1, maxit = 1)
  data_g1_win_imputed = complete(data_g1_win_mice, 1)
  data_g1_win_imputed %>% write_csv("data_g1_win_imputed.csv")
  
  data_f1_lose = data %>% select(where(is.numeric) & contains('Lose_F1'))
  data_f2_lose = data %>% select(where(is.numeric) & contains('Lose_F2'))
  data_f3_lose = data %>% select(where(is.numeric) & contains('Lose_F3'))
  data_d1_lose = data %>% select(where(is.numeric) & contains('Lose_D1')) 
  data_d2_lose = data %>% select(where(is.numeric) & contains('Lose_D2'))
  data_g1_lose = data %>% select(where(is.numeric) & contains('Lose_G1'))
  
  data_f1_lose_mice = mice(data_f1_lose, method = "cart", m = 1, maxit = 1)
  data_f1_lose_imputed = complete(data_f1_lose_mice, 1)
  data_f1_lose_imputed %>% write_csv("data_f1_lose_imputed.csv")
  
  data_f2_lose_mice = mice(data_f2_lose, method = "cart", m = 1, maxit = 1)
  data_f2_lose_imputed = complete(data_f2_lose_mice, 1)
  data_f2_lose_imputed %>% write_csv("data_f2_lose_imputed.csv")
  
  data_f3_lose_mice = mice(data_f3_lose, method = "cart", m = 1, maxit = 1)
  data_f3_lose_imputed = complete(data_f3_lose_mice, 1)
  data_f3_lose_imputed %>% write_csv("data_f3_lose_imputed.csv")
  
  data_d1_lose_mice = mice(data_d1_lose, method = "cart", m = 1, maxit = 1)
  data_d1_lose_imputed = complete(data_d1_lose_mice, 1)
  data_d1_lose_imputed %>% write_csv("data_d1_lose_imputed.csv")
  
  data_d2_lose_mice = mice(data_d2_lose, method = "cart", m = 1, maxit = 1)
  data_d2_lose_imputed = complete(data_d2_lose_mice, 1)
  data_d2_lose_imputed %>% write_csv("data_d2_lose_imputed.csv")
  
  data_g1_lose_mice = mice(data_g1_lose, method = "cart", m = 1, maxit = 1)
  data_g1_lose_imputed = complete(data_g1_lose_mice, 1)
  data_g1_lose_imputed %>% write_csv("data_g1_lose_imputed.csv")
}


prep_impute_mice = function(data) {
  data = data %>% select(contains("F1") |
                        contains("F2") |
                        contains("F3") |
                        contains("D1") |
                        contains("D2") |
                        contains("G1"))
  impute = function(sub) {
    counter = 1
    print(colnames(sub))
    print(paste0("started running impute"))
    data_imputed = mice::mice(data = sub, meth = 'cart', maxit = 1, seed = 500)
    print(paste0("finished running impute"))
    return(complete(data_imputed, 1))
  }

  
  
  print("role-based sub-data-sets created")
  
  no_cores = detectCores()
  print("num cores is")
  print(no_cores)
  
  registerDoParallel(makeCluster(no_cores))
  
  print("cluster registration complete")
  
  foreach(m = isplitCols(data, chunks = 6),
          .combine = 'cbind',
          .packages = 'mice') %dopar% impute(m)
  
  print("for each execution complete")
}

debug = function(data) {
  cols = as.vector(colnames(data))
  str = ""
  for (col in cols) {
    str = paste0(str, "+", col)
  }
  print(str)
}