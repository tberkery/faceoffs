library(mice)
library(tidyverse)
library(doParallel)
library(foreach)

role_index = 1
roles = c("F1", "F2", "F3", "D1", "D2", "G1")
counter = 1

prep_impute = function(data) {
  
  impute = function(data) {
    data_imputed = mice::mice(data = data, meth = 'cart', seed = 500)
    imputed_1 = complete(data_imputed, 1) %>% saveRDS("imputed_data_", paste0(roles[role_index], "_", counter, ".rds"))
    counter = counter + 1
    imputed_2 = complete(data_imputed, 2) %>% saveRDS("imputed_data_", paste0(roles[role_index], "_", counter, ".rds"))
    counter = counter + 1
    imputed_3 = complete(data_imputed, 3) %>% saveRDS("imputed_data_", paste0(roles[role_index], "_", counter, ".rds"))
    counter = counter + 1
    imputed_4 = complete(data_imputed, 4) %>% saveRDS("imputed_data_", paste0(roles[role_index], "_", counter, ".rds"))
    counter = counter + 1
    imputed_5 = complete(data_imputed, 5) %>% saveRDS("imputed_data_", paste0(roles[role_index], "_", counter, ".rds"))
    counter = counter + 1
    role_index = role_index + 1
    return(c(imputed_1, imputed_2, imputed_3, imputed_4, imputed_5))
  }

  data_f1 = data %>% select(where(is.numeric) & contains('F1')) %>% head(100)
  data_f2 = data %>% select(where(is.numeric) & contains('F2')) %>% head(100)
  data_f3 = data %>% select(where(is.numeric) & contains('F3')) %>% head(100)
  data_d1 = data %>% select(where(is.numeric) & contains('D1')) %>% head(100)
  data_d2 = data %>% select(where(is.numeric) & contains('D2')) %>% head(100)
  data_g1 = data %>% select(where(is.numeric) & contains('G1')) %>% head(100)
  
  print("role-based sub-data-sets created")
  
  no_cores = detectCores()
  print("num cores is")
  print(no_cores)
  
  registerDoParallel(makeCluster(no_cores), outfile = "debug_file.txt")
  
  print("cluster registration complete")
  
  foreach(data_set = c(data_f1, data_f2, data_f3, data_d1, data_d2, data_g1),
          .combine = cbind) %dopar% impute(data_set)
  
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