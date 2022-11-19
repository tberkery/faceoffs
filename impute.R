library(mice)
library(tidyverse)
library(doParallel)
library(foreach)
library(itertools)

prep_impute = function(data) {
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

  # data_f1 = data %>% select(where(is.numeric) & contains('F1')) %>% head(100)
  # data_f2 = data %>% select(where(is.numeric) & contains('F2')) %>% head(100)
  # data_f3 = data %>% select(where(is.numeric) & contains('F3')) %>% head(100)
  # data_d1 = data %>% select(where(is.numeric) & contains('D1')) %>% head(100)
  # data_d2 = data %>% select(where(is.numeric) & contains('D2')) %>% head(100)
  # data_g1 = data %>% select(where(is.numeric) & contains('G1')) %>% head(100)
  
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