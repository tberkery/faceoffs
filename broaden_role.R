library(tidyverse)
data = read_csv("all_df_updated.csv")
general_cols = colnames(data %>% select(contains('Win_F1')) %>% select(-starts_with('Win_F1')) %>% select(where(is.numeric))) # use Win_F1 as example
general_cols <- gsub('_Win_F1', '', general_cols) # strip _Win_F1 suffix to get all generalizable column names
general_cols = general_cols[-1] # drop Win_F1
general_cols = general_cols[-1] # drop API ID
g_col = general_cols[[1]]
summary_df = data %>% select(-contains("_Win_") & -contains("_Lose_")) #select(1:34, winner_xg, loser_xg, contains('_G1'))
for (g_col in general_cols) {
  g_col_win = paste0(g_col, "_Win_") # format generalized col with _Win_ following it
  g_col_loss = paste0(g_col, "_Lose_") # same for _Loss_
  g_col_win_all = paste0(g_col, "_Win_All")
  g_col_win_F = paste0(g_col, "_Win_F")
  g_col_win_D = paste0(g_col, "_Win_D")
  g_col_lose_all = paste0(g_col, "_Lose_All")
  g_col_lose_F = paste0(g_col, "_Lose_F")
  g_col_lose_D = paste0(g_col, "_Lose_D")
  # This process is necessary to avoid accidentally including (and summing) columns 
  # that have overlap each other in terms of what they start with.
  partial_win = data %>% select(starts_with(g_col_win))
  partial_lose = data %>% select(starts_with(g_col_loss))
  # detect whether stat that needs to be averaged or summed
  # i.e. contains per_60 or percent or %
  if (grepl("%", g_col) | grepl("_per_", g_col) | grepl("_Per_", g_col) | grepl("_Percent_", g_col) | grepl("_percent_", g_col) | grepl("Draft", g_col)) { # this stat should be averaged
    print(paste0("Performing AVERAGING for ", g_col))
    partial_win = partial_win %>%
      mutate(sum_all = .[[1]] + .[[2]] + .[[3]] + .[[4]] + .[[5]]) %>% # referencing columns based on index
      mutate(sum_F = .[[1]] + .[[2]] + .[[3]]) %>% 
      mutate(sum_D = .[[4]] + .[[5]]) %>% 
      mutate(sum_all = sum_all / 5) %>%
      mutate(sum_F = sum_F / 3) %>%
      mutate(sum_D = sum_D / 2) %>%
      rename(!!g_col_win_all:=sum_all) %>% # !! indicates use value of immediately following variable
      rename(!!g_col_win_F:=sum_F) %>%
      rename(!!g_col_win_D:=sum_D) 
    
    partial_lose = partial_lose %>%
      mutate(sum_all = .[[1]] + .[[2]] + .[[3]] + .[[4]] + .[[5]]) %>% # referencing columns based on index
      mutate(sum_F = .[[1]] + .[[2]] + .[[3]]) %>% 
      mutate(sum_D = .[[4]] + .[[5]]) %>% 
      mutate(sum_all = sum_all / 5) %>%
      mutate(sum_F = sum_F / 3) %>%
      mutate(sum_D = sum_D / 2) %>%
      rename(!!g_col_lose_all:=sum_all) %>%
      rename(!!g_col_lose_F:=sum_F) %>%
      rename(!!g_col_lose_D:=sum_D) 
    
    summary_df[[g_col_win_all]] = partial_win[[g_col_win_all]]
    summary_df[[g_col_win_F]] = partial_win[[g_col_win_F]]
    summary_df[[g_col_win_D]] = partial_win[[g_col_win_D]]
    
    summary_df[[g_col_lose_all]] = partial_win[[g_col_lose_all]]
    summary_df[[g_col_lose_F]] = partial_win[[g_col_lose_F]]
    summary_df[[g_col_lose_D]] = partial_win[[g_col_lose_D]]
  } else { # this stat should be summed
    print(paste0("Performing SUMMING for ", g_col))
    partial_win = partial_win %>%
      mutate(sum_all = .[[1]] + .[[2]] + .[[3]] + .[[4]] + .[[5]]) %>% # referencing columns based on index
      mutate(sum_F = .[[1]] + .[[2]] + .[[3]]) %>% 
      mutate(sum_D = .[[4]] + .[[5]]) %>% 
      rename(!!g_col_win_all:=sum_all) %>% # !! indicates use value of immediately following variable
      rename(!!g_col_win_F:=sum_F) %>%
      rename(!!g_col_win_D:=sum_D) 
    
    partial_lose = partial_lose %>%
      mutate(sum_all = .[[1]] + .[[2]] + .[[3]] + .[[4]] + .[[5]]) %>% # referencing columns based on index
      mutate(sum_F = .[[1]] + .[[2]] + .[[3]]) %>% 
      mutate(sum_D = .[[4]] + .[[5]]) %>% 
      rename(!!g_col_lose_all:=sum_all) %>%
      rename(!!g_col_lose_F:=sum_F) %>%
      rename(!!g_col_lose_D:=sum_D) 
    
    summary_df[[g_col_win_all]] = partial_win[[g_col_win_all]]
    summary_df[[g_col_win_F]] = partial_win[[g_col_win_F]]
    summary_df[[g_col_win_D]] = partial_win[[g_col_win_D]]
    
    summary_df[[g_col_lose_all]] = partial_win[[g_col_lose_all]]
    summary_df[[g_col_lose_F]] = partial_win[[g_col_lose_F]]
    summary_df[[g_col_lose_D]] = partial_win[[g_col_lose_D]]
  }
  
  # rename columns by index and apply appropriate transformation, grouping by
  # Win_F, Win_D, Lose_F, and Lose_D
  
  # in new dataframe, insert appropriately transformed values as columns (should be 4 cols)
}
summary_df %>% write_csv("recoded_roles_updated.csv")
