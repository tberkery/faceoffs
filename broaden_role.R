library(tidyverse)

general_cols = colnames(data %>% select(contains('Win_F1'))) # use Win_F1 as example
general_cols <- gsub('_Win_F1', '', general_cols) # strip _Win_F1 suffix to get all generalizable column names
general_cols = general_cols[-1] # drop Win_F1
general_cols = general_cols[-1] # drop API ID
for (g_col in general_cols) {
  g_col_win = paste0(g_col, "_Win_") # format generalized col with _Win_ following it
  g_col_loss = paste0(g_col, "_Loss_") # same for _Loss_
  # This process is necessary to avoid accidentally including (and summing) columns 
  # that have overlap each other in terms of what they start with.
  partial = data %>% select(starts_with(g_col_win) | starts_with(g_col_loss))
  
  # detect whether stat that needs to be averaged or summed
  # i.e. contains per_60 or percent or %
  
  # rename columns by index and apply appropriate transformation, grouping by
  # Win_F, Win_D, Lose_F, and Lose_D
  
  # in new dataframe, insert appropriately transformed values as columns (should be 4 cols)
}