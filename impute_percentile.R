library(tidyverse)
percentile_threshold = 0.2
determine_thresholds_via_percentile = function(dataset, percentile_threshold) {
  general_cols = colnames(dataset %>% select(contains('Win_F1')) %>% select(-starts_with('Win_F1')) %>% select(-starts_with('Pos')) %>% select(where(is.numeric))) # use Win_F1 as example
  general_cols <- gsub('_Win_F1', '', general_cols) # strip _Win_F1 suffix to get all generalizable column names
  general_cols = general_cols[-1] # drop Win_F1
  general_cols = general_cols[-1] # drop API ID
  low_is_good_stats = c("Draft_Rd", "Draft_Ov", "iHA", "iPENT2", "iPENT5", "FOL", "RelTM_GA_per_60", "RelTM_SA_per_60", "RelTM_FA_per_60", "RelTM_CA_per_60",
                        "xGA_per_60_on_ice", "GA", "SA", "FA", "xGA")
  goaltending_stats = c("Sv_Percent_goaltending", "FSv_Percent", "xFSv_Percent", "dFSv_Percent", "GSAA", "GSAx",
                       "TOI_EV", "TOI_SH", "FA_EV", "FA_SH", "EVD_GAR_goaltending", "SHD_GAR_goaltending", "Take_GAR_goaltending",
                       "Draw_GAR_goaltending", "GAR_goaltending", "WAR_goaltending", "SPAR_goaltending")
  team_stats = c("GF%_team", "SF%_team", "FF%_team", "CF%_team", "xGF_Percent_team", "GF_per_60_team", "GA_per_60_team", "SF_per_60_team", "SA_per_60_team",
                 "FF_per_60_team", "FA_per_60_team", "CF_per_60_team", "CA_per_60_team", "xGF_per_60_team", "xGA_per_60_team", "G_Plus_Minus_per_60_team",
                 "S_Plus_Minus_per_60_team", "F_Plus_Minus_per_60_team", "C_Plus_Minus_per_60_team", "xG_Plus_Minus_per_60_team", "Sh_Percent_Adjusted_team", 
                 "Sv_Percent_Adjusted_team", "W_team", "L_team", "OL_team", "Points_team", "Points_Percent_team", "GF_team", "GA_team")
  high_is_good_stats = setdiff(general_cols, c(low_is_good_stats, goaltending_stats, team_stats))
  statuses = c("Win", "Lose")
  positions = c("F1", "F2", "F3", "D1", "D2")
  metric = c()
  replacement_level_value = c()
  for (stat in high_is_good_stats) {
    for (status in statuses) {
      for (pos in positions) {
        col_name = paste0(stat, "_", status, "_", pos)
        print(col_name)
        tenth_percentile = dataset %>% 
          pull(!!col_name) %>% 
          quantile(percentile_threshold, na.rm = TRUE)
        metric = c(metric, col_name)
        replacement_level_value = c(replacement_level_value, tenth_percentile)
      }
    }
  }
  for (stat in low_is_good_stats) {
    for (status in statuses) {
      for (pos in positions) {
        col_name = paste0(stat, "_", status, "_", pos)
        print(col_name)
        tenth_percentile = dataset %>% 
          pull(!!col_name) %>% 
          quantile(1.0 - percentile_threshold, na.rm = TRUE)
        metric = c(metric, col_name)
        replacement_level_value = c(replacement_level_value, tenth_percentile)
      }
    }
  }
  for (stat in c(goaltending_stats, team_stats)) {
    for (status in statuses) {
      for (pos in positions) {
        col_name = paste0(stat, "_", status, "_", pos)
        print(col_name)
        tenth_percentile = dataset %>% 
          pull(!!col_name) %>% 
          quantile(0.5, na.rm = TRUE)
        metric = c(metric, col_name)
        replacement_level_value = c(replacement_level_value, tenth_percentile)
      }
    }
  }
  replacement_thresholds = data.frame(metric, replacement_level_value)
  replacement_thresholds = replacement_thresholds %>%
    mutate(replacement_level_value = as.numeric(replacement_level_value))
  replacement_thresholds %>% write_csv("replacement_thresholds.csv")
  return(replacement_thresholds)
}

impute_by_percentile_threshold = function(dataset, replacement_thresholds) {
  dataset_new = dataset %>%
    select(-c(coords_x, coords_y, event_distance, event_angle, home_zonestart, starts_with("Pos_")))
  for (i in 1:nrow(replacement_thresholds)) {
    metric <- replacement_thresholds[i, "metric"]
    print(metric)
    rl_value <- replacement_thresholds[i, "replacement_level_value"]
    dataset_new[[metric]] <- ifelse(is.na(dataset_new[[metric]]), rl_value, dataset_new[[metric]])
  }
  dataset_imputed = dataset_new %>% select(-contains("replacement"))
  na_cols <- colSums(is.na(dataset_imputed)) > 0
  remove_cols <- colnames(dataset_imputed)[na_cols]
  remove_cols = setdiff(remove_cols, c("event_zone", "event_team"))
  dataset_imputed = dataset_imputed %>%
    select(-all_of(remove_cols))
  dataset_imputed_no_nas = dataset_imputed %>% drop_na() # this line atm removes just 1 out of 298,411 observations!
  dataset_imputed_no_nas %>% write_csv("percentile_imputed_dataset.csv")
  return(dataset_imputed_no_nas)
}
