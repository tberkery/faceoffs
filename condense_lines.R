library(tidyverse)
condense_to_line_matchups = function(data_all) {
  #avg_cols = colnames(data %>% select(contains("%") | contains("_per_") | contains("_Per_") | contains("_Percent_") | contains("_percent_") | contains("Draft")))
  #num_cols = length(colnames(data))
  data_all = read_csv("recoded_roles_updated_with_names.csv")
  mut_cols = colnames(data_all) %>% select(74:829) %>% select(where(is.numeric)))
  line_matchups = data_all %>%
    select(season_x, Win_F1_Name, Win_F2_Name, Win_F3_Name, Win_D1_Name, Win_D2_Name,
           Lose_F1_Name, Lose_F2_Name, Lose_F3_Name, Lose_D1_Name, Lose_D2_Name, winner_xg, loser_xg) %>%
    mutate(num = 1) %>%
    group_by(season_x, Win_F1_Name, Win_F2_Name, Win_F3_Name, Win_D1_Name, Win_D2_Name,
             Lose_F1_Name, Lose_F2_Name, Lose_F3_Name, Lose_D1_Name, Lose_D2_Name) %>%
    summarize(count = n(),
              total = sum(num, na.rm = TRUE),
           winner_xg = mean(winner_xg, na.rm = TRUE), 
           loser_xg = mean(loser_xg, na.rm = TRUE),
           .groups = "keep"
           ) %>%
    distinct(.keep_all = TRUE) %>%
    arrange(desc(total))
  data_line_matchups = data_all %>%
    group_by(season_x, event_zone.x, Win_F1_Name, Win_F2_Name, Win_F3_Name, Win_D1_Name, Win_D2_Name,
             Lose_F1_Name, Lose_F2_Name, Lose_F3_Name, Lose_D1_Name, Lose_D2_Name) %>%
    mutate(across(all_of(mut_cols), ~mean(., na.rm = TRUE))) %>%
    mutate(count = n()) %>%
    distinct(.keep_all = TRUE)
}