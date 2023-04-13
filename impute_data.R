dataset = read_csv('dataset_old.csv')
impute_data = function(dataset){
  
data = dataset %>%
  filter(!is.na(Win_F1), !is.na(Win_F2), !is.na(Win_F3)) %>%
  select(Win_F1, Win_F2, Win_F3) %>%
  bind_rows(dataset %>%
              filter(!is.na(Lose_F1), !is.na(Lose_F2), !is.na(Lose_F3)) %>%
              select(Lose_F1, Lose_F2, Lose_F3) %>%
              rename(Win_F1 = Lose_F1,
                     Win_F2 = Lose_F2,
                     Win_F3 = Lose_F3))

model1 = lm(Win_F3 ~ Win_F1 + Win_F2, data = data)

model2 = lm(Win_F2 ~ Win_F3 + Win_F1, data = data)

model3 = lm(Win_F1 ~ Win_F3 + Win_F2, data = data)

model4 = lm(Win_F2 ~ Win_F3, data = data)

model5 = lm(Win_F2 ~ Win_F1, data = data)

model6 = lm(Win_F1 ~ Win_F2, data = data)

data = dataset %>%
  filter(!is.na(Win_D1), !is.na(Win_D2)) %>%
  select(Win_D1, Win_D2) %>%
  bind_rows(
    dataset %>%
      filter(!is.na(Lose_D1), !is.na(Lose_D2)) %>%
      select(Lose_D1, Lose_D2) %>%
      rename(Win_D1 = Lose_D1,
             Win_D2 = Lose_D2))

model7 = lm(Win_D2 ~ Win_D1, data = data)

model8 = lm(Win_D1 ~ Win_D2, data = data)

new_df = dataset %>%
  mutate(
    Win_F1 = ifelse(is.na(Win_F1) &
                      !is.na(Win_F2) &
                      is.na(Win_F3),
                    predict(model6, dataset),
                    Win_F1),
    Win_F2 = ifelse(!is.na(Win_F1) &
                      is.na(Win_F2) &
                      is.na(Win_F3),
                    predict(model5, dataset),
                    Win_F2),
    Win_F2 = ifelse(is.na(Win_F1) &
                      is.na(Win_F2) &
                      !is.na(Win_F3),
                    predict(model4, dataset),
                    Win_F2),
    Win_D1 = ifelse(is.na(Win_D1) &
                      !is.na(Win_D2),
                    predict(model8, dataset),
                    Win_D1),
    Win_D2 = ifelse(is.na(Win_D2) &
                      !is.na(Win_D1),
                    predict(model7, dataset),
                    Win_D2),
    Win_F1 = ifelse(is.na(Win_F1) &
                      !is.na(Win_F2) &
                      !is.na(Win_F3),
                    predict(model3, dataset),
                    Win_F1),
    Win_F2 = ifelse(is.na(Win_F2) &
                      !is.na(Win_F1) &
                      !is.na(Win_F3),
                    predict(model2, dataset),
                    Win_F2),
    Win_F3 = ifelse(is.na(Win_F3) &
                      !is.na(Win_F2) &
                      !is.na(Win_F1),
                    predict(model1, dataset),
                    Win_F3)
  ) %>%
  rename(
    Winner_F1 = Win_F1,
    Winner_F2 = Win_F2,
    Winner_F3 = Win_F3,
    Winner_D1 = Win_D1,
    Winner_D2 = Win_D2,
    Win_F1 = Lose_F1,
    Win_F2 = Lose_F2,
    Win_F3 = Lose_F3,
    Win_D1 = Lose_D1,
    Win_D2 = Lose_D2
  ) %>%
  mutate(Win_F1 = ifelse(is.na(Win_F1) &
                           !is.na(Win_F2) &
                           is.na(Win_F3),
                         predict(model6, dataset),
                         Win_F1),
         Win_F2 = ifelse(!is.na(Win_F1) &
                           is.na(Win_F2) &
                           is.na(Win_F3),
                         predict(model5, dataset),
                         Win_F2),
         Win_F2 = ifelse(is.na(Win_F1) &
                           is.na(Win_F2) &
                           !is.na(Win_F3),
                         predict(model4, dataset),
                         Win_F2),
         Win_D1 = ifelse(is.na(Win_D1) &
                           !is.na(Win_D2),
                         predict(model8, dataset),
                         Win_D1),
         Win_D2 = ifelse(is.na(Win_D2) &
                           !is.na(Win_D1),
                         predict(model7, dataset),
                         Win_D2),
         Win_F1 = ifelse(is.na(Win_F1) &
                           !is.na(Win_F2) &
                           !is.na(Win_F3),
                         predict(model3, dataset),
                         Win_F1),
         Win_F2 = ifelse(is.na(Win_F2) &
                           !is.na(Win_F1) &
                           !is.na(Win_F3),
                         predict(model2, dataset),
                         Win_F2),
         Win_F3 = ifelse(is.na(Win_F3) &
                           !is.na(Win_F2) &
                           !is.na(Win_F1),
                         predict(model1, dataset),
                         Win_F3)) %>%
  rename(
    Lose_F1 = Win_F1,
    Lose_F2 = Win_F2,
    Lose_F3 = Win_F3,
    Lose_D1 = Win_D1,
    Lose_D2 = Win_D2,
    Win_F1 = Winner_F1,
    Win_F2 = Winner_F2,
    Win_F3 = Winner_F3,
    Win_D1 = Winner_D1,
    Win_D2 = Winner_D2) %>%
  mutate(Win_F1_Perc = apply(as_tibble(Win_F1), 2, function(x) ecdf(x)(x) * 100)[,'value'],
         Win_F2_Perc = apply(as_tibble(Win_F2), 2, function(x) ecdf(x)(x) * 100)[,'value'],
         Win_F3_Perc = apply(as_tibble(Win_F3), 2, function(x) ecdf(x)(x) * 100)[,'value'],
         Win_D1_Perc = apply(as_tibble(Win_D1), 2, function(x) ecdf(x)(x) * 100)[,'value'],
         Win_D2_Perc = apply(as_tibble(Win_D2), 2, function(x) ecdf(x)(x) * 100)[,'value'],
         Lose_F1_Perc = apply(as_tibble(Lose_F1), 2, function(x) ecdf(x)(x) * 100)[,'value'],
         Lose_F2_Perc = apply(as_tibble(Lose_F2), 2, function(x) ecdf(x)(x) * 100)[,'value'],
         Lose_F3_Perc = apply(as_tibble(Lose_F3), 2, function(x) ecdf(x)(x) * 100)[,'value'],
         Lose_D1_Perc = apply(as_tibble(Lose_D1), 2, function(x) ecdf(x)(x) * 100)[,'value'],
         Lose_D2_Perc = apply(as_tibble(Lose_D2), 2, function(x) ecdf(x)(x) * 100)[,'value'])

df_Win_F1 <- new_df %>%
  select(contains('Win_F1')) %>%
  mutate_if(is.numeric, ~if_else(is.na(.), quantile(., Win_F1_Perc/100, na.rm = TRUE), .)) # replace NA values with percentile

df_Win_F2 <- new_df %>%
  select(contains('Win_F2')) %>%
  mutate_if(is.numeric, ~if_else(is.na(.), quantile(., Win_F2_Perc/100, na.rm = TRUE), .)) # replace NA values with percentile

df_Win_F3 <- new_df %>%
  select(contains('Win_F3')) %>%
  mutate_if(is.numeric, ~if_else(is.na(.), quantile(., Win_F3_Perc/100, na.rm = TRUE), .)) # replace NA values with percentile

df_Win_D1 <- new_df %>%
  select(contains('Win_D1')) %>%
  mutate_if(is.numeric, ~if_else(is.na(.), quantile(., Win_D1_Perc/100, na.rm = TRUE), .)) # replace NA values with percentile

df_Win_D2 <- new_df %>%
  select(contains('Win_D2')) %>%
  mutate_if(is.numeric, ~if_else(is.na(.), quantile(., Win_D2_Perc/100, na.rm = TRUE), .)) # replace NA values with percentile

df_Lose_F1 <- new_df %>%
  select(contains('Lose_F1')) %>%
  mutate_if(is.numeric, ~if_else(is.na(.), quantile(., Lose_F1_Perc/100, na.rm = TRUE), .)) # replace NA values with percentile

df_Lose_F2 <- new_df %>%
  select(contains('Lose_F2')) %>%
  mutate_if(is.numeric, ~if_else(is.na(.), quantile(., Lose_F2_Perc/100, na.rm = TRUE), .)) # replace NA values with percentile

df_Lose_F3 <- new_df %>%
  select(contains('Lose_F3')) %>%
  mutate_if(is.numeric, ~if_else(is.na(.), quantile(., Lose_F3_Perc/100, na.rm = TRUE), .)) # replace NA values with percentile

df_Lose_D1 <- new_df %>%
  select(contains('Lose_D1')) %>%
  mutate_if(is.numeric, ~if_else(is.na(.), quantile(., Lose_D1_Perc/100, na.rm = TRUE), .)) # replace NA values with percentile

df_Lose_D2 <- new_df %>%
  select(contains('Lose_D2')) %>%
  mutate_if(is.numeric, ~if_else(is.na(.), quantile(., Lose_D2_Perc/100, na.rm = TRUE), .)) # replace NA values with percentile

general = new_df %>%
  select(!contains('Lose', ignore.case = F) & !contains('Win', ignore.case = F))

full_df = general %>%
  bind_cols(df_Win_F1, df_Win_F2, df_Win_F3, df_Win_D1, df_Win_D2,
            df_Lose_F1, df_Lose_F2, df_Lose_F3, df_Lose_D1, df_Lose_D2)

full_df$winner

write_csv(full_df, 'all_df.csv')

return(full_df)
}