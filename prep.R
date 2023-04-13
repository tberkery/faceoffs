library(tidyverse)

data = read_csv("temporary_training_set.csv")
data = data %>%
  mutate(net_xg = winner_xg + loser_xg) %>%
  mutate(faceoff_type = case_when(
    event_zone != 'Neu' &  ((last_faceoff_winner == home_team & home_zone == 'Off') | (last_faceoff_winner != home_team & home_zone == 'Def')) ~ 'Off zone won by Off team',
    event_zone != 'Neu' &  ((last_faceoff_winner != home_team & home_zone == 'Off') | (last_faceoff_winner == home_team & home_zone == 'Def')) ~ 'Def zone won by Def team',
    event_zone != 'Neu' &  ((last_faceoff_winner == home_team & home_zone == 'Def') | (last_faceoff_winner != home_team & home_zone == 'Off')) ~ 'Def zone won by Def team',
    event_zone != 'Neu' &  ((last_faceoff_winner != home_team & home_zone == 'Def') | (last_faceoff_winner == home_team & home_zone == 'Off')) ~ 'Off zone won by Off team',
    TRUE ~ 'other'
  ))
data = data %>%
  select(faceoff_type, net_xg, Win_F1, Win_F2, Win_F3, Win_D1, Win_D2, Win_G1, Lose_F1, Lose_F2, Lose_F3, Lose_D1, Lose_D2, Lose_G1,
         starts_with('G_') & !contains('Plus_Minus'), starts_with('A1_'), starts_with('A2_'), starts_with('iCF_'), starts_with('SH%_'), starts_with('GIVE_') & !contains('GAR'), starts_with('TAKE_') & !contains('GAR'),
         starts_with('OZS_'), starts_with('NZS_'), starts_with('DZS_'), starts_with('ixG'))
colnames(data)
data = data %>%
  filter(faceoff_type == 'Off zone won by Off team') %>%
  select(-faceoff_type)
cols = colnames(data %>% select(-net_xg))
data <- data %>%
  mutate(across(all_of(cols), ~scale(.))) %>%
  mutate(across(all_of(cols), ~as.numeric(.)))
data %>% write_csv('temp_train.csv')