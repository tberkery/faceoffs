library(tidyverse)

pbp_22 = read_csv("EH_pbp_query_20222023.csv")

pbp_22_pp = pbp_22 %>%
  filter(game_strength_state %in% c("5v4", "4v5"))

`%notin%` = Negate(`%in%`)
pp_info = pbp_22 %>%
  arrange(game_id, season, game_seconds) %>%
  filter(event_type %in% c("FAC", "GOAL", "SHOT", "PENL")) %>%
  #filter(event_zone == "Def", game_strength_state == "4v5") %>%
  mutate(power_play_team = case_when(
    game_strength_state %in% c("4v5", "5v4") & home_skaters == 5 & away_skaters == 4 ~ home_team,
    game_strength_state %in% c("4v5", "5v4") & home_skaters == 4 & away_skaters == 5 ~ away_team,
    TRUE ~ "NONE"
  )) %>%
  #mutate(power_play_team = zoo::na.locf(power_play_team, fromLast = F, na.rm = F)) %>%
  mutate(next_pos_xG_event = case_when(
    event_type %in% c("GOAL", "SHOT") & event_team == power_play_team & pred_goal > 0 ~ game_seconds,
    TRUE ~ NA_real_
  )) %>%
  mutate(next_pos_xG_event = zoo::na.locf(next_pos_xG_event, fromLast = T, na.rm = F)) %>%
  mutate(end_PP = case_when(
   lag(home_team) != home_team & lag(away_team) != away_team ~ lag(game_seconds),
   lag(home_skaters) != home_skaters ~ lag(game_seconds),
   lag(away_skaters) != away_skaters ~ lag(game_seconds),
   TRUE ~ NA_real_
  )) %>%
  mutate(end_PP = zoo::na.locf(end_PP, fromLast = T, na.rm = F))

pp_snapshot = pp_info %>% 
  select(season, game_id, game_seconds, game_strength_state, event_type, event_team, event_zone, pred_goal, power_play_team, next_pos_xG_event, end_PP) 

pp_info_sub = pp_info %>%
  filter(event_type == "FAC", game_strength_state %in% c("5v4", "4v5")) %>%
  select(game_id, season, game_seconds, event_type, event_team, power_play_team, next_pos_xG_event) %>%
  mutate(transition_time = next_pos_xG_event - game_seconds) %>%
  filter(transition_time >= 0) #, transition_time <= 2 * 60)

idx_no_next_pos_xG_event = pp_info_sub$transition_time > 2 * 60
idx_yes_next_pos_xG_event = pp_info_sub$transition_time  <= 2 * 60

no_next_pos_xG_event = unique(pp_info_sub$transition_time)[idx_no_next_pos_xG_event]
yes_next_pos_xG_event = unique(pp_info_sub$transition_time)[idx_yes_next_pos_xG_event]

print(length(yes_next_pos_xG_event) / (length(yes_next_pos_xG_event) + length(no_next_pos_xG_event))) # estimate of percentage of time 

pp_off_loss = big_join_pp %>%
  arrange(game_id, season, game_seconds) %>%
  # filter(event_zone == "Def", game_strength_state == "4v5") %>%
  mutate(power_play_team = case_when(
    event_type == "FAC" & event_zone == "Def" & game_strength_state == "4v5" ~ ifelse(event_team == home_team, away_team, home_team),
    event_type == "FAC" & event_zone == "Off" & game_strength_state == "5v4" ~ event_team,
    TRUE ~ as.character(NA_real_)
  )) %>%
  mutate(power_play_team = zoo::na.locf(power_play_team, fromLast = F, na.rm = F)) %>%
  mutate(power_play_team_zone = case_when(
    power_play_team == event_team ~ event_zone,
    power_play_team != event_team ~ case_when(
      event_zone == "Off" ~ "Def",
      event_zone == "Def" ~ "Off",
      event_zone == "Neu" ~ "Neu"
    )
  )) %>%
  mutate(last_faceoff_time = case_when(
    event_type == "FAC" ~ game_seconds,
    TRUE ~ NA_real_
  )) %>%
  mutate(last_faceoff_time = zoo::na.locf(last_faceoff_time, fromLast = F, na.rm = F)) %>%
  mutate(last_faceoff_type = case_when(
    event_type == "FAC" & power_play_team == event_team ~ "PP_Win",
    event_type == "FAC" & power_play_team != event_team ~ "PP_Loss",
    TRUE ~ as.character(NA_real_)
  )) %>%
  mutate(last_faceoff_type = zoo::na.locf(last_faceoff_type, fromLast = F, na.rm = F)) %>%
  mutate(next_pos_xG_event = case_when(
    (event_type == "SHOT" | event_type == "GOAL") & pred_goal > 0 & power_play_team_zone == "Off" ~ game_seconds,
    TRUE ~ NA_real_
  )) %>%
  mutate(next_stoppage = case_when(
    (event_type %in% c("PGSTR", "PGEND", "FAC", "STOP")) ~ game_seconds,
    TRUE ~ NA_real_
  )) %>%
  mutate(next_pos_xG_event = zoo::na.locf(next_pos_xG_event, fromLast = T, na.rm = F)) %>%
  mutate(next_stoppage = zoo::na.locf(next_pos_xG_event, fromLast = T, na.rm = F))

pp_off_loss_summary = pp_off_loss %>%
  filter(game_strength_state %in% c("5v4", "4v5")) %>%
  #filter(event_type == "FAC") %>%
  mutate(stoppage_before_offense = ifelse(next_stoppage <= next_pos_xG_event, TRUE, FALSE)) %>%
  mutate(lost_time = next_pos_xG_event - game_seconds) %>%
  filter(lost_time >= 0) %>% # if lost_time is negative, we changed games
  select(game_date, season, game_id, game_seconds, event_team, power_play_team, power_play_team_zone, event_type, lost_time, game_seconds, next_pos_xG_event, stoppage_before_offense)

pp_off_win = big_join_pp %>%
  filter(event_zone == "Off", game_strength_state == "5v4")


# --- SEPARATE PP ANALYSIS ---

hr_df = read_csv("hockey_ref_skaters_2022-2023.csv") # went in and renamed two instances of PP to PPG and PPA, respectively
colnames(hr_df) = hr_df[1,]
hr_df = hr_df[-1,]
hr_df = hr_df %>%
  select(Player, Pos, PPG, PPA, G, A, PTS) %>%
  mutate(across(3:7, ~as.numeric(.)))
hr_df = hr_df %>%
  mutate(PPP = as.numeric(PPG) + as.numeric(PPA)) %>%
  mutate(PP_Points_Percentage = as.numeric(PPP) / as.numeric(PTS))

hr_df_def = hr_df %>%
  filter(Pos == "D") %>%
  arrange(desc(PTS))
