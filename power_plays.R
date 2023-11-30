library(tidyverse)

big_join = read_csv("updated_big_join_new.csv")

big_join_pp = big_join %>%
  filter(game_strength_state %in% c("5v4", "4v5"))

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
  filter(event_type == "FAC") %>%
  mutate(stoppage_before_offense = ifelse(next_stoppage <= next_pos_xG_event, TRUE, FALSE)) %>%
  mutate(lost_time = next_pos_xG_event - game_seconds) %>%
  filter(lost_time >= 0) %>% # if lost_time is negative, we changed games
  select(game_date, season, game_id, event_type, lost_time, game_seconds, next_pos_xG_event, stoppage_before_offense)

pp_off_win = big_join_pp %>%
  filter(event_zone == "Off", game_strength_state == "5v4")
