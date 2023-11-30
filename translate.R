library(tidyverse)

big_join = read_csv("updated_big_join_new.csv")
big_join = big_join %>%
  mutate(season = zoo::na.locf(season, fromLast = FALSE, na.rm = FALSE))
preds_from_model = readRDS("preds__temp.rds")
preds_sub = preds_from_model %>%
  select(game_id, season, event_player_1, event_player_2, game_seconds, event_type, FA_zone_time)
bj_sub = big_join %>%
  select(game_id, season, event_player_1, event_player_2, game_seconds, event_type)
faceoffs_sub = preds_sub %>%
  inner_join(bj_sub, by = c('event_player_1', 'event_player_2', 'game_seconds', 'event_type', 'game_id', 'season'))

bj_contextualized = big_join %>%
  left_join(faceoffs_sub, by = c('event_player_1', 'event_player_2', 'game_seconds', 'event_type', 'game_id', 'season'))

applicable_games = faceoffs_sub %>%
  distinct(game_id, season)

bj_contextualized = bj_contextualized %>%
  inner_join(applicable_games, by = c('game_id', 'season'))

bj_c = bj_contextualized %>%
  mutate(interim = dplyr::case_when(
    event_type == "FAC" ~ as.character(game_seconds),
    TRUE ~ as.character(NA_real_)
  )) %>%
  mutate(last_faceoff_gs = zoo::na.locf(interim, fromLast = FALSE, na.rm = FALSE)) %>%
  mutate(last_faceoff_gs = as.numeric(last_faceoff_gs)) %>%
  mutate(interim = dplyr::case_when(
    event_type == "FAC" ~ as.character(game_seconds + FA_zone_time),
    TRUE ~ as.character(NA_real_)
  )) %>%
  mutate(last_faceoff_faz = zoo::na.locf(interim, fromLast = FALSE, na.rm = FALSE)) %>%
  mutate(last_faceoff_faz = as.numeric(last_faceoff_faz)) %>%
  select(-interim)

bj = bj_c %>%
  mutate(FA_xG_status = ifelse(game_seconds >= last_faceoff_gs & game_seconds <= last_faceoff_faz, TRUE, FALSE)) %>%
  filter(last_faceoff_gs >= 0,
         last_faceoff_faz >= 0,
         game_seconds >= 0)

summary_stats = bj %>%
  arrange(season, game_id, game_seconds) %>%
  mutate(ZT_since_last_event = case_when(
    event_type == "FAC" ~ 0,
    game_seconds >= lag(game_seconds) ~ game_seconds - lag(game_seconds),
    TRUE ~ NA_real_,
  )) %>%
  filter(FA_xG_status == TRUE) %>%
  group_by(game_id, season, game_strength_state, event_zone, last_faceoff_gs, FA_xG_status) %>%
  summarize(cumulative_xG = sum(pred_goal, na.rm = TRUE),
            cumulative_ZT = sum(ZT_since_last_event, na.rm = TRUE),
            .groups = 'keep')

summary_figures = bj %>%
  arrange(season, game_id, game_seconds) %>%
  mutate(ZT_since_last_event = case_when(
    event_type == "FAC" ~ 0,
    game_seconds >= lag(game_seconds) ~ game_seconds - lag(game_seconds),
    TRUE ~ NA_real_,
  )) %>%
  filter(FA_xG_status == TRUE) %>%
  filter(event_zone %in% c("Off", "Neu", "Def")) %>%
  group_by(game_strength_state, event_zone) %>%
  summarize(cumulative_xG = sum(pred_goal, na.rm = TRUE),
            cumulative_ZT = sum(ZT_since_last_event, na.rm = TRUE),
            xG_per_ZT = sum(pred_goal, na.rm = TRUE) / sum(ZT_since_last_event, na.rm = TRUE),
            .groups = 'keep')

five_on_five = summary_stats %>%
  filter(game_strength_state == "5v5")

five_on_four = summary_stats %>%
  filter(game_strength_state == "5v4")

four_on_five = summary_stats %>%
  filter(game_strength_state == "4v5")

print(sum(five_on_five$cumulative_xG, na.rm = TRUE) / sum(five_on_five$cumulative_ZT, na.rm = TRUE))
print(sum(five_on_four$cumulative_xG, na.rm = TRUE) / sum(five_on_four$cumulative_ZT, na.rm = TRUE))
print(sum(four_on_five$cumulative_xG, na.rm = TRUE) / sum(four_on_five$cumulative_ZT, na.rm = TRUE))
