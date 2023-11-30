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
    event_type == "FAC" ~ as.character(FA_zone_time),
    TRUE ~ as.character(NA_real_)
  )) %>%
  mutate(last_faceoff_faz = zoo::na.locf(interim, fromLast = FALSE, na.rm = FALSE)) %>%
  mutate(last_faceoff_faz = as.numeric(last_faceoff_faz)) %>%
  select(-interim)

