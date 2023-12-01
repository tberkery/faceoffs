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

summary_players = bj %>%
  arrange(season, game_id, game_seconds) %>%
  mutate(ZT_since_last_event = case_when(
    event_type == "FAC" ~ 0,
    game_seconds >= lag(game_seconds) ~ game_seconds - lag(game_seconds),
    TRUE ~ NA_real_,
  )) %>%
  filter(FA_xG_status == TRUE) %>%
  filter(event_zone %in% c("Off", "Neu", "Def")) %>%
  group_by(event_player_1, game_strength_state, event_zone) %>%
  summarize(cumulative_xG = sum(pred_goal, na.rm = TRUE),
            cumulative_ZT = sum(ZT_since_last_event, na.rm = TRUE),
            xG_per_ZT = sum(pred_goal, na.rm = TRUE) / sum(ZT_since_last_event, na.rm = TRUE),
            .groups = 'keep')

player_scoring_rates = summary_players %>%
  filter(game_strength_state == "5v5",
         event_zone == "Off")

five_on_five = summary_stats %>%
  filter(game_strength_state == "5v5")

five_on_four = summary_stats %>%
  filter(game_strength_state == "5v4")

four_on_five = summary_stats %>%
  filter(game_strength_state == "4v5")

print(sum(five_on_five$cumulative_xG, na.rm = TRUE) / sum(five_on_five$cumulative_ZT, na.rm = TRUE))
print(sum(five_on_four$cumulative_xG, na.rm = TRUE) / sum(five_on_four$cumulative_ZT, na.rm = TRUE))
print(sum(four_on_five$cumulative_xG, na.rm = TRUE) / sum(four_on_five$cumulative_ZT, na.rm = TRUE))

off_rating_contextualized = off_rating_contextualized %>% 
  mutate(median_IXG = median_IZT * 0.0044171540) %>% # used to use 0.0044171540
  mutate(plus_one = median_IXG * 82 * 1,
         plus_two = median_IXG * 82 * 2,
         plus_three = median_IXG * 82 * 3,
         plus_four = median_IXG * 82 * 4,
         plus_five = median_IXG * 82 * 5,
         plus_six = median_IXG * 82 * 6)
         
visual_data = off_rating_contextualized %>% 
  filter(count >= 5) %>%
  select(event_player_1, season, median_IZT, median_IXG) %>%
  rename(Player = event_player_1,
         Season = season,
         IZT = median_IZT,
         NXG = median_IXG)
visual_data %>% write_csv("table_1_data.csv")

additional_faceoff_win_implications = off_rating_contextualized %>%
  filter(count >= 5) %>%
  select(event_player_1, season, contains("plus")) %>%
  rename(Player = event_player_1,
         Season = season,
         `+1` = plus_one,
         `+2` = plus_two,
         `+3` = plus_three,
         `+4` = plus_four,
         `+5` = plus_five,
         `+6` = plus_six) %>%
  arrange(desc(`+1`))
additional_faceoff_win_implications %>% write_csv("table_2_data.csv")

pbp_2021 = read_csv("EH_pbp_query_20212022.csv")
fac_2021 = pbp_2021 %>%
  filter(event_type == "FAC")

fac_deploy_summary = fac_2021 %>%
  select(event_zone, event_player_1, event_player_2) %>%
  mutate(player_1_zone = event_zone,
         player_2_zone = case_when(
           event_zone == "Off" ~ "Def",
           event_zone == "Def" ~ "Off",
           event_zone == "Neu" ~ "Neu"
         ))

fac_deploy_winner = fac_deploy_summary %>%
  select(event_player_1, player_1_zone) %>%
  mutate(outcome = "win") %>%
  rename(event_player = event_player_1,
         effective_event_zone = player_1_zone)

fac_deploy_loser = fac_deploy_summary %>%
  select(event_player_2, player_2_zone) %>%
  mutate(outcome = "loss") %>%
  rename(event_player = event_player_2,
         effective_event_zone = player_2_zone)

fac_deploy = fac_deploy_winner # now we only want to measure faceoff wins b/c no incremental value gained on faceoff loss #fac_deploy = rbind(fac_deploy_winner, fac_deploy_loser)

deployment_summary = fac_deploy %>%
  group_by(event_player, effective_event_zone) %>%
  summarize(count = n(), .groups = 'keep') %>%
  pivot_wider(names_from = c(effective_event_zone),
              values_from = c(count)) %>%
  select(-`NA`) %>%
  mutate(across(where(is.numeric), ~replace_na(., 0)))
deployment_summary %>% write_csv("faceoff_usage_by_situation_2021.csv")  

off_rating_contextualized_more = off_rating_contextualized %>%
  inner_join(deployment_summary, by = c('event_player_1' = 'event_player'))

off_rating_contextualized_more = off_rating_contextualized_more %>%
  mutate(seasonal_NXG = (Off + Def) * 0.0044171540 * median_IZT)

off_rating_contextualized_more %>% 
  write_csv("seasonal_offensive_rating_NXG.csv")

sorted_ratings = off_rating_contextualized_more %>%
  filter(count >= 5) %>%
  arrange(desc(median_IXG))

top_ratings = sorted_ratings %>%
  head(36)

focus_list = as.vector(top_ratings$event_player_1)

top_seasonal_nxg_impact = off_rating_contextualized_more %>%
  filter(event_player_1 %in% focus_list) %>%
  arrange(desc(seasonal_NXG))

top_seasonal_nxg_impact_formatted_for_excel = top_seasonal_nxg_impact %>%
  arrange(desc(median_IZT)) %>%
  select(event_player_1, seasonal_NXG)

top_seasonal_nxg_impact_formatted_for_excel %>% write_csv("top_seasonal_nxg_impact_formatted_for_excel.csv")
