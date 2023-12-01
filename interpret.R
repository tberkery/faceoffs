library(tidyverse)

mod_info = bundle::unbundle(readRDS("initial_faceoff_model.rds"))
data = readRDS("final_dataset.rds")
data_test = readRDS("final_dataset_2022-2023.rds") %>%
  mutate(faceoff_winner = ifelse(home_zone == event_zone, event_player_2, event_player_1))

relevant_cols = colnames(data_test)

preds = predict(fitted_wf, data_test) %>%
  bind_cols(data_test)

# RAW RATING

winner_grouped = preds %>%
  select(event_player_1, season, event_type, .pred) %>%
  group_by(event_player_1, season, event_type) %>%
  summarize(sum_ZT = sum(.pred, na.rm = TRUE),
            mean_ZT = mean(.pred, na.rm = TRUE),
            median_ZT = median(.pred, na.rm = TRUE),
            sd_ZT = sd(.pred, na.rm = TRUE),
            count = n(),
            .groups = 'keep') %>%
  mutate(faceoff_type = "off_win")

loser_grouped = preds %>%
  select(event_player_2, season, event_type, .pred) %>%
  group_by(event_player_2, season, event_type) %>%
  summarize(sum_ZT = sum(.pred, na.rm = TRUE),
            mean_ZT = mean(.pred, na.rm = TRUE),
            median_ZT = median(.pred, na.rm = TRUE),
            sd_ZT = sd(.pred, na.rm = TRUE),
            count = n(),
            .groups = 'keep') %>%
  mutate(faceoff_type = "off_loss")


off_rating = rbind(winner_grouped, loser_grouped %>%
                     rename(event_player_1 = event_player_2)) %>%
  group_by(event_player_1, season) %>%
  summarize(sum_IZT = first(sum_ZT) - last(sum_ZT),
            mean_IZT = (first(mean_ZT) * first(count) - last(mean_ZT) * last(count)) / (first(count) + last(count)),
            count_off = first(count),
            count_def = last(count),
            count = first(count) + last(count),
            .groups = 'keep')

# GAM based adjustment

adjuster = mgcv::gam(data = off_rating, formula = mean_IZT ~ s(count_off) + s(count_def))
expectation_given_deployment = predict(adjuster, newdata = off_rating)
off_rating_adjusted = off_rating
off_rating_adjusted$xIZT = expectation_given_deployment
off_rating_adjusted = off_rating_adjusted %>%
  mutate(IZT_vs_exp = mean_IZT - xIZT)

# RATING RELATIVE TO EXPECTED

mean_win_ZT = mean(preds$.pred)
mean_lose_ZT = -1 * mean_win_ZT # just for cosmetics

preds_rel_expected = preds %>%
  mutate(.pred = .pred - mean_win_ZT)

winner_grouped_rel_expected  = preds_rel_expected  %>%
  select(event_player_1, season, event_type, .pred) %>%
  group_by(event_player_1, season, event_type) %>%
  summarize(sum_ZT = sum(.pred, na.rm = TRUE),
            mean_ZT = mean(.pred, na.rm = TRUE),
            median_ZT = median(.pred, na.rm = TRUE),
            sd_ZT = sd(.pred, na.rm = TRUE),
            count = n(),
            .groups = 'keep') %>%
  mutate(faceoff_type = "off_win")

loser_grouped_rel_expected  = preds_rel_expected  %>%
  select(event_player_2, season, event_type, .pred) %>%
  group_by(event_player_2, season, event_type) %>%
  summarize(sum_ZT = sum(.pred, na.rm = TRUE),
            mean_ZT = mean(.pred, na.rm = TRUE),
            median_ZT = median(.pred, na.rm = TRUE),
            sd_ZT = sd(.pred, na.rm = TRUE),
            count = n(),
            .groups = 'keep') %>%
  mutate(faceoff_type = "off_loss")

off_rating_rel_expected = rbind(winner_grouped_rel_expected, loser_grouped_rel_expected %>%
                     rename(event_player_1 = event_player_2)) %>%
  group_by(event_player_1, season) %>%
  summarize(sum_IZT = first(sum_ZT) - last(sum_ZT),
            mean_IZT = (first(mean_ZT) * first(count) - last(mean_ZT) * last(count)) / (first(count) + last(count)),
            count_off = first(count),
            count_def = last(count),
            count = first(count) + last(count),
            .groups = 'keep')

off_rating_rel_expected = off_rating_rel_expected %>%
  arrange(desc(mean_IZT))

# ADJUST EXPECTED BY OPPONENT

mean_count = round(mean(winner_grouped$count) + mean(loser_grouped$count))
median_ZT_val = median(preds$.pred)

loser_grouped_sub = loser_grouped %>%
  select(event_player_2, season, event_type, median_ZT, count) %>%
  mutate(median_ZT = case_when(
    count < mean_count ~ (count * median_ZT + (mean_count - count) * median_ZT_val) / mean_count,
    TRUE ~ median_ZT
  ))

winner_grouped_sub = winner_grouped %>%
  select(event_player_1, season, event_type, median_ZT, count) %>%
  mutate(median_ZT = case_when(
    count < mean_count ~ (count * median_ZT + (mean_count - count) * median_ZT_val) / mean_count,
    TRUE ~ median_ZT
  ))

winner_grouped_contextualized = preds %>%
  select(event_player_1, event_player_2, season, event_type, .pred) %>%
  inner_join(loser_grouped_sub, by = c('event_player_2', 'season', 'event_type'), suffix = c("", "_opp")) %>%
  mutate(.pred = .pred - median_ZT) %>%
  group_by(event_player_1, season, event_type) %>%
  summarize(sum_ZT = sum(.pred, na.rm = TRUE),
            mean_ZT = mean(.pred, na.rm = TRUE),
            median_ZT = median(.pred, na.rm = TRUE),
            sd_ZT = sd(.pred, na.rm = TRUE),
            count = n(),
            .groups = 'keep') %>%
  mutate(faceoff_type = "off_win")

loser_grouped_contextualized = preds %>%
  select(event_player_1, event_player_2, season, event_type, .pred) %>%
  inner_join(winner_grouped_sub, by = c('event_player_1', 'season', 'event_type'), suffix = c("", "_opp")) %>%
  mutate(.pred = median_ZT - .pred) %>% # note the order flip here
  group_by(event_player_2, season, event_type) %>%
  summarize(sum_ZT = sum(.pred, na.rm = TRUE),
            mean_ZT = mean(.pred, na.rm = TRUE),
            median_ZT = median(.pred, na.rm = TRUE),
            sd_ZT = sd(.pred, na.rm = TRUE),
            count = n(),
            .groups = 'keep') %>%
  mutate(faceoff_type = "off_win")

off_rating_contextualized = rbind(winner_grouped_contextualized, loser_grouped_contextualized %>%
                     rename(event_player_1 = event_player_2)) %>%
  group_by(event_player_1, season) %>%
  summarize(sum_IZT = first(sum_ZT) - last(sum_ZT),
            mean_IZT = (first(mean_ZT) * first(count) - last(mean_ZT) * last(count)) / (first(count) + last(count)),
            median_IZT = (first(median_ZT) * first(count) - last(median_ZT) * last(count)) / (first(count) + last(count)),
            count_off = first(count),
            count_def = last(count),
            count = first(count) + last(count),
            .groups = 'keep') %>%
  select(-sum_IZT, -mean_IZT) %>%
  mutate(median_IZT = case_when(
    count < mean_count ~ (count ^ 2 * median_IZT + (mean_count - count) ^ 2 * 0) / (count ^ 2 + (mean_count - count)^2),
    TRUE ~ median_IZT
  ))

