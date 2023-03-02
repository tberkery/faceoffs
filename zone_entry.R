library(tidyverse)

create_zone_entries = function(pbp, games) {
  pbp_cols = colnames(pbp)
  pbp = pbp %>%
    filter(game_period > 0) %>% # currently including overtime(s)
    mutate(full_periods_elapsed = ifelse(game_period == 1, 0, game_period - 1)) %>%
    mutate(minutes_elapsed_in_period = 20 - 1 - as.numeric(substr(clock_time, 1, 2))) %>%
    mutate(seconds_elapsed_in_minute = 60 - as.numeric(substr(clock_time, 4, 5))) %>%
    mutate(minutes_elapsed_in_period = case_when(
      clock_time == '20:00' ~ 0,
      seconds_elapsed_in_minute == 60 ~ minutes_elapsed_in_period + 1, # if clock_time is 13:00, then 7:00 has elapsed. w/o this line, minutes elapsed would incorrectly be 6
      TRUE ~ minutes_elapsed_in_period
    )) %>%
    mutate(seconds_elapsed_in_minute = case_when(
      seconds_elapsed_in_minute == 60 ~ 0, # same edge case as prev comment
      TRUE ~ seconds_elapsed_in_minute
    )) %>%
    mutate(game_seconds = full_periods_elapsed * 20 * 60 + minutes_elapsed_in_period * 60 + seconds_elapsed_in_minute) %>%
    select(-c(full_periods_elapsed, minutes_elapsed_in_period, seconds_elapsed_in_minute))
  games = games %>%
    mutate(game_date = substr(effective_game_date, 1, 10))
  zone_entries = games %>%
    rename(game_period = Period,
           clock_time = Time) %>%
    filter(game_period == '1' | game_period == '2' | game_period == '3') %>% # Remark: will omit overtime(s)/shootout
    mutate(game_period = as.numeric(game_period))
  zone_entries = zone_entries %>%
    mutate(full_periods_elapsed = ifelse(game_period == 1, 0, game_period - 1)) %>%
    mutate(minutes_elapsed_in_period = 20 - 1 - as.numeric(substr(clock_time, 1, 2))) %>%
    mutate(seconds_elapsed_in_minute = 60 - as.numeric(substr(clock_time, 4, 5))) %>%
    mutate(minutes_elapsed_in_period = case_when(
      clock_time == '20:00' ~ 0,
      seconds_elapsed_in_minute == 60 ~ minutes_elapsed_in_period + 1, # if clock_time is 13:00, then 7:00 has elapsed. w/o this line, minutes elapsed would incorrectly be 6
      TRUE ~ minutes_elapsed_in_period
    )) %>%
    mutate(seconds_elapsed_in_minute = case_when(
      seconds_elapsed_in_minute == 60 ~ 0, # same edge case as prev comment
      TRUE ~ seconds_elapsed_in_minute
    )) %>%
    mutate(game_seconds = full_periods_elapsed * 20 * 60 + minutes_elapsed_in_period * 60 + seconds_elapsed_in_minute) %>%
    select(-c(full_periods_elapsed, minutes_elapsed_in_period, seconds_elapsed_in_minute)) %>%
    mutate(game_date = substr(game_date, 1, 10),
           event_type = 'ZONE_ENTRY',
           event_description = paste0(Entry.type, " entry by ", Entry.by, " against ", Defended.by),
           event_detail = NA,
           event_zone = 'Off', # TODO: Verify that every zone change is into Offensive zone
           event_team = substr(Entry.by, -3, -1),
           event_player_1 = Entry.by,
           event_player_2 = Defended.by,
           event_player_3 = NA, # ignore for now
           event_length = NA,
           num_on = NA,
           num_off = NA,
           players_on = NA,
           players_off = NA,
           home_on_1 = NA,
           home_on_2 = NA,
           home_on_3 = NA,
           home_on_4 = NA,
           home_on_5 = NA,
           home_on_6 = NA,
           home_on_7 = NA,
           away_on_1 = NA,
           away_on_2 = NA,
           away_on_3 = NA,
           away_on_4 = NA,
           away_on_5 = NA,
           away_on_6 = NA,
           away_on_7 = NA,
           home_goalie = NA,
           away_goalie = NA,
           home_score = NA,
           away_score = NA,
           game_score_state = NA,
           game_strength_State = NA,
           home_zone = NA,
           pbp_distance = NA, # TODO: figure out if you need and how to do
           event_distance = NA, # Same as prev
           event_angle = NA, # Same as prev
           home_zonestart = NA,
           face_index = NA, # Same as prev
           pen_index = NA, # Same as prev
           shift_index = NA,
           pred_goal = NA, # Same as prev
           home_skaters = NA,
           away_skaters = NA,
           is_pp = NA,
           season = NA,
           game_id = NA,
           session = NA,
           event_index = NA,
           coords_x = NA,
           coords_y = NA,
           game_strength_state = NA
           ) # %>%
    # filter((Team.strength == 5 & Opp.strength == 4) |
    #       (Team.strength == 4 & Opp.strength == 5))
  
  zone_entries = zone_entries %>%
    select(all_of(pbp_cols))
  
  #pbp_with_zone_entries = pbp %>%
  #  mutate(game_date = substr(game_date, 1, 10)) %>%
  #  rbind(zone_entries)
  
  #pbp_with_zone_entries = pbp_with_zone_entries %>%
  #  mutate(game_id_numeric = as.double(game_id))
  #pbp_with_zone_entries = pbp_with_zone_entries %>%
  #  mutate(game_id = fill(game_id_numeric)) %>% # Note that a Sznajder event is never the first chronological event of a game
  #  arrange(game_id, game_seconds) %>%
  #  select(-game_id_numeric)
  
  # mutate_fields = c('players_on', 'players_off', 'home_on_1', 'home_on_2', 'home_on_3',
  #                   'home_on_4', 'home_on_5', 'home_on_6', 'home_on_7', 'away_on_1', 
  #                   'away_on_2', 'away_on_3', 'away_on_4', 'away_on_5', 'away_on_6',
  #                   'away_on_7', 'home_goalie', 'away_goalie', 'home_team', 'away_team',
  #                   'home_skaters', 'away_skaters', 'home_score', 'away_score',
  #                   'game_score_state', 'game_strength_state', 'home_zone',
  #                   'shift_index', 'season', 'game_id', 'session', 'event_index',
  #                   'game_strength_state', 'event_team')
  # 
  # pbp_with_zone_entries = pbp_with_zone_entries %>%
  #   fill(is_pp) %>%
  #   mutate(across(all_of(mutate_fields), ~ifelse(event_type == 'ZONE_ENTRY', lag(., 1), .)))

  # pbp_with_zone_entries = pbp_with_zone_entries %>%
  #   #mutate(across(all_of(vecs), ~ifelse(is.na(.), lag(., 1), .)))
  #   mutate(
  #     players_on = ifelse(event_type == 'ZONE_ENTRY', lag(players_on, 1), players_on),
  #     players_off = ifelse(event_type == 'ZONE_ENTRY', lag(players_off, 1), players_off),
  #     home_on_1 = ifelse(event_type == 'ZONE_ENTRY', lag(home_on_1, 1), home_on_1),
  #     home_on_2 = ifelse(event_type == 'ZONE_ENTRY', lag(home_on_2, 1), home_on_2),
  #     home_on_3 = ifelse(event_type == 'ZONE_ENTRY', lag(home_on_3, 1), home_on_3),
  #     home_on_4 = ifelse(event_type == 'ZONE_ENTRY', lag(home_on_4, 1), home_on_4),
  #     home_on_5 = ifelse(event_type == 'ZONE_ENTRY', lag(home_on_5, 1), home_on_5),
  #     home_on_6 = ifelse(event_type == 'ZONE_ENTRY', lag(home_on_6, 1), home_on_6),
  #     home_on_7 = ifelse(event_type == 'ZONE_ENTRY', lag(home_on_7, 1), home_on_7),
  #     away_on_1 = ifelse(event_type == 'ZONE_ENTRY', lag(away_on_1, 1), away_on_1),
  #     away_on_2 = ifelse(event_type == 'ZONE_ENTRY', lag(away_on_2, 1), away_on_2),
  #     away_on_3 = ifelse(event_type == 'ZONE_ENTRY', lag(away_on_3, 1), away_on_3),
  #     away_on_4 = ifelse(event_type == 'ZONE_ENTRY', lag(away_on_4, 1), away_on_4),
  #     away_on_5 = ifelse(event_type == 'ZONE_ENTRY', lag(away_on_5, 1), away_on_5),
  #     away_on_6 = ifelse(event_type == 'ZONE_ENTRY', lag(away_on_6, 1), away_on_6),
  #     away_on_7 = ifelse(event_type == 'ZONE_ENTRY', lag(away_on_7, 1), away_on_7),
  #     home_goalie = ifelse(event_type == 'ZONE_ENTRY', lag(home_goalie, 1), home_goalie),
  #     away_goalie = ifelse(event_type == 'ZONE_ENTRY', lag(away_goalie, 1), away_goalie),
  #     home_team = ifelse(event_type == 'ZONE_ENTRY', lag(home_team, 1), home_team),
  #     away_team = ifelse(event_type == 'ZONE_ENTRY', lag(away_team, 1), away_team),
  #     home_skaters = ifelse(event_type == 'ZONE_ENTRY', lag(home_skaters, 1), home_skaters),
  #     away_skaters = ifelse(event_type == 'ZONE_ENTRY', lag(away_skaters, 1), away_skaters),
  #     home_score = ifelse(event_type == 'ZONE_ENTRY', lag(home_score, 1), home_score),
  #     away_score = ifelse(event_type == 'ZONE_ENTRY', lag(away_score, 1), away_score),
  #     game_score_state = ifelse(event_type == 'ZONE_ENTRY', lag(game_score_state, 1), game_score_state),
  #     game_strength_state = ifelse(event_type == 'ZONE_ENTRY', lag(game_strength_state, 1), game_strength_state),
  #     home_zone = ifelse(event_type == 'ZONE_ENTRY', lag(home_zone, 1), home_zone),
  #     shift_index = ifelse(event_type == 'ZONE_ENTRY', lag(shift_index, 1), shift_index),
  #     is_pp = ifelse(event_type == 'ZONE_ENTRY', lag(is_pp, 1), is_pp),
  #     season = ifelse(event_type == 'ZONE_ENTRY', lag(season, 1), season),
  #     game_id = ifelse(event_type == 'ZONE_ENTRY', lag(game_id, 1), game_id),
  #     game_date = ifelse(event_type == 'ZONE_ENTRY', lag(game_date, 1), game_date),
  #     session = ifelse(event_type == 'ZONE_ENTRY', lag(session, 1), session),
  #     event_index = ifelse(event_type == 'ZONE_ENTRY', lag(event_index, 1), event_index),
  #     season = ifelse(event_type == 'ZONE_ENTRY', lag(season, 1), season),
  #     game_id = ifelse(event_type == 'ZONE_ENTRY', lag(game_id, 1), game_id),
  #     session = ifelse(event_type == 'ZONE_ENTRY', lag(session, 1), session),
  #     event_index = ifelse(event_type == 'ZONE_ENTRY', lag(event_index, 1) + 0.1, event_index), # Add 0.1 to keep distinct
  #     game_strength_state = ifelse(event_type == 'ZONE_ENTRY', lag(game_strength_state, 1), game_strength_state),
  #     event_team = ifelse(event_type == 'ZONE_ENTRY', substr(event_player_1, -3, -1), event_player_1) # inner ifelse logic handles single digit numbers (e.g. 8PIT vs. 18PIT)
  #   )
  return(zone_entries)
  #return(pbp_with_zone_entries)
}

create_zone_exits = function(pbp, games_zone_exits) {
  pbp_cols = colnames(pbp)
  pbp = pbp %>%
    mutate(full_periods_elapsed = ifelse(game_period == 1, 0, game_period - 1)) %>%
    mutate(minutes_elapsed_in_period = 20 - 1 - as.numeric(substr(clock_time, 1, 2))) %>%
    mutate(seconds_elapsed_in_minute = 60 - as.numeric(substr(clock_time, 4, 5))) %>%
    mutate(minutes_elapsed_in_period = case_when(
      clock_time == '20:00' ~ 0,
      seconds_elapsed_in_minute == 60 ~ minutes_elapsed_in_period + 1, # if clock_time is 13:00, then 7:00 has elapsed. w/o this line, minutes elapsed would incorrectly be 6
      TRUE ~ minutes_elapsed_in_period
    )) %>%
    mutate(seconds_elapsed_in_minute = case_when(
      seconds_elapsed_in_minute == 60 ~ 0, # same edge case as prev comment
      TRUE ~ seconds_elapsed_in_minute
    )) %>%
    mutate(game_seconds = full_periods_elapsed * 20 * 60 + minutes_elapsed_in_period * 60 + seconds_elapsed_in_minute) %>%
    select(-c(full_periods_elapsed, minutes_elapsed_in_period, seconds_elapsed_in_minute))
  games_zone_exits = games_zone_exits %>%
    mutate(game_date = substr(effective_game_date, 1, 10))
  zone_exits = games_zone_exits %>%
    rename(game_period = Period,
           clock_time = Time) %>%
    filter(game_period == '1' | game_period == '2' | game_period == '3') %>% # Remark: will omit overtime(s)/shootout
    mutate(game_period = as.numeric(game_period))
  zone_exits = zone_exits %>%
    mutate(full_periods_elapsed = ifelse(game_period == 1, 0, game_period - 1)) %>%
    mutate(minutes_elapsed_in_period = 20 - 1 - as.numeric(substr(clock_time, 1, 2))) %>%
    mutate(seconds_elapsed_in_minute = 60 - as.numeric(substr(clock_time, 4, 5))) %>%
    mutate(minutes_elapsed_in_period = case_when(
      clock_time == '20:00' ~ 0,
      seconds_elapsed_in_minute == 60 ~ minutes_elapsed_in_period + 1, # if clock_time is 13:00, then 7:00 has elapsed. w/o this line, minutes elapsed would incorrectly be 6
      TRUE ~ minutes_elapsed_in_period
    )) %>%
    mutate(seconds_elapsed_in_minute = case_when(
      seconds_elapsed_in_minute == 60 ~ 0, # same edge case as prev comment
      TRUE ~ seconds_elapsed_in_minute
    )) %>%
    mutate(game_seconds = full_periods_elapsed * 20 * 60 + minutes_elapsed_in_period * 60 + seconds_elapsed_in_minute) %>%
    select(-c(full_periods_elapsed, minutes_elapsed_in_period, seconds_elapsed_in_minute)) %>%
    mutate(game_date = substr(game_date, 1, 10),
           event_type = ifelse(`Entry?` == 'Y', 'ZONE_EXIT', 'FAILED_ZONE_EXIT'),
           event_description = paste0("Exit attempt by ",
                                      Attempt, " with pass target ", Pass.Target, " resulting in ", Result,
                                      " in ", Direction, " direction"),
           event_detail = NA,
           event_zone = 'Off', # TODO: Verify that every zone change is into Offensive zone
           event_team = substr(`Entry?`, -3, -1),
           event_player_1 = `Entry?`,
           event_player_2 = `Pressured?`,
           event_player_3 = NA, # ignore for now
           event_length = NA,
           num_on = NA,
           num_off = NA,
           players_on = NA,
           players_off = NA,
           home_on_1 = NA,
           home_on_2 = NA,
           home_on_3 = NA,
           home_on_4 = NA,
           home_on_5 = NA,
           home_on_6 = NA,
           home_on_7 = NA,
           away_on_1 = NA,
           away_on_2 = NA,
           away_on_3 = NA,
           away_on_4 = NA,
           away_on_5 = NA,
           away_on_6 = NA,
           away_on_7 = NA,
           home_goalie = NA,
           away_goalie = NA,
           home_score = NA,
           away_score = NA,
           game_score_state = NA,
           game_strength_State = NA,
           home_zone = NA,
           pbp_distance = NA, # TODO: figure out if you need and how to do
           event_distance = NA, # Same as prev
           event_angle = NA, # Same as prev
           home_zonestart = NA,
           face_index = NA, # Same as prev
           pen_index = NA, # Same as prev
           shift_index = NA,
           pred_goal = NA, # Same as prev
           home_skaters = NA,
           away_skaters = NA,
           is_pp = NA,
           season = NA,
           game_id = NA,
           session = NA,
           event_index = NA,
           coords_x = NA,
           coords_y = NA,
           game_strength_state = NA
    ) #%>%
    #filter((Team.strength == 5 & Opp.strength == 4) |
    #         (Team.strength == 4 & Opp.strength == 5))
  
  zone_exits = zone_exits %>%
    select(all_of(pbp_cols))
  
  return(zone_exits)
  
  #pbp_pp_start_and_end = pbp %>%
  #  filter(is_pp == TRUE & (lag(is_pp, 1) == FALSE | lead(is_pp, 1) == FALSE))
  
  # pbp_with_zone_exits = pbp %>%
  #   mutate(game_date = substr(game_date, 1, 10)) %>%
  #   rbind(zone_exits)
  # 
  # pbp_with_zone_exits = pbp_with_zone_exits %>%
  #   arrange(game_id, game_seconds)
  # 
  # mutate_fields = c('players_on', 'players_off', 'home_on_1', 'home_on_2', 'home_on_3',
  #                   'home_on_4', 'home_on_5', 'home_on_6', 'home_on_7', 'away_on_1', 
  #                   'away_on_2', 'away_on_3', 'away_on_4', 'away_on_5', 'away_on_6',
  #                   'away_on_7', 'home_goalie', 'away_goalie', 'home_team', 'away_team',
  #                   'home_skaters', 'away_skaters', 'home_score', 'away_score',
  #                   'game_score_state', 'game_strength_state', 'home_zone',
  #                   'shift_index', 'season', 'game_id', 'session', 'event_index',
  #                   'game_strength_state', 'event_team')
  # 
  # pbp_with_zone_exits = pbp_with_zone_exits %>%
  #   mutate(across(all_of(mutate_fields), ~ifelse(event_type == 'ZONE_EXIT', lag(., 1), .))) %>%
  #   fill(is_pp)
  # 
  # return(pbp_with_zone_exits)
}
