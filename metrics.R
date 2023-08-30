library(tidyverse)
library(stringr)

# Centralized function for calling all functions relating to generating metrics/objective labels
seed_metrics = function(big_join) {
  big_join_objective = big_join %>% 
    remove_faceoff_zone_entries() %>%
    identify_last_faceoff_winner() %>%
    populate_event_team_for_zone_changes() %>%
    extend_over_zone_changes() %>%
    identify_event_team_relative_to_faceoff() %>%
    identify_zone_change_team_relative_to_faceoff() %>%
    identify_event_team_positioning_at_last_faceoff()
  FA_zone_time = big_join_objective %>%
    compute_FA_zone_time()
  
  off_win_subset = FA_zone_time %>%
    drop_na(is_FA, end_FA, start_FA) %>%
    filter(event_type == "FAC", last_faceoff_winner_faceoff_zone == "Off") %>%
    mutate(FA_zone_time = replace_na(FA_zone_time, 0))
  source("parse_roles.R")
  mega_dict = assemble_stats()
  role_encoded = off_win_subset %>%
    identify_roles(mega_dict)
  prep_for_join = role_encoded %>%
    rowwise() %>%
    mutate(Win_Players = ifelse(
        home_team == event_team,
        str_c(sort(c(home_on_1, home_on_2, home_on_3, home_on_4, home_on_5, home_on_6)), collapse = ", "),
        str_c(sort(c(away_on_1, away_on_2, away_on_3, away_on_4, away_on_5, away_on_6)), collapse = ", ")
      ),
      Lose_Players = ifelse(
        home_team != event_team,
        str_c(sort(c(home_on_1, home_on_2, home_on_3, home_on_4, home_on_5, home_on_6)), collapse = ", "),
        str_c(sort(c(away_on_1, away_on_2, away_on_3, away_on_4, away_on_5, away_on_6)), collapse = ", ")
      )) %>%
    ungroup() %>%
    select(-starts_with("home_on_"), -starts_with("away_on_"))
  
  line_stats = read_csv("training_data_all_offensive_offensive_2023.csv") %>%
    group_by(season, Win_Players, Lose_Players) %>%
    mutate(across(where(is.numeric), ~max(., na.rm = TRUE))) %>%
    distinct(season, Win_Players, Lose_Players, .keep_all = TRUE)
  joined = prep_for_join %>%
    inner_join(line_stats, by = c('season', 'Win_Players', 'Lose_Players'))
  return(prep_for_join)
}

# Function to extract three characters following the first occurrence of a number
extract_three_chars <- function(text) {
  result <- str_extract(text, "(?<=\\d{1,2})\\w{4}")
  if (is.na(result)) {
    return(NA)
  } else {
    result = remove_non_capital_letters(result)
    return(result)
  }
}

# Function for stripping characters that cannot be part of valid team abbreviation
remove_non_capital_letters <- function(text) {
  # Use gsub to remove spaces and numbers
  cleaned_text <- gsub("[^A-Z]", "", text)
  return(cleaned_text)
}

# Fill NAs for rows based on most recent EARLIER non-NA value.
extend_over_zone_changes = function(df) {
  cols = c("season", "game_id", "game_date", "game_period", "num_on", "num_off",
           "players_on", "players_off", "home_on_1", "home_on_2", "home_on_3",
           "home_on_4", "home_on_5", "home_on_6", "away_on_1", "away_on_2",
           "away_on_3", "away_on_4", "away_on_5", "away_on_6", "home_goalie",
           "away_goalie", "home_team", "away_team", "home_skaters", "away_skaters",
           "home_score", "away_score", "game_score_state", "game_strength_state",
           "teams", "home_zonestart", "face_index", "pen_index", "shift_index")
  for (col in cols) {
    df = df %>%
      fill(!!sym(col), .direction = "down")
  }
  return(df)
}

remove_faceoff_zone_entries = function(df) {
  df = df %>%
    filter(!(event_type == "ZONE_ENTRY" & lag(event_type, 1) == "FAC" & lag(game_seconds, 1) == game_seconds))
  return(df)
}

# Populate event_team for custom created ZONE_ENTRY and ZONE_EXIT listings
# from Corey Sznajder data
extract_team_from_zone_change_record = function(df) {
  df$zone_change_team <- sapply(df$event_description, extract_three_chars)
  return(df)
}

# Note for every record who won the last faceoff.
identify_last_faceoff_winner = function(df) {
  fo_df = df %>%
    mutate(last_faceoff_winner = NA) %>%
    mutate(last_faceoff_winner = ifelse(
      event_type == "FAC", event_team, NA
    )) %>%
    fill(last_faceoff_winner, .direction = "down") %>%
    mutate(last_faceoff_winner_home = ifelse(last_faceoff_winner == home_team, TRUE, FALSE)) %>%
    mutate(last_faceoff_winner_zone = ifelse(event_type == "FAC", str_sub(event_description, 9, 11), NA)) %>%
    fill(last_faceoff_winner_zone, .direction = "down")
  return(fo_df)
}

# Identify event perpetrator in terms of WINNER or LOSER of last faceoff.
identify_event_team_relative_to_faceoff = function(df) {
  fo_df = df %>%
    mutate(event_team_relative_to_faceoff = ifelse(last_faceoff_winner == event_team, "WINNER", "LOSER"))
  return(fo_df)
}


# Alternate way to populate event_team for custom created ZONE_ENTRY and ZONE_EXIT listings
# from Corey Sznajder data
populate_event_team_for_zone_changes = function(df) {
  fo_df = df
  fo_df$zone_change_event_team = sapply(df$event_description, extract_three_chars)
  fo_df = fo_df %>%
    mutate(event_team = case_when(
      event_type == "ZONE_ENTRY" | event_type == "ZONE_EXIT" ~ zone_change_event_team,
      TRUE ~ event_team
    )) %>%
    ungroup() %>%
    select(-zone_change_event_team)
  return(fo_df)
}

# Identify team perpetrating zone change as either last faceoff winner of last faceoff loser.
identify_zone_change_team_relative_to_faceoff = function(df) {
  # Assumes you have already called populate_event_team_for_zone_change, meaning you should always
  # have an event_team.
  fo_df = df
  fo_df = fo_df %>%
    mutate(event_zone_relative_to_faceoff_winner = case_when(
      event_team == last_faceoff_winner ~ event_zone,
      event_team != last_faceoff_winner ~ case_when(
        event_zone == "Off" ~ "Def",
        event_zone == "Def" ~ "Off",
        TRUE ~ event_zone # includes neutral zone case ("Neu")
      )
    ))
  
}

identify_event_team_positioning_at_last_faceoff = function(df) {
  fo_df = df %>%
    mutate(last_faceoff_winner_faceoff_zone = ifelse(event_type == "FAC", event_zone, NA)) %>%
    fill(last_faceoff_winner_faceoff_zone, .direction = "down") %>%
    mutate(event_team_positioning_last_faceoff = 
             ifelse(event_team == last_faceoff_winner, 
                    case_when(
                      last_faceoff_winner_faceoff_zone == "Off" ~ "OFFENSIVE",
                      last_faceoff_winner_faceoff_zone == "Def" ~ "DEFENSIVE",
                      last_faceoff_winner_faceoff_zone == "Neu" ~ "NEUTRAL"
                    ), 
                    case_when(
                      last_faceoff_winner_faceoff_zone == "Off" ~ "DEFENSIVE",
                      last_faceoff_winner_faceoff_zone == "Def" ~ "OFFENSIVE",
                      last_faceoff_winner_faceoff_zone == "Neu" ~ "NEUTRAL"
                    )
                  )
          ) %>%
    mutate(last_faceoff_loser_faceoff_zone = ifelse(event_type == "FAC", case_when(
      event_zone == "Off" ~ "Def",
      event_zone == "Def" ~ "Off",
      event_zone == "Neu" ~ "Neu"
    ), NA)) %>%
    fill(last_faceoff_loser_faceoff_zone, .direction = "down")
  
  return(fo_df)
}

compute_FA_zone_time = function(df) {
  
  # We have a zone change if any of the following occur (not mutually exclusive)
  # - If offensive zone win... time before next stoppage or detected ZONE_ENTRY or ZONE_EXIT
  # - If not offensive zone win... by definition 0 FA zone time.

  df = df %>%
    group_by(game_id, season) %>%
    mutate(start_FA = ifelse(event_type == "FAC" & event_zone != "Neu", game_seconds, NA)) %>%
    fill(start_FA, .direction = "down") %>%
    mutate(next_stoppage = ifelse(
      event_type %in% c("STOP", "GOAL", "PGSTR", "PGEND", "PENL", "PEND", "GEND"), game_seconds, NA)) %>% # excluding FAC
    fill(next_stoppage, .direction = "up") %>%
    mutate(next_neutral_zone_event = ifelse(event_zone == "Neu", game_seconds, NA)) %>%
    fill(next_neutral_zone_event, .direction = "up") %>%
    mutate(next_zone_exit = ifelse(event_type == "ZONE_EXIT", game_seconds, NA)) %>%
    fill(next_zone_exit, .direction = "up") %>%
    mutate(across(c(next_stoppage, next_neutral_zone_event, next_zone_exit), ~ifelse(event_type == "FAC", ., NA))) %>%
    fill(next_stoppage, .direction = "down") %>%
    fill(next_neutral_zone_event, .direction = "down") %>%
    fill(next_zone_exit, .direction = "down")
  df = df %>%
    mutate(is_FA = ifelse(game_seconds >= start_FA &
                            game_seconds < next_stoppage &
                            game_seconds < next_neutral_zone_event &
                            game_seconds < next_zone_exit, TRUE, FALSE)) %>%
    mutate(is_FA = ifelse(last_faceoff_winner_faceoff_zone == "Off", is_FA, NA))
  
  df$end_FA = pmin(df$next_stoppage, df$next_neutral_zone_event, df$next_zone_exit, na.rm = TRUE)
  df = df %>%
    mutate(end_FA = ifelse(is_FA == TRUE, end_FA, NA)) %>%
    # fill(end_FA, .direction = "down") %>%
    mutate(FA_zone_time = ifelse(is_FA, end_FA - start_FA, NA))
  return(df)
}

# This function is a convenience utility for checking for odd results.
audit = function(df) {
  df_audit = df %>%
    select(game_id, game_date, game_seconds, clock_time, event_type, 
                  event_zone, event_team, last_faceoff_winner, last_faceoff_winner_zone,
                  last_faceoff_winner_home, home_zone, event_team_relative_to_faceoff,
                  event_zone_relative_to_faceoff_winner)
  return(df_audit)
}

# OFFENSIVE WIN, OFFENSIVE LOSS,
# DEFENSIVE WIN, DEFENSIVE LOSS
# handle both teams at once in this function
identify_situation = function(df) {
  return(NULL)
}

# Incremental FA zone time
# - FA Zone time if won
# - FA Zone time if lost

# Probability of next faceoff location
get_next_faceoff_location = function(df) {
  df_fac = df %>%
    filter(event_type == 'FAC') %>%
    mutate(next_faceoff_location = lead(home_zone, 1)) %>%
    mutate(next_event_team = lead(event_team, 1)) %>%
    mutate(next_faceoff_location = case_when(
      next_faceoff_location == "Neu" ~ next_faceoff_location,
      event_team == home_team & next_event_team == home_team ~ next_faceoff_location,
      event_team == away_team & next_event_team == away_team ~ ifelse(next_faceoff_location == "Off", "Def", "Off"),
      event_team == home_team & next_event_team == away_team 
    ))
    filter(game_id == lead(game_id, 1), game_date == lead(game_date, 1))
  df_updated = df %>%
    left_join(df_fac, by = c('season', 'game_id',
                             'game_period', 'event_type'),
              suffix = c("", "_ignore")) %>%
    select(-ends_with("_ignore"))
  quick_look = df_updated %>% 
    ungroup() %>% 
    filter(event_type == "FAC") %>% 
    select(next_faceoff_location) %>% 
    #drop_na()
  print(paste0("Before joining for next faceoff location, there are ", nrow(df_fac), " faceoffs."))
  print(paste0("After joining for next faceoff location, there are ", nrow(quick_look)))
  return(df_updated)
}
# Percentage of favorable zone time before next faceoff
# Percentage of corsi before next faceoff
# Probability of more favorable zone time before next faceoff than other team
# Probability of successful clear
get_successful_clear_info = function(df) {
  
}
# Probability of transition to other team shot attempt
get_counter_shot_attempt_info = function(df) {
  
}
# Probability of transition to meaningful zone time
get_meaningful_zone_time_status = function(df) {
  
}
# Expected number of zone changes before meaningful zone time

# Zone Time Rating (ZTR) + Situational Favorability Rating (SFR) + Favorable Faceoff Differential (FFD) + Goals per minute offensive zone time i.e. Offensive Rating (OFF) 
# + Goals Allowed per Minute Defensive i.e. Defensive Rating (DEF)

# compute_FA_zone_time = function(df) {
#   
#   # We have a zone change if any of the following occur (not mutually exclusive)
#   # - An event occurring after an offensive/defensive faceoff is detected in the neutral zone
#   # - A zone exit or zone entry occurs
#   # - Also halt counting if there is a play stoppage
#   
#   df = df %>%
#     mutate(start_FA = ifelse(event_type == "FAC" & event_zone != "Neu", game_seconds, NA)) %>%
#     fill(start_FA, .direction = "down") %>%
#     mutate(next_stoppage = ifelse(
#       event_type %in% c("FAC", "STOP", "GOAL", "PGSTR", "PGEND", "PENL", "PEND", "GEND"), game_seconds, NA)) %>%
#     fill(next_stoppage, .direction = "up") %>%
#     mutate(next_neutral_zone_event = ifelse(event_zone == "Neu", game_seconds, NA)) %>%
#     fill(next_neutral_zone_event, .direction = "up") %>%
#     mutate(next_zone_exit = ifelse(event_type == "ZONE_EXIT" | (event_type == "ZONE_ENTRY" & !(game_seconds == lag(game_seconds) & lag(event_type) == "FAC")), game_seconds, NA)) %>%
#     fill(next_zone_exit, .direction = "up") # %>%
#   # mutate(next_escaped_event = ifelse(!is.na(event_zone) & !is.na(event_team) &
#   #                                      (
#   #                                        (event_team != last_faceoff_winner & event_zone == last_faceoff_loser_faceoff_zone) |
#   #                                        (event_team == last_faceoff_winner & event_zone == last_faceoff_winner_zone)
#   #                                      ), 
#   #                                   NA, game_seconds)) %>%
#   # fill(next_escaped_event, .direction = "up")
#   
#   df = df %>%
#     mutate(is_FA = ifelse(game_seconds >= start_FA &
#                             game_seconds <= next_stoppage &
#                             game_seconds <= next_neutral_zone_event &
#                             # game_seconds <= next_escaped_event &
#                             game_seconds <= next_zone_exit, TRUE, FALSE)) %>%
#     # fill(is_FA, .direction = "down") %>%
#     mutate(end_FA = ifelse(is_FA, min(c_across(c(next_stoppage, next_neutral_zone_event, next_zone_exit))), NA)) %>%
#     fill(end_FA, .direction = "down") %>%
#     mutate(FA_zone_time = ifelse(is_FA, end_FA - start_FA, NA))
#   return(df)
# }