library(tidyverse)
library(stringr)

seed_metrics = function() {
  big_join_objective = big_join %>% 
    identify_last_faceoff_winner() %>%
    populate_event_team_for_zone_changes() %>%
    extend_over_zone_changes() %>%
    identify_event_team_relative_to_faceoff() %>%
    identify_zone_change_team_relative_to_faceoff()
  
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

remove_non_capital_letters <- function(text) {
  # Use gsub to remove spaces and numbers
  cleaned_text <- gsub("[^A-Z]", "", text)
  return(cleaned_text)
}

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
extract_team_from_zone_change_record = function(df) {
  df$zone_change_team <- sapply(df$event_description, extract_three_chars)
  return(df)
}

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
identify_event_team_relative_to_faceoff = function(df) {
  fo_df = df %>%
    mutate(event_team_relative_to_faceoff = ifelse(last_faceoff_winner == event_team, "WINNER", "LOSER"))
  return(fo_df)
}

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

compute_time_until_zone_change = function(faceoffs, big_join) {
  # For faceoff winner
  faceoffs_and_zone_changes = big_join %>%
    filter(event_type == "FAC" | event_type == "ZONE_ENTRY" | event_type == "ZONE_EXIT" | event_type == "GOAL") %>%
    filter(!(event_type == "FAC" & event_zone == "Neu")) %>%
    filter(!grepl("OZF", event_description))
  faceoffs_and_zone_changes_updated = extract_team_from_zone_change_record(faceoffs_and_zone_changes)
  faceoffs_and_zone_changes_updated = identify_last_faceoff_winner(faceoffs_and_zone_changes_updated)
  # For faceoff loser
  
  # Difference
}

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