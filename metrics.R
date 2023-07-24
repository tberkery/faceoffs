library(tidyverse)

seed_metrics = function(data) {
  
}

# OFFENSIVE WIN, OFFENSIVE LOSS,
# DEFENSIVE WIN, DEFENSIVE LOSS
# handle both teams at once in this function
identify_situation = function(df) {
  
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
      event_team == away_team & next_event_team == away_team ~ if(next_faceoff_location == "Off", "Def", "Off")
      event_team == home_team & next_event_team == away_team ~ 
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
    drop_na()
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