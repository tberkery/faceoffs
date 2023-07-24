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
    mutate(next_faceoff_location = lead(event_zone, 1))
  df_updated = df %>%
    left_join(df_fac, by = c('season', 'game_id', 'event_index',
                             'game_period', 'game_seconds', 'event_type'),
              suffix = c("", "_ignore")) %>%
    select(-ends_with("_ignore"))
  print(paste0("Before joining for next faceoff location, there are ", nrow(df_fac), " faceoffs."))
  print(paste0("After joining for next faceoff location, there are ", nrow(df_updated 
                                                                           %>% filter(event_type == "FAC") 
                                                                           %>% drop_na(next_faceoff_location)
                                                                           )))
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