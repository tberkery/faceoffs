library(tidyverse)
library(anytime)
library(stringr)
library(fuzzyjoin)
library(lubridate)

load_sznajder_2022 = function() {
  
  source("load_data_22-23.R")
  load_season(2022, 2023) # same note... this is 2020-2021 and 2021-2022.
  all_zone_entries = read_csv("zone_entries_intermediate.csv")
  all_zone_exits = read_csv("zone_exits_intermediate.csv")

  big_join = join_entries_22_23(2022, 2023, all_zone_entries, all_zone_exits)
  big_join = read_csv("zone_entries_joined.csv")
  source("join_pbp_and_sznajder.R")
  
  #Stops working here
  pbp_with_role = condition(big_join, c(2022))
  mega_dict = assemble_stats()
  dataset = get_role_encoded_stats(pbp_with_role, mega_dict)
  dataset %>% write_csv("new_dataset_updated_2022.csv")
  dataset = subset_relevant_cols(dataset)
}


join_entries_22_23 = function(start_year, end_year, zone_entries, zone_exits) {
  zone_entries_sample = zone_entries %>%
    mutate(pbp = 0,
           home_team = 
             case_when(
               home_team == 'LAK' ~ 'L.A',
               home_team == 'NJD' ~ 'N.J',
               home_team == 'SJS' ~ 'S.J',
               home_team == 'TBL' ~ 'T.B',
               TRUE ~ home_team
             ),
           away_team = 
             case_when(
               away_team == 'LAK' ~ 'L.A',
               away_team == 'NJD' ~ 'N.J',
               away_team == 'SJS' ~ 'S.J',
               away_team == 'TBL' ~ 'T.B',
               TRUE ~ away_team
             ),
           teams = paste(pmin(home_team,away_team), pmax(home_team,away_team), sep = ',')
    )
  
  #Enter Zone Exits File Name Here
  #zone_exits_sample <- read_csv("zone_exits_sample.csv") %>%
  zone_exits_sample = zone_exits %>%
    mutate(pbp = 0,
           home_team = 
             case_when(
               home_team == 'LAK' ~ 'L.A',
               home_team == 'NJD' ~ 'N.J',
               home_team == 'SJS' ~ 'S.J',
               home_team == 'TBL' ~ 'T.B',
               TRUE ~ home_team
             ),
           away_team = 
             case_when(
               away_team == 'LAK' ~ 'L.A',
               away_team == 'NJD' ~ 'N.J',
               away_team == 'SJS' ~ 'S.J',
               away_team == 'TBL' ~ 'T.B',
               TRUE ~ away_team
             ),
           teams = paste(pmin(home_team,away_team), pmax(home_team,away_team), sep = ',')
    )

  for (iterative_year in (start_year:(end_year-1))) {
    print("getting EHPBPB")
    print(iterative_year)
    pbp_iter = read_csv(paste0('EH_pbp_query_', iterative_year, iterative_year + 1, '.csv')) %>%
      mutate(pbp = 1,
             teams = paste(pmin(home_team,away_team), pmax(home_team,away_team), sep = ','),
             game_date = anydate(game_date))
    # pbp = rbind(pbp, pbp_iter)
    pbp = pbp_iter
  }
  
  entries_games = zone_entries_sample %>%
    group_by(game_date, teams) %>%
    distinct(game_date, teams) %>%
    summarise(entries_game = n()) %>%
    arrange(game_date) 
  
  exits_games = zone_exits_sample %>%
    group_by(game_date, teams) %>%
    distinct(game_date, teams) %>%
    summarise(exits_game = n()) %>%
    arrange(game_date) 
  
  joined = entries_games %>%
    full_join(exits_games,
              by = c('game_date' = 'game_date',
                     'teams' = 'teams')) 
  
  joined = joined %>%
    mutate(game_date = anydate(game_date))
  
  pbp = pbp %>%
    mutate(game_date = anydate(game_date))
  get_pbp_game_match = function(joined, pbp){
    for (i in 1:nrow(joined)){
      print(i)
      
      date = as.vector(joined[i,1])[[1]]
      playing_teams = as.character(joined[i,2])
      
      look = pbp %>%
        filter(playing_teams == teams &
                 game_date < date) %>%
        distinct(game_id) %>%
        mutate(team = playing_teams,
               date = date) %>%
        head(1)
      
      if(nrow(look) == 0){
        look = data.frame(
          game_id = NA,
          team = NA,
          date = NA
        )
      } else {
        pbp = pbp %>%
          filter(game_id != look$game_id)
      }
      
      if(i == 1){
        new_info = look
      } else {
        new_info = new_info %>% bind_rows(look)
      }
    }
    return(new_info)
  }
  
  pbp_matches = get_pbp_game_match(joined, pbp)
  
  entries_games2 = zone_entries_sample %>%
    left_join(pbp_matches,
              by = c('teams' = 'team',
                     'game_date' = 'date')) %>%
    mutate(game_id.x= ifelse(is.na(game_id.x), game_id.y),  game_id.x) %>%
    select(-game_id.y) %>%
    rename(game_id = game_id.x)
  
  exits_games2 = zone_exits_sample %>%
    left_join(pbp_matches,
              by = c('teams' = 'team',
                     'game_date' = 'date')) %>%
    mutate(game_id.x= ifelse(is.na(game_id.x), game_id.y),  game_id.x) %>%
    select(-game_id.y) %>%
    rename(game_id = game_id.x)
  
  entries_games2_temp = entries_games2 %>%
    mutate(game_date = substr(game_date, 1, 10),
           clock_time = substr(clock_time, 1, 5))
  
  exits_games2_temp = exits_games2 %>%
    mutate(game_date = substr(game_date, 1, 10),
           clock_time = substr(clock_time, 1, 5))
  
  big_join = pbp %>%
    mutate(game_date = substr(game_date, 1, 10),
           clock_time = substr(clock_time, 1, 5)) %>%
    bind_rows(entries_games2_temp) %>%
    bind_rows(exits_games2_temp) %>%
    group_by(game_id) %>%
    arrange(game_id, game_seconds)
  
  
  write_csv(big_join, 'zone_entries_joined.csv')
  return(big_join)
}
