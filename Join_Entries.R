#Enter Zone Exits File Name Here

library(tidyverse)
year = 2017
join_entries = function(year, zone_entires, zone_exits) {
  #Enter Zone Entries File Name Here
  #zone_entries_sample <- read_csv("zone_entries_sample2.csv") %>%
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
  
  #Enter ZEH PBP Data Here
  pbp = read_csv(paste0('EH_pbp_query_', year, year + 1, '.csv')) %>%
    rbind(read_csv(paste0('EH_pbp_query_', year + 1, year + 2, '.csv'))) %>%
    mutate(pbp = 1,
           teams = paste(pmin(home_team,away_team), pmax(home_team,away_team), sep = ','))
  
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
  
  big_join = pbp %>%
    mutate(game_date = substr(game_date, 1, 10),
           clock_time = substr(clock_time, 1, 5)) %>%
    bind_rows(entries_games2) %>%
    bind_rows(exits_games2) %>%
    group_by(game_id) %>%
    arrange(game_id, game_seconds)
  
  
  #write_csv(big_join, 'zone_entries_joined.csv')
  return(big_join)
}
