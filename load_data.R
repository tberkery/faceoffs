library(tidyverse)
library(stringr)
library(fuzzyjoin)
library(lubridate)
source("link.R")
source("zone_entry.R")

start_year = 2017
end_year = 2018

load_eh_pbp = function(start_year, end_year) {
  next_year = start_year + 1
  pbp = read_csv(paste0("EH_pbp_query_", start_year, next_year, ".csv")) %>%
    mutate(is_pp = (game_strength_state == '5v4' |
                      game_strength_state == '4v5')) #%>%
    #filter(is_pp == TRUE)
  one_plus = next_year
  for (year in one_plus:end_year) {
    next_year = year + 1
    pbp_year = read_csv(paste0("EH_pbp_query_", year, next_year, ".csv"))
    pbp_year_pp = pbp_year %>%
      mutate(is_pp = (game_score_state == '5v4' |
                        game_score_state == '4v5')) %>%
      #filter(is_pp == TRUE)
    pbp = rbind(pbp, pbp_year_pp)
  }
  pbp_initial = pbp
  return(pbp)
}

load_sznajder_game_reports = function(start_year, end_year, pbp) {
  # Rmk: need to rename Sznajder's "2021 Season" directory to "2020-21 Season"
  # Rmk: "Game Reports" directory name, and whether it is even a directory or instead an excel file,
  # agitatingly varies by season.
  games_zone_entries = NULL # declare object in this scope
  games_zone_exits = NULL
  team_lookup = lookup_team(pbp)
  pbp = pbp %>%
    mutate(clock_time = format(clock_time, '%H:%M:%S')) %>%
    mutate(clock_time = substr(clock_time, 1, 5))
  for (year in 2017:2017) {
    formatted_season = paste0(year, "-", year + 1 - 2000, " Season")

    game_files = list.files(path = paste0("./Corey Sznajder Data/", formatted_season, "/Game Reports"), 
                            pattern='.xlsx', all.files=TRUE, full.names = FALSE)
    for (file in game_files) {
      season_game_index = word(file, 1)
      away_team = str_to_title(word(file, 2))
      i = 3
      if (word(file, 3) != 'at') { # if not 'at', then multi-word away team
        while(word(file, i) != 'at') {
          i = i + 1
        }
        away_team = str_to_title(word(file, 2, i - 1))
        # minus one b/c we don't want to include the 'at'
      }
      j = i + 1 # skip over space
      home_team_length = str_count(file ,"\\W+") - j # this is zero if one word home team
      home_team = str_to_title(str_sub(word(file, j, j + home_team_length), end = -6))
      # str_sub and -6 b/c we want to remove the ".xlsx"
      file_with_path = paste0("./Corey Sznajder Data/", formatted_season, "/Game Reports/", file)
      game_file_zone_entries = openxlsx::read.xlsx(file_with_path, 'Raw Entries')
      game_file_zone_exits = openxlsx::read.xlsx(file_with_path, 'Zone Exits Raw Data')
      if (is_null(game_file_zone_entries) | is_null(game_file_zone_exits)) { # skip over games with empty zone entry file or empy zone exit file (e.g. one Dallas/Colorado game)
        next
      }
      file_creation_date = file.info(file_with_path)$ctime # Read when file was first created. Will use as game date.
      # TODO: before dataframe transformations, need to fix the fact that "GOAL" column is duplicated
      game_file_zone_entries = game_file_zone_entries %>%
        mutate(season_game_index = season_game_index,
               home_team = home_team,
               away_team = away_team)
      game_file_zone_entries = game_file_zone_entries %>% # home_team_temp and away_team_temp are currently full city names. Opp is abbreviations.
        left_join(team_lookup, by = c('home_team' = 'city')) %>%
        left_join(team_lookup, by = c('away_team' = 'city'), suffix = c('_home', '_away')) %>%
        mutate(home_team = abbreviation_home,
               away_team = abbreviation_away) %>%
        mutate(home_team_temp = home_team,
               away_team_temp = away_team) %>%
        mutate(home_team = ifelse(home_team_temp == Opp, away_team_temp, home_team_temp),
               away_team = ifelse(away_team_temp == Opp, away_team_temp, home_team_temp),
               effective_game_date = file_creation_date) %>%
        select(-c(home_team_temp, away_team_temp))
      sznajder_game_ids_and_teams = game_file_zone_entries %>%
        select(season_game_index, home_team, away_team) %>%
        distinct(season_game_index, .keep_all = TRUE)
      game_file_zone_exits = game_file_zone_exits %>%
        mutate(season_game_index = season_game_index,
               effective_game_date = file_creation_date) %>%
        inner_join(sznajder_game_ids_and_teams, by = 'season_game_index')
      
      game_file_zone_entries = game_file_zone_entries %>%
        mutate(Time = openxlsx::convertToDateTime(Time, origin = "1900-01-01")) %>%
        mutate(Time = format(as.POSIXct(Time), format = '%H:%M:%S')) %>%
        mutate(Time = as.character(Time)) %>%
        mutate(Time = substr(Time, 1, 5)) %>% # Now a string format of MM:SS
        left_join(team_lookup, by = c('home_team' = 'city')) %>%
        left_join(team_lookup, by = c('away_team' = 'city'), suffix = c('_home', '_away')) %>%
        mutate(home_team = abbreviation_home,
               away_team = abbreviation_away)
      game_file_zone_exits = game_file_zone_exits %>%
        mutate(Time = openxlsx::convertToDateTime(Time, origin = "1900-01-01")) %>%
        mutate(Time = format(as.POSIXct(Time), format = '%H:%M:%S')) %>%
        mutate(Time = as.character(Time)) %>%
        mutate(Time = substr(Time, 1, 5)) %>% # Now a string format of MM:SS
        left_join(team_lookup, by = c('home_team' = 'city')) %>%
        left_join(team_lookup, by = c('away_team' = 'city'), suffix = c('_home', '_away')) %>%
        mutate(home_team = abbreviation_home,
               away_team = abbreviation_away)
      
      games_zone_entries = rbind(games_zone_entries, game_file_zone_entries)
      games_zone_exits = rbind(games_zone_exits, game_file_zone_exits)
      print(paste0(season_game_index, " ", away_team, " at ", home_team))
    }
  }
  
  pbp_with_sznajder = pbp %>%
    mutate(home_team = case_when(
      home_team == 'T.B' ~ 'TBL',
      home_team == 'N.J' ~ 'NJD',
      home_team == 'S.J' ~ 'SJS',
      home_team == 'L.A' ~ 'LAK',
      TRUE ~ home_team
    )) %>%
    mutate(away_team = case_when(
      away_team == 'T.B' ~ 'TBL',
      away_team == 'N.J' ~ 'NJD',
      away_team == 'S.J' ~ 'SJS',
      away_team == 'L.A' ~ 'LAK',
      TRUE ~ away_team
    ))
  
  sznajder_games = games_zone_entries %>%
    select(season_game_index, home_team, away_team) %>%
    distinct(season_game_index, home_team, away_team, .keep_all = TRUE)
  
  zone_entries = create_zone_entries(pbp_with_sznajder, games_zone_entries)
  zone_exits = create_zone_exits(pbp_with_sznajder, games_zone_exits)
  # pbp_with_sznajder = pbp_with_sznajder %>%
  #   arrange(game_id, game_seconds)
  # pbp_with_sznajder = create_zone_exits(pbp_with_sznajder, games_zone_exits)
  # pbp_with_sznajder = pbp_with_sznajder %>%
  #   arrange(game_id, game_seconds)
  # return(pbp_with_sznajder)
}

filter_games = function(pbp_with_sznajder) {
  ggplot(pbp_with_sznajder, aes(x = home_team)) + geom_histogram(stat = "count")
  sznajder_games = pbp_with_sznajder %>%
    select(game_date, home_team, away_team, event_type) %>%
    filter(event_type %in% c('ZONE_ENTRY', 'ZONE_EXIT', 'FAILED_ZONE_EXIT')) %>%
    group_by(game_date, home_team, away_team) %>%
    summarize(count = n(), .groups = 'keep') %>%
    drop_na() %>%
    filter(count >= 121) %>% # TODO: SOMEWHAT ARBITRARY... REDEFINE LATER %>%
    ungroup() %>%
    distinct(game_date, home_team, away_team, .keep_all = TRUE)
  
  pbp_games = pbp_with_sznajder %>%
    select(game_date, home_team, away_team, event_type) %>%
    filter(event_type %in% c('SHOT')) %>% # using faceoffs as event for determining if data is in PBP data
    group_by(game_date, home_team, away_team) %>%
    summarize(count = n(), .groups = 'keep') %>%
    drop_na() %>%
    filter(count >= 5) %>% # at leat five faceoffs --> game is in PBP
    ungroup() %>%
    distinct(game_date, home_team, away_team, .keep_all = TRUE)
  
  print(nrow(pbp_with_sznajder))
  
  # Experiment to address issue of Sznajder tracking games post gameday. Not yet working.
  # pbp_with_sznajder_remaining= pbp_with_sznajder %>%
  #   anti_join(sznajder_games, by = c('game_date', 'home_team', 'away_team')) %>%
  #   anti_join(sznajder_games, by = c('game_date', 'home_team' = 'away_team', 'away_team' = 'home_team'), suffix = c('', '_reversed'))
  # 
  # for (i in 1:5) {
  #   pbp_with_sznajder_remaining = pbp_with_sznajder_remaining %>%
  #     mutate(game_date = as.Date(game_date) - i) %>% # subtract i days from sznajder game_date
  #     mutate(game_date = substr(game_date, 1, 8)) # cast date back to character so types agree in joins
  #   current_join = pbp_with_sznajder_remaining %>%
  #     inner_join(sznajder_games, by = c('game_date', 'home_team', 'away_team')) %>%
  #     inner_join(sznajder_games, by = c('game_date', 'home_team' = 'away_team', 'away_team' = 'home_team'), suffix = c('', '_reversed')) %>%
  #     filter(count > 0 | count_reversed > 0) 
  #   pbp_with_zone_changes = rbind(pbp_with_zone_changes, current_join)
  #   pbp_with_sznajder_remaining = pbp_with_sznajder_remaining %>%
  #     anti_join(sznajder_games, by = c('game_date', 'home_team', 'away_team')) %>%
  #     anti_join(sznajder_games, by = c('game_date', 'home_team' = 'away_team', 'away_team' = 'home_team'), suffix = c('', '_reversed'))
  # }
  
  pbp_with_zone_changes = pbp_with_sznajder %>%
    left_join(sznajder_games, by = c('game_date', 'home_team', 'away_team')) %>%
    left_join(sznajder_games, by = c('game_date', 'home_team' = 'away_team', 'away_team' = 'home_team'), suffix = c('_sznajder', '_sznajder_reversed')) %>%
    filter(count_sznajder > 0 | count_sznajder_reversed > 0) %>% # eliminate records from games w/ 0 (or NA) count of zone changes in Sznajder data
    arrange(game_id, game_seconds)
  print(nrow(pbp_with_zone_changes))
  # I have no idea why this cuts the dataset from 100,000 to about 1,700.
  pbp_game_ids = unique(pbp$game_id)
  pbp_with_zone_changes = pbp_with_zone_changes %>%
    filter(game_id %in% pbp_game_ids) %>%
    arrange(game_id, game_seconds)
  # pbp_with_zone_changes = pbp_with_zone_changes %>%
  #   left_join(pbp_games, by = c('game_date', 'home_team', 'away_team')) %>%
  #   left_join(pbp_games, by = c('game_date', 'home_team' = 'away_team', 'away_team' = 'home_team'), suffix = c('_pbp', '_pbp_reversed')) %>%
  #   filter(count_pbp > 0 | count_pbp_reversed > 0) %>% # eliminate records from games w/ 0 (or NA) count of zone changes in Sznajder data
  #   arrange(game_date, home_team, away_team, game_seconds)
  print(colnames(pbp_with_zone_changes))
  print(nrow(pbp_with_zone_changes))
}

reset = function() {
  pbp = pbp_initial
  pbp = pbp %>%
    mutate(clock_time = format(clock_time, '%H:%M:%S')) %>%
    mutate(clock_time = substr(clock_time, 1, 5))
  return(pbp)
}