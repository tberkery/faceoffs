library(tidyverse)
library(stringr)
library(fuzzyjoin)
library(lubridate)
source("link.R")
source("zone_entry.R")

start_year = 2022
end_year = 2023

load_eh_pbp = function(start_year, end_year) {
  next_year = start_year + 1
  pbp = read_csv(paste0("EH_pbp_query_", start_year, next_year, ".csv")) %>%
    mutate(is_pp = (game_strength_state == '5v4' |
                      game_strength_state == '4v5')) #%>%
  #filter(is_pp == TRUE)
  one_plus = next_year
  if (one_plus < end_year - 1) {
    for (year in one_plus:(end_year - 1)) {
      next_year = year + 1
      pbp_year = read_csv(paste0("EH_pbp_query_", year, next_year, ".csv"))
      pbp_year_pp = pbp_year %>%
        mutate(is_pp = (game_score_state == '5v4' |
                          game_score_state == '4v5')) %>%
        #filter(is_pp == TRUE)
        pbp = rbind(pbp, pbp_year_pp)
    }
  }
  pbp_initial = pbp
  print(pbp)
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
  for (year in start_year:(end_year - 1)) {
    formatted_season = paste0(year, "-", year + 1 - 2000, " Season")
    game_files = list.files(path = paste0("./Corey Sznajder Data/", formatted_season, "/Game Sheets"), 
                              pattern='.xlsx', all.files=TRUE, full.names = FALSE)
    file = game_files[[1]] # for debugging/line-by-line-running convenience
    for (file in game_files) {
      # will need to add if here for 2020 team name adjustments
      season_game_index = word(file, 1)
      # if (season_game_index == 20245) { # this game is not tracking entries, just skip it 
      #   next
      # }
      if (word(file, 3) == '@' | word(file, 3) == 'at'){
        away_team = word(file, 2)
        home_team = word(file, 4)
        home_team = gsub("\\.xlsx$", "", home_team)
        # home_team = toupper(str_to_title(str_sub(home_team, 1, 3)))
      }
      if (word(file, 3) == 'vs.') {
        home_team = word(file, 2)
        away_team = word(file, 4)
        away_team = gsub("\\.xlsx$", "", away_team)
      }
      
      home_team = case_when(
        home_team == 'TB' ~ 'TBL',
        home_team == 'NJ' ~ 'NJD',
        home_team == 'SJ' ~ 'SJS',
        home_team == 'LA' ~ 'LAK',
        home_team == 'BO' ~ 'BOS',
        home_team == 'CHi' ~ 'CHI',
        home_team == 'VAn' ~ 'VAN',
        home_team == 'TOr' ~ 'TOR',
        home_team == 'NHS' ~ 'NSH',
        home_team == 'Tb' ~ 'TBL',
        home_team == 'DEt' ~ 'DET',
        home_team == 'PWG' ~ 'WPG',
        home_team == 'PIt' ~ 'PIT',
        home_team == 'CAr' ~ 'CAR',
        home_team == 'VNA' ~ 'VAN',
        home_team == 'NSh' ~ 'NSH',
        home_team == 'J' ~ 'SJS',
        home_team == 'B' ~ 'TBL',
        home_team == 'NY' ~ 'NYI',
        home_team == 'BU' ~ 'BUF',
        TRUE ~ home_team
      )
      away_team = case_when(
        away_team == 'TB' ~ 'TBL',
        away_team == 'NJ' ~ 'NJD',
        away_team == 'SJ' ~ 'SJS',
        away_team == 'LA' ~ 'LAK',
        away_team == 'BO' ~ 'BOS',
        away_team == 'CHi' ~ 'CHI',
        away_team == 'VAn' ~ 'VAN',
        away_team == 'TOr' ~ 'TOR',
        away_team == 'NHS' ~ 'NSH',
        away_team == 'Tb' ~ 'TBL',
        away_team == 'DEt' ~ 'DET',
        away_team == 'PWG' ~ 'WPG',
        away_team == 'PIt' ~ 'PIT',
        away_team == 'CAr' ~ 'CAR',
        away_team == 'VNA' ~ 'VAN',
        away_team == 'NSh' ~ 'NSH',
        away_team == 'J' ~ 'SJS',
        away_team == 'B' ~ 'TBL',
        away_team == 'NY' ~ 'NYI',
        away_team == 'BU' ~ 'BUF',
        TRUE ~ away_team
      )
      
      # TODO: read-in game_file_zone_entries
      # TODO: read-in game_file_zone_exits
      
      file_with_path = paste0("./Corey Sznajder Data/", formatted_season, "/Game Sheets/", file)
      mtry = try(openxlsx::read.xlsx(file_with_path, 'Tracking'), silent = TRUE)
      if (class(mtry) != "try-error") {
        game_file = openxlsx::read.xlsx(file_with_path, 'Tracking')
      } else {
        message("File doesn't exist, please check")
        print(file_with_path)
        next
      }
      game_file_zone_entries <- game_file[, colnames(game_file)[c(1:3, 21:27)]]
      game_file_zone_exits <- game_file[, colnames(game_file)[c(1:3, 28:31)]]
      
      if (is_null(game_file_zone_entries) | is_null(game_file_zone_exits)) { # skip over games with empty zone entry file or empy zone exit file (e.g. one Dallas/Colorado game)
        next
      }
      
      
      if("L.Ane" %in% colnames(game_file_zone_entries))
      {
        game_file_zone_entries = game_file_zone_entries %>%
          rename("Lane" = "L.Ane")
      }
      if("F\\" %in% colnames(game_file_zone_entries))
      {
        game_file_zone_entries = game_file_zone_entries %>%
          rename("Entry.By" = "F\\")
      }      
      if("Entry.Type" %in% colnames(game_file_zone_entries))
      {
        game_file_zone_entries = game_file_zone_entries %>%
          rename("Entry.type" = "Entry.Type")
      }
      if("Entry.By" %in% colnames(game_file_zone_entries))
      {
        game_file_zone_entries = game_file_zone_entries %>%
          rename("Entry.by" = "Entry.By")
      }
      if("Defended.By" %in% colnames(game_file_zone_entries))
      {
        game_file_zone_entries = game_file_zone_entries %>%
          rename("Defended.by" = "Defended.By")
      }
      if("Pass?" %in% colnames(game_file_zone_entries))
      {
        game_file_zone_entries = game_file_zone_entries %>%
          rename("Middle.driver" = "Pass?")
      } # not sure if these are same will leave for now but may want to remove pass and add empty middle driver
      if("Goalie.touch?" %in% colnames(game_file_zone_entries) == FALSE)
      {
        game_file_zone_entries <- cbind(game_file_zone_entries, 'Goalie.touch?' = NA)
      }
      if("Controlled?" %in% colnames(game_file_zone_entries) == FALSE)
      {
        game_file_zone_entries <- cbind(game_file_zone_entries, 'Controlled?' = NA)
      }
      if("Fen.total" %in% colnames(game_file_zone_entries) == FALSE)
      {
        game_file_zone_entries <- cbind(game_file_zone_entries, 'Fen.total' = NA)
      }
      if("Goal.total" %in% colnames(game_file_zone_entries) == FALSE)
      {
        game_file_zone_entries <- cbind(game_file_zone_entries, 'Goal.total' = NA)
      }
      if("Fail" %in% colnames(game_file_zone_entries) == FALSE)
      {
        game_file_zone_entries <- cbind(game_file_zone_entries, 'Fail' = NA)
      }
      if("Game" %in% colnames(game_file_zone_entries) == FALSE)
      {
        game_file_zone_entries <- cbind(game_file_zone_entries, 'Game' = season_game_index)
      }
      if("Opp" %in% colnames(game_file_zone_entries) == FALSE)
      {
        game_file_zone_entries <- cbind(game_file_zone_entries, 'Opp' = away_team)
      }
      if("Location" %in% colnames(game_file_zone_entries) == FALSE)
      {
        game_file_zone_entries <- cbind(game_file_zone_entries, 'Location' = 'Home')
      }
      if("Lane" %in% colnames(game_file_zone_entries) == TRUE)
      {
        game_file_zone_entries <- game_file_zone_entries[,!names(game_file_zone_entries) %in% c("Lane")]
      }
      if("Strength" %in% colnames(game_file_zone_entries) == TRUE)
      {
        game_file_zone_entries <- game_file_zone_entries[,!names(game_file_zone_entries) %in% c("Strength")]
      }
      if("Chance?" %in% colnames(game_file_zone_entries) == TRUE)
      {
        game_file_zone_entries <- game_file_zone_entries[,!names(game_file_zone_entries) %in% c("Chance?")]
      }
      # that should fully adjust game_file_zone_entries to match 2017
      # need to change strength for that as well, this is placeholder, think we just 5v5 rn
      if("Team.strength" %in% colnames(game_file_zone_entries) == FALSE)
      {
        game_file_zone_entries <- cbind(game_file_zone_entries, 'Team.strength' = 5)
      }
      if("Opp.strength" %in% colnames(game_file_zone_entries) == FALSE)
      {
        game_file_zone_entries <- cbind(game_file_zone_entries, 'Opp.strength' = 5)
      }
      
      #exits 
      
      if("PL.Ayer" %in% colnames(game_file_zone_exits))
      {
        game_file_zone_exits = game_file_zone_exits %>%
          rename("Player" = "PL.Ayer")
      }
      if("Retrieval" %in% colnames(game_file_zone_exits))
      {
        game_file_zone_exits = game_file_zone_exits %>%
          rename("Assist?" = "Retrieval")
      }
      if("Direction" %in% colnames(game_file_zone_exits) == FALSE)
      {
        game_file_zone_exits <- cbind(game_file_zone_exits, Direction = NA)
      }
      if("Pressure" %in% colnames(game_file_zone_exits))
      {
        game_file_zone_exits = game_file_zone_exits %>%
          rename("Pressured?" = "Pressure")
      }
      if("Exit" %in% colnames(game_file_zone_exits))
      {
        game_file_zone_exits = game_file_zone_exits %>%
          rename("Player" = "Exit")
      }
      if("Controlled.Entry?" %in% colnames(game_file_zone_exits) == FALSE)
      {
        game_file_zone_exits <- cbind(game_file_zone_exits, 'Controlled.Entry?' = NA)
      }
      if("Player" %in% colnames(game_file_zone_exits))
      {
        game_file_zone_exits = game_file_zone_exits %>%
          rename("Attempt" = "Player")
      }
      if("Assist?" %in% colnames(game_file_zone_exits))
      {
        game_file_zone_exits = game_file_zone_exits %>%
          rename("Pass.Target" = "Assist?")
      }
      if("Controlled.Entry?" %in% colnames(game_file_zone_exits))
      {
        game_file_zone_exits = game_file_zone_exits %>%
          rename("Entry?" = "Controlled.Entry?")
      }
      if("Strength" %in% colnames(game_file_zone_exits) == TRUE)
      {
        game_file_zone_exits <- game_file_zone_exits[,!names(game_file_zone_exits) %in% c("Strength")]
      }
      # those should fully adjust the exits to match 2017
      
      file_creation_date = file.info(file_with_path)$ctime # Read when file was first created. Will use as game date.
      team_home = home_team
      team_away = away_team
      print(team_home)
      print(team_away)
      # TODO: before dataframe transformations, need to fix the fact that "GOAL" column is duplicated
      game_file_zone_entries = game_file_zone_entries %>%
        mutate(season_game_index = season_game_index,
               home_team = home_team,
               away_team = away_team,
               Opp = away_team)

      
      game_file_zone_entries = game_file_zone_entries %>% 
        mutate(Opp = away_team) %>%
        mutate(home_team_temp = home_team,
               away_team_temp = away_team) %>%
        mutate(home_team = ifelse(home_team_temp == Opp, away_team_temp, home_team_temp),
               away_team = ifelse(away_team_temp == Opp, away_team_temp, home_team_temp),
               effective_game_date = file_creation_date) %>%
        select(-c(home_team_temp, away_team_temp)) %>%
        mutate(abbreviation_home = home_team,
               abbreviation_away = away_team)
      
      
      sznajder_game_ids_and_teams = game_file_zone_entries %>%
        select(season_game_index, home_team, away_team) %>%
        distinct(season_game_index, .keep_all = TRUE)
      
      
      game_file_zone_exits = game_file_zone_exits %>%
        mutate(season_game_index = season_game_index,
               effective_game_date = file_creation_date) %>%
        mutate(home_team = home_team, away_team = away_team)
      
      
      
      
      game_file_zone_entries = game_file_zone_entries %>%
        mutate(Time = openxlsx::convertToDateTime(Time, origin = "1900-01-01")) %>%
        mutate(Time = format(as.POSIXct(Time), format = '%H:%M:%S')) %>%
        mutate(Time = as.character(Time)) %>%
        mutate(Time = substr(Time, 1, 5)) #%>% # Now a string format of MM:SS

      game_file_zone_exits = game_file_zone_exits %>%
        mutate(Time = openxlsx::convertToDateTime(Time, origin = "1900-01-01")) %>%
        mutate(Time = format(as.POSIXct(Time), format = '%H:%M:%S')) %>%
        mutate(Time = as.character(Time)) %>%
        mutate(Time = substr(Time, 1, 5)) #%>% # Now a string format of MM:SS

      print(paste0(season_game_index, " ", away_team, " at ", home_team))
      games_zone_entries = rbind(games_zone_entries, game_file_zone_entries)
      games_zone_exits = rbind(games_zone_exits, game_file_zone_exits)
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
  zone_entries %>% write_csv("zone_entries_intermediate.csv")
  zone_exits %>% write_csv("zone_exits_intermediate.csv")
  return(pbp_with_sznajder)
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

load_season = function(start_year, end_year) {
  eh_pbp = load_eh_pbp(start_year, end_year)
  load_sznajder_game_reports(start_year, end_year, eh_pbp)
}
