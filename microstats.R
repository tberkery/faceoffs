library(tidyverse)

# 2017
year = 2017
read_microstats = function(year) {
  game_files = list.files(path = paste0("./Corey Sznajder Data/20", year - 2000, "-", year - 1999, " Season/Team Pages/"), 
                          pattern='.xlsx', all.files=TRUE, full.names = FALSE)
  entries_2017 = NULL
  exits_2017 = NULL
  entries_defense_2017 = NULL
  pass_types_2017 = NULL
  first = TRUE
  prev_cols_entries = NULL
  prev_cols_exits = NULL
  prev_cols_entries_defense = NULL
  prev_cols_pass_types = NULL
  for (file in game_files) {
    print(file)
    game_file_entries = openxlsx::read.xlsx(paste0("./Corey Sznajder Data/20", year - 2000, "-", year - 1999, " Season/Team Pages/", file), 'Entries')
    game_file_exits = openxlsx::read.xlsx(paste0("./Corey Sznajder Data/20", year - 2000, "-", year - 1999, " Season/Team Pages/", file), 'Exits')
    game_file_entries_defense = openxlsx::read.xlsx(paste0("./Corey Sznajder Data/20", year - 2000, "-", year - 1999, " Season/Team Pages/", file), 'Entry Defense')
    game_file_pass_types = openxlsx::read.xlsx(paste0("./Corey Sznajder Data/20", year - 2000, "-", year - 1999, " Season/Team Pages/", file), 'Pass Types')
    
    if (!first) {
      entries_2017_cols = colnames(entries_2017)
      exits_2017_cols = colnames(exits_2017)
      game_file_entries_cols = colnames(game_file_entries)
      game_file_exits_cols = colnames(game_file_exits)
      
      entries_defense_2017_cols = colnames(entries_defense_2017)
      game_file_entries_defense_cols = colnames(game_file_entries_defense)
      pass_types_2017_cols = colnames(pass_types_2017)
      game_file_pass_types_cols = colnames(game_file_pass_types)
      
      entries_2017 = entries_2017 %>% select(intersect(entries_2017_cols, game_file_entries_cols))
      game_file_entries =  game_file_entries %>% select(intersect(entries_2017_cols, game_file_entries_cols))
      exits_2017 = exits_2017 %>% select(intersect(exits_2017_cols, game_file_exits_cols))
      game_file_exits =  game_file_exits %>% select(intersect(exits_2017_cols, game_file_exits_cols))
      
      game_file_entries_defense = game_file_entries_defense %>% select(intersect(entries_defense_2017_cols, game_file_entries_defense_cols))
      entries_defense_2017 = entries_defense_2017 %>% select(intersect(entries_defense_2017_cols, game_file_entries_defense_cols))
      
      game_file_pass_types = game_file_pass_types %>% select(intersect(pass_types_2017_cols, game_file_pass_types_cols))
      pass_types_2017 = pass_types_2017 %>% select(intersect(pass_types_2017_cols, game_file_pass_types_cols))
      
      print(setdiff(colnames(game_file_entries), prev_cols_entries))
      print(setdiff(colnames(game_file_exits), prev_cols_exits))
      print(setdiff(colnames(game_file_entries_defense), prev_cols_entries_defense))
      print(setdiff(colnames(game_file_pass_types), prev_cols_pass_types))
    }
    entries_2017 = rbind(entries_2017, game_file_entries)
    exits_2017 = rbind(exits_2017, game_file_exits)
    entries_defense_2017 = rbind(entries_defense_2017, game_file_entries_defense)
    pass_types_2017 = rbind(pass_types_2017, game_file_pass_types)
    first = FALSE
    prev_cols_entries = colnames(entries_2017)
    prev_cols_exits = colnames(exits_2017)
    prev_cols_entries_defense = colnames(entries_defense_2017)
    prev_cols_pass_types = colnames(pass_types_2017)
  }
  
  cols_2017 = c(colnames(entries_2017), colnames(exits_2017), colnames(pass_types_2017))
  combined_2017 = entries_2017 %>%
    left_join(exits_2017, by = 'Player', suffix = c('_entries', '_exits')) %>%
    #left_join(entries_defense_2017, by = 'Player', suffix = c('', '_entry_defense')) %>%
    #left_join(pass_types_2017, by = 'Player', suffix = c('_entry_defense', '_pass_type'))
    left_join(pass_types_2017, by = 'Player', suffix = c('', '_pass_type'))
}

read_microstats_2018 = function() {
  microstats_2018 = openxlsx::read.xlsx(paste0("./Corey Sznajder Data/2018-19 Season/Team Pages/", file), 'Entries')
  
  game_files = list.files(path = "./Corey Sznajder Data/2018-19 Season/Team Pages/", 
                          pattern='.xlsx', all.files=TRUE, full.names = FALSE)
  entries_2018 = NULL
  exits_2018 = NULL
  entries_defense_2018 = NULL
  pass_types_2018 = NULL
  first = TRUE
  prev_cols_entries = NULL
  prev_cols_exits = NULL
  prev_cols_entries_defense = NULL
  prev_cols_pass_types = NULL
  for (file in game_files) {
    print(file)
    game_file_entries = openxlsx::read.xlsx(paste0("./Corey Sznajder Data/2018-19 Season/Team Pages/", file), 'Entries')
    game_file_exits = openxlsx::read.xlsx(paste0("./Corey Sznajder Data/2018-19 Season/Team Pages/", file), 'Exits')
    game_file_entries_defense = openxlsx::read.xlsx(paste0("./Corey Sznajder Data/2018-19 Season/Team Pages/", file), 'Entry Defense')
    game_file_pass_types = openxlsx::read.xlsx(paste0("./Corey Sznajder Data/2018-19 Season/Team Pages/", file), 'Pass Types')
    
    if (!first) {
      entries_2018_cols = colnames(entries_2018)
      exits_2018_cols = colnames(exits_2018)
      game_file_entries_cols = colnames(game_file_entries)
      game_file_exits_cols = colnames(game_file_exits)
      
      entries_defense_2018_cols = colnames(entries_defense_2018)
      game_file_entries_defense_cols = colnames(game_file_entries_defense)
      pass_types_2018_cols = colnames(pass_types_2018)
      game_file_pass_types_cols = colnames(game_file_pass_types)
      
      entries_2018 = entries_2018 %>% select(intersect(entries_2018_cols, game_file_entries_cols))
      game_file_entries =  game_file_entries %>% select(intersect(entries_2018_cols, game_file_entries_cols))
      exits_2018 = exits_2018 %>% select(intersect(exits_2018_cols, game_file_exits_cols))
      game_file_exits =  game_file_exits %>% select(intersect(exits_2018_cols, game_file_exits_cols))
      
      game_file_entries_defense = game_file_entries_defense %>% select(intersect(entries_defense_2018_cols, game_file_entries_defense_cols))
      entries_defense_2018 = entries_defense_2018 %>% select(intersect(entries_defense_2018_cols, game_file_entries_defense_cols))
      
      game_file_pass_types = game_file_pass_types %>% select(intersect(pass_types_2018_cols, game_file_pass_types_cols))
      pass_types_2018 = pass_types_2018 %>% select(intersect(pass_types_2018_cols, game_file_pass_types_cols))
      
      print(setdiff(colnames(game_file_entries), prev_cols_entries))
      print(setdiff(colnames(game_file_exits), prev_cols_exits))
      print(setdiff(colnames(game_file_entries_defense), prev_cols_entries_defense))
      print(setdiff(colnames(game_file_pass_types), prev_cols_pass_types))
    }
    entries_2018 = rbind(entries_2018, game_file_entries)
    exits_2018 = rbind(exits_2018, game_file_exits)
    entries_defense_2018 = rbind(entries_defense_2018, game_file_entries_defense)
    pass_types_2018 = rbind(pass_types_2018, game_file_pass_types)
    first = FALSE
    prev_cols_entries = colnames(entries_2018)
    prev_cols_exits = colnames(exits_2018)
    prev_cols_entries_defense = colnames(entries_defense_2018)
    prev_cols_pass_types = colnames(pass_types_2018)
  }

  combined_2018 = entries_2018 %>%
    left_join(exits_2018, by = 'Player', suffix = c('_entries', '_exits')) %>%
    #left_join(entries_defense_2018, by = 'Player', suffix = c('', '_entry_defense')) %>%
    #left_join(pass_types_2018, by = 'Player', suffix = c('_entry_defense', '_pass_type'))
    left_join(pass_types_2018, by = 'Player', suffix = c('', '_pass_type'))
  
  return(combined_2018)
}

microstats_2017 = read_microstats(2017)
microstats_2018 = read_microstats(2018)
microstats_2019 = read_microstats(2019)

known_cols = c(colnames(entries_2018), colnames(exits_2018), colnames(pass_types_2018))

year = 2020
read_full_season_microstats = function(year, known_cols) {
  file = NULL
  microstats_data = NULL
  if (year == 2020) {
    file = "Full Season Stats.xlsx"
    microstats_data = openxlsx::read.xlsx(paste0("./Corey Sznajder Data/", file), 'Player List')
  } else {
    file = "Formatted 2021-22 Season Totals Sheet.xlsx"
    microstats_data = openxlsx::read.xlsx(paste0("./Corey Sznajder Data/", file), 'Sheet1')
  }
  print(setdiff(colnames(microstats_2017), colnames(microstats_data)))
  microstats_data = microstats_data %>%
    rename()
  
}

microstats = NULL
for (year in 2017:2022) {
  microstats_year = read_microstats(year)
  microstats = rbind(microstats, microstats_year)
  print(year)
}
read_microstats_2017(2019)
