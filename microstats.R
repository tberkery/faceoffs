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
    game_file_entries <- game_file_entries[, !duplicated(colnames(game_file_entries))]
    game_file_entries = game_file_entries %>%
      mutate(year = year)
    print("worked")
    
    game_file_exits = openxlsx::read.xlsx(paste0("./Corey Sznajder Data/20", year - 2000, "-", year - 1999, " Season/Team Pages/", file), 'Exits') 
    game_file_exits <- game_file_exits[, !duplicated(colnames(game_file_exits))]
    game_file_exits = game_file_exits %>%
      mutate(year = year)
    print("worked")
    
    game_file_entries_defense = openxlsx::read.xlsx(paste0("./Corey Sznajder Data/20", year - 2000, "-", year - 1999, " Season/Team Pages/", file), 'Entry Defense') 
    game_file_entries_defense <- game_file_entries_defense[, !duplicated(colnames(game_file_entries_defense))]
    game_file_entries_defense = game_file_entries_defense %>%
      mutate(year = year)
    print("worked")
    
    game_file_pass_types = openxlsx::read.xlsx(paste0("./Corey Sznajder Data/20", year - 2000, "-", year - 1999, " Season/Team Pages/", file), 'Pass Types')
    game_file_pass_types <- game_file_pass_types[, !duplicated(colnames(game_file_pass_types))]
    game_file_pass_types = game_file_pass_types %>%
      mutate(year = year)
    print("worked")
    
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
    left_join(exits_2017, by = c('Player', 'year'), suffix = c('_entries', '_exits')) %>%
    select(-contains('#')) %>%
    #left_join(entries_defense_2017, by = 'Player', suffix = c('', '_entry_defense')) %>%
    #left_join(pass_types_2017, by = 'Player', suffix = c('_entry_defense', '_pass_type'))
    left_join(pass_types_2017, by = c('Player', 'year'), suffix = c('', '_pass_type'))
  return(combined_2017)
}

microstats_2017 = read_microstats(2017)
microstats_2018 = read_microstats(2018)
microstats_2019 = read_microstats(2019)

#known_cols = c(colnames(entries_2018), colnames(exits_2018), colnames(pass_types_2018))

year = 2021
read_full_season_microstats = function(year) {
  file = NULL
  microstats_data = NULL
  if (year == 2020) {
    file = "Full Season Stats.xlsx"
    microstats_data = openxlsx::read.xlsx(paste0("./Corey Sznajder Data/", file), 'Player List')
  } else {
    file = "Formatted 2021-22 Season Totals Sheet.xlsx"
    microstats_entries = openxlsx::read.xlsx(paste0("./Corey Sznajder Data/", file), 'Entries') %>%
      rename(`Entries.w/.Passing.Play.Per.60` = 19) %>%
      mutate(Player = Players,
             Entries = `5v5.Entries`,
             Carries = `Sum.of.5v5.TOI` * `Carries/60` / 60,
             `Carry-in%` = `Carry%`) %>%
      mutate(year = year)
    microstats_exits = openxlsx::read.xlsx(paste0("./Corey Sznajder Data/", file), 'Exits') 
    microstats_exits <- microstats_exits[, !duplicated(colnames(microstats_exits))]
    microstats_exits = microstats_exits %>%
      mutate(Player = Row.Labels) %>%
      mutate(year = year)
    microstats_passes = openxlsx::read.xlsx(paste0("./Corey Sznajder Data/", file), 'Passes') 
    microstats_passes <- microstats_passes[, !duplicated(colnames(microstats_passes))]
    
    microstats_passes = microstats_passes%>%
      mutate(year = year) %>%
      mutate(Player = Row.Labels)
  }
  combined_2021 = microstats_entries %>%
    left_join(microstats_exits, by = c('Player', 'year'), suffix = c('_entries', '_exits')) %>%
    select(-contains('#')) %>%
    #left_join(entries_defense_2017, by = 'Player', suffix = c('', '_entry_defense')) %>%
    #left_join(pass_types_2017, by = 'Player', suffix = c('_entry_defense', '_pass_type'))
    left_join(microstats_passes, by = c('Player', 'year'), suffix = c('', '_pass_type'))
  
  combined_2021 = combined_2021 %>%
    rename(Passes = Setups,
           `Exit%` = `Successful.Exit%`,
           `Possesion.Exit%` = `Exit.w/.Possession%`,
           Exits = Zone.Exits,
           Possession.Exits = `Exits.w/.Possession`,
           `Fail%` = `Failed.Exit%`,
           `Low-to-High` = `Low-to-High.Passes`,
           `Behind.Net` = `Behind.Net.Passes`,
           Deflection = `Sum.of.Deflections`,
           OZ = Sum.of.OZ.Assist,
           NZ = Sum.of.NZ.Assist,
           DZ = Sum.of.DZ.Assist,
           ) %>%
    mutate(#`High.Danger` = `HD.Passes/60` * Sum.of.5v5.TOI_entries / 60,
           `One-timer` = `One-timer/60` * Sum.of.5v5.TOI_entries / 60,
           Clear = `Sum.of.5v5.TOI_entries` * `Clears.per.60` / 60,
           Fails = `Sum.of.5v5.TOI_entries` * `Failed.Exit.per.60` / 60)
  return(combined_2021)
}
microstats_2021 = read_full_season_microstats(2021)
in_all = intersect(intersect(colnames(microstats_2017), colnames(microstats_2018)) , colnames(microstats_2019))
microstats_prev = microstats_2017 %>% select(any_of(in_all)) %>% rbind(microstats_2018 %>% select(any_of(in_all))) %>% rbind(microstats_2019 %>% select(any_of(in_all)))
#microstats_prev = microstats_prev %>% select(-contains('dump'), -contains('Dump'), -(contains('Shots') & contains('Off')))#, #-any_of(Left_entries, Center_entries, Right_entries, Pass, Carry, Transition, Icing, Left_exits, Center_exits, Right_exits, Pos_pass_type, Left, Center, Right, OZ, NZ, DZ, OL, OC, OR, Stretch.Passes, Home.Plate))

cols_prev = colnames(microstats_prev)
cols_2021 = colnames(microstats_2021)
cols_both = intersect(cols_prev, cols_2021)
microstats_prev = microstats_prev %>% select(all_of(cols_both))
microstats_2021 = microstats_2021 %>% select(all_of(cols_both))
print(setdiff(colnames(microstats_prev), colnames(combined_2021)))

year = 2021
read_microstats_2021_updated = function(year) {
  file = NULL
  microstats_data = NULL
  if (year == 2020) {
    file = "Full Season Stats.xlsx"
    microstats_data = openxlsx::read.xlsx(paste0("./Corey Sznajder Data/", file), 'Player List')
  } else {
    file = "Formatted 2021-22 Season Totals Sheet Updated for Reading.xlsx"
    microstats_data = openxlsx::read.xlsx(paste0("./Corey Sznajder Data/", file), 'Sheet1') 
    microstats_data <- microstats_data[, !duplicated(colnames(microstats_data))]
    microstats_data = microstats_data %>%
      rename_with(~str_remove(., 'Sum.of.')) %>% # Super awesome, cool new dplyr feature
      #rename(`Entries.w/.Passing.Play.Per.60` = 19) %>%
      mutate(Player = `Row.Labels`) %>%#,
             #Entries = `5v5.Entries`,
             #Carries = `Sum.of.5v5.TOI` * `Carries/60` / 60,
             #`Carry-in%` = `Carry%`) %>%
      mutate(year = year)
  }
  combined_2021 = microstats_data
  combined_2021 = combined_2021 %>%
    rename(#Player = Row.Labels,
           Entries = `5v5.Entries`,
           # Address Caryy-in%
           Passes = Setups,
           # Address Pass%
           # Address Dump-ins
           Possession.Exits = `Exits.w/.Possession`,
           # Address Fail%
           
           `Exit%` = `Successful.Exit%`,
           `Possesion.Exit%` = `Exit.w/.Possession%`,
           Exits = Zone.Exits,
           
           `Fail%` = `Failed.Exit%`,
           `Low-to-High` = `Low-to-High.Passes`,
           `Behind.Net` = `Behind.Net.Passes`,
           Deflection = `Sum.of.Deflections`,
           OZ = Sum.of.OZ.Assist,
           NZ = Sum.of.NZ.Assist,
           DZ = Sum.of.DZ.Assist,
    ) %>%
    #mutate(#`High.Danger` = `HD.Passes/60` * Sum.of.5v5.TOI_entries / 60,
      #`One-timer` = `One-timer/60` * Sum.of.5v5.TOI_entries / 60,
      #Clear = `Sum.of.5v5.TOI_entries` * `Clears.per.60` / 60,
      #Fails = `Sum.of.5v5.TOI_entries` * `Failed.Exit.per.60` / 60)
    rename_with(~str_remove(., 'Sum of '))
  return(combined_2021)
}

all_seen_cols = c(colnames(microstats_2017), colnames(microstats_2018), colnames(microstats_2019))
print(all_seen_cols)

print(setdiff(all_seen_cols, microstats_2021))


microstats = NULL
for (year in 2017:2022) {
  microstats_year = read_microstats(year)
  microstats = rbind(microstats, microstats_year)
  print(year)
}
read_microstats_2017(2019)
