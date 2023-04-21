library(tidyverse)

# 2017
game_files = list.files(path = "./Corey Sznajder Data/2017-18 Season/Team Pages/", 
                        pattern='.xlsx', all.files=TRUE, full.names = FALSE)
entries_2017 = NULL
exits_2017 = NULL
entries_defense_2017 = NULL
pass_types_2017 = NULL
first = TRUE
prev_cols_entries = NULL
prev_cols_exits = NULL
for (file in game_files) {
  print(file)
  game_file_entries = openxlsx::read.xlsx(paste0("./Corey Sznajder Data/2017-18 Season/Team Pages/", file), 'Entries')
  game_file_exits = openxlsx::read.xlsx(paste0("./Corey Sznajder Data/2017-18 Season/Team Pages/", file), 'Exits')
  #game_file_entries_defense = openxlsx::read.xlsx(paste0("./Corey Sznajder Data/2017-18 Season/Team Pages/", file), 'Entry Defense')
  #game_file_pass_types = openxlsx::read.xlsx(paste0("./Corey Sznajder Data/2017-18 Season/Team Pages/", file), 'Pass Types')
  
  if (!first) {
    entries_2017_cols = colnames(entries_2017)
    exits_2017_cols = colnames(exits_2017)
    game_file_entries_cols = colnames(game_file_entries)
    game_file_exits_cols = colnames(game_file_exits)
    entries_2017 = entries_2017 %>% select(intersect(entries_2017_cols, game_file_entries_cols))
    game_file_entries =  game_file_entries %>% select(intersect(entries_2017_cols, game_file_entries_cols))
    exits_2017 = exits_2017 %>% select(intersect(exits_2017_cols, game_file_exits_cols))
    game_file_exits =  game_file_exits %>% select(intersect(exits_2017_cols, game_file_exits_cols))
    #entries_defense_2017 = entries_defense_2017 %>% select(colnames(game_file_entries_defense))
    #pass_types_2017 = pass_types_2017 %>% select(colnames(game_file_pass_types))
    print(setdiff(colnames(game_file_entries), prev_cols_entries))
    print(setdiff(colnames(game_file_exits), prev_cols_exits))
  }
  entries_2017 = rbind(entries_2017, game_file_entries)
  exits_2017 = rbind(exits_2017, game_file_exits)
  #entries_defense_2017 = rbind(entries_defense_2017, game_file_entries_defense)
  #pass_types_2017 = rbind(pass_types_2017, game_file_pass_types)
  first = FALSE
  prev_cols_entires = colnames(entries_2017)
  prev_cols_exits = colnames(exits_2017)
}

