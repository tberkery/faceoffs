# Load required packages
library(readxl)
library(dplyr)
library(tidyverse)

# Read the data from Excel sheets
entries <- openxlsx::read.xlsx("./Corey Sznajder Data/2022-23 Season/2022-23 Season Goalie Sheet.xlsx", sheet = 'Entries')
exits <- openxlsx::read.xlsx("./Corey Sznajder Data/2022-23 Season/2022-23 Season Goalie Sheet.xlsx", sheet = 'Exits')
shots <- openxlsx::read.xlsx("./Corey Sznajder Data/2022-23 Season/2022-23 Season Goalie Sheet.xlsx", sheet = 'Shots')

new_col_names <- entries[4,]
names(entries) <- new_col_names
names(entries)[names(entries) == 'Row Labels'] <- 'Player'
entries <- entries[-(1:4),]

new_col_names <- exits[4,]
names(exits) <- new_col_names
names(exits)[names(exits) == 'Row Labels'] <- 'Player'
exits <- exits[-(1:4),]

new_col_names <- shots[4,]
names(shots) <- new_col_names
names(shots)[names(shots) == 'Row Labels'] <- 'Player'
shots <- shots[-(1:4),]

# Rename columns and create new dataframes
entries_22 <- entries %>%
  select(Player = Players, Entries = `5v5 Entries`, Carries = `Carry-ins`, 
         `Carry-in%` = `Carry%`, Passes = `Entries w/ Passing Play`, 
         `Pass% /` = `Pass%`, `Recovered.Dump-ins` = `Recovered Dump-ins`)

exits_22 <- exits %>%
  select(Player = Player, Exits = `Zone Exits `, 
         `Possession.Exits` = `Exit w/ Possession%`, `Exit%` = `Successful Exit%`, 
         `Fail%` = `Failed Exit%`, Clear = `Sum of Clears`)

shots_22 <- shots %>%
  select(Player = Player, `Low-to-High` = `Low-to-High Passes`, 
         `Behind.net` = `Behind Net Passes`, `One-timer` = `Sum of One-timer`,
         NZ = `Sum of NZ Assist `, DZ = `Sum of DZ Assist `, 
         Deflections = `Sum of Deflections`, 'Cross-Slot-Passes' = `Cross-Slot Passes`)
names(shots_22)[names(shots_22) == 'Cross-Slot-Passes'] <- 'High.Danger'
shots_22$High.Danger <- as.numeric(shots_22$High.Danger)
shots_22$Behind.net <- as.numeric(shots_22$Behind.net)
shots_22 <- shots_22 %>% mutate(High.Danger = High.Danger + Behind.net)



# Merge the dataframes by Player
microstats_22 <- reduce(list(entries_22, exits_22, shots_22), full_join, by = "Player")
microstats_old = read_csv("microstats.csv")
microstats_22 = microstats_22 %>%
  rename(`Pass%` = `Pass% /`,
         Behind.Net = Behind.net,
         Deflection = Deflections
         ) %>%
  mutate(year = 2022) %>%
  select(all_of(colnames(microstats_old)))
write.csv(microstats_22, file = "microstats_22.csv", row.names = FALSE)
