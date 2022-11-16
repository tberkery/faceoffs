library(tidyverse)
library(Boruta)

data_no_g = data_filtered %>%
  select(-contains('_G1'))

drop_na = data_no_g %>% drop_na()

#boruta.bank_train <- Boruta(faceoff_winning_team_xG_since_faceoff~., data = drop_na, doTrace = 2, maxRuns = 11)
#saveRDS(boruta.bank_train, 'boruta_file.rds')
boruta.bank_train = readRDS('boruta_file.rds')

boruta.bank <- TentativeRoughFix(boruta.bank_train)
print(boruta.bank)

getSelectedAttributes(boruta.bank, withTentative = F)
bank_df <- attStats(boruta.bank)

