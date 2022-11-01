library(tidyverse)

read_sznajder = function()
  for (year in c(2019, 2020, 2021, 2022)) {
    sznajder_year = read_csv(paste0("sznajder_", year))
  }