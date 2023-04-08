
assemble_stats = function() {
  source("driver.R")
  mega_dict = connect_skaters_and_goaltending_to_team_performance()
}

load_sznajder = function() {
  source("load_data.R")
  load_season(2017, 2019) # note that this does the 2017-2018 and 2018-2019 seasons (i.e. boundaries are inclusive, exclusive)
  zone_entries_17_18_19 = read_csv("zone_entries_intermediate.csv")
  zone_exits_17_18_19 = read_csv("zone_exits_intermediate.csv")
  source("load_data_20-22.R")
  load_season(2020, 2022) # same note... this is 2020-2021 and 2021-2022.
  zone_entries_20_21_22 = read_csv("zone_entries_intermediate.csv")
  zone_exits_20_21_22 = read_csv("zone_exits_intermediate.csv")
  all_zone_entries = rbind(zone_entries_17_18_19, zone_entries_20_21_22)
  all_zone_exits = rbind(zone_exits_17_18_19, zone_exits_20_21_22)
  source("Join_Entries.R")
  big_join = join_entries(2017, 2022, all_zone_entries, all_zone_exits)
}