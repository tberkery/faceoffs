# Introduction
How much do faceoffs actually matter? Many of the seismic developments in hockey analytics recently have been characterized by the importance of puck possession, a key determinant in the latest models such as expected goals and other metrics like Corsi. Yet amid this shift to focusing on possession, there is yet to be consensus or any similarly robust models on the importance of faceoffs, the most frequent and decisive determinant of possession. This project analyzes how faceoffs drive offensive and defensive results and impact teams winning games.

# Data
Our data primarily comes from two places, both of which are publicly available sources:

* **Evolving Hockey**: we use two key categories of data from *Evolving Hockey* in building our statistical models: (1) play-by-play data (i.e. tabulated events from hockey games) and (2) player performance data (i.e. measures of performance by player season)

* **All Three Zones**: we specifically rely on these microstats hand-tracked by Corey Sznajder to detect zone changes in the subset of games he tracks, which is key to conceptions of faceoff attributability in our models.

We encourage anyone aiming to replicate our work to go to https://evolving-hockey.com/ and https://www.allthreezones.com/ to download the publicly available data. To assist with easy replication, we offer the following detailed list of instructions on how we store the data from both these websites in terms of how we access them in our code when building and training our statistical models.

## Evolving Hockey
* **Play-by-Play Data:** navigate to the "Queries" tab on the *Evolving Hockey* website and then select "PBP Query". You will need to download every season separately by changing the "Season" field in the form that pops up. Note that you will want the date range to encompass each full season. We recommend using the earliest- and latest- possible selectable dates, which typically are around October 7th to June 13th (within a few days in either direction). Make sure to select all listed event options in the "Event Types" field. Then click "Submit" and use the "Downlload .csv" button at the bottom left to save the result. We recommend renaming the downloaded file to "merged_pbp_with_shots_[Season Start Year]_[Season_Start_Year_Plus_One]" for convenience.

* At this point, you have the play-by-play data and can shift to considering measured performance by player seasons. Navigate to the "Standard" tab and then select "Goalie Tables". Under "Seasons", select every season listed. Click "Submit". Use the "Download .csv" button to download this data. We recommend naming it "EH_goalies.csv".

* Navigate to the "GAR" tab. You are going to want data from both "GAR Skater Tables" and "GAR Goalie Tables". Select each (at separate times). Select all seasons under "Seasons". Click "Submit" and then "Download .csv" in the bottom right. We recommend naming the two downloaded files in the end "EH_goalies_gar.csv" and "EH_skaters_gar.csv".

* Navigate back to the "Standard" tabl and select "Skater Tables". Note under "Table Type" in the ensuing form that there are four types of skater tables: "Box Score", "On-Ice", "Relative to Teammate", and "Zones". For each skater table type separately, select the skater table type, set "Season" to all possible seasons from the drop down, click "Submit", and hit "Download .csv" at the bottom left. Note as you do this that some of these tables have an option for an adjustment in the "Adjustment" field. Any time an option exists, select it. We recommend naming these skater tables, respectively, "EH_skaters_box.csv", "EH_skaters_on_ice_5v5_adjusted.csv", "EH_skaters_relative_5v5_adjusted.csv", and "EH_skaters_zones_5v5.csv".

* Finally, we want to track some data on team-level performance. Navigate to "Standard" again and select "Team Tables". Note that there is a "Stats" tab and a "Standings" tab (among one other tab as well at the top). For each of the "Stats" tab and "Standings" tab, select every available season under "Season" and keep the default settings (including any adjustments). Click submit, then click "Download.csv". We recommend naming these files, respectively, "EH_teams_adjusted.csv" and "EH_teams_standings.csv"

At this point, you should have the core data from *Evolving Hockey* relevant for replicating our process.

## All Three Zones
Navigate to https://www.allthreezones.com/. Make an account and note that there is a thread of posts from Corey Sznajder and those in the All Three Zones community. Mixed in these posts are posts from Corey Sznajder mentioning that he is making all of the raw data for each given year he did this available in a Dropbox folder, which he links. Navigate to that Dropbox folder and download it. If you download it and make no changes other than copying it into your working directory and renaming the folder you copied "Corey Sznajder Data", you should have all of the All Three Zones data you need. As an aside, you may notice that a major part of the code in this repo is dedicated to connecting the otherwise disparate *Evolving Hockey* data and *All Three Zones* data such that zone changes tracked in the *All Three Zones* data appear seamlessly in the *Evolving Hockey* play-by-play data as if they were an event tracked initially in the play-by-play dataset (which they weren't).
