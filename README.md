# Introduction
How much do faceoffs actually matter? Many of the seismic developments in hockey analytics recently have been characterized by the importance of puck possession, a key determinant in the latest models such as expected goals and other metrics like Corsi. Yet amid this shift to focusing on possession, there is yet to be consensus or any similarly robust models on the importance of faceoffs, the most frequent and decisive determinant of possession. This project analyzes how faceoffs drive offensive and defensive results and impact teams winning games.

# Data
Our data primarily comes from two places, both of which are publicly available sources:

* **Evolving Hockey**: we use two key categories of data from *Evolving Hockey* in building our statistical models: (1) play-by-play data (i.e. tabulated events from hockey games) and (2) player performance data (i.e. measures of performance by player season)

* **All Three Zones**: we specifically rely on these microstats hand-tracked by Corey Sznajder to detect zone changes in the subset of games he tracks, which is key to conceptions of faceoff attributability in our models.

We encourage anyone aiming to replicate our work to go to (https://evolving-hockey.com/)[https://evolving-hockey.com/] and (https://www.allthreezones.com/)[https://www.allthreezones.com/] to download the publicly available data. To assist with easy replication, we offer the following detailed list of instructions on how we store the data from both these websites in terms of how we access them in our code when building and training our statistical models.

## Evolving Hockey
* **Play-by-Play Data:** navigate to the "Queries" tab on the *Evolving Hockey* website and then select "PBP Query". You will need to download every season separately by changing the "Season" field in the form that pops up. Note that you will want the date range to encompass each full season. We recommend using the earliest- and latest- possible selectable dates, which typically are around October 7th to June 13th (within a few days in either direction). Make sure to select all listed event options in the "Event Types" field. Then click "Submit" and use the "Downlload .csv" button at the bottom left to save the result. We recommend renaming the downloaded file to "merged_pbp_with_shots_[Season Start Year]_[Season_Start_Year_Plus_One]" for convenience.

* At this point, you have the play-by-play data and can shift to considering measured performance by player seasons.
