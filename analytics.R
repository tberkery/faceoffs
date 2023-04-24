library(tidyverse)
library(ggplot2)

off_off = read_csv("training_data_all_offensive_offensive.csv")
def_def = read_csv("training_data_all_defensive_defensive.csv")

summary(off_off$net_xg)

ggplot(off_off, aes(x = TOI_Win_All, y = net_xg, group = GAR_Win_All)) + geom_density(kernel = 'gaussian')

geom_smooth(method = 'lm')
ggplot(off_off, aes(x = net_xg)) + geom_histogram(bins = 100)
ggplot(off_off, aes(x = GAR_Win_All, y = net_xg)) + geom_density(kernel = 'gaussian')
