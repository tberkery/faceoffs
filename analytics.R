library(tidyverse)
library(ggplot2)

off_off = read_csv("training_data_all_offensive_offensive.csv")
def_def = read_csv("training_data_all_defensive_defensive.csv")

summary(off_off$net_xg)

ggplot(off_off, aes(x = TOI_Win_All, y = net_xg, group = GAR_Win_All)) + geom_density(kernel = 'gaussian')

geom_smooth(method = 'lm')
ggplot(off_off, aes(x = net_xg)) + geom_histogram(bins = 100)
ggplot(off_off, aes(x = GAR_Win_All, y = net_xg)) + geom_density(kernel = 'gaussian')


temp2 = pbp %>% select(game_id, event_type) %>% filter(event_type == 'FAC') %>% group_by(game_id) %>% mutate(event_type = n()) %>% distinct(.keep_all = TRUE) %>% arrange(desc(event_type))
temp = pbp %>% select(event_type) %>% filter(event_type == 'FAC')
temp = pbp %>% select(event_type, event_zone) %>% filter(event_type == 'FAC')
temp = pbp %>% select(event_type, event_zone) %>% filter(event_type == 'FAC') %>% group_by(event_zone) %>% mutate(event_type = n()) %>% distinct(.keep_all = TRUE)
View(temp)
(98686+98823) / (98686+98823+91670)
off / tot * 0.023 + def / tot * 0.020