# --------------------------------------------------------------
# FIGURE: Aggregated umpire accuracy per day over seson
#
# TITLE: Effects of Pitch Location and Count on 
# Professional Baseball Umpires' Ball/Strike Decisions
#
# APS 2017
# Aaron R. Baggett, Ph.D.
# May 27, 2017
# --------------------------------------------------------------

# Load package libraries
library(dplyr)
library(ggplot2)
library(repmis)

# Load *pfx_16* data
repmis::source_data("https://github.com/aaronbaggett/aps-2017/blob/master/data/pfx_16.Rda?raw=true")

# Umpire accuracy rates per game over season
cum_acc <- pfx_16 %>% 
  group_by(game_date) %>% 
  summarize(mean_acc = mean(u_test_adj)) %>% 
  mutate(day_num = 1:length(game_date))

# Line plot of cumulative accuracy across season
ggplot(data = cum_acc, mapping = aes(x = day_num, y = mean_acc)) + 
  geom_line(color = "dodgerblue3") + 
  geom_vline(xintercept = 181, color = "tomato") +
  geom_hline(yintercept = 0.9359443, color = "tomato") +
  scale_x_continuous(breaks = seq(0, 205, 10), 
    name = "\nDay Number of Season") +
  scale_y_continuous(breaks = seq(0, 1, .01), 
    name = "Mean Accuracy per Day\n") +
  theme_bw() + theme(panel.grid.minor = element_blank())
