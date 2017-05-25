# --------------------------------------------------------------
# FIGURE: Individual umpire accuracy over seson
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

# Individual umpire accuracy rate for season
ind_acc <- pfx_16 %>% 
  group_by(umpire) %>% 
  summarize(mean_acc = mean(u_test_adj),
    sd_acc = sd(u_test_adj)) %>% 
  arrange(mean_acc)

# Relevel umpire names to prevent ggplot resorting
ind_acc$umpire <- factor(ind_acc$umpire, levels = ind_acc$umpire)

# Dot plot of individual umpire accuracy for season
ggplot(data = ind_acc, mapping = aes(x = mean_acc, y = umpire, group = 1)) + 
  geom_point(color = "dodgerblue3") + geom_line(color = "dodgerblue3") + 
  geom_vline(xintercept = 0.9357033, color = "tomato") +
  scale_x_continuous(breaks = seq(0, 1, .005), 
    name = "\nMean Accuracy Over Season") +
  scale_y_discrete(name = "Umpire\n") +
  theme_bw() + theme(panel.grid.minor = element_blank())
