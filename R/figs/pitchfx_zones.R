# --------------------------------------------------------------
# FIGURE: All location zones in PITCHf/x data
#
# TITLE: Effects of Pitch Location and Count on 
# Professional Baseball Umpires' Ball/Strike Decisions
#
# APS 2017
# Aaron R. Baggett, Ph.D.
# May 24, 2017
# --------------------------------------------------------------

# Load package libraries
library(ggplot2)

# PITCHf/x Native Zones
strike_zones <- data.frame(
x1 = rep(-1.5:0.5, each = 3),
x2 = rep(-0.5:1.5, each = 3),
y1 = rep(1.5:3.5, 3),
y2 = rep(2.5:4.5, 3),
z = factor(c(7, 4, 1, 8, 5, 2, 9, 6, 3))
)

ball_zones <- data.frame(
xmin = -3,
xmax = 3,
ymin = 0,
ymax = 6)

# Black and White
ggplot() + 
  xlim(-3, 3) + xlab("") +
  ylim(0, 6) + ylab("") +
  geom_rect(data = ball_zones, 
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
      fill = "grey65", color = "grey20") +
  geom_segment(aes(x = 0, y = 1.5, xend = 0, yend = 0), color = "grey20") +
  geom_segment(aes(x = 0, y = 6, xend = 0, yend = 4.5), color = "grey20") +
  geom_segment(aes(x = -3, y = 3, xend = -1.5, yend = 3), color = "grey20") +
  geom_segment(aes(x = 3, y = 3, xend = 1.5, yend = 3), color = "grey20") +
  geom_text(aes(x = 1.5, y = 5, label = "12"), 
    size = 7, fontface = 2, color = I("grey20")) +
  geom_text(aes(x = -1.5, y = 5, label = "11"), 
    size = 7, fontface = 2, color = I("grey20")) +
  geom_text(aes(x = 1.5, y = 1, label = "14"), 
    size = 7, fontface = 2, color = I("grey20")) +
  geom_text(aes(x = -1.5, y = 1, label = "13"), 
    size = 7, fontface = 2, color = I("grey20")) +
  geom_rect(data = strike_zones, 
    aes(xmin = x1, xmax = x2, ymin = y2, ymax = y1), fill = "grey75", color = "grey20") +
    scale_fill_brewer(palette = "Pastel1") +
  geom_text(data = strike_zones, 
    aes(x = x1 + (x2 - x1)/2, y = y1 + (y2 - y1)/2, label = z), 
    size = 7, fontface = 2, color = I("grey20")) + 
  scale_x_continuous(limits = c(-3, 3), breaks = seq(-3, 3, 0.75), 
    name = "\nWidth of Strike and Ball Zone Area") +
  scale_y_continuous(limits = c(0, 6), breaks = seq(0, 6, 1.00), 
    name = "Height of Strike and Ball Zone Area\n") + theme_bw() +
  theme(legend.position = "none") + 
  theme(panel.grid.minor.x = element_blank(), panel.grid.major.x = element_blank()) +
  theme(panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank())
