# --------------------------------------------------------------
# FIGURE: Pitch location regions
#
# TITLE: Effects of Pitch Location and Count on 
# Professional Baseball Umpires' Ball/Strike Decisions
#
# APS 2017
# Aaron R. Baggett, Ph.D.
# May 27, 2017
# --------------------------------------------------------------

# Load package libraries
library(ggplot2)

# PITCHf/x Native Zones
zone.regs <- data.frame(
x1 = rep(-1.5:0.5, each = 3),
x2 = rep(-0.5:1.5, each = 3),
y1 = rep(1.5, each = 9),
y2 = rep(4.5, each = 9),
rh = rep(c("RHB INNER", "RHB MIDDLE", "RHB OUTER"), each = 3),
lh = rep(c("LHB OUTER", "LHB MIDDLE", "LHB INNER"), each = 3)
)

# Black and White
ggplot() + 
  xlim(-3, 3) + xlab("") +
  ylim(0, 6) + ylab("") +
  geom_rect(data = zone.regs, 
    aes(xmin = x1, xmax = x2, ymin = y2, ymax = y1), fill = "grey75", color = "grey20") +
    scale_fill_brewer(palette = "YlGnBu") +
  geom_text(data = zone.regs, 
    aes(x = x1 + .5, y = 3.15, label = lh), 
    size = 3.25, fontface = 2, color = I("grey20")) + 
  geom_text(data = zone.regs, 
    aes(x = x2 - 0.5, y = 2.85, label = rh), 
    size = 3.25, fontface = 2, color = I("grey20")) + 
  scale_x_continuous(limits = c(-3, 3), breaks = seq(-3, 3, 0.75), 
    name = "\nWidth of Strike Zone Area") +
  scale_y_continuous(limits = c(0, 6), breaks = seq(0, 6, 1.00), 
    name = "Height of Strike Zone Area\n") + theme_bw() +
  theme(legend.position = "none") + 
  theme(panel.grid.minor.x = element_blank(), panel.grid.major.x = element_blank()) +
  theme(panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank())
