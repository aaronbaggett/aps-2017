# --------------------------------------------------------------
# MODEL:
# Predicts probability of accurate umpire decision with fixed
# effects for ball-strike count (*bs_count*), with random effects
# for umpire
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
library(lme4)

# Load *pfx_16* data
repmis::source_data("https://github.com/aaronbaggett/aps-2017/blob/master/data/pfx_16.Rda?raw=true")

# Inverse logit function
inv.logit <- function(x){
	1 / (1 + exp(-x))
}

count_glm <- glmer(u_test ~ factor(bs_count) + (1 | umpire), 
  data = pfx_16, family = binomial, nAGQ = 0)
