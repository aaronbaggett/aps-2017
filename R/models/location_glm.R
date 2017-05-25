# --------------------------------------------------------------
# MODEL:
# Predicts probability of accurate umpire decision with fixed
# effects and interactions for pitch location (*zone_reg*) 
# and ball-strike count (*bs_count*), with random effects for 
# pitch location, ball-strike count, and umpire
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

glm_model <- glmer(u_test_adj ~ factor(zone_reg)*factor(bs_count) + 
    (factor(zone_reg) + factor(bs_count) | umpire), 
  data = pfx_16, family = binomial, nAGQ = 0)
