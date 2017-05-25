# --------------------------------------------------------------
# FIGURE: Predicted probability of accurate decision given 
# count and location
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
repmis::source_data("https://github.com/aaronbaggett/aps-2017/blob/master/data/glm_model.Rda?raw=true")

# Inverse logit function
inv.logit <- function(x){
	1 / (1 + exp(-x))
}

# Extract random components for variance/covariance table
glm_model_vc <- as.data.frame(VarCorr(glm_model))
names(glm_model_vc) <- c("Group", "V1", "V2", "VCov", "SD/Cor")
xtable(glm_model_vc[, -1])

# Model 3d predicted probabilities table
pfx_16.pred <- pfx_16
X <- model.matrix(terms(glm_model), data = pfx_16.pred)
b <- fixef(glm_model)
pred.logit <- X %*% b

pred.prob  <- inv.logit(pred.logit)

pfx_16.pred2 <- data.frame(cbind("pred.logit" = pred.logit, 
  "pred.prob" = pred.prob, zone = pfx_16.pred$zone_reg, 
  count = pfx_16.pred$bs_count))

pfx_16.pred2$zone <- as.factor(pfx_16.pred2$zone)
levels(pfx_16.pred2$zone) <- c("Ball", "Inner", "Middle", "Outer")

pfx_16.pred2$count <- as.factor(pfx_16.pred2$count)
levels(pfx_16.pred2$count) <- c("Neutral", "Batter", "Pitcher")

colnames(pfx_16.pred2)[1:2] <- c("pred.logit","pred.prob")
(pred_prob_table <- unique(pfx_16.pred2[order(pfx_16.pred2$zone, 
  pfx_16.pred2$count), ])
)

# Plot pedicted probabilities by count and location
ggplot(data = pred_prob_table, aes(x = count, y = pred.prob, group = zone, 
  shape = zone)) + geom_line(aes(linetype = zone), color = "gray30") + 
  geom_point(aes(shape = zone), color = "gray30", size = 3) + 
  theme_bw() + theme(panel.grid.minor = element_blank()) +
  theme(panel.grid.major = element_blank()) +
  xlab("\nBall-Strike Count Advantage") + 
  ylab("Umpire Decision Accuracy\n") +
  scale_x_discrete(labels = c("Batter", "Neutral", "Pitcher")) +
  scale_y_continuous(limits = c(0.84, 0.99), breaks = seq(0.84, 0.99, 0.015)) +
  theme(legend.title = element_blank()) +
  theme(legend.position = c(0.09, 0.19)) +
  theme(legend.background = element_rect(colour = NA, fill = NA)) +
  theme(legend.title = element_blank())
