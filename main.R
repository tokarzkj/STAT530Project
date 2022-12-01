# This file contains a very straight-forward example using the Fish Market. 
# Probably the best option if we want to get the project done quick.

# Dataset is from https://www.kaggle.com/datasets/aungpyaeap/fish-market

# Link to Google Docs I started:
# https://docs.google.com/document/d/1f98D2EjpBOp1ExCzr1Fxuy8Vmk6Tbwh0-8Ihsb91nc0/edit?usp=sharing
# If we want to be fancy we can use this latex writer
# https://www.overleaf.com/6297936779xqkywtccqbrc

# Read in Data. Using this fish market as a placeholder.
fishData = read.csv("./Fish.csv")
attach(fishData)
# There is a couple instances of fish with 0 weight
#fishData = subset(fishData, Weight > 0)

###################################################################################################################
# 1. Draw graphs to interpret the property of the data. What do you find?
###################################################################################################################

# Species name breaks pairs, so I ignored the first column when setting up our data.
pairs(fishData[2:7])


round(cor(fishData[, c(2:7)]), 3)


###################################################################################################################
# 2. Select and fit the model. Summarize model results
###################################################################################################################
l1sq <- Length1^2
l2sq <- Length2^2
l3sq <- Length3^3
hsq <- Height^2
wsq <- Width^2


model <- lm(formula = Weight ~ Length3 + Height + Width + l3sq + hsq + wsq, data = fishData)
summary(model)

###################################################################################################################
# 3. Analyze the contribution of each predictor
###################################################################################################################

# Use tobs to test individual contributions
# Each tobs is listed in the summary, so we can just calculate the t alpha/2
n = nrow(fishData)
k = 7
qt(1 - 0.05/2, n - k)

# Page 88 has relevant details about T-tests in multivariable model.


# Significance of Regression
# This should indicate that at least one of our parameters is linearly significantly related to y.
model2 <- lm(formula = Weight ~ 1., data = fishData)
anova(model2, model)

alpha = 0.05
qf(1 - alpha, 6, 152)

###################################################################################################################
# 4. Check the model adequacy
###################################################################################################################


plot(x = fitted(model), y=rstandard(model), panel.last = abline(h = 0, lty = 2))

qqnorm(residuals(model), main = "", datax = TRUE)
qqline(residuals(model), datax = TRUE)

###################################################################################################################
# 5. Summarize the analysis results
###################################################################################################################

model.reduced <- lm(formula = Weight ~ l3sq + wsq, data = fishData)
summary(model.reduced)

plot(x = fitted(model.reduced), y=rstandard(model.reduced), panel.last = abline(h = 0, lty = 2))
qqnorm(residuals(model.reduced), main = "", datax = TRUE)
qqline(residuals(model.reduced), datax = TRUE)
