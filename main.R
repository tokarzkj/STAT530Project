# This file contains a very straight-forward example using the Fish Market. 
# Probably the best option if we want to get the project done quick.

# Dataset is from https://www.kaggle.com/datasets/aungpyaeap/fish-market

# Link to Google Docs I started:
# https://docs.google.com/document/d/1f98D2EjpBOp1ExCzr1Fxuy8Vmk6Tbwh0-8Ihsb91nc0/edit?usp=sharing
# If we want to be fancy we can use this latex writer
# https://www.overleaf.com/6297936779xqkywtccqbrc

# Read in Data. Using this fish market as a placeholder.
fishData = read.csv("./Fish.csv")

# There is a couple instances of fish with 0 weight
fishData = subset(fishData, Weight > 0)

###################################################################################################################
# 1. Draw graphs to interpret the property of the data. What do you find?
###################################################################################################################

# Species name breaks pairs, so I ignored the first column when setting up our data.
pairs(fishData[2:7])

# Weight doesn't look linear compared to other values. We can check our distribution of Weight.
hist(fishData$Weight, col='steelblue', main='Original')
hist(log10(fishData$Weight), col='coral', main='Log Transformation')

# Log transformation can make our distribution a bit better and therefore better fulfill the assumption
# that our data is normally distributed. We could also work out a more complex model.

#This is assigned to fishData now, so we can use weight_log as our y.
fishData$Weight_log = log10(fishData$Weight)

###################################################################################################################
# 2. Select and fit the model. Summarize model results
###################################################################################################################
model <- lm(formula = Weight_log ~ Length1 + Length2 + Length3 + Height + Width, data = fishData)
summary(model)

###################################################################################################################
# 3. Analyze the contribution of each predictor
###################################################################################################################

# Use tobs to test individual contributions
# Each tobs is listed in the summary, so we can just calculate the t alpha/2
n = nrow(fishData)
k = 6
qt(1 - 0.05/2, n - k)

# Reviewing page 88 in the book regarding T tests and MLR models it appears that 
# Height contribute the most to this model relative to the other regressors.


# Significance of Regression
# This should indicate that at least one of our parameters is linearly significantly related to y.
model2 <- lm(formula = Weight_log ~ 1., data = fishData)
anova(model2, model)

alpha = 0.05
qf(1 - alpha, 5, 153)

###################################################################################################################
# 4. Check the model adequacy
###################################################################################################################


# Looks like a not good model
plot(x = fitted(model), y=rstandard(model), panel.last = abline(h = 0, lty = 2))

qqnorm(residuals(model), main = "", datax = TRUE)
qqline(residuals(model), datax = TRUE)

###################################################################################################################
# 5. Summarize the analysis results
###################################################################################################################