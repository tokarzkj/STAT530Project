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

# Log transformation can make our distribution a bit better and therefore better fulfill the assumption
# that our data is normally distributed. We could also work out a more complex model.

#This is assigned to fishData now, so we can use weight_log as our y.
fishData$Weight_log = sqrt(fishData$Weight)


# Species name breaks pairs, so I ignored the first column when setting up our data.
pairs(fishData[2:7])

# Weight doesn't look linear compared to other variables.
hist(fishData$Weight, col='steelblue', main='Original')
hist(fishData$Weight_log, col='coral', main='Log Transformation')

###################################################################################################################
# 2. Select and fit the model. Summarize model results
###################################################################################################################
model <- lm(formula = Weight_log ~ Length1 + Length2 + Length3 + Height + Width, data = fishData)
summary(model)
# Both R^2 and adjusted R^2 decreased slightly.

###################################################################################################################
# 3. Analyze the contribution of each predictor
###################################################################################################################

# Use tobs to test individual contributions
# Each tobs is listed in the summary, so we can just calculate the t alpha/2
n = nrow(fishData)
k = 6
qt(1 - 0.05/2, n - k)

# Page 88 has relevant details about T-tests in multivariable model.


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



# I am pretending we are sticking with the current model and will focus on using the multicollinearity topic.
# Totally cool with throwing this model away in favor of the second order one.
round(cor(fishData[, c(2:8)]), 3)

# Length 3 (I think that is the cross measurement) is the strongest. Based on class, we could keep that and toss Length 1 & 2
model.reduced = lm(formula = Weight_log ~ Length3 + Height + Width, data = fishData)
summary(model.reduced)

n = nrow(fishData)
k = 6
qt(1 - 0.05/2, n - k)

# None of these look much better which is probably because a linear model on non-linear relationship will never get better
plot(x = fitted(model.reduced), y=rstandard(model.reduced), panel.last = abline(h = 0, lty = 2))

qqnorm(residuals(model.reduced), main = "", datax = TRUE)
qqline(residuals(model.reduced), datax = TRUE)
