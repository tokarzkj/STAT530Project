# This file contains a very straight-forward example using the Fish Market. 
# Probably the best option if we want to get the project done quick.
# Link to Google Docs I started:
# https://docs.google.com/document/d/1f98D2EjpBOp1ExCzr1Fxuy8Vmk6Tbwh0-8Ihsb91nc0/edit?usp=sharing
# If we want to be fancy we can use this latex writer
# https://www.overleaf.com/6297936779xqkywtccqbrc

# Read in Data. Using this fish market as a placeholder.
fishData = read.csv("./Fish.csv")[2:7]
attach(fishData)
# 1. Draw graphs to interpret the property of the data. What do you find?

# Species name breaks pairs, so I ignored the first column when setting up our data.
pairs(fishData)

# 2. Select and fit the model. Summarize model results

model <- lm(formula = Weight ~ ., data = fishData)
summary(model)

# 3. Analyze the contribution of each predictor

# Should we use t-test and f-test here?

# Significance of Regression
model2 <- lm(formula = Weight ~ 1., data = fishData)
anova(model2, model)

alpha = 0.05
qf(1 - alpha, 5, 153)


# 4. Check the model adequacy

# 5. Summarize the analysis results