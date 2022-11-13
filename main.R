# This file contains a very straight-forward example using the Fish Market. 
# Probably the best option if we want to get the project done quick.

# Dataset is from https://www.kaggle.com/datasets/aungpyaeap/fish-market

# Link to Google Docs I started:
# https://docs.google.com/document/d/1f98D2EjpBOp1ExCzr1Fxuy8Vmk6Tbwh0-8Ihsb91nc0/edit?usp=sharing
# If we want to be fancy we can use this latex writer
# https://www.overleaf.com/6297936779xqkywtccqbrc

# Read in Data. Using this fish market as a placeholder.
fishData = read.csv("./Fish.csv")
# 1. Draw graphs to interpret the property of the data. What do you find?

# Species name breaks pairs, so I ignored the first column when setting up our data.
pairs(fishData[2:7])

# 2. Select and fit the model. Summarize model results

model <- lm(formula = Weight ~ Length1 + Length2 + Length3 + Height + Width, data = fishData)
summary(model)

# 3. Analyze the contribution of each predictor

# Use tobs to test individual contributions

# b1 = 0 - Length1
b1_tobs = (62.355 - 0)/40.209

# b2 = 0 - Length2
b2_tobs = (-6.527 - 0)/41.759

# b3 = 0 - Length3
b3_tobs = (-29.026 - 0)/17.353

# b4 = 0 - Height
b4_tobs = (28.297 - 0)/8.729

# b5 = 0 - Width
b5_tobs = (22.473 - 0)/20.372

print(b1_tobs)
print(b2_tobs)
print(b3_tobs)
print(b4_tobs)
print(b5_tobs)

n = nrow(fishData)
k = 6
qt(1 - 0.05/2, n - k)

# Reviewing page 88 in the book regarding T tests and MLR models it appears that 
# Height contribute the most to this model relative to the other regressors.


# Significance of Regression
# This should indicate that at least one of our parameters is linearly significantly related to y.
model2 <- lm(formula = Weight ~ 1., data = fishData)
anova(model2, model)

alpha = 0.05
qf(1 - alpha, 5, 153)

# 4. Check the model adequacy
# Looks like a not good model
plot(x = fitted(model), y=rstandard(model), panel.last = abline(h = 0, lty = 2))

qqnorm(residuals(model), main = "", datax = TRUE)
qqline(residuals(model), datax = TRUE)


# 5. Summarize the analysis results