# This file contains a very straight-forward example using the Fish Market. 
# Probably the best option if we want to get the project done quick.


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

# 4. Check the model adequacy

# 5. Summarize the analysis results