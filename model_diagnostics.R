fishData = read.csv("./Fish.csv")

# There is a couple instances of fish with 0 weight
fishData = subset(fishData, Weight > 0)

# Species name breaks pairs, so I ignored the first column when setting up our data.
pairs(fishData[2:7])

# Weight doesn't look linear compared to other values. We can check our distribution of Weight.
hist(fishData$Weight, col='steelblue', main='Original')
hist(log10(fishData$Weight), col='coral', main='Log Transformation')

# Log transformation can make our distribution a bit better and therefore better fulfill the assumption
# that our data is normally distributed. We could also work out a more complex model.

#This is assigned to fishData now, so we can use weight_log as our y.
fishData$Weight_log = log10(fishData$Weight)

# New pairs plot with Weight_log
pairs(fishData[3:8])

# From the pairs plot, we can see that the regressors are not independent, have 
# look at collinearity and correlation between exp. variables
round(cor(fishData[,c(3:8)]), 3)

# All correlation coeff. > 0.5, problematic...

################################################################################

# Initial model to test regressors
attach(fishData)
model <- lm(formula = Weight_log ~ Length1 + Length2 + Length3 + Height + Width)
summary(model)

# Use tobs to test individual contributions
# Each tobs is listed in the summary, so we can just calculate the t alpha/2
n = nrow(fishData)
k = 6
qt(1 - 0.05/2, n - k)

# t_obs for Height and Length3 are less than t_crit, so we drop them
model_update = lm(formula = Weight_log ~ Length1 + Length2 + Width)
summary(model_update)
plot(x = fitted(model), y=rstandard(model), panel.last = abline(h = 0, lty = 2))

# residuals suggest polynomial model, pairs plot suggest some variables may be related, 
# take a look at squaring significant coefficients
lsq1 = Length1**2
lsq2 = Length2**2
model1 = lm(formula = Weight_log ~ Length1 + Length2 + Width+ lsq1 )
summary(model1)
plot(x = fitted(model1), y=rstandard(model1), panel.last = abline(h = 0, lty = 2))

model2 = lm(formula = Weight_log ~ Length1 + Length2 + Width+ lsq2 )
summary(model2)
plot(x = fitted(model2), y=rstandard(model2), panel.last = abline(h = 0, lty = 2))

wsq = Width**2
model3 = lm(formula = Weight_log ~ Length1 + Length2 + Width+ wsq+lsq1)
summary(model3)
plot(x = fitted(model3), y=rstandard(model3), panel.last = abline(h = 0, lty = 2))

# model3 has = better adj. R-sqr value and residual plot looks better than other 2 models, 
# next: try to add interaction terms ince the residuals plot still does not look ideal 
# (residuals are still clustered a bit)
