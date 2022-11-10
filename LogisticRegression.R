# Because I am extra here is a logistic regression file that we can play with.

candyData = read.csv("./candy-data.csv")[2:13]
attach(candyData)
pairs(formula = ~ sugarpercent + pricepercent + winpercent, data = candyData)

model <- lm(formula = winpercent ~ ., data = candyData)
summary(model)