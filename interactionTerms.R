fishData = read.csv("./Fish.csv")
attach(fishData)

l1sq <- Length1^2
l2sq <- Length2^2
l3sq <- Length3^3
hsq <- Height^2
wsq <- Width^2
l3w <- Length3 * Width
l3h <- Length3 * Height
wh <- Width * Height


model <- lm(formula = Weight ~  Length3 + Height + Width + Species)
summary(model)

n = nrow(fishData)
k = 6 
qt(1 - 0.05/2, n - k)

model <- lm(formula = Weight ~  Length3 + Height + Width + wsq + l3sq + l3w + hsq + l3h + wh +Species)
summary(model)

plot(x = fitted(model), y=rstandard(model), panel.last = abline(h = 0, lty = 2))

qqnorm(residuals(model), main = "", datax = TRUE)
qqline(residuals(model), datax = TRUE)
