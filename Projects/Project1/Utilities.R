View(Utilities)
library(alr3)
library(mosaic)
library(car)
library(lmtest)

mplot(Utilities)

#Insights for X
hist(Utilities$temp)
box(Utilities$temp)
stripchart(Utilities$temp, method = "stack")

plot(temp ~ year, data = Utilities)
plot(temp ~ month, data = Utilities)

#X is not interesting to me, but it's useful to explain y

#Insights for Y
favstats(Utilities$totalbill)

u.lm <- lm(totalbill ~ temp, data = Utilities)
summary(u.lm)

plot(totalbill ~ temp, data = Utilities)
abline(u.lm)

plot(u.lm, which = 1:4)

qqPlot(u.lm$residuals)
boxCox(u.lm)

plot(sqrt(totalbill) ~ temp, data = Utilities)
u.sqrt.lm <- lm(sqrt(totalbill) ~ temp, data = Utilities)
abline(u.sqrt.lm)

summary(u.sqrt.lm)

b <- coef(u.sqrt.lm)
b

plot(totalbill ~ temp, data = Utilities)
curve((b[1] + b[2]*x)^2, add = TRUE)

