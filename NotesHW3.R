#Assumption pg 103

# R code to make things easier

cars.lm <- lm(dist~speed, data = cars)
summary(cars.lm)

#Finding Standard Deviation of residuals
sd(cars.lm$residuals)

#Square root of the MSE
summary(cars.lm)$sigma

plot(cars.lm)

#Semi-Studentized Residuals
semistud <- (cars.lm$residuals) - mean(cars.lm$residuals)/summary(cars.lm)$sigma

semistud

stem(semistud)

hist(semistud)

stripchart(semistud, method = 'stack')

boxplot(cars.lm$residuals)


#sequence plot
plot(cars.lm$residuals, type = 'b', pch = 16, cex = 0.8)

par(mfrow = c(2,1))
plot(cars.lm, which  = 1:6)

#residuals verses x

plot(cars.lm$residuals ~ cars$speed)


plot(dist~speed, data = cars)
cars.lm <- lm(dist~speed, data = cars)
abline(cars.lm)
summary(cars.lm)

#This is how to get the yhat's out of your lm object.
cars.lm$fitted.values

SSE <- sum(cars.lm$residuals^2)
SSR <- sum( (cars.lm$fitted.values - mean(cars$dist))^2)
SSTO <- SSE + SSR

anova(cars.lm)

#semistudentized residuals converts the fitted values residuals into standard deviations

plot(height ~ age, data = Loblolly)
lob.lm <- lm(height ~ age, data = Loblolly)
abline(lob.lm)
plot(lob.lm, which = 1)

#Brown - Forsythe Test




#Breusch-Pagan Test
bptest(car.lm)

#library(lawstat)
library(car)
qqPlot(cars.lm$residuals)
qq.car <- qqnorm(cars.lm$residuals)
names(qq.car)

cor(qq.car$x, qq.car$y)




