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
