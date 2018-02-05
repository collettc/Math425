cars.lm <- lm(dist ~ speed, data = cars)

plot(dist ~ speed, data=cars)
cars.lm <- lm(dist ~ speed, data=cars)
abline(cars.lm)
summary(cars.lm)
confint(cars.lm)
predict(cars.lm, data.frame(speed = 15), interval="prediction")
predict(cars.lm, data.frame(speed = 15), interval="confidence")
par(mfrow=c(2,2), mai=c(0.6,0.6,0.1,0.1))
plot(dist ~ speed, data=cars)
par(mfrow=c(2,2), mai=c(0.8,0.8,0.1,0.1))
plot(dist ~ speed, data=cars)
plot(dist ~ speed, data=cars)
par(mfrow=c(2,2), mai=c(0.8,0.8,0.1,0.1))
plot(dist ~ speed, data=cars)
plot(cars.lm, which=1:2)
plot(cars.lm$residuals)
summary(cars.lm)
confint(cars.lm)
predict(cars.lm, data.frame(speed = 15), interval="prediction")
predict(cars.lm, data.frame(speed = 15), interval="confidence")



boxCox(cars.lm)

boxCox(cars.lm, lambda = seq(0,4,.1))

lob.lm <- lm(height ~ age, data = Loblolly)

boxCox(lob.lm, lambda = seq(1,2,.1))
plot(height^1.3 ~ age, data = Loblolly)
lob.lm.1.3 <- lm(height^1.3 ~ age, data = Loblolly)
abline(lob.lm.1.3)
summary(lob.lm.1.3)

plot(height ~ age, data  = Loblolly)
b <- coef(lob.lm.1.3)
b
curve((b[1] +b[2]*x)^(1/1.3), add = TRUE)
