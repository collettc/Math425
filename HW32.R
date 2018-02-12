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

#There's an issues with  regression and we see that the variance gets larger
plot(height ~ age, data = Loblolly)
lob.lm <- lm(height ~ age, data = Loblolly)
abline(lob.lm)

#We tranform our Y values by 1.3 found by boxcox
boxCox(lob.lm, lambda = seq(1,2,.1))

#We plot the regression in our faux universe
plot(height^1.3 ~ age, data = Loblolly)
lob.lm.1.3 <- lm(height^1.3 ~ age, data = Loblolly)
abline(lob.lm.1.3)

#We pull out our values for this regression...
summary(lob.lm.1.3)

#And plot them into the real world.
plot(height ~ age, data  = Loblolly)
b <- coef(lob.lm.1.3)
b
curve((b[1] +b[2]*x)^(1/1.3), add = TRUE)



#Three different tables to know

summary(lob.lm)
anova(lob.lm)
pureErrorAnova(lob.lm)

bptest(lob.lm)

plot(height ~ age, data = Loblolly) 
lob.lm <- lm(height ~ age, data = Loblolly)
abline(lob.lm)
plot(lob.lm, which = 1)

lob.sqrt.lm <- lm(sqrt(height) ~ age, data = Loblolly)
plot(sqrt(height) ~ age, data = Loblolly)
abline(lob.sqrt.lm)
#Protect the X variable with "I" 
plot(sqrt(height) ~ I(sqrt(age)), data = Loblolly)

lob.sqrt.lm.with.x <- lm(sqrt(height) ~ I(log10(age)), data = Loblolly)
abline(lob.sqrt.lm.with.x)

lob.sqrt.lg.lm <- lm(sqrt(height) ~ I(log10(age)), data = Loblolly)
plot(sqrt(height) ~ I(log10(age)), data = Loblolly)
abline(lob.sqrt.lg.lm)

b2 <- coef(lob.sqrt.lg.lm)

curve((b2[1] + b2[2]*log10(x))^2, add = TRUE, col = "skyblue")


#Lowess curve

lines(lowess(Loblolly$age, Loblolly$height), col = "red")
