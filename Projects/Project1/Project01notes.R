library(car)
library(alr3)
library(lmtest)
library(ggplot2)
library(dplyr)

happinessred <- happiness %>%
  mutate(Population = Population/1000000)

plot(Happiness ~ Population, data = happinessred)
happinessred.lm <- lm(Happiness ~ Population, data = happinessred)
abline(happinessred.lm)
summary(happinessred.lm)





##This is getting the BoxCox. It doesn't seem to be very effective. 

plot(Happiness ~ Population, data = happinessred)
happinessred.lm <- lm(Happiness ~ Population, data = happinessred)
abline(happinessred.lm)

#We tranform our Y values by 1.3 found by boxcox
boxCox(happinessred.lm, lambda = seq(0,4,0.1))

#We plot the regression in our faux universe
plot(Happiness^2 ~ Population, data = happinessred)
happinessred.lm.2 <- lm(Happiness^2 ~ Population, data = happinessred)
abline(happinessred.lm.2)

#We pull out our values for this regression...
summary(happinessred.lm.2)

#And plot them into the real world.
plot(Happiness ~ Population, data  = happinessred)
b <- coef(happinessred.lm.2)
b
curve((b[1] +b[2]*x)^2, add = TRUE)








#This is our Breush - Pagan Test. Still seems that Null is true. 
bptest(happiness.lm)

##Assumptions. 

par(mfrow=c(1,1))
plot(happiness.lm$residuals, pch = 16, cex = 0.8)


#Sequence plot of residuals - should show chaos

plot(happiness.lm$residuals)

#Residuals vs. Fitted or X values

plot(happiness.lm$residuals ~ happiness.lm$fitted.values)

#QQ plot

qqPlot(happiness.lm$residuals)


#semistudentized residuals converts the fitted values residuals into standard deviations
happiness.semistud <- (happiness.lm$residuals) - mean(happiness.lm$residuals)/summary(happiness.lm)$sigma

happiness.semistud

hist(happiness.semistud)

#Summary
summary(happinessred.lm)

#ANOVA TABLE 
anova(happinessred.lm)

#Pure Error ANOVA
pureErrorAnova(happinessred.lm)



#Review

###
Confidence band
SSR
SSE78


#Utilities

plot(totalbill ~ temp, data = Utilities)
#Put the Lowess Curve
lines(lowess(Utilities$temp, Utilities$totalbill))
lin.lm <- lm(totalbill ~ temp, data = Utilities)
abline(lin.lm)
summary(lin.lm)

#Prediction Interval

predict(lin.lm, data.frame(temp = 20), interval = "prediction")
abline(h =c(157.7899, 316.0275), lty=2)

#boxcox

boxCox(lin.lm)
plot(sqrt(totalbill) ~ temp, data = Utilities)
sqrt.lm <- lm(sqrt(totalbill) ~ temp, data = Utilities)
abline(sqrt.lm)
predict(sqrt.lm, data.frame(temp=20), interval="prediction")
abline(h = c(12,23175, 18.34745), lty = 2)
abline(v=20, lty=2)
plot(totalbill ~ temp, data = Utilities)
b <- coef(sqrt.lm)
curve((b[1] + b[2]*x)^2, add = TRUE)
abline(lin.lm, col = "red")
abline(h=c(12.23175^2, 18.34745^2), col = "red")
plot(totalbill ~ I(exp(-temp)), data = Utilities)

xp <- 1/Utilities$temp
plot(totalbill ~ I(1/temp), data = Utilities)
