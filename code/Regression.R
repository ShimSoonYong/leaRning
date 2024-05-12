setwd("C:/Users/user/OneDrive/바탕 화면/leaRning")

#Simple Linear Regression
caterpillardata <- read.table("datasets/caterpillar.txt", header = T)
attach(caterpillardata)
head(caterpillardata)

plot(tannin, growth, pch = 19, col = "red", 
     xlab = "% of tannin in diet", ylab = "growth rate")
lm(growth ~ tannin)
detach(caterpillardata)

lm(growth ~ tannin, data=caterpillardata)
lm(growth ~ tannin, data=caterpillardata) -> caterpillar_model
summary(caterpillar_model)
summary(lm(growth ~ tannin, data=caterpillardata))
anova(lm(growth ~ tannin, data=caterpillardata))

#Multiple Linear Regression
ozone_pollution <- read.table("datasets/ozone_pollution.txt", header = T)
attach(ozone_pollution)
pairs(ozone_pollution, col = "red")
detach(ozone_pollution)

attach(ozone_pollution)
ozone_mod1 <- lm(ozone ~ rad + temp + wind)
summary(ozone_mod1)
anova(ozone_mod1)

plot( rad,ozone, xlim = c(0, 400), ylim = c(-100, 300))
abline(a = ozone_mod1$coefficients[1], b = ozone_mod1$coefficients[2])
plot( temp,ozone )
abline(a = ozone_mod1$coefficients[1], b = ozone_mod1$coefficients[3])
plot( wind,ozone,xlim = c(-25, 50), ylim = c(0, 200))
abline(a = ozone_mod1$coefficients[1], b = ozone_mod1$coefficients[4])

#No intercept Regression model
ozone_mod2 <- lm(ozone ~ rad + temp + wind + 0)
summary(lm(ozone ~ rad + temp + wind - 1))
anova(ozone_mod2)

plot( rad,ozone, xlim = c(0, 400), ylim = c(-100, 300))
abline(a = 0, b = ozone_mod2$coefficients[1])
plot( temp,ozone)
abline(a = 0, b = ozone_mod2$coefficients[2])
plot( wind,ozone,xlim = c(-25, 50), ylim = c(0, 200))
abline(a = 0, b = ozone_mod2$coefficients[3])

anova(ozone_mod1, ozone_mod2)

detach(ozone_pollution)

#Multiple Linear Regression with Categorical Covariates
yields <- read.table("datasets/yields.txt", header=T)
yields


yields_long <- stack(yields)
head(yields_long)

names(yields_long) <- c("yield", "soil")
head(yields_long)

yields_model <- lm(yield ~ soil, data = yields_long)
summary(yields_model)

#Can arbitrarily modify reference category
yields_long$soil <- factor(yields_long$soil, levels = c("loam", "clay", "sand"))
yields_model2 <- lm(yield ~ soil, data = yields_long)
summary(yields_model2)

dummy_matrix <- model.matrix(~ yields_long$soil -1)
head(dummy_matrix)

new_frame <- data.frame(yields_long, dummy_matrix)
head(new_frame)
yields_model3 <- lm(yield ~ yields_long.soilloam + yields_long.soilclay + yields_long.soilsand -1, data = new_frame)
summary(yields_model3)

#Interaction between covariates
gain <- read.table("datasets/Gain.txt", header = T)
summary(gain)
gain$Genotype <- as.factor(gain$Genotype)

attach(gain)
gain_mod1 <- lm(Weight ~ Sex + Age + Genotype)
summary(gain_mod1)
t.test(gain_mod1$residuals)

coplot(Weight ~ Age | Sex)
coplot(Weight ~ Genotype | Sex )
interaction.plot(Genotype, Sex, Weight)

gain_mod2 <- lm(Weight ~ Sex * Age + Genotype)
summary(gain_mod2)

gain_mod3 <- lm(Weight ~ Age * Genotype * Sex)
summary(gain_mod3)

anova(gain_mod1, gain_mod2, gain_mod3)
detach(gain)

#ANOVA: Same model, different Output
summary(ozone_mod1)
anova(ozone_mod1)

attach(ozone_pollution)
ozone_mod1a <- lm(ozone ~ wind + rad + temp)
summary(ozone_mod1a)
anova(ozone_mod1a)
detach(ozone_pollution)

#Extracting model Information
names(ozone_mod1)

ozone_mod1$coefficients
ozone_mod1[1]
ozone_mod1$coefficients[2]

coef_ozone_mod1 <- as.vector(ozone_mod1$coefficients)
coef_ozone_mod1

names(summary(ozone_mod1))
summary(ozone_mod1)$sigma
resid_ozone_mod1 <- summary(ozone_mod1)$residuals
head(resid_ozone_mod1)

coef(ozone_mod1)
sigma(ozone_mod1)
residuals(ozone_mod1)
fitted(ozone_mod1)

#Fitting models
attach(ozone_pollution)
lm(ozone ~ rad + temp + wind)
lm(ozone ~ rad + wind)
detach(ozone_pollution)

#Comparing nested models
attach(ozone_pollution)
ozone_mod2 <- lm(ozone ~ wind)
anova(ozone_mod2, ozone_mod1)

ozone_mod3 <- lm(ozone ~ 1)
anova(ozone_mod3, ozone_mod1)

ozone_mod4 <- lm(ozone ~ wind + temp)
summary(ozone_mod4)
anova(ozone_mod4, ozone_mod1)

#Comparing non-nested models
ozone_mod5 <- lm(ozone ~ temp + wind)
ozone_mod6 <- lm(ozone ~ rad + wind)
ozone_mod7 <- lm(ozone ~ rad + temp)
AIC(ozone_mod5,ozone_mod6,ozone_mod7)

#Dealing with large numbers of covariates
model_all <- lm(ozone ~., data = ozone_pollution)
model_auto <- step(model_all, direction = "both")
summary(model_auto)
detach(ozone_pollution)

#Checking model assumptions
attach(ozone_pollution)
ozone_res <- predict(ozone_mod1) - ozone
ozone_stdres <- rstandard(ozone_mod1)
head(cbind(ozone_res, ozone_stdres))

plot(ozone_mod1)

#Checking for linearity
plot(rad, ozone_stdres, pch = 20, col = "red",
     ylab = "Standardized Residuals", xlab = "Solar radiation",
     ylim = c(-2, 4.5))
abline(a=0, b=0, lty=3)
plot(temp, ozone_stdres, pch = 20, col = "red",
     ylab = "Standardized Residuals", xlab = "Air temperature",
     ylim = c(-2, 4.5))
abline(a=0, b=0, lty = 3)
plot(wind, ozone_stdres, pch = 20, col = "red",
     ylab = "Standardized Residuals", xlab = "Wind speed",
     ylim = c(-2, 4.5))
abline(a = 0, b = 0, lty = 3)

#Checking for normality and homoscedasticity of errors
plot(rad, ozone_stdres, pch = 20, col = "red",
     ylab = "Standardized Residuals", xlab = "Solar radiation",
     ylim = c(-2, 4.5))
abline(a=0, b=0, lty=3)
plot(temp, ozone_stdres, pch = 20, col = "red",
     ylab = "Standardized Residuals", xlab = "Air temperature",
     ylim = c(-2, 4.5))
abline(a=0, b=0, lty = 3)
plot(wind, ozone_stdres, pch = 20, col = "red",
     ylab = "Standardized Residuals", xlab = "Wind speed",
     ylim = c(-2, 4.5))
abline(a = 0, b = 0, lty = 3)

qqnorm(ozone_stdres, col = "red", main = "Normal QQ plot",
       ylab = "Standardized residuals", xlab = "Quantiles of N(0,1)")
qqline(ozone_stdres)
detach(ozone_pollution)

#Checking for independence of errors
library(car)
cost_profit<-read.table("datasets/cost_profit.txt",header=T)
attach(cost_profit)
model_cost1<-lm(profit~cost)
durbinWatsonTest(model_cost1)
detach(cost_profit)

#Checking for influential observations
attach(cost_profit)
reg<-lm(profit~cost)
influence.measures(reg)
influence.measures(reg)$is.inf
detach(cost_profit)

#Checking for collinearity
attach(ozone_pollution)
library(car)
summary(ozone_mod1)
vif(ozone_mod1)
detach(ozone_pollution)

#Transforming Covariates
attach(ozone_pollution)
sq_temp<-temp^2
ozone_mod8<-lm(ozone~wind+rad+sq_temp)
summary(ozone_mod8)
ozone8_stdres<-rstandard(ozone_mod8)
plot(sq_temp,ozone8_stdres,col="red")
abline(0,0, lty=3)
plot(temp,ozone8_stdres, col="red")
abline(0,0, lty=3)

library(MASS)
boxcox(ozone~rad+temp+wind)

ozone_mod9<-lm(log(ozone) ~wind+rad+temp)
summary(ozone_mod9)
ozone9_stdres<-rstandard(ozone_mod9)

plot(rad, ozone9_stdres, pch=20, col="blue",
     ylab="Standardized Residuals", xlab="Solar radiation",
     ylim=c(-2,4.5))
abline(0,0, lty=3)
plot(temp, ozone9_stdres, pch=20, col="blue",
     ylab="Standardized Residuals", xlab="Air temperature",
     ylim=c(-2,4.5))
abline(0,0, lty=3)
plot(wind, ozone9_stdres, pch=20, col="blue",
     ylab="Standardized Residuals", xlab="Wind speed",
     ylim=c(-2,4.5))
abline(0,0, lty=3)

detach(ozone_pollution)

#Weighted Least Squares
ipomopsis<-read.table("datasets/ipomopsis.txt",header=T)
names(ipomopsis)
attach(ipomopsis)

ip_mod1<-lm(Fruit~Grazing+Root)
summary(ip_mod1)

ip_mod2<-lm(Fruit~Grazing, weights=Root)
summary(ip_mod2)

plot(Fruit, Root, col=as.factor(Grazing), pch=20,
     xlab = "Root diameter", ylab = "Seed production")
legend("topleft", legend=c("Grazed","Ungrazed"),
       col = c("black","red"), pch=20)

#Interpretation of model
summary(caterpillar_model)

summary(gain_mod1)

summary(gain_mod2)

#Making predictions
attach(ozone_pollution)
predict(ozone_mod1, data.frame(rad=c(110,110),temp=c(60,80),wind=c(15.3,9.5)))
predict(ozone_mod1, data.frame(rad=110,temp=80,wind=9.5),interval="confidence", level=0.95)
predict(ozone_mod1, data.frame(rad=110,temp=80,wind=9.5),interval="prediction", level=0.95)

X<-data.frame(rad=c(0:400),temp=c(seq(50,100,(100-50)/400)),wind=c(seq(0,25,25/400)))

ozone_conf<-data.frame(predict(ozone_mod1,X,
                    interval = "confidence", level=0.95))
ozone_pred<-data.frame(predict(ozone_mod1,X,
                    interval = "prediction", level=0.95))

#Plotting final model
plot(rad,ozone, col="red", pch=20)
lines(X$rad,ozone_conf$fit)
lines(X$rad,ozone_conf$lwr,col="blue", lty=2)
lines(X$rad,ozone_conf$upr,col="blue",lty=2)
lines(X$rad,ozone_pred$lwr,col="skyblue",lty=1)
lines(X$rad,ozone_pred$upr,col="skyblue",lty=1)
abline(a=ozone_mod1$coefficients[1],b=ozone_mod1$coefficients[2], col="green")

plot(temp,ozone,col="red",pch=20)
lines(X$temp,ozone_conf$fit)
lines(X$temp,ozone_conf$lwr,col="blue", lty=2)
lines(X$temp,ozone_conf$upr,col="blue",lty=2)
lines(X$temp,ozone_pred$lwr,col="skyblue",lty=1)
lines(X$temp,ozone_pred$upr,col="skyblue",lty=1)
abline(a=ozone_mod1$coefficients[1],b=ozone_mod1$coefficients[3], col="green")


plot(wind,ozone,col="red",pch=20)
lines(X$wind,ozone_conf$fit)
lines(X$wind,ozone_conf$lwr,col="blue", lty=2)
lines(X$wind,ozone_conf$upr,col="blue",lty=2)
lines(X$wind,ozone_pred$lwr,col="skyblue",lty=1)
lines(X$wind,ozone_pred$upr,col="skyblue",lty=1)
abline(a=ozone_mod1$coefficients[1],b=ozone_mod1$coefficients[4], col="green")

summary(ozone_mod1)
