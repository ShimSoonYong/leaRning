setwd("C:/Users/user/OneDrive/바탕 화면/leaRning")
library(scales)

# Model checking 
ozonepollution <- read.table("Datasets/ozone_pollution.txt", header=T)

lm_ozone <- glm(ozone ~ rad + temp + wind, family = gaussian(),
                data = ozonepollution)
summary(lm_ozone)

stdres_lm <- rstandard(lm_ozone)
plot(lm_ozone$fitted.values, stdres_lm, col=hue_pal()(2)[1],
     pch=20, ylab="standardised residuals",
     xlab="fitted values")
abline(lm(stdres_lm ~ lm_ozone$fitted.values), col = "blue")

# Choose the error distribution
glm_ozone_g <- glm(ozone ~ rad + temp + wind, family = Gamma(),
                   data = ozonepollution)
glm_ozone_gl <- glm(ozone ~ rad + temp + wind, 
                    family = Gamma(link = "log"),
                    data=ozonepollution)
summary(glm_ozone_g)
summary(glm_ozone_gl)# Smallest AIC

stdres_glm <- rstandard(glm_ozone_gl)
plot(glm_ozone_gl$fitted.values, stdres_glm, col = hue_pal()(2)[2],
     pch=20, ylab="standardised residuals", xlab = "fitted values")
abline(lm(stdres_glm ~ glm_ozone_gl$fitted.values), col = hue_pal()(2)[1])

# Tuning the link function
glm_ozone_p <- glm(ozone ~ rad + temp + wind,
                   family = Gamma(link = power(0.25)),
                   data = ozonepollution)
summary(glm_ozone_p)

stdres_glm <- rstandard(glm_ozone_p)
plot(glm_ozone_p$fitted.values, stdres_glm, col = hue_pal()(2)[2],
     pch=20, ylab="standardised residuals", xlab = "fitted values")
abline(lm(stdres_glm ~ glm_ozone_p$fitted.values), col = hue_pal()(2)[1])
plot(glm_ozone_p)
plot(glm_ozone_gl)

# Interpretation
coef_temp <- glm_ozone_gl$coefficients[3]
exp(2 * coef_temp)

# Prediction
covs <- data.frame(rad = c(250, 200),
                   temp = c(64, 62),
                   wind = c(10.3, 15.7))
z <- qnorm(0.995)

preds<-predict(glm_ozone_gl, newdata=covs, type = "response",
               se.fit = T)

preds$se.fit# Standard error of continuous variable
point_est<-preds$fit
ci_lower<-preds$fit - z * preds$se.fit
ci_upper<-preds$fit + z * preds$se.fit
data.frame(ci_lower, point_est, ci_upper)

# Count data and GLM
species <- read.table("Datasets/species.txt", header = T,
                      colClasses = list(pH = "factor"))
head(species)
cols = data.frame(pH = levels(species$pH), col = hue_pal()(3))
plot(species[,2:3], col = cols[match(species$pH, cols$pH), 2],
     pch = 19)
legend("topright", legend = c("high", "mid", "low"),
       pch = rep(19, 3), col = hue_pal()(3), title = "pH level")

species.mod1 <- glm(Species ~ Biomass + pH, poisson, data = species)
summary(species.mod1)

species.mod2 <- glm(Species ~ Biomass * pH, poisson, data = species)
summary(speices.mod2)
anova(species.mod1, species.mod2, test = "Chi")

# Plotting by group factors
plot(species[,2:3], col=cols[match(species$pH, cols$pH),2], pch=19)
legend("topright", legend=c("high", "mid", "low"), pch=rep(19,3),
       col=hue_pal()(3), title="pH level")
x<-seq(0,10,0.1)
for (levs in levels(species$pH)){
  lines(x, exp(predict(species.mod2,
                       list(Biomass=x, pH=rep(levs, length(x))))),
        col=cols[match(levs, cols$pH),2]
        )
}

# Dispersion problem at GLM
cellcounts<-read.table("Datasets/cells.txt", header = T,
                       colClasses=c("numeric", rep("factor", 4)))
names(cellcounts)
head(cellcounts)

barplot(table(cellcounts$cells), col="blue", xlab="# of dmged cells",
        ylab="Each sample size", main="Damaged cells")
barplot(tapply(cellcounts$cells, cellcounts$smoker, mean),
        col=c("blue","red"), main="Mean dmged cells by smoking")
barplot(tapply(cellcounts$cells, cellcounts$weight, mean),
        col = c("green", "red", "orange"), 
        main = "Mean dmged cells\nby weight levels")
barplot(tapply(cellcounts$cells, cellcounts$sex, mean),
        col=c("red","blue"), main="Mean dmged cells\nby gender")
barplot(tapply(cellcounts$cells, cellcounts$age, mean),
        col=hue_pal()(3), main="Mean dmged cells\nby ages")

# Modelling
cells_mod1<-glm(cells~smoker+sex+age+weight, family=poisson,
                data=cellcounts)
summary(cells_mod1)
cells_mod1$deviance/cells_mod1$df.residual

cells_mod2<-glm(cells~smoker+sex+age+weight, family = quasipoisson,
                data=cellcounts)
summary(cells_mod2)
cells_mod2$deviance/cells_mod2$df.residual

cells_mod3<-glm(cells~smoker*sex*age*weight,
                quasipoisson, data=cellcounts)
anova(cells_mod3, test="F")

cells_mod4<-glm(cells~smoker*weight, quasipoisson, data=cellcounts)
summary(cells_mod4)

tapply(cellcounts$cells, list(cellcounts$smoker, cellcounts$weight),
       mean)->cells_by_weight
barplot(cells_by_weight, col=hue_pal()(2), beside=T,
        ylab="damaged cells", xlab="body mass")
legend(c(1.2,4),c(3.4,2.5), c("non-smoker", "smoker"),
       fill=hue_pal()(2))

# An alternative to Poisson counts
library(MASS)
data("quine")
head(quine)
ftable(quine$Eth, quine$Sex, quine$Age, quine$Lrn)

# minimal model
quine_mod1<-glm(Days~Eth+Age+Sex+Lrn, data=quine,
                family = poisson);summary(quine_mod1)
quine_mod1$deviance/quine_mod1$df.residual # Under fitted

quine_mod2<-glm(Days~Eth+Age+Sex+Lrn, data=quine,
                family=quasipoisson());summary(quine_mod2)

quine_mod3<-glm(Days~Eth*Age+Sex*Age+Eth*Sex*Lrn,
                data=quine, family=quasipoisson);summary(quine_mod3)

quine_mod4<-glm.nb(Days~Eth+Sex+Age+Lrn, 
                   data=quine);summary(quine_mod4)

quine_mod5<-glm.nb(Days~Eth*Age+Sex*Age+Eth*Sex*Lrn,
                   data=quine);summary(quine_mod5)

par(mfrow=c(3,2))
plot(quine_mod1, which=1, col=hue_pal()(5)[1],
     main = "Model 1")
plot(quine_mod2, which=1, col=hue_pal()(5)[2],
     main = "Model 2")
plot(quine_mod3, which=1, col=hue_pal()(5)[3],
     main = "Model 3")
plot(quine_mod4, which=1, col=hue_pal()(5)[4],
     main = "Model 4")
plot(quine_mod5, which=1, col=hue_pal()(5)[5],
     main = "Model5")
par(mfrow=c(1,1))

## Count table data and GLM

# Log-linear model
# Plotting residuals for log-linear models with categorical covariates
mosaicplot(HairEyeColor, shade = T, type = "deviance")

# Modelling
induced<-read.table("Datasets/induced.txt", header = T,
                    colClasses = c(rep("factor", 3), "numeric"))
induced

# Begin by fitting a saturated model
induced_mod1<-glm(Count~Tree*Aphid*Caterpillar, family=poisson,
                  data=induced);summary(induced_mod1)

# Updating a saturated model!
induced_mod2<-update(induced_mod1, ~. -Tree:Aphid:Caterpillar)
anova(induced_mod1, induced_mod2, test="Chi")

induced_mod3<-update(induced_mod2, ~. -Aphid:Caterpillar)
anova(induced_mod2, induced_mod3, test="Chi")

# Modelling without thinking
induced_mod1a<-glm(Count~Aphid*Caterpillar, family=poisson,
                   data=induced)
induced_mod2a<-update(induced_mod1a, ~. -Aphid:Caterpillar)
anova(induced_mod1a, induced_mod2a, test='Chi')

# Searching for important covariates
as.table(by(induced$Count, 
            INDICES = list(induced$Tree, induced$Caterpillar),
         FUN=sum))
### The moral: ALWAYS examine data before start modelling ###

lizards<-read.table("Datasets/lizards.txt", header = T,
                    colClasses = c("numeric", rep("factor", 5)))
head(lizards)
attach(lizards)

ftable(tapply(n, list(species, sun, height, perch, time), sum))

lizards_mod1<-glm(n~sun*height*perch*time*species,poisson)
lizards_mod1$aic

lizards_mod2<-glm(n~sun+height+perch+time+species,poisson)
lizards_mod2$aic

lizards_mod3<-glm(n~(sun+height+perch+time)*species, poisson)
lizards_mod3$aic

anova(lizards_mod3, lizards_mod1, test="Chi")

# Dropping interactions that are hard to explain
lizards_mod4<-update(lizards_mod1, ~. 
                     -sun:height:perch:time:species
                     -height:perch:time:species
                     -sun:perch:time:species
                     -sun:height:time:species
                     -sun:height:perch:species
                     -sun:height:perch:time)
anova(lizards_mod4, lizards_mod1, test="Chi")
lizards_mod4$aic
lizards_mod1$aic

lizards_mod5<-glm(n~(sun*height+perch*time+sun*perch
                     +sun*time+height*perch+height*time)*species,
                  poisson)
anova(lizards_mod5, lizards_mod4, test="Chi")
anova(lizards_mod3, lizards_mod5, test="Chi")
matrix(c("Model 5 AIC: ",lizards_mod5$aic, 
      "Model 4 AIC: ",lizards_mod4$aic,
      "Model 3 AIC: ",lizards_mod3$aic), nrow=3, byrow = T)

lizards_mod6<-step(lizards_mod5, lower=lizards_mod3,
                   upper=lizards_mod5, trace=0)
anova(lizards_mod6, lizards_mod5, test="Chi")
anova(lizards_mod3, lizards_mod6, test="Chi")

summary(lizards_mod6)

par(mfrow=c(2, 2))
plot(lizards_mod6)

# Combining insignificant factor and base factor
levels(time)
levels(time)[c(1,3)] <- "Not.mid.day"
lizards_mod5a<-glm(n~(sun*height+perch*time+sun*perch+sun*time+
                        height*perch+height*time)*species,
                   poisson)
lizards_mod6a<-step(lizards_mod5, lower=lizards_mod3, 
                    upper=lizards_mod5, trace=0)
summary(lizards_mod6a)


plot(lizards_mod6a)
par(mfrow=c(1,1))

detach(lizards)
# Spine plot = Mosaic plot
spino<-read.table("Datasets/spino.txt", header = T,
                  colClasses = rep("factor", 2))
head(spino)
attach(spino)
condition<-factor(condition, c("much.worse", "worse",
                               "no.change", "better",
                               "much.better"))
treatment<-factor(treatment, c("placebo", "drug.A", "drug.B"))

spineplot(condition~treatment, col=hue_pal()(5))
table(condition, treatment)

# Making contingency table
spino_df<-as.data.frame.table(table(condition, treatment))
colnames(spino_df)<-c("condition","treatment", "count")
spino_df
detach(spino)

spino_agg<-aggregate(spino, spino, length)
nrow(spino_agg)
spino_agg # aggregate() function deletes zero cases

attach(spino_df)

spino_mod1<-glm(count~condition*treatment, poisson)
spino_mod2<-glm(count~condition+treatment, poisson)
anova(spino_mod1, spino_mod2, test="Chi") # Not enough data
detach(spino_df)

## Proportion data and GLM (555 in pdf)

# Theoretical background
# Traditional transformations of proportion data
plot(seq(-1,1,0.01), asin(seq(-1,1,0.01)), main="acrsine function")
abline(h=0)
abline(v=0.5)
plot(seq(0,1,0.01), qnorm(seq(0,1,0.01)), main="probit function")
abline(h=0)
abline(v=0.5)

# Mean vs. variance for a Binomial distribution
variance<-function(x){
  return(x*(1-x))
}
curve(variance(Mean), xname="Mean", ylab="Variance", col="red")

# Logistic function
# It makes probability p to the same scale of linear predictor
x<-seq(-60,60,0.1)
a<-1
b<-0.1
p<-exp(a+b*x)/(1+exp(a+b*x))
plot(x,p,xlab="x",ylab="p",type="l",col=hue_pal()(2)[1],
     main="Linear")
plot(x,log(p/(1-p)),xlab="x",ylab="logit=log(p/q)",
     type="l",col=hue_pal()(2)[2], main="Logistic")


## Logistic regression with binomial errors
## It is a modelling of log-odds!
sexratio<-read.table("Datasets/sexratio.txt",header=T)
head(sexratio)
attach(sexratio)

p<-males/(males+females)
plot(density, p, ylab="proportion male",xlab='density',
     col=hue_pal()(2)[1])
plot(log(density), p, ylab="proportion male",xlab="log(density)",
     col=hue_pal()(2)[2])

# Modelling
y<-cbind(males,females)
sex_mod1<-glm(y~density,binomial)
summary(sex_mod1) # Under fitted as residual deviance >>> df

# log transformation to improve fitting
sex_mod2<-glm(y~log(density),binomial);summary(sex_mod2)
par(mfrow=c(2,2))
plot(sex_mod2)
par(mfrow = c(1, 1))

# Plotting the final model
range(log(density)) # the range is 0~6
xv<-seq(0,6,0.01)
ev<-data.frame(density=exp(xv))
yv<-predict(sex_mod2,newdata=ev,type="response")

plot(log(density), p, ylab="Proportion of male",
     xlab="log(density)",col=hue_pal()(2)[1],
     main="log scaled")
lines(xv,yv,col=hue_pal()(2)[2])

plot(density,p,ylab="Proportion of male",
     xlab="density",col=hue_pal()(2)[1],
     main="Not scaled")
lines(exp(xv),yv,col=hue_pal()(2)[2])
detach(sexratio)

# Predicting x from y
bioassay<-read.table("Datasets/bioassay.txt",header=T)
head(bioassay)
attach(bioassay)

y<-cbind(dead,batch-dead)
bioassay_mod<-glm(y~log(dose),binomial)

library(MASS)
dose.p(bioassay_mod,p=c(0.5,0.9,0.95)) # This is that
# log(dose)=2.306981
exp(2.306981) # Target value!
detach(bioassay)


## Proportion data with categorical explanatory variables
germination<-read.table("Datasets/germination.txt",header=T,
                        colClasses=c(rep("numeric",2),
                                     rep("factor",2)))
head(germination)
attach(germination)

y<-cbind(count,sample-count)

levels(Orobanche) # a73, a75
levels(extract) # bean, cucumber

germ_mod1<-glm(y~Orobanche*extract,binomial);summary(germ_mod1)

# Checking overdispersion
pchisq(germ_mod1$deviance, germ_mod1$df.residual, lower.tail=F)
# It is significantly under fitted at the a=0.05

germ_mod2<-glm(y~Orobanche*extract,quasibinomial)
germ_mod3<-update(germ_mod2,~. -Orobanche:extract)
anova(germ_mod3,germ_mod2,test="F") # No significant difference

# Watch whether further model simplification is possible
anova(germ_mod3, test="F") # Orobanche genotype can be deleted

germ_mod4<-update(germ_mod3, ~. -Orobanche)
anova(germ_mod4, germ_mod3,test="F")
# There is no reason to retain Orobanche in the model.
coef(germ_mod4)

# Coefficients at real scale
1/(1+exp(-coef(germ_mod4)[1])) # mean germination of bean was 37%.
1/(1+exp(-(coef(germ_mod4)[1]+coef(germ_mod4)[2])))
# The mean germination rate was 63% with cucumber.

tapply(predict(germ_mod4,type="response"),extract,mean)
# tapply(value column, category columns, applying function)

# Comparing predictions with the sample means.
p<-count/sample
tapply(p,extract,mean) # Not a correct approach

# Summation by factors first, proportion next (by factors)
germ<-tapply(count,extract,sum)
size<-tapply(sample,extract,sum)
as.vector(germ)/as.vector(size) # Same with predictions!
detach(germination)

## Binomial GLM with ordered categorical covariates
head(esoph)
help(esoph)
attach(esoph) 

esoph_mod1<-glm(cbind(ncases,ncontrols)~agegp+alcgp*tobgp,binomial)
summary(esoph_mod1)
pchisq(esoph_mod1$deviance,esoph_mod1$df.residual,lower.tail = F)
# .L means linear relationship (a=b)
# .Q means quadratic relationship (a^2=b)
# .C means cubic(3 degree) relationship (a^3=b)
# numbers like .4, .5, etc. test for higher-order polynomial effects

esoph_mod2<-glm(cbind(ncases,ncontrols)~agegp+alcgp+tobgp,binomial)
anova(esoph_mod1,esoph_mod2, test="Chisq")
summary(esoph_mod2)
pchisq(esoph_mod2$deviance,esoph_mod2$df.residual,lower.tail = F)

p<-ncases/(ncases+ncontrols)
par(mfrow=c(1,3))
plot(p~alcgp,col=hue_pal()(3)[1], xlab="Alcohol",
     cex.alb=1.5, cex.axis=1.5)
plot(p~tobgp,col=hue_pal()(3)[2],xlab="Tobacco",
     cex.lab=1.5, cex.axis=1.5)
plot(p~agegp,col=hue_pal()(3)[3],xlab="Age",
     cex.lab=1.5,cex.axis=1.5)
par(mfrow=c(1,1))

# Reducing each covariate to just three groups
alcgp2<-alcgp
levels(alcgp2)[2:3]<-"40-119"
levels(alcgp2)

tobgp2<-tobgp
levels(tobgp2)[2:3]<-"10-30"
levels(tobgp2)

agegp2<-agegp
levels(agegp2)[4:6]<-"55+"
levels(agegp2)[1:2]<-"under45"
levels(agegp2)

esoph_mod3<-glm(cbind(ncases,ncontrols)~agegp2+alcgp2+tobgp2,binomial)
summary(esoph_mod3)
pchisq(esoph_mod3$deviance,esoph_mod3$df.residual)

alcgp3<-factor(alcgp,ordered=F)
agegp3<-factor(agegp2,ordered=F)
tobgp3<-factor(tobgp2,ordered=F)

esoph_mod4<-glm(cbind(ncases,ncontrols)~agegp3+alcgp3+tobgp3,binomial)
summary(esoph_mod4)
pchisq(esoph_mod4$deviance,esoph_mod4$df.residual)
detach(esoph)


## Binomial GLM with categorical and continuous covariates
flowering<-read.table("Datasets/flowering.txt",header = T,
                      colClasses=list(variety="factor"))
head(flowering)
tail(flowering)
attach(flowering)
y<-cbind(flowered,number-flowered)
pf<-flowered/number
plot(dose,jitter(pf),xlab="dose",ylab="proportion flowered",
     col=hue_pal()(5)[as.vector(as.numeric(factor(variety)))],
     pch=19)
legend("topleft", legend=levels(variety),
       pch=rep(19,5),title="variety",col=hue_pal()(5))

flow_mod1<-glm(y~dose*variety,binomial)
summary(flow_mod1) # Overdispersion observed

# Watch the dispersion by plotting
plot(dose,jitter(pf),xlab="dose",ylab="proportion flowered",
     col=hue_pal()(5)[as.vector(as.numeric(factor(variety)))],
     pch=19)
legend("topleft", legend=levels(variety),
       pch=rep(19,5),title="variety",col=hue_pal()(5))
xv<-seq(0,35,0.1)
for (i in 1:5){
  vn<-rep(levels(variety)[i], length(xv))
  yv<-predict(flow_mod1,list(variety=factor(vn),dose=xv),
              type = "response")
  lines(xv,yv,col=hue_pal()(5)[i])
} # The model is reasonable for A, E genotypes
# moderate for C genotype, but very poor for B and D.

tapply(pf, list(dose,variety),mean)
detach(flowering)

# Revisiting lizards
lizards<-read.table("Datasets/lizards.txt", header = T,
                    colClasses = c("numeric", rep("factor", 5)))
levels(lizards$time)[c(1,3)] <- "Not.mid.day"

lizards_sort<-lizards[order(lizards$species,
                            lizards$sun,
                            lizards$height,
                            lizards$perch,
                            lizards$time),]
lizards_sort_top<-lizards_sort[1:24,]
names(lizards_sort_top)[1]<-"Ag"
lizards_sort_top<-lizards_sort_top[,-6]
head(lizards_sort_top)

lizards_new<-data.frame(lizards_sort$n[25:48],lizards_sort_top)
names(lizards_new)[1]<-"Ao"
head(lizards_new)
attach(lizards_new)

lizards_model<-glm(cbind(Ag,Ao)~. , binomial, data=lizards_new)
summary(lizards_model)
lizards_model2<-glm(cbind(Ag,Ao)~ sun*height*perch*time,binomial)
summary(lizards_model2)
anova(lizards_model,lizards_model2, test="Chi")

lizards_model3<-update(lizards_model, ~. -time)
anova(lizards_model3, lizards_model, test="Chi")
summary(lizards_model3) # Best fit is initial model

plot(sun,Ag/Ao, xlab="sunlight", ylab="proportion")
plot(height,Ag/Ao,xlab="height", ylab="proportion")
plot(perch,Ag/Ao,xlab="perch", ylab="proportion")
plot(time,Ag/Ao,xlab="time", ylab="proportion")
lizards_model$coefficients
exp(lizards_model$coefficients)
detach(lizards_new)


## Binary response variables and GLM
isolation<-read.table("Datasets/isolation.txt",header=T)
head(isolation)
names(isolation)[3]<-"iso"
attach(isolation)

iso_mod1<-glm(incidence~area*iso,binomial)
iso_mod2<-glm(incidence~area+iso,binomial)
anova(iso_mod2,iso_mod1,test="Chi")
summary(iso_mod2)

library(ggplot2)
ggplot(data=isolation)+
  geom_point(aes(area,incidence))+
  geom_smooth(method="glm",aes(area,incidence),
              method.args=list(family="binomial"))+
  theme(legend.position = "none")+
  ggtitle("Incidence by Area")
  
ggplot(data=isolation)+
  geom_point(aes(iso,incidence))+
  geom_smooth(method="glm",aes(iso,incidence),
              method.args=list(family="binomial"))+
  theme(legend.position = "none")+
  ggtitle("Incidence by Isolation")
detach(isolation)

## Graphical tests of the fit of the logistic curve to data
occupation<-read.table("Datasets/occupation.txt",header=T)
head(occupation)
attach(occupation)

# A rug plot with a fitted logistic curve!
ggplot()+
  geom_rug(aes(resources[occupied==0], occupied[which(occupied==0)]),
           sides="b")+
  geom_rug(aes(resources[occupied==1],occupied[which(occupied==1)]),
           sides="t")+
  geom_smooth(aes(resources,occupied),method="glm",
              method.args=list(family="binomial"))

# Evalution of probabilty for rugged values
occ_cut<-cut(resources,5)
tapply(occupied,occ_cut,sum) # Values containing 1
# curved bracket: from, but not including
# square bracket: to and including
# option: right=TRUE, which means right sides are included.

table(occ_cut) # Number of data in each bin.
occ_probs<-tapply(occupied,occ_cut,sum)/table(occ_cut)
occ_probs

# Plotting residuals
occ_probs<-as.vector(occ_probs)
resmeans<-as.vector(tapply(resources,occ_cut,mean))
se<-as.vector(sqrt(occ_probs*(1-occ_probs)/table(occ_cut)))
up<-occ_probs+se
down<-occ_probs-se
ggplot()+
  geom_rug(aes(resources[occupied==0], occupied[which(occupied==0)]),
           sides="b")+
  geom_rug(aes(resources[occupied==1],occupied[which(occupied==1)]),
           sides="t")+
  geom_smooth(aes(resources,occupied),method="glm",
              method.args=list(family="binomial"))+
  geom_point(aes(resmeans,occ_probs), col="red", size=5)+
  geom_errorbar(aes(resmeans,ymax=up,ymin=down, col="red"))+
  theme(legend.position = "none")+
  ylab("Occupied or not")+
  xlab("Resources")
detach(isolation)

## Mixed covariate types with a binary response
infection<-read.table("Datasets/infection.txt",header=T,
                      colClasses=c("factor",rep("numeric",2),"factor"))
head(infection)
attach(infection)

par(mfrow=c(1,2))
boxplot(weight~infected,col="red")  
boxplot(age~infected,col="blue")  
par(mfrow=c(1,1))

library(gridExtra)
plot1 <- ggplot()+
  geom_boxplot(aes(infected,weight),fill="red")+
  ggtitle("Weight")
plot2 <- ggplot()+
  geom_boxplot(aes(infected,age),fill="blue")+
  ggtitle("Age")
grid.arrange(plot1, plot2, ncol = 2)
table(sex,infected)

val<-as.data.frame(table(sex,infected))
ggplot()+
  geom_tile(aes(val$sex,val$infected,fill=val$Freq))+
  scale_fill_gradient(low = "black", high = "white")+
  labs(x="Sex",y="Infection",fill = "Frequency",
       title="Infection by sex")

# Fit a maximal model
inf_mod1<-glm(infected~age*weight*sex,family=binomial)
summary(inf_mod1)

# Examine the model which just has main effects
inf_mod2<-glm(infected~age+weight+sex,binomial)
summary(inf_mod2)

# Fitting quadratic terms
inf_mod3<-glm(infected~age+weight+sex+I(weight^2)+I(age^2),
              family=binomial)
summary(inf_mod3)


## Spine plot and logistic regression
wasps<-read.table("Datasets/wasps.txt",header=T,
                  colClasses=list(fate="factor"))
head(wasps)
attach(wasps)

table(density,fate)
spineplot(fate~density,col=hue_pal()(2))

# Conditional density plot
cdplot(fate~density, col=hue_pal()(2))
cdplot(fate~log(density),col=hue_pal()(2))

# Quantifying the trend by logistic regression
wasps_mod1<-glm(fate~density,binomial);summary(wasps_mod1)
wasps_mod2<-glm(fate~log(density),binomial);summary(wasps_mod2)

# Plotting the model
plot(jitter(log(density)), as.numeric(fate)-1,
     col=hue_pal()(3)[1],xlim=c(0,5),xlab="jittered log(density)",
     ylab="proportion parasitised", pch=19) # Data points

xv<-seq(0,5,0.01) # Calculate Predicted line
yv<-1/(1+1/exp(coef(wasps_mod2)[1]+coef(wasps_mod2)[2]*xv))

lines(xv,yv,col=hue_pal()(3)[2]) # Prediction line


tapply(as.numeric(fate)-1,density,sum) # Calculating the residuals
den<-c(3.75,16,32,64) 
pd<-c(3/15,5/16,20/32,52/64)
eb<-sqrt(pd*(1-pd)/den)

# Plotting the residuals
points(log(den),pd,cex=2,col=hue_pal()(4)[3], pch=19)
for (i in 1:4){
  lines(rep(log(den[i]),2),c(pd[i]-eb[i],pd[i]+eb[i]),
        col=hue_pal()(4)[4])
}

library(ggplot2)
ggplot()+
  geom_point(aes(log(density),as.numeric(fate)-1),color=hue_pal()(4)[1],
             size=3)+
  xlim(0,5)+ylim(-0.1,1)+
  geom_smooth(method="glm",method.args=list(family="binomial"),
              aes(log(density),as.numeric(fate)-1),
              color=hue_pal()(4)[2])+
  geom_point(aes(log(den),pd),size=5,color=hue_pal()(4)[3])+
  geom_errorbar(aes(log(den),ymin=pd-eb,ymax=pd+eb), 
                color=hue_pal()(4)[4])
detach(wasps)  

## Bootstrapping a GLM
timber<-read.table("Datasets/timber.txt",header=T)
head(timber)
attach(timber)

timber_model<-glm(log(volume)~log(girth)+log(height))
summary(timber_model)

# install.packages("boot")
library(boot)
model.boot1<-function(data,indices){
  sub.data<-data[indices,]
  model<-glm(log(volume)~log(girth)+log(height))
  coef(model)
}
timber.boot<-boot(timber,model.boot1,R=2000)
timber.boot

yhat<-fitted(timber_model)
resids<-resid(timber_model)
res_data<-data.frame(resids,girth,height)

model.boot2<-function(res_data,i){
  y<-yhat+res_data[i,1]
  nd<-data.frame(y,girth,height)
  model<-glm(y~log(girth)+log(height))
  coef(model)
}
perms<-boot(res_data,model.boot2,R=2000,sim="permutation")
perms

boot.ci(perms,index=1,conf=0.99)$bca[c(1,4,5)]
boot.ci(perms,index=2,conf=0.99)$bca[c(1,4,5)]
boot.ci(perms,index=3,conf=0.99)$bca[c(1,4,5)]
