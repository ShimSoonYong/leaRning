setwd("C:/Users/user/OneDrive/바탕 화면/leaRning/Datasets")

#two-sided t-test
exams<-read.csv("exams.csv")
head(exams)
plot(exams,pch=20)
cor(exams)
t.test(x=exams$Exam1,mu=60,conf.level = 0.92)

#Wilcoxon Signed Rank test
light<-read.table("light.txt",header=T)
head(light)
summary(light$speed)
hist(light$speed,main="speed of light",xlab="speed sybtracted by 299,000",col="red")

wilcox.test(light$speed,mu=990)
wilcox.test(exams$Exam1,exams$Exam2,paired=T,conf.int=T)

#two-sample t-test
load("tulips.Rdata")
boxplot(list(GardenA,GardenB),
        ylab="Height (cm)",
        varwidth = T,
        names=c("GardenA","GardenB"),
        col=c("red","blue"),
        notch=T)
t.test(x=GardenA,y=GardenB,conf.level=0.9,var.equal=T)

#F-test for t-test
var.test(x=exams$Exam1,y=exams$Exam3)
if (var.test(x=exams$Exam1,y=exams$Exam3)$p.value<0.01){
  exams13_var_equal<-"T"
}else{
  exams13_var_equal<-"F"
}
t.test(x=exams$Exam1,y=exams$Exam3,conf.level = 0.9,
       paired = T,var.equal = exams13_var_equal)

#Kruskal-Wallis test
kruskal.test(exams)

#Kolmogorov-Smirnov test
A<-length(GardenA)
B<-length(GardenB)
plot(seq(0,1,length=A),cumsum(sort(GardenA)/sum(GardenA)),type="l",
     ylab="cumulative probability",xlab="percentage of samples considered",
     col=c("red","blue"))
lines(seq(0,1,length=B),cumsum(sort(GardenB)/sum(GardenB)),col="blue")
legend(x=0.7,y=0.4,legend = c("GardenA","GardenB"),col=c("red","blue"),lty=1)
boxplot(GardenA,col="red",ylim=c(0,12))
boxplot(GardenB,col="blue",ylim=c(0,12))

#Test whether A's distribution and B's distribution
ks.test(GardenA,GardenB)

#Test a data similar to a specific distribution
ks.test(GardenA,"pt",ncp=mean(GardenA),df=length(GardenA)-1)

#checking for normality
qqnorm(exams$Exam1,main="Exam1",col="red",cex.lab=1,xaxt="n",yaxt="n")
qqline(exams$Exam1)
qqnorm(exams$Exam2,main="Exam2",col="green",cex.lab=1.5,xaxt="n",yaxt="n")
qqline(exams$Exam2)
qqnorm(exams$Exam3,main="Exam3",col="blue",cex.lab=1.5,xaxt="n",yaxt="n")
qqline(exams$Exam3)

# install.packages("BSDA")
library(BSDA)
z.test(x=exams$Exam1,y=exams$Exam2,
       sigma.x = sd(exams$Exam1),
       sigma.y=sd(exams$Exam2))

## Comparing variances
load("tulips.RData")
# F-test
var.test(GardenA,GardenB,conf.level=0.99)

## Tests for multiple sample variances
# Choose between the Bartlett test and Fligner-Killeen test
refs<-read.table("refuge.txt",header=T)
names(refs)

tapply(refs$B,refs$T,var)
which(refs$T==9) # Checking the index of unique data point

bartlett.test(refs$B[-31],refs$T[-31])
fligner.test(refs$B[-31],refs$T[-31])


## Tests for discrete and categorical data

## Sign test without tie processing
binom.test(x=1,n=9,p=0.5)

## Two sample sign test without tie processing
sign.test<-function(x,y){
  if (length(x)!=length(y)) 
    stop("The two variables must be the same length")
  d<-x-y
  binom.test(sum(d>0),length(d))
}
sign.test(exams$Exam1,exams$Exam2)
round(pbinom(35,length(exams$Exam1),prob=0.5,lower.tail=T)*2,5)

## Sign tests with tie processing
library(BSDA)
SIGN.test(c(1:9),md=8.5)
# Equivalent when input tie processing command
round(SIGN.test(c(1:9),md=8.5)$p.value,4)==round(
  binom.test(x=1,n=9,p=0.5)$p.value,4)

## Two sample sign test with tie processing
SIGN.test(exams$Exam1,exams$Exam2)
# Equivalent when input tie processing command
round(SIGN.test(exams$Exam1,exams$Exam2)$p.value,4)==round(
  sign.test(exams$Exam1[-which(exams$Exam1==exams$Exam2)],
            exams$Exam2[-which(exams$Exam1==exams$Exam2)])$p.value,4)


## Test to compare proportions

## Simple test
#install.packages("EMT")
library(EMT)
multinomial.test(observed=c(4,47,12),
                 prob=c(0.111,0.712,0.177))

## Test by confidence interval
#install.packages("DescTools")
library(DescTools)
MultinomCI(x=c(4,47,12),conf.level=0.92)

## G-test for large data
GTest(x=c(4,47,12),y=c(0.111,0.712,0.177))

# Test to check dependency by Poisson distribution

# Using base R plot
banks<-read.table("Datasets/cases.txt",header=T)
names(banks)
banks_table<-table(banks$cases)

library(scales)

par(mfrow=c(1,2))
barplot(banks_table,ylab="Frequency",xlab="Cases",ylim=c(0,35),
        col=hue_pal()(2)[1],main = "Bankruptcies")
barplot(c(dpois(0:9,1.775),1-ppois(9,1.775))*80,
        names=as.character(0:10),ylab="Frequency",xlab="Cases",
        col=hue_pal()(2)[2],ylim=c(0,35),main="Poisson")
par(mfrow=c(1,1))

# Using ggplot2
library(ggplot2)

bk<-as.data.frame.table(banks_table)
bk<-data.frame(bk,name=rep("Bankruptcies",11))
poi<-data.frame(Freq=c(dpois(0:9,1.775),1-ppois(9,1.775))*80,
                Var1=0:10,name=rep("Poisson",11))
bk
bp<-rbind(bk,poi)

ggplot(data=bp,aes(Var1,Freq,fill=name))+
  facet_grid(cols=vars(name))+
  geom_bar(stat="identity",color="black")+
  theme(legend.position = "none")+
  labs(y="Frequency",x="Cases")

# Checking variance/mean ratio
var(banks$cases)/mean(banks$cases) # Almost 3 times!

## Goodness-of-fit test
GTest(x=banks_table,p=c(dpois(0:9,1.775),1-ppois(9,1.775)))

# Alternative distribution: Negative Binomial distribution
mean(banks$cases)^2/(var(banks$cases)-mean(banks$cases))

# Bases R plot
par(mfrow=c(1,2))
barplot(banks_table,ylab="Frequency",xlab="Cases",col=hue_pal()(2)[1],
        ylim=c(0,35),main="Bankruptcies")
nb<-c(dnbinom(0:9,size=0.89,mu=1.75),
  1-pnbinom(9,size=0.89,mu=1.75))*80
barplot(nb,names=as.character(0:10),ylab="Frequency",xlab="Cases",
        col=hue_pal()(2)[2],ylim=c(0,35),main="Nevgative Binomial")
par(mfrow=c(1,1))

# ggplot2
bk
nb_df<-data.frame(Var1=0:10,Freq=nb,name=rep("NBinom",11))
df<-rbind(bk,nb_df)
ggplot(data=df,aes(Var1,Freq,fill=name))+
  facet_grid(cols=vars(name))+
  geom_bar(stat="identity",color="black")+
  labs(y="Frequency",x="Cases")+
  theme(legend.position = "none",
        text = element_text(size=30),
        axis.title = element_text(size=20),
        axis.text = element_text(size=10))+lims(y=c(0,35))

# Confirmatory G-test
GTest(x=banks_table,p=nb/80) # Great fit of 97.92%!

## Testing Contingency tables
ftable(HairEyeColor)

# the Association plot!
assocplot(margin.table(HairEyeColor,c(1,2)))
# the margin.table reduces the 3D table to its first two dims.
# Black bars show more individuals were observed than expected.
# Red bars show fewer individuals were observed than expected.
# The expected values are calculated under null hypothesis

GTest(x=matrix(c(38,14,11,51),nrow=2)) # 우도비 검정
chisq.test(matrix(c(38,14,11,51),nrow=2)) # 카이제곱 검정
fisher.test(matrix(c(38,14,11,51),nrow = 2))# 피셔의 정확검정



### Bootstrapping ###
boot_means<-numeric(10000)
for (i in 1:10000){
  boot_means[i]<-mean(sample(light$speed,replace = T))
}
summary(boot_means)

hist(boot_means,main = "",xlab = "sample means",col = hue_pal()(1))

# Create 93% CI
quantile(boot_means,probs=c(0.035,0.965))

library(boot)
mymean<-function(light, i){
  mean(light$speed[i])
}
boot_means_package<-boot(light,statistic = mymean,R=10000)
boot.ci(boot_means_package,conf = 0.93)



### Multiple tests
1-(1-0.04)*(1-0.03)

p.adjust(p= c (0.04, 0.03, 0.17, 0.12, 
               0.01, 0.18, 0.02, 0.04, 0.21, 0.08),
         method = "holm")
