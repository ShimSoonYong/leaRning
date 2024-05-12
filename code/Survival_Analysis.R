setwd("C:/Users/user/OneDrive/바탕 화면/leaRning")
library(survival)

cancer<-read.table("datasets/cancer.txt",header=T)
attach(cancer)
head(cancer)
Surv(time=death,event=status)

#Kaplan-Meier Survival model
cancer_surv<-survfit(Surv(death,status)~treatment,type='kaplan-meier')
plot(cancer_surv,ylab="Probability of survival",xlab = "Time", main="Kaplan-Meier Curve",
     mark.time = T,col=c("red","blue","green","brown"))
legend("topright",legend = c("Drug A", "Drug B","Drug C","Placebo"),
       fill=c("red","blue","green","brown"),bty="n")

compare_treat<-survdiff(Surv(death,status)~treatment)
compare_treat

detach(cancer)

#Proportional Hazard model, PH model
t<-0:100
sv_exp<-function(a,t){
  return(exp(-a*t))
}
sv_wei<-function(a,t,b){
  return(exp(-(a*t)**b))
}
sv_gom<-function(a,t,b){
  return(exp(-(a/b)*(exp(b*t)-1)))
}
l_log<-function(a,t,b){
  return(1/(1+a*(t**b)))
}

plot(x=0,y=1,ylim=c(0,1),
         xlim=c(0,100),
     ylab="Probability of survival, S(t)",
     xlab="Time (t)")

lines(t,sv_exp(1,t),col="red")
lines(t,sv_exp(0.1,t),col="green")
lines(t,sv_exp(0.01,t),col="blue")
legend("topright",legend=c("a=1.00","a=0.10","a=0.01"),bty="n",
       fill=c("red","green","blue"))

plot(x=0,y=1,ylim=c(0,1),
     xlim=c(0,100),
     ylab="Probability of survival, S(t)",
     xlab="Time (t)")

lines(t,sv_wei(1,t,2),col="red")
lines(t,sv_wei(0.1,t,2),col="green")
lines(t,sv_wei(0.1,t,3),col="blue")
legend("topright",legend=c("a=1.0, b=2.0","a=0.1, b=2.0","a=0.1, b=3.0"),bty="n",
       fill=c("red","green","blue"))

plot(x=0,y=1,ylim=c(0,1),
     xlim=c(0,100),
     ylab="Probability of survival, S(t)",
     xlab="Time (t)")

lines(t,sv_gom(0.01,t,0.1),col="red")
lines(t,sv_gom(0.05,t,0.1),col="green")
lines(t,sv_gom(0.005,t,0.01),col="blue")
legend("topright",legend=c("a=0.01, b=0.1","a=0.05, b=0.1","a=0.005, b=0.01"),bty="n",
       fill=c("red","green","blue"))

plot(x=0,y=1,ylim=c(0,1),
     xlim=c(0,100),
     ylab="Probability of survival, S(t)",
     xlab="Time (t)")

lines(t,l_log(1,t,2),col="red")
lines(t,l_log(0.1,t,1),col="green")
lines(t,l_log(0.5,t,0.5),col="blue")
legend("topright",legend=c("a=1, b=2","a=0.1, b=1","a=0.5, b=0.5"),bty="n",
       fill=c("red","green","blue"))

haz_exp<-function(a){
  return(a)
}
haz_wei<-function(a,t,b){
  return(a*b*(a*t)**(b-1))
}
haz_gom<-function(a,t,b){
  return(a*exp(b*t))
}
haz_llg<-function(a,t,b){
  return((a*b*t**(b-1))/(1+a*t**b))
}

plot(0, 1, main = "Exponential Hazard",
     ylab = "Hazard, h(t)", xlab = "Time (t)", type = "l",
     xlim = c(0, 100), ylim = c(0, 1.5))
abline(a = haz_exp(1), b=0, col = "red")
abline(haz_exp(0.1), 0, col = "green")
abline(haz_exp(0.01), 0, col = "blue")
legend("topright", legend = c("a=1.00","a=0.1","a=0.01"),
       fill = c("red","green","blue",
             bty = "n"))

plot(0, 0, main = "Weibull Hazard",
     ylab = "Hazard, h(t)", xlab = "Time (t)",type="l",
     xlim = c(0, 100), ylim = c(0, 200))
lines(t, haz_wei(1, t, 2), col = "red")
lines(t, haz_wei(0.1, t, 2), col = "green")
lines(t, haz_wei(0.1, t, 3), col = "blue")
legend("topleft", legend = c("a=1, b=2", "a=0.1, b=2", "a=0.1, b=3"),
       bty = "n", fill = c("red", "green", "blue"))

plot(0, 0, main = "Gompertz Hazard",
     ylab = "Hazard, h(t)", xlab = "Time (t)", type = "l",
     xlim = c(0, 100), ylim = c(0, 200))
lines(t, haz_gom(0.01, t, 0.1), col = "red")
lines(t, haz_gom(0.05, t, 0.1), col = "green")
lines(t, haz_gom(0.005, t, 0.1), col="blue")
lines(t, haz_gom(0.005, t, 0.01), col="lightblue")
legend("topleft", legend = c("a=0.01, b=0.1", "a=0.05, b=0.1", "a=0.005, b=0.1", "a=0.005, b=0.01"),
       bty = "n", fill = c("red", "green", "blue", "lightblue"))

plot(0, 0, main = "Log-Logistic Hazard",
     ylab = "Hazard, h(t)", xlab = "Time (t)", type = "l",
     xlim = c(0, 100), ylim = c(0, 1))
lines(t, haz_llg(1, t, 2), col="red")
lines(t, haz_llg(0.1, t, 1), col="green")
lines(t, haz_llg(0.5, t, 0.5), col="blue")
legend("topright", legend = c("a=1, b=2", "a=0.1, b=1", "a=0.5, b=0.5"),
       bty = "n", fill = c("red", "green", "blue"))

#Cox proportional Hazard model
roaches<-read.table("datasets/roaches.txt", header = T)
attach(roaches)

roaches$group <- as.factor(roaches$group)
summary(roaches)

roach_surv <- survfit(Surv(death, status) ~ group)
plot(roach_surv, col = c("red", "green", "blue"),
     ylab = "Probability of survival",
     xlab= "Time", main = "The three groups of insects")
legend("topright", bty = "n", 
       legend = c("A", "B", "C"),
       fill = c("red", "green", "blue"))

roach_model_ph1 <- coxph(Surv(death, status) ~ weight + group)
summary(roach_model_ph1)

roach_model_ph2 <- coxph(Surv(death, status) ~ group)
summary(roach_model_ph2)
'''Estimated Hazard is
h(t|X)=h(t)exp(0.56groupB + 1.01groupC)
'''

roach_ph <- cox.zph(roach_model_ph2)#Null hypothesis: Hazards are proportional
print(roach_ph)
plot(roach_ph)

#Accelerated Failure Time models, AFT model
roach_model_exp1 <- survreg(Surv(death, status) ~ weight + group,
                            dist = "exponential")
summary(roach_model_exp1)

roach_model_wei1 <- survreg(Surv(death, status) ~ weight + group)
summary(roach_model_wei1)

roach_model_wei2 <- survreg(Surv(death, status) ~ group)
summary(roach_model_wei2)

#Prediction of mean time-to-event, these includes the censored obs.
tapply(predict(roach_model_wei2), group, mean)

#Actual mean of time-to-event
tapply(death[status==1], group[status==1], mean)

#Mean age at death and censoring for each group
tapply(death, group, mean)

plot(roach_surv, ylab = "Probability of survival", 
     xlab = "Time", mark.time = T,
     col = c("red", "green", "blue"))
legend("topright", legend = c("A", "B", "C"),
       fill = c("red", "green", "blue"),
       bty = "n")
lines(predict(roach_model_wei2, newdata = list(group = "A"), type = "quantile", 
              p = seq(0.01, 0.99, 0.01)), seq(0.99, 0.01, -0.01),
      col = "red")
lines(predict(roach_model_wei2, newdata = list(group = "B"), type = "quantile", 
              p = seq(0.01, 0.99, 0.01)), seq(0.99, 0.01, -0.01),
      col = "green")
lines(predict(roach_model_wei2, newdata = list(group = "C"), type = "quantile", 
              p = seq(0.01, 0.99, 0.01)), seq(0.99, 0.01, -0.01),
      col = "blue")
detach(roaches)
