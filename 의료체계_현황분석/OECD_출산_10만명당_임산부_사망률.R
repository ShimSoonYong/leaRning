pdeath<-read.csv("의료체계_현황분석/출산_10만명당_임산부_사망률_OECD회원국.csv")
head(pdeath)
tail(pdeath)

library(dplyr)
library(forecast)

Kdeath<-pdeath%>%select(Year,Korea)
head(Kdeath)
tail(Kdeath)
Kdeath<-ts(Kdeath[,2],start=1960,frequency = 1)
Kmodel<-auto.arima(Kdeath)
Kpred<-forecast(Kmodel)

USAdeath<-pdeath%>%select(Year,USA)
USAdeath<-ts(USAdeath[,2],start=1960,frequency = 1)
USAmodel<-auto.arima(USAdeath)
USApred<-forecast(USAmodel)

JPdeath<-pdeath%>%select(Year,Japan)
JPdeath<-ts(JPdeath[,2],start=1960,frequency = 1)
JPmodel<-auto.arima(JPdeath)
JPpred<-forecast(JPmodel)

FRdeath<-pdeath%>%select(Year,France)
FRdeath<-ts(FRdeath[,2],start=1960,frequency = 1)
FRmodel<-auto.arima(FRdeath)
FRpred<-forecast(FRmodel)

GEdeath<-pdeath%>%select(Year,Germany)
GEdeath<-ts(GEdeath[,2],start=1960,frequency = 1)
GEmodel<-auto.arima(GEdeath)
GEpred<-forecast(GEmodel)

SPdeath<-pdeath%>%select(Year,Spain)
SPdeath<-ts(SPdeath[,2],start=1960,frequency = 1)
SPmodel<-auto.arima(SPdeath)
SPpred<-forecast(SPmodel)

plot(Kpred,col="red",
     main = "OECD pregnant women deaths by 0.1M",
     ylab="Death",
     xlab = "Year",ylim = c(0,100),xlim=c(1960,2030))
par(new=T)
plot(USApred,col="blue",main="",ylab="",xlab="",
     ylim = c(0,100),xlim=c(1960,2030))
legend(x=c(1960,1990),y=c(70,100),legend=c("Korea","USA"),
       col=c("red","blue"),lty=1,cex=2)


plot(Kpred,col="red",
     main = "OECD pregnant women deaths by 0.1M",
     ylab="Death",
     xlab = "Year",ylim = c(0,100),xlim=c(1960,2030))
par(new=T)
plot(JPpred,col="green",main="",ylab="",xlab="",
     ylim = c(0,100),xlim=c(1960,2030))
legend(x=c(1960,1990),y=c(70,100),legend=c("Korea","Japan"),
       col=c("red","green"),lty=1,cex=2)


plot(Kpred,col="red",
     main = "OECD pregnant women deaths by 0.1M",
     ylab="Death",
     xlab = "Year",ylim = c(0,100),xlim=c(1960,2030))
par(new=T)
plot(FRpred,col="skyblue",main="",ylab="",xlab="",
      ylim = c(0,100),xlim=c(1960,2030))
legend(x=c(1960,1990),y=c(70,100),legend=c("Korea","France"),
       col=c("red","skyblue"),lty=1,cex=2)


plot(Kpred,col="red",
     main = "OECD pregnant women deaths by 0.1M",
     ylab="Death",
     xlab = "Year",ylim = c(0,100),xlim=c(1960,2030))
par(new=T)
plot(SPpred,col="brown",main="",ylab="",xlab="",
     ylim = c(0,100),xlim=c(1960,2030))
legend(x=c(1960,1990),y=c(70,100),legend=c("Korea","Spain"),
       col=c("red","brown"),lty=1,cex=2)

plot(Kpred,col="red",
     main = "OECD pregnant women deaths by 0.1M",
     ylab="Death",
     xlab = "Year",ylim = c(0,100),xlim=c(1960,2030))
par(new=T)
plot(SPpred,col="yellow",main="",ylab="",xlab="",
     ylim = c(0,100),xlim=c(1960,2030))
legend(x=c(1960,1990),y=c(70,100),legend=c("Korea","Germany"),
       col=c("red","yellow"),lty=1,cex=2)

