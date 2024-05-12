costs<-read.csv("의료체계_현황분석/보건서비스_지출비_OECD회원국.csv")

library(dplyr)
library(forecast)
Kcost<-cforecastKcost<-costs%>%
  filter(Time!="Year")%>%
  select(Time,Korea)
head(Kcost)
tail(Kcost)
Kcost<-ts(Kcost[,2],start=1970,frequency = 1)

UScost<-costs%>%
  filter(Time!="Year")%>%
  select(Time,USA)
head(UScost)
tail(UScost)
UScost<-cbind(UScost$Time,as.numeric(UScost$USA)*1200)
UScost<-ts(UScost[,2],start=1970,frequency = 1)


JPcost<-costs%>%
  filter(Time!="Year")%>%
  select(Time,Japan)
head(JPcost)
tail(JPcost)
JPcost<-cbind(JPcost$Time,as.numeric(JPcost$Japan)*10)
JPcost<-ts(JPcost[,2],start = 1970,frequency = 1)


Gcost<-costs%>%
  filter(Time!="Year")%>%
  select(Time,Germany)
head(Gcost)
tail(Gcost)
Gcost<-cbind(Gcost$Time,as.numeric(Gcost$Germany)*1400)
Gcost<-ts(Gcost[,2],start=1970,frequency = 1)

Fcost<-costs%>%
  filter(Time!="Year")%>%
  select(Time,France)
head(Fcost)
tail(Fcost)
Fcost<-cbind(Fcost$Time,as.numeric(Fcost$France)*1400)
Fcost<-ts(Fcost[,2],start = 1970,frequency = 1)

Scost<-costs%>%
  filter(Time!="Year")%>%
  select(Time,Spain)
head(Scost)
tail(Scost)
Scost<-cbind(Scost$Time,as.numeric(Scost$Spain)*1400)
Scost<-ts(Scost[,2],start = 1970,frequency = 1)


lim<-9.5e+08
plot(Kcost,main="OECD countries' medical costs",
     ylab="Costs",xlab="Year",col="red",
     ylim=c(0,lim))
par(new=T)
plot(JPcost,main="",ylab="",xlab="",col="blue",ylim=c(0,lim))
par(new=T)
plot(UScost,main="",ylab="",xlab="",col="green",ylim=c(0,lim))
par(new=T)
plot(Gcost,main="",ylab="",xlab="",col="yellow",ylim=c(0,lim))
par(new=T)
plot(Scost,main="",ylab="",xlab="",col="brown",ylim=c(0,lim))
legend(x=c(1970,1990),y=c(4.5e+08,8.0e+08),
       legend = c("Korea","USA","Japan","Germany","Spain"),
       col = c("red","blue","green","yellow","brown"),lty = 1,
       cex=1)
plot(Kcost,main="Korea's medical costs",ylab="Costs",xlab="Year",col="red")
