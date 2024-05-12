docs<-read.csv("의료체계_현황분석/의료종사자수_OECD회원국.csv")
head(docs)

#Korean model
library(dplyr)
korean_docs<-docs %>% 
  filter(Country=="Korea") %>%
  select(!Country)
head(korean_docs)
tail(korean_docs)

ts_korean_docs<-ts(korean_docs[,3],start=1981,frequency = 1)

library(forecast)
Kmodel<-auto.arima(ts_korean_docs)
Kmodel_pred<-forecast(Kmodel)
Kmodel_pred

#USA model
USA_docs<-docs %>%
  filter(Country=="USA") %>%
  select(!c(Country))
head(USA_docs)
tail(USA_docs)
ts_USA_docs<-ts(USA_docs[,3],start=1993,frequency = 1)
USA_model<-auto.arima(ts_USA_docs)
USA_pred<-forecast(USA_model)
USA_pred

#Japan model
Jap_docs<-docs %>%
  filter(Country=="Japan") %>%
  select(!c(Country))
head(Jap_docs)
tail(Jap_docs)
ts_Jap_docs<-ts(Jap_docs[,3],start=1960,frequency = 1)
Jmodel<-auto.arima(ts_Jap_docs)
Jpred<-forecast(Jmodel)

#Germany model
Ger_docs<-docs %>%
  filter(Country=="Germany")%>%
  select(!Country)
head(Ger_docs)
tail(Ger_docs)
ts_Ger_docs<-ts(Ger_docs[,3],start=1991,frequency = 1)
Gmodel<-auto.arima(ts_Ger_docs)
Gpred<-forecast(Gmodel)

#France model
Fra_docs<-docs %>%
  filter(Country=="France")%>%
  select(!Country)
head(Fra_docs)
tail(Fra_docs)
ts_Fra_docs<-ts(Fra_docs[,3],start=2000,frequency = 1)
Fmodel<-auto.arima(ts_Fra_docs)
Fpred<-forecast(Fmodel)

#Spain model
Spa_docs<-docs %>%
  filter(Country=="Spain")%>%
  select(!Country)
head(Spa_docs)
tail(Spa_docs)
ts_Spa_docs<-ts(Spa_docs[,3],start=1980,frequency = 1)
Smodel<-auto.arima(ts_Spa_docs)
Spred<-forecast(Smodel)

#Visualization
plot(Kmodel_pred,xlim=c(1960,2031),ylim = c(0,6),
     main="OECD doctors devided by population of 1000",
     xlab="Year",ylab="# of Doctors",col="red")
par(new=T)
plot(USA_pred,main="",xlab="",ylab="",
     xlim=c(1960,2031),ylim = c(0,6),
     col="blue")
par(new=T)
plot(Jpred,main="",xlab="",ylab="",xlim=c(1960,2031),ylim = c(0,6),
     col="green")
par(new=T)
plot(Gpred,main="",xlab="",ylab="",xlim=c(1960,2031),ylim = c(0,6),
     col="yellow")
par(new=T)
plot(Fpred,main="",xlab="",ylab="",xlim=c(1960,2031),ylim = c(0,6),
     col="skyblue")
par(new=T)
plot(Spred,main="",xlab="",ylab="",xlim=c(1960,2031),ylim = c(0,6),
     col="brown")
legend(x=c(1960,1980),y=c(3,6),
       legend = c("Korea","USA","Japan","Germany","France","Spain","Predict"),
       col = c("red","blue","green","yellow","skyblue","brown","grey"),lty = 1,
       cex=0.6)


