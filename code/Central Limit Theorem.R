#the central limit theorem
unif_data<-runif(6000,min = 0,max = 10)
unif_samples<-matrix(unif_data,ncol=6)
hist(unif_data,main="",xlab="",col='red',breaks = 11)
hist(rowMeans(unif_samples),main="",xlab="",col="green",breaks=21,freq=F)

mean(rowMeans(unif_samples))
sd(rowMeans(unif_samples))

seq(0,10,0.01)->x
lines(x,dnorm(x,mean=5,sd=sqrt(100/72)),col="blue",lwd=2)

#by bigger samples
unif_data=runif(2*100000,min=0,max=10)
unif_samples<-matrix(unif_data,ncol=40)
hist(unif_data,main="",xlab="",col="red",breaks = 11)
y<-hist(rowMeans(unif_samples),plot=F)
x<-seq(0,10,0.01)
hist(rowMeans(unif_samples),
     main = "",xlab = "",col='green',
     breaks = 41,freq=F,
     ylim=c(0,max(dnorm(x,mean=5,sd=sqrt(100/(12*40)))
            ,y$density)))
lines(x,dnorm(x,mean=5,sd=sqrt(100/(12*40))),col="blue",lwd=2)

sd(rowMeans(unif_samples))
sqrt(100/(12*40))      

#by gamma random numbers
gamma_data<-rgamma(2*100000,shape=12,rate=4)
gamma_samples<-matrix(gamma_data,ncol=40)
hist(gamma_data,main = "",xlab="",col="red",breaks=20)
x<-seq(2.4,4,0.01)
y<-hist(rowMeans(gamma_samples),plot=F)
hist(rowMeans(gamma_samples),main="",xlab="",col="green",
     breaks=41,freq=F,
     ylim=c(0,max(
       dnorm(x,mean=3,
             sd=sqrt(
               12/(4*4*40)
             )),y$density
     )))
lines(x,dnorm(
  x,mean=3,sd=sqrt(
    12/(4*4*40)
  )
),col="blue",lwd=2)
     
mean(rowMeans(gamma_samples))
sd(rowMeans(gamma_samples))
sqrt(12/(4*4*40))
