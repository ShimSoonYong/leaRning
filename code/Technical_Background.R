#exponential functions
x=seq(0,10,0.01)
plot(x,exp(x),type="l",col="red")
plot(x,log(x),type="l",col="blue")

#trigometric functions
x<-seq(0,2*pi,0.01)
sinx<-sin(x)
cosx<-cos(x)
tanx<-tan(x)
plot(x,sinx,ylim=c(-3,3),type = "l",col="red")
lines(x,cosx,col="blue")
lines(x,tanx,col='green')
legend(2,3,legend=c("sin(x)","cos(x)","tan(x)"),lwd=rep(1,3),col=c("red","blue",'green'),bty="n")

asin(sin(pi))

#power laws
x<-seq(0,2,0.01)
plot(x,1/x^2,type="l",ylab=expression(x^b),col="purple",ylim = c(0,4))
lines(x,1/x,col="skyblue")
abline(h=1,col='green')
lines(x,x^0.5,col='tan')
lines(x,x,col="blue")
lines(x,x^2,col="red")
legend(0.9,4,bty='n',legend=c("b=-2","b=-1","b=0","b=0.5","b=1","b=2"),col=c('purple','skyblue','green','tan','blue','red'))

#polynomial functions
x<-seq(0,10,0.01)
plot(x,5*x-0.2*x^2,type="l",col="red",xaxt="n",yaxt="n")
lines(x,5*x-0.4*x^2,col="blue")
lines(x,2+4*x-0.6*x^2+0.04*x^3,col="green")
lines(x,2+4*x+2*x^2-0.6*x^3+0.04*x^4,col="skyblue")

plot(x,x/(2+5*x),type="l",col="red",xaxt="n",yaxt="n")
plot(x,1/(x-2+4/x),type="l",col="red",xaxt="n",yaxt="n")
plot(x,1/(x^2-2+4/x),type="l",col="red",xaxt="n",yaxt="n")

#gamma function
t<-seq(0.2,4,0.01)
plot(t,gamma(t),type="l",col="red")
abline(h=1,lty=2,col="green")

#asymptotic functions
A<-(1/44.44-1/70.59)/(1/0.2-1/0.6)
a<-1/A
a
b=1/0.2*(a*0.2/44.44-1)
b

#sigmoid functions
x<-seq(0,10,0.01)
y<-100/(1+90*exp(-1*x))
plot(x,y,type="l",col="blue")
y<-20+100/(1+exp(0.8*(3-x)))
plot(x,y,ylim=c(0,140),type='l',col='red')
x<-seq(0,100,0.1)
y<-50*exp(-5*exp(-0.08*x))
plot(x,y,type='l',col='red')
x<-seq(-200,100,0.1)
y=100*exp(-exp(0.02*x))
plot(x,y,type='l',col='red')

#biexponential function
x<-seq(0,10,0.01)
biexp_plot<-function(a,b,c,d,i){
  y<-a*exp(b*x)+c*exp(d*x)
  plot(x,y,type='l',col='blue')
}
biexp_plot(10,-0.8,10,-0.05,1)
biexp_plot(10,-0.8,10,0.05,1)
biexp_plot(200,0.2,-1,0.7,3)
biexp_plot(200,0.05,300,-0.5,4)

#differentiation
D(expression(2*x^3),"x")
dxy<-deriv(~2*x^3,"x")
eval(dxy,envir=list(x=1:3))
D(expression(a*exp(-b*x)),"x")
D(expression(a/(1+b*exp(-c*x))),"x")
trig_exp<-expression(sin(x+2*y))  
D(trig_exp,"x")
D(trig_exp,"y")

#intergration
integrate(dnorm,lower=-1.96,upper=1.96)
pnorm(1.96)-pnorm(-1.96)
our_fn<-function(x){
  exp(-x)
}
integrate(our_fn,lower=0,upper=Inf)

x<-seq(0,20,0.01)
y<-exp(-x)
plot(x,y,type='l',col='red')
polygon(x=c(0,x,20),y=c(0,y,0),col='skyblue',border=NA)

install.packages("deSolve")
library(deSolve,quietly = T)

phmodel<-function(t,state,parameters){
  with(as.list(c(state,parameters)),{
    dv<-r*v*(K-v)/K-b*v*n
    dn<-c*v*n-d*n
    result<-c(dv,dn)
    list(result)
  })
}

#differential equations
times<-seq(0,500)
parameters<-c(r=0.4,K=1000,b=0.02,c=0.01,d=0.3)
initial<-c(v=50,n=10)
phm_output<-ode(y=initial,time=times,func=phmodel,parms=parameters)
plot(phm_output[,1],phm_output[,2],ylim = c(0,max(phm_output[,2:3])),type='l',ylab='abundace',xlab='time',col='red')
lines(phm_output[,1],phm_output[,3],col='blue')
legend(300,60,legend=c("plant","herbivore"),lty=1,col=c('red','blue'))

plot(phm_output[,2],phm_output[,3],xlim=c(0,max(phm_output[,2:3])),
     ylim=c(0,max(phm_output[,2:3])),type='l',
     ylab='plant',xlab='herbivore',col='red')
