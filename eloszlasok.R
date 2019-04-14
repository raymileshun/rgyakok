
#Normális eloszlás m várható érték, s szórás paraméterrel
m<-3
s<-2 

dnorm(seq(-5,5,0.1),mean=m,sd=s)
pnorm(seq(-5,5,0.1),mean=m,sd=s)
qnorm(seq(0,1,0.05),mean=m,sd=s)
 
m-s-3  #-2
m+s+3  #8
par(mfrow=c(1,3)) #1x3 rajzfelület
plot(seq(-2,8,0.1),pnorm(seq(-2,8,0.1),mean=m,sd=s),type="l",main="Eloszlásfv")
plot(seq(-2,8,0.1),dnorm(seq(-2,8,0.1),mean=m,sd=s),type="l",main="Sûrûségfv")
plot(seq(0,1,0.05),qnorm(seq(0,1,0.05),mean=m,sd=s),type="l",main="Kvantilisfv")

#Standard normális eloszlás 0 várható érték, 1 szórás paraméterrel
m<-0
s<-1
pnorm(seq(-5,5,0.1),mean=m,sd=s)
dnorm(seq(-5,5,0.1),mean=m,sd=s)
qnorm(seq(0,1,0.05),mean=m,sd=s)

par(mfrow=c(1,3))
plot(seq(-5,5,0.1),pnorm(seq(-5,5,0.1),mean=m,sd=s),type="l",main="Eloszlásfv")
plot(seq(-5,5,0.1),dnorm(seq(-5,5,0.1),mean=m,sd=s),type="l",main="Sûrûségfv")
plot(seq(0,1,0.05),qnorm(seq(0,1,0.05),mean=m,sd=s),type="l",main="Kvantilisfv")

#Kvantilis fv az eloszlásfv inverze!
par(mfrow=c(1,2))
plot(seq(-2,2,0.1),pnorm(seq(-2,2,0.1),mean=m,sd=s),type="l",main="Eloszlásfv")
lines(seq(0,1,0.1),pnorm(seq(0,1,0.1),mean=m,sd=s),col="green")
abline(a=0,b=1,lty=2)

plot(seq(0,1,0.05),qnorm(seq(0,1,0.05),mean=m,sd=s),type="l",xlim=c(-0.5,1.5),main="Kvantilisfv")
lines(seq(-0.5,1.5,0.1),pnorm(seq(-0.5,1.5,0.1),mean=m,sd=s),col="green")
abline(a=0,b=1,lty=2)

#követi a sürüségfvt
par(mfrow=c(1,1))
hist(rnorm(1000,mean=m,sd=s),freq=F)  
points(seq(-4,4,0.1),dnorm(seq(-4,4,0.1),mean=m,sd=s),type="l",col="red")


#Hova tömörülnek a pontok?
m<-5
s<-1 
par(mfrow=c(1,4))
plot(rnorm(10,mean=m,sd=s),rnorm(10,mean=m,sd=s))
plot(rnorm(100,mean=m,sd=s),rnorm(100,mean=m,sd=s))
plot(rnorm(1000,mean=m,sd=s),rnorm(1000,mean=m,sd=s))
plot(rnorm(10000,mean=m,sd=s),rnorm(10000,mean=m,sd=s))

#Normalitás tesztelés: QQNORM()
par(mfrow=c(1,1))
x<-rnorm(100,0,1)
qqnorm(x)
qqline(x,distribution=qnorm, col='red')
sq<-quantile(x,probs = c(0.25,0.75));sq  #sample quantiles
tq<-qnorm(c(0.25,0.75));tq

#Hogy kaptuk meg az egyenest?
par(mfrow=c(1,2))
qqnorm(x)
qqline(x,distribution=qnorm, col='red')

qqnorm(x)
lines(tq,sq,type="l",col="red",lwd=3)
abline(lm(sq ~ tq))  #Két pontra lineáris regsresszió

#airquality
str(airquality)
summary(airquality)
qqnorm(airquality$Temp)
qqline(airquality$Temp,distribution=qnorm, col='red')
qqnorm(airquality$Wind)
qqline(airquality$Wind,distribution=qnorm, col='red')

##################################################################

#Egyenletes eloszlás [a,b]
a<-0
b<-6

punif(seq(a,b,0.1),min=a,max=b)   
dunif(seq(a,b,0.1),min=a,max=b)
qunif(seq(0,1,0.1),min=a,max=b)

par(mfrow=c(1,3))
plot(seq(a-3,b+3,0.1),punif(seq(a-3,b+3,0.1),min=a,max=b),type="l",main="Eloszlásfv")
plot(seq(a-3,b+3,0.1),dunif(seq(a-3,b+3,0.1),min=a,max=b),type="l",main="Sûrûségfv")
plot(seq(0,1,0.05),qunif(seq(0,1,0.05),min=a,max=b),type="l",main="Kvantilisfv")


#követi a sürüségfvt
par(mfrow=c(1,2))
hist(runif(1000,min=a,max=b),freq = F)  
points(seq(a-0.1,b+0.1,0.1),dunif(seq(a-0.1,b+0.1,0.1),min=a,max=b),type="l",col="red")
hist(runif(10000,min=a,max=b),freq = F)
points(seq(a-0.1,b+0.1,0.1),dunif(seq(a-0.1,b+0.1,0.1),min=a,max=b),type="l",col="red")

#Hova tömörülnek a pontok?
par(mfrow=c(2,2))
plot(runif(10,min=a,max=b),runif(10,min=a,max=b))
plot(runif(100,min=a,max=b),runif(100,min=a,max=b))
plot(runif(1000,min=a,max=b),runif(1000,min=a,max=b)) 
plot(runif(10000,min=a,max=b),runif(10000,min=a,max=b))


#Egyenletes tesztelése: Khi-négyzet próba
x<-c(15,12,7,12,10,4)
p<-rep(1/6,6)  #p<-rep(1/length(x),length(x))

test<-chisq.test(x,p=p);test
test$p.value  #p.value>0.05 H0 (azaz illeszkedik), különben H1 
test$observed
test$expected
sum((test$observed-test$expected)^2/test$expected)  #X-squared
length(x)-1 #df

#Egyenletes tesztelése: Khi-négyzet próba
x<-abs(rnorm(10,mean=4,sd=4));x  #Nagyon szór
p<-rep(1/length(x),length(x));p

test<-chisq.test(x,p=p);test
test$p.value  #p.value>0.05 H0 (azaz illeszkedik), különben H1 


##################################################################
#Exponenciális eloszlás lambda paraméterrel (mean=1/rate >>> rate=lambda)
lambda<-3
pexp(seq(0,5,0.1),rate=lambda)
dexp(seq(0,5,0.1),rate=lambda)
qexp(seq(0,1,0.1),rate=lambda)

par(mfrow=c(1,3))
plot(seq(-2,5,0.1),pexp(seq(-2,5,0.1),rate=lambda),type="l",main="Eloszlásfv")
plot(seq(0,5,0.1),dexp(seq(0,5,0.1),rate=lambda),type="l",main="Sûrûségfv")
plot(seq(0,1,0.05),qexp(seq(0,1,0.05),rate=lambda),type="l",main="Kvantilisfv")


#követi a sürüségfvt
par(mfrow=c(1,1))
h<-hist(rexp(1000,rate=lambda),freq = F)  
points(seq(0,5,0.1),dexp(seq(0,5,0.1),rate=lambda),type="l",col="red")
h$breaks

#Hova tömörülnek a pontok?
par(mfrow=c(1,4))  
plot(rexp(10,rate=lambda),rexp(10,rate=lambda))
plot(rexp(100,rate=lambda),rexp(100,rate=lambda))
plot(rexp(1000,rate=lambda),rexp(1000,rate=lambda)) 
plot(rexp(10000,rate=lambda),rexp(10000,rate=lambda)) 

###############################################################

#Gamma eloszlás a hely, lambda skála paraméterrel 
lambda<-2
a<-2.5
pgamma(seq(0,5,0.1),shape=a, scale=lambda)
dgamma(seq(0,5,0.1),shape=a, scale=lambda)
 
par(mfrow=c(1,2))
plot(seq(-2,20,0.1),pgamma(seq(-2,20,0.1),shape=a,scale=lambda),type="l",main="Eloszlásfv")
plot(seq(-2,20,0.1),dgamma(seq(-2,20,0.1),shape=a,scale=lambda),type="l",main="Sûrûségfv")

lambda<-2
a<-1 #Exp(lambda)

plot(dgamma(seq(0,5,0.1),shape=a,rate=lambda),type="l",main="Gamma(1,labda)")
plot(dexp(seq(0,5,0.1),rate=lambda),type="l",main="Exp(lamda)")

rgamma(10,shape=a,scale=lambda)

#követi a sürüségfvt
lambda<-2
a<-2.5
par(mfrow=c(1,1))
hist(rgamma(1000,shape=a,scale=lambda),freq=F) 
points(seq(-2,20,0.1),dgamma(seq(-2,20,0.1),shape=a,scale=lambda),type="l",main="Sûrûségfv",col="red")
 

#Hova tömörülnek a pontok?
par(mfrow=c(1,2))
plot(rgamma(100,shape=a,scale=lambda),rgamma(100,shape=a,scale=lambda))
plot(rgamma(1000,shape=a,scale=lambda),rgamma(1000,shape=a,scale=lambda))


###############################################################

#Cauchy eloszlás location - hely, scale - skála paraméterrel 

pcauchy(seq(-5,5,0.1),0,1)
dcauchy(seq(-5,5,0.1),0,1)

par(mfrow=c(1,2)) 
plot(seq(-5,5,0.1),pcauchy(seq(-5,5,0.1),0,1),type="l",main="Eloszlásfv")
plot(seq(-5,5,0.1),dcauchy(seq(-5,5,0.1),0,1),type="l",main="Sûrûségfv")

l<-2  
s<-4
pcauchy(seq(-5,5,0.1),location=l,scale=s)
dcauchy(seq(-5,5,0.1),location=l,shape=s)

par(mfrow=c(1,2)) 
plot(seq(-5,5,0.1),pcauchy(seq(-5,5,0.1),location=l,scale=),type="l",main="Eloszlásfv")
plot(seq(-5,5,0.1),dcauchy(seq(-5,5,0.1),location=l,scale=),type="l",main="Sûrûségfv")

#Véletlenszám generálás
x<-rcauchy(100,0,1);x
summary(x)  #kiugró értékek

#követi a sürüségfvt
par(mfrow=c(1,2))
hist(x,freq=F) #bajvan, kiugró értékek
y<-x[x>-10 & x<10];y  #szûrés
summary(y) 
hist(y,freq=F) 
points(seq(-5,5,0.1),dcauchy(seq(-5,5,0.1),0,1),type="l",main="Sûrûségfv",col="red")

#Hova tömörülnek a pontok?
par(mfrow=c(1,2))
plot(rcauchy(100,0,1),rcauchy(100,0,1))
plot(rcauchy(1000,0,1),rcauchy(1000,0,1))

###############################################################

#Weibull eloszlás c=0 -eltolás, b -skála (scale), a-alak (shape) paraméterrel 
a<-0.5
b<-1
pweibull(seq(0,3,0.1),shape=a,scale=b)
dweibull(seq(-5,5,0.1),shape=a,scale=b)

par(mfrow=c(1,2)) 
plot(seq(0,3,0.1),pweibull(seq(0,3,0.1),shape=a,scale=b),type="l",main="Eloszlásfv")
plot(seq(-5,5,0.1),dweibull(seq(-5,5,0.1),shape=a,scale=b),type="l",main="Sûrûségfv")

#Speciális esetek
par(mfrow=c(1,2)) 
lambda<-3
plot(seq(-3,5,0.1),dexp(seq(-3,5,0.1),rate=lambda),type="l",main="Exponenciális Sürüségfv")
plot(seq(-3,5,0.1),dweibull(seq(-3,5,0.1),shape=1,scale=1/lambda),type="l",main="Weibull Sûrûségfv")


plot(seq(-3,3,0.1),dnorm(seq(-3,3,0.1),0,1),type="l",main="Normális Sürüségfv")
plot(seq(-3,3,0.1),dweibull(seq(-3,3,0.1),shape=3.57,scale=1),type="l",main="Weibull Sûrûségfv")

#Csak az alak paramétert változtatva hogy változik?
par(mfrow=c(1,1)) 
curve(dweibull(x,shape=0.5,scale=1),-3,3)
points(seq(-3,3,0.1),dweibull(seq(-3,3,0.1),shape=1,scale=1),col="red",type="l") # Exponenciális
points(seq(-3,3,0.1),dweibull(seq(-3,3,0.1),shape=2,scale=1),col="blue",type="l") #Rayleigh
points(seq(-3,3,0.1),dweibull(seq(-3,3,0.1),shape=3.57,scale=1),col="green",type="l") #Normális