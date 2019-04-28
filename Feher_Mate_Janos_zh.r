###Név: Fehér Máté János
###Neptun-kód: BZ33U3
###2019-04-17

#1. feladat
custom.F1<-function(x){
	x<-sort(x)
	count<-0 #ha van több ismétlõdõ elem, akkor ez a változó >1
	legnagyobb<-c() #üres vektor
	n<-length(x)
	for(i in 2:n){
		if(x[i]==x[i-1]){
			count<-count+1
			legnagyobb[count]<-x[i]
		}
		
	}
	if(count>0){
		return(max(legnagyobb))
	} else {
		return("NINCS")
	}

}

tesztV1<-c(1,5,3,-2,4,2,0,6)
tesztV2<-c(1,3,5,2,-1,5,3)

teszt1<-custom.F1(tesztV1);teszt1
teszt2<-custom.F1(tesztV2);teszt2

#2. feladat

n<-1000
min<-0
max<-1
#set.seed(42)
u<-runif(n,min,max)
p1<-2
p2<-4
 
v<-qlnorm(u,mean=p1,sd=p2);v
#c) részfeladat
szurt_v<-v[v>-10 & v<10]

par(mfrow=c(1,1))
hist(szurt_v,main="Generált",freq=F)
points(seq(-10,10,0.01),dlnorm(seq(-10,10,0.01),mean=p1,sd=p2),type="l",col="red")

#b) részfeladat
x<-v
 
loglik<-function(y,par){
  loglik<-sum(dlnorm(y,mean=par[1], sd=par[2],log=TRUE))
  return(-loglik)
}
 
opt<-nlm(f=loglik, p=c(2,2),y=x);opt
opt$estimate




#3. feladat

n<-100
x1<-rbinom(n,1,prob=0.5)
x2<-rpois(n,lambda=30)
x3<-rpois(n,lambda=2)
x4<-rpois(n,lambda=12)
x5<-rnorm(n,mean=100,sd=30)
y<-50+2.5*x2+5*x3+4*x4+rnorm(n,10,5)
data<-data.frame(nem=x1,eletkor=x2,projekt=x3,iskola=x4,IQ=round(x5,1),fizetes=round(y,1))
attach(data)
#a) részfeladat
Fmodell<-lm(fizetes~nem+eletkor+projekt+iskola+IQ,data=data)
summary(Fmodell)
#b) részfeladat
Rmodell<-lm(fizetes~eletkor+projekt+iskola,data=data)
summary(Rmodell)
#Az Fmodell jobban illeszkedik, mivel kisebb az error és nagyobb az R-squared érték.
#c) részfeladat
predict(Fmodell,newdata=data.frame(nem=1,eletkor=40,IQ=130,iskola=14,projekt=1),level=0.90)


#4. feladat
H<-c(0.13,0.12,0.15,0.11,0.17)
SZ<-c(0.24,0.41,0.27,0.16,0.33)

#library(quadprog)
#Ha az értékpapírok hozama egymástól függetlenek,
#akkor a kovariancia mátrix (helyett) egy diagonális mátrix lesz,
#ahol az átlóban a szórásnégyzetek szerepelnek
portfolio_fv = function(H, SZ, r) {
  n <- length(H)
  Dmat <- diag(SZ)
  dvec <- rep(0, times=n)
  Amat <- cbind(H, rep(1, times=n), diag(n))
  bvec <- c(r, 1, rep(0,times=n))
  meq <- 2
  portfolio <- solve.QP(Dmat, dvec, Amat, bvec, meq)
  weights <- round(portfolio$solution, digits=4)
  return (list(weights = weights, risk = portfolio$value))
}
 
opt<-portfolio_fv(H, SZ, 0.15);opt #Optimális portfólió 15%-os elvárt hozammal
w<-opt$weights
 
labels<-c("A","B","C","D","E")
names(w)<-labels;w
which.max(w) #Mibe fektessünk be a legtöbbet
which.min(w) #Mibe a legkevesebbet
sum(w) #1
 
par(mfrow=c(1,1))
pie(w,labels,main="Tortadiagram")
############



