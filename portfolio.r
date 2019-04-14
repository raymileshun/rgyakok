##�rai feladat, mondta hogy ilyen lehet zhban. Line�ris regresszi�t kell
##csin�lni.
##Oldal�n a Stat II. oldalon az uj_feladatsor a 49.-es p�ld�t
##csin�ltuk. (a parancsokat kommentezni is kell, ez az �gymond
## "magyar�z�s")

x<-c(12.6,12.4,11.6,10.4,4.4)
y<-c(81.1,78.5,75.4,74,65.4)
plot(x,y)

n<-length(x);n
b1<-(sum(x*y)-n*mean(x)*mean(y))/(sum(x^2)-n*mean(x)^2);b1
b0<-mean(y)-b1*mean(x);b0

y_hat<-b0+b1*x;y_hat #becs�lt �lettartam
lines(x,y_hat,col="blue")
b0+b1*12 #12 iskolai �vesnek a becs�lt �lettartama
points(12,b0+b1*12,col="blue",lwd=3,pch=2)
text(12,b0+b1*12,paste(round(b0+b1*12)),c(1,0))

##Feladat v�ge.


##� megold�sa:

x<-c(12.6,12.4,11.6,10.4,4.4) #tanul�s
y<-c(81.1,78.5,75.4,74,65.4)  #v�rhat� �lettartam
lm(y~x) #Intercept:  x=0-hoz tartoz� y �rt�k, azaz ha valaki
        #0 �vet tanul, akkor ennyi a v�rhat� �lettartama  (beta0)
	  #M�sik �rt�k: ha 1 �vvel t�bbet tanul, akkor annyi a n�vekedett
	  #�lettartam. Azaz ha az x �rt�k�t egyel n�velj�k akkor az
	  #y annyival n�vekszik
predict(lm(y~x), newdata=data.frame(x=12)) #Azaz 12 �ves iskol�zatts�gi
					  #szintn�l annyi a v�rhat� �lettartam (77.79)

##Megold�s�nak v�ge




#Portf�li� tervez�s
Legyen x0 kezdeti t�k�nk, melyet n db befektet�si eszk�zbe fektethet�nk
Ezek megt�r�l�si r�t�ja: R1,...,Rn (vals�gi v�ltoz�)
	V�rhat� hozam:  E(Ri)=ri

Tekints�k a befektet�si eszk�z�k covarianciam�trix�t
	Cij=cov(Ri,Rj)

Megjegyz�s: ha a befektet�si eszk�z�k f�ggetlenek:
	cov(Ri,Rj)=0
	cov(Ri,Ri)= szigma^2 (sz�r�sn�gyzet)
Ilyen esetben csak a f��tl�ba vannak �rt�kek

C�l: hozam->MAX
     kock�zat->MIN

  k�rd�s: Hogyan fektess�k be a p�nz�nket?(Melyik eszk�zbe mennyit? -> s�lyok)
############################################################
************************************************************


#Adatok gener�l�sa
X<-matrix(0,ncol=12,nrow=11)

#Kis hozam, Kis kock�zat - Nagy hozam, Nagy kock�zat
for (i in 1:10){
  set.seed(i)
  X[i,]<-rnorm(12,mean=i,sd=i)}

#Kock�zatmentes banki k�tv�ny
X[11,]<-rep(0.05,n=12)  

X<-t(X)
head(X)
colnames(X)<-paste(1:11,'term�k',sep='.')
row.names(X)<-paste(1:12,"h�",sep=".")
head(X)

#�ves hozam
r<-colMeans(X);r
par(mfrow=c(1,2))
plot(1:11,r,xlab="Befektet�si eszk�z�k", ylab="�ves hozam")

#Sz�r�s
sigma<-NULL
for (i in 1:11){
  sigma[i]<-sd(X[,i])}
names(sigma)<-names(r)
plot(1:11,sigma,xlab="Befektet�si eszk�z�k", ylab="Sz�r�s")

#Fektess�nk be!
par(mfrow=c(1,1))
plot(0:10,0:10,type="n",xlab="Kock�zat",ylab="Hozam",main="Lehets�ges portf�li�k")


#1. Csak a legnagyobb hozam�ba fektess�k a p�nz�nket.
which.max(r)
which.max(sigma)  #egyben a legnagyobb kock�zat� is
w1<-c(0,0,0,0,0,0,1,0,0,0,0) #7. eszk�zbe tessz�k az 1 egys�gnyi p�nz�nket
h1<-sum(r*w1);h1
k1<-sqrt(w1%*%cov(X)%*%w1);k1
points(k1,h1,col="blue",pch=16)
text(k1,h1,"1",c(1,0))

#2. Egyform�n fektess�k be p�nz�nket
w2<-rep(1/11,11)
h2<-sum(r*w2);h2
k2<-sqrt(w2%*%cov(X)%*%w2);k2
points(k2,h2,col="blue",pch=16)
text(k2,h2,"2",c(1,0))

#3. Csak k�tv�nybe fektess�k be p�nz�nket
w3<-c(rep(0,10),1)
h3<-sum(r*w3);h3
k3<-sqrt(w3%*%cov(X)%*%w3);k3
points(k3,h3,col="blue",pch=16)
text(k3,h3,"3",c(1,0))

#4. Kaptunk egy tippet
sort(r) 
w4<-c(0, 0, 0, 0.1, 0, 0.2, 0.25, 0, 0.25, 0.2, 0 ) #5 legnagyobb hozam�ba rakunk
sum(w4)
h4<-sum(r*w4);h4
k4<-sqrt(w4%*%cov(X)%*%w4);k4
points(k4,h4,col="blue",pch=16)
text(k4,h4,"4",c(1,0))

#5. Egy�ni �tlet:
w5<-c(0,0.3,0,0.2,0.1,0.2,0,0,0,0,0.2)
sum(w5)
h5<-sum(r*w4);h4
k5<-sqrt(w5%*%cov(X)%*%w5);k5
points(k5,h5,col="green",pch=16)
text(k5,h5,"5",c(1,0))

install.packages('quadprog')
library(quadprog)

#Mi lesz az optim�lis port�li�? 
#Azaz a legnagyobb hozam� �s legkisebb kock�zat�?
portfolio_fv <- function(data,r)
{
  n <- ncol(data)   #adatsor oszlopainak a sz�ma
  Dmat <-  cov(data)   ### kovarianciam�trix
  dvec <-  rep(0, times=n)  ### nullvektor n elem�
  Amat <-  cbind(colMeans(data), rep(1, n),diag(n))   #oszloponk�nt �sszerakja a m�trixot
  bvec <-  c(r, 1, rep(0, times=n))    #vektort �ll�t el�
  meq <-  2
  portfolio = solve.QP(Dmat, dvec, Amat, bvec, meq)   #optimaliz�ci�!!!
  weights = round(portfolio$solution, digits = 4)  #megold�s kerek�t�se-->> S�LYOK
  names(weights) = colnames(data)
  list(weights = weights,risk = portfolio$value,return = r)
}

data<-X[,1:10]  #Kivessz�k ak�tv�nyt
r <- mean(r[-11]) #kivessz�k a k�tv�ny hozam�t
opt_portfolio<-portfolio_fv(data,r)

#Optim�lis portf�li� s�lyai
w_opt<-round(opt_portfolio$weights*100,2);w_opt

#Optim�lis portf�il� hozama
r_opt<-opt_portfolio$return;r_opt 

#Optim�lis portf�il� kock�zata
risk_opt<-sqrt(opt_portfolio$weights%*%cov(data)%*%opt_portfolio$weights);risk_opt

points(risk_opt,r_opt,col="red",pch=16)
text(risk_opt,r_opt,"opt",c(1,0))

## Hat�kony portf�li�k g�rb�je
#Azaz az adott kock�zati szinten a legnagyobb hozam� port�fli�!
eff_port_fv <- function(data)
{
  n <-  ncol(data)
  ri <-  colMeans(data)
  p<- 3*n
  r <- seq(min(ri), max(ri), length=p)
  weights <-  rep(0, n)
  weights[which.min(ri)] <-  1
  for (i in 2:(p-1)) {
    newWeights <- portfolio_fv(data, r[i])$weights
    weights <-  rbind(weights, newWeights)}  #soronk�nt
  newWeights <-  rep(0, n)
  newWeights[which.max(ri)]<-  1
  weights <- rbind(weights, newWeights)
  weights <-  round(weights, 4)
  colnames(weights) = colnames(data)
  rownames(weights) = 1:p
  list(weights = weights, r = r)
  }

eff_port<-eff_port_fv(data)

#A k�l�nb�z� hozamokhoz tartoz� optim�lis portf�li�k s�lyai
weights <- eff_port$weights;weights
apply(weights,1,sum) #ellen�rz�s, sor�sszeg 1
r_eff<-eff_port$r;r_eff

#A k�l�nb�z� hozamokhoz tartoz� optim�lis portf�li�k kock�zatai
risks <- NULL
for (i in 1:nrow(weights)) {
  new_risk <- sqrt(weights[i, ] %*% cov(data) %*% weights[i, ])
  risks <- c(risks, new_risk)
}
risks

#Hat�kony portf�li�k g�rb�je
points(risks, r_eff,type="l")





##P�LDA### Swiss pension fund assets returns benchmark - Hozamok a Sv�jci nyugd�jalapokn�l
install.packages('fBasics')
library(fBasics)
data <- 100 * LPP2005REC[, 1:6]; data[1:10,] #100* az LPP2005REC adatsor 1-6 oszlopai
plot(LPP2005REC[, 1:6])

# PORTF�LI� ELV�RT HOZAMA (oszop�tlagok �tlaga)
r <- mean(colMeans(data)) 

#AZ OPTIM�LIS PORTF�LI�
portfolio <- portfolio_fv(data,r);portfolio   

#EREDM�NY
weights <- portfolio$weights*100;weights  #s�lyok kiirat�sa
sum(weights)   #s�lyok �sszege 100
c(weightedReturn = round((weights %*% colMeans(data))[[1]],3), r = round(100 * r, 3))  #ELLen�rz�s
Weights <- weights[weights > 0]   #csak a pozit�v s�ly�ak
pie(Weights, labels = names(Weights),main="LPP2005 Portfolio Weights")

r_opt<-portfolio$return
risk_opt<-sqrt(portfolio$weights%*%cov(data)%*%portfolio$weights)

eff_port<-eff_port_fv(data)
weights <- eff_port$weights;weights
r<-eff_port$r;r

risks <- NULL
for (i in 1:nrow(weights)) {
  new_risk <- sqrt(weights[i, ] %*% cov(data) %*% weights[i, ])
  risks <- c(risks, new_risk)
}
risks
plot(risks, r,type="l",xlab="Kock�zat",ylab="Hozam",main="Hat�kony portf�li�k g�rb�je")
points(risk_opt,r_opt,col="red",pch=16)
text(0.18,0.05,"(0.24,0.043)")
