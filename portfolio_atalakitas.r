##Átalakítás befektetéshez

#hozam és szórásvektor meg van adva

n = length(hozamok)
  Dmat = diag(szorasok)
  dvec = rep(0, n)
  Amat = cbind(hozamok, rep(1, n), diag(n))
  
r=colMeans(Amat)  
w1<-c() #befektetés
h1<-sum(r*w1);h1
#k1<-sqrt(w1%*%cov(X)%*%w1);k1
k1<-sqrt(w1%*%Dmat)%*%w1);k1  ##vagy ide lehet hogy a kovariancia Dmat kell.
