##eloszlásfüggvény az x^3 volt, ennek kellett az inverze
##2. feladat

n<-1000
x<-runif(n);
generaltak <- x^(1/3) 
generaltak <- qunif(generaltak)
temp<-seq(0,1,0.01)
hist(generaltak,freq=F)
points(temp,(temp ^ 2)*3 , type="l", col="red")

gen_varhato <- mean(generaltak)
gen_szoras <- sd(generaltak)

gen_varhato
gen_szoras
