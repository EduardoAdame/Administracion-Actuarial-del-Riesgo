
#Ajuste de una distribuci?n te?rica a la severidad de la cartera de vida colectiva
#del grupo Barcel. Esta base de siniestralidad es usada por el ?rea de 
#suscripci?n para estudiar la cartera y dar un precio de la prima a trav?s del
#c?lculo de una cuota de riesgo m?todos establecidos por la compa??a.

##Cargamos la paqueteria necesaria

library(actuar)
library(psych)
library(ADGofTest)
library(fitdistrplus)
library(BB)
library(MASS)
library(evir)
library(evd)
library(ismev)
library(vcd)
library(ChainLadder)
library(lattice)
library(mixtools)
library(mclust)
library(mixdist)
library(eva)
library(stats)


#Cargamos la direcci?n de trabajo:


#Cargamos los datos de Montos a los que les ajustaremos alguna distribuci?n 
#Param?trica

monto<-read.csv("Barcel.csv")

length(monto)

attach(monto)

summary(Monto)

#El valor m?nimo de la siniestralidad es de $14,0186 pesos, mientras, el mayor es de $1,065,238 pesos.
#esto ya nos deber?a de hablar de un rango bastante grande y de alguna deviaci?n bastante fuerte,
#esto  porque el tercer cuartil es  $409, 612 pesos. Es decir, la m?xima observaci?n se encuentra
#poco mas de 2 veces m?s alejada que el tercer cuartil.


#Calculamos el Rango
max(Monto)-min(Monto)

#Calculamos la desviaci?n est?ndar
sd(Monto)


#Calculamos el rango intercuantil
Q1=quantile(Monto,probs=c(0.25))
Q3=quantile(Monto,probs=c(0.75))

Q3-Q1


#Veamos el comportamiento de su histograma

hist(Monto/1000,col="springgreen3",
     border="white",main="Histograma: Monto por Fallecimiento",
     xlab="Monto en MDP", ylab="Densidad",ylim=c(0,0.004),xlim=c(0,1200),
     col.main="cyan4",col.lab="black",breaks=15,prob=T,cex.lab=1.1,lwd=2)
lines(density(Monto/1000),col="mediumorchid4",lwd=2)

boxplot(Monto,col="springgreen3",main="Boxplot: Monto",
        col.main="cyan4",lwd=2.5)

Forma<- function(x) {
  n <- length(x)
  mean<-sum(x)/n
  M <-rep(0,4)
  for(i in 1:4){
    M[i] <- sum((x-mean)^i)/n
  }
  skew<-M[3]/(M[2])^(3/2)
  kurt<-(M[4]/(M[2]^2))-3
  return(list(Momentos=M,sesgo=skew,curtosis=kurt))
}

Forma(Monto)

Monto.fd<-ecdf(Monto)

plot(Monto.fd,col="springgreen3",main="Funci?n de distribuci?n emp?rica: Monto",
     xlab="x",ylab=expression(hat(F)[n](x)),col.main="cyan4",
     col.lab="black",cex.lab=1.1)

#Funci?n de Distribuci?n

par(mfrow=c(1,1))
descdist(Monto,discrete=FALSE,boot=5000,obs.pch = 20, boot.col="springgreen3")



#Normal  

parefit<-fitdist(Monto,"norm", method="mme")
parefit
ks.test(Monto,"pnorm",mean=parefit$estimate[1],sd=parefit$estimate[2])
ad.test(Monto, pnorm,mean=parefit$estimate[1],sd=parefit$estimate[2])
plot(density(Monto),col="springgreen3",main="Ajuste a una distribuci?n Normal",cex.lab=1.1,
     xlab="Monto",ylab="Densidad",col.main="cyan4",col.lab="black",lwd=3)
curve(dnorm(x,mean=parefit$estimate[1],sd=parefit$estimate[2]),from=0,to=max(Monto),add=TRUE,col="mediumorchid4",lwd=2)


#Exponencial  
parefit<-fitdist(Monto,"exp", method="mme")
parefit
ks.test(Monto,"pexp",rate=parefit$estimate[1])
ad.test(Monto, pexp,rate=parefit$estimate[1])
plot(density(Monto),col="springgreen3",main="Ajuste a una distribuci?n Exponencial",cex.lab=1.1,
     xlab="Monto",ylab="Densidad",col.main="cyan4",col.lab="black",lwd=3)
curve(dexp(x,rate=parefit$estimate[1]),from=0,to=max(Monto),add=TRUE,col="mediumorchid4",lwd=2)


#Gamma

parefit<-fitdist(Monto,"gamma", method="mme")
parefit
ks.test(Monto,"pgamma",shape=parefit$estimate[1],rate=parefit$estimate[2])
ad.test(Monto, pgamma,shape=parefit$estimate[1],rate=parefit$estimate[2])
plot(density(Monto),col="springgreen3",main="Ajuste a una distribuci?n Gamma",cex.lab=1.1,
     xlab="Monto",ylab="Densidad",col.main="cyan4",col.lab="black",lwd=3)
curve(dgamma(x,shape=parefit$estimate[1],rate=parefit$estimate[2]),from=0,to=max(Monto),add=TRUE,col="mediumorchid4",lwd=2)


#Lognormal

parefit<-fitdist(Monto,"lnorm", method="mle")
parefit
ks.test(Monto,"plnorm",mean=parefit$estimate[1],sdlog=parefit$estimate[2])
ad.test(Monto, plnorm,mean=parefit$estimate[1],sdlog=parefit$estimate[2])
plot(density(Monto),col="springgreen3",main="Ajuste a una distribuci?n Lognormal",cex.lab=1.1,
     xlab="Monto",ylab="Densidad",col.main="cyan4",col.lab="black",lwd=3)
curve(dlnorm(x,mean=parefit$estimate[1],sdlog=parefit$estimate[2]),from=0,to=max(Monto),add=TRUE,col="mediumorchid4",lwd=2)




# Burr
parefit<-fitdist(Monto,"burr",start=list(shape1=5,shape2=1,scale=30),"mle")
parefit
ks.test(Monto,"pburr",shape1=parefit$estimate[1],shape2=parefit$estimate[2],scale=parefit$estimate[3])
ad.test(Monto, pburr,shape1=parefit$estimate[1],shape2=parefit$estimate[2],scale=parefit$estimate[3])
plot(density(Monto),col="springgreen3",main="Ajuste a una distribuci?n Burr",cex.lab=1.1,
     xlab="Monto",ylab="Densidad",col.main="cyan4",col.lab="black",lwd=3)
curve(dburr(x,shape1=parefit$estimate[1],shape2=parefit$estimate[2],scale=parefit$estimate[3]),from=0,to=max(Monto),add=TRUE,col="mediumorchid4",lwd=2)


# Weibull

parefit<-fitdist(Monto,"weibull", lower=0, "mle")
parefit
ks.test(Monto,"pweibull",shape=parefit$estimate[1],scale=parefit$estimate[2])
ad.test(Monto, pweibull,shape=parefit$estimate[1],scale=parefit$estimate[2])
plot(density(Monto),col="springgreen3",main="Ajuste a una distribuci?n Weibull",cex.lab=1.1,
     xlab="Monto",ylab="Densidad",col.main="cyan4",col.lab="black",lwd=3)
curve(dweibull(x,shape=parefit$estimate[1],scale=parefit$estimate[2]),from=0,to=max(Monto),add=TRUE,col="mediumorchid4",lwd=2)



##Todo parece indicar que la Siniestralidad de Grupo Barcel Sigue una Distribuci?n Lognormal
##de par?metros  12.6317719, 0.4378533

##Vamos a comparar las densidades y funci?n de distribuciones de ambas distribuciones vs la cartera


parefitgamma<-fitdist(Monto,"gamma", method="mme")
parefitgamma


parefitlognor<-fitdist(Monto,"lnorm", method="mle")
parefitlognor


### Comparando las densidades:

par(mfrow=c(1,2))

par(cex=0.8)
plot(density(Monto),type="l",col="springgreen3",main="Ajuste Densidad Gamma",col.main="cyan4",xlab="",ylab="",lwd=3)
curve(dgamma(x,shape=parefitgamma$estimate[1],rate=parefitgamma$estimate[2]),from=0,to=max(Monto),add=TRUE,col="mediumorchid4",lwd=2)
k<-c ("Densidad Datos Emp?rica","Densidad Te?rica estimada") 
legend (300000,0.000003,paste(k), bty="n",text.col=c("springgreen3","mediumorchid4"), cex=1)


plot(density(Monto),type="l",col="springgreen3",main="Ajuste Densidad Lognormal",col.main="cyan4",xlab="",ylab="",lwd=3)
curve(dlnorm(x,meanlog=parefitlognor$estimate[1], sdlog=parefitlognor$estimate[2]),from=0,to=max(Monto),add=TRUE,col="mediumorchid4",lwd=2)
k<-c ("Densidad Datos Emp?rica","Densidad te?rica estimada") 
legend (300000,0.000003,paste(k),bty="n",text.col=c("springgreen3","mediumorchid4"),cex=1)



### Comparando las funciones de Distribuci?n

par(mfrow=c(1,2))

plot(ecdf(Monto),verticals=TRUE,do.points=FALSE,col.hor="springgreen3", col.vert="palegreen",main="Ajuste Distribuci?n Gamma",col.main="cyan4",ylab="",lwd=3)
curve(pgamma(x,shape=parefitgamma$estimate[1], rate=parefitgamma$estimate[2]),from=0,to=max(Monto),add=TRUE,col="mediumorchid4",lwd=2)
k<-c ("Distribuci?n Datos Emp?ricos","Distribuci?n Te?rica Estimada") 
legend (100000,0.6,paste(k),col=c("springgreen3","mediumorchid4"), cex=0.9,bty="n",text.col=c("springgreen3","mediumorchid4"))

plot(ecdf(Monto),verticals=TRUE,do.points=FALSE,col.hor="springgreen3", col.vert="palegreen",main="Ajuste Distribuci?n Lognormal",col.main="cyan4",ylab="",lwd=3)
curve(plnorm(x,meanlog=parefitlognor$estimate[1], sdlog=parefitlognor$estimate[2]),from=0,to=max(Monto),add=TRUE,col="mediumorchid4",lwd=2)
k<-c ("Distribuci?n Datos Emp?ricos","Distribuci?n Te?rica Estimada") 
legend (100000,0.6,paste(k),col=c("springgreen3","mediumorchid4"), cex=0.9,bty="n",text.col=c("springgreen3","mediumorchid4"))


## Las gr?ficas de PPplot

par(mfrow=c(1,2))


plot(ppoints(length(Monto)),sort(pgamma(Monto,shape=parefitgamma$estimate[1], rate=parefitgamma$estimate[2])),pch=19,col="springgreen3",main="PPplot para el Ajuste Gamma",
     col.main="cyan4",xlab="Percentiles te?ricos",ylab="Percentiles muestrales",cex.lab=1.1)
abline(c(0,1),col="mediumorchid4",lwd=2)


plot(ppoints(length(Monto)),sort(plnorm(Monto,meanlog=parefitlognor$estimate[1], sdlog=parefitlognor$estimate[2])),pch=19,col="springgreen3",main="PPplot para el Ajuste Lognormal",
     col.main="cyan4",xlab="Percentiles te?ricos",ylab="Percentiles muestrales",cex.lab=1.1)
abline(c(0,1),col="mediumorchid4",lwd=2)


## Las gr?ficas de QQplot

par(mfrow=c(1,2))


plot(qgamma(ppoints(Monto),shape=parefitgamma$estimate[1], rate=parefitgamma$estimate[2]),pch=19,col="springgreen3",sort(Monto),main="QQplot para el Ajuste Gamma",
     col.main="cyan4",xlab="Cuantiles te?ricos",ylab="Cuantiles muestrales",cex.lab=1.1)
abline(c(0,1),col="mediumorchid4",lwd=2)


plot(qlnorm(ppoints(Monto),meanlog=parefitlognor$estimate[1], sdlog=parefitlognor$estimate[2]),pch=19,col="springgreen3",sort(Monto),main="QQplot para el Ajuste Lognormal",
     col.main="cyan4",xlab="Cuantiles te?ricos",ylab="Cuantiles muestrales",cex.lab=1.1)
abline(c(0,1),col="mediumorchid4",lwd=2)



fit.lognormal<-fitdist(Monto,"lnorm")
fit.gamma<-fitdist(Monto,"gamma", "mme")

gofstat(list(fit.gamma, fit.lognormal))


## Resumen con las cuatro gr?ficas en 1 s?lo espacio

par(mfrow=c(2,2))

#Densidad
plot(density(Monto),type="l",col="springgreen3",main="Ajuste Densidad Lognormal",col.main="cyan4",xlab="",ylab="",lwd=3)
curve(dlnorm(x,meanlog=parefitlognor$estimate[1], sdlog=parefitlognor$estimate[2]),from=0,to=max(Monto),add=TRUE,col="mediumorchid4",lwd=2)

#Distribuci?n
plot(ecdf(Monto),verticals=TRUE,do.points=FALSE,col.hor="springgreen3", col.vert="palegreen",main="Ajuste Distribuci?n Lognormal",col.main="cyan4",ylab="",lwd=3)
curve(plnorm(x,meanlog=parefitlognor$estimate[1], sdlog=parefitlognor$estimate[2]),from=0,to=max(Monto),add=TRUE,col="mediumorchid4",lwd=2)

#PPplot
plot(ppoints(length(Monto)),sort(plnorm(Monto,meanlog=parefitlognor$estimate[1], sdlog=parefitlognor$estimate[2])),pch=19,col="springgreen3",main="PPplot para el Ajuste Lognormal",
     col.main="cyan4",xlab="Percentiles te?ricos",ylab="Percentiles muestrales",cex.lab=1.1)
abline(c(0,1),col="mediumorchid4",lwd=2)

#QQplot
plot(qgamma(ppoints(Monto),shape=parefitgamma$estimate[1], rate=parefitgamma$estimate[2]),pch=19,col="springgreen3",sort(Monto),main="QQplot para el Ajuste Gamma",
     col.main="cyan4",xlab="Cuantiles te?ricos",ylab="Cuantiles muestrales",cex.lab=1.1)
abline(c(0,1),col="mediumorchid4",lwd=2)


### C?lculo y comparaci?n de prima bajo esta metodolog?a.

alea=rlnorm(100000,meanlog=parefitlognor$estimate[1], sdlog=parefitlognor$estimate[2])

siniespMedia<-mean(alea)
siniespMediana<-median(alea)
siniespQ75<-quantile(alea,.75)
siniespQ80<-quantile(alea,.80)
siniespQ90<-quantile(alea,.90)
siniespQ95<-quantile(alea,.95)


siniespMedia
siniespMediana
siniespQ75
siniespQ80
siniespQ90
siniespQ95

SA<-237766113.83

#Comparaci?n Con Prima calculada como la Media

PRMedia<-siniespMedia
SAMediana<-237766113.83
CuotaMedia<-(PRMedia/SA)*1000
DifMedia<-PRMedia-744609.99

PRMedia
SAMedia
CuotaMedia
DifMedia


#Comparaci?n Con Prima calculada como la Mediana

PRMediana<-siniespMediana
SAMediana<-237766113.83
CuotaMediana<-(PRMediana/SA)*1000
DifMediana<-PRMediana-744609.99

PRMediana
SAMediana
CuotaMediana
DifMediana


#Comparaci?n Con Prima calculada como Q75

PRQ75<-siniespQ75
SAQ75<-237766113.83
CuotaQ75<-(PRQ75/SA)*1000
DifQ75<-PRQ75-744609.99

PRQ75
SAQ75
CuotaQ75
DifQ75


#Comparaci?n Con Prima calculada como Q80

PRQ80<-siniespQ80
SAQ80<-237766113.83
CuotaQ80<-(PRQ80/SA)*1000
DifQ80<-PRQ80-744609.99

PRQ80
SAQ80
CuotaQ80
DifQ80

#Comparaci?n Con Prima calculada como Q90

PRQ90<-siniespQ90
SAQ90<-237766113.83
CuotaQ90<-(PRQ90/SA)*1000
DifQ90<-PRQ90-744609.99

PRQ90
SAQ90
CuotaQ90
DifQ90


#Comparaci?n Con Prima calculada como Q95

PRQ95<-siniespQ95
SAQ95<-237766113.83
CuotaQ95<-(PRQ95/SA)*1000
DifQ95<-PRQ95-744609.99

PRQ95
SAQ95
CuotaQ95
DifQ95


#?CU?L ES EL M?S CERCANO?

siniespQ98<-quantile(alea,.98)
siniespQ98
