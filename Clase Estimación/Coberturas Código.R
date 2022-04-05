## Deducibles --------------------------------------------------------------

## Exponencial

## Generamos datos (accidentes)
set.seed(553878)
n<-1000000; lambda<-1/100

X<-rexp(n = n,rate = lambda)

## Fijamos un deducible
d<-50

## Pérdida asegurable (YL)
YL<-ifelse(test = X>d,yes = 1,no = 0)
YL<-YL*(X-d)

#Histograma de los datos
hist(YL[YL>0],probability = T,col="red")

#Función de densidad
fYL<-function(y,lambda=1/100,d=50){
  ifelse(test = y==0,
         yes=pexp(q = d,rate = lambda),
         no=dexp(x = y+d, rate=lambda))
}

#¿Es de densidad?
integral<-integrate(f = fYL,lower = 0.0001,upper = Inf)
integral<-integral$value

fYL(0)+integral

#Graficamos la parte > 0
##Emp??rica
plot(density(YL),
     col="red",lwd=2,
     main = "Densidades")

plot(fYL,from = 0.001,to = 2000,
     col="blue",lwd=2,add=T)

#¿Cuántos cayeron en 0?
sum(YL==0)/length(YL)
fYL(0)

#Bondad de ajuste
observados<-c(sum(YL==0),sum(YL>0)) ; observados
esperados<-c(fYL(0),1-fYL(0)) ; esperados

#Ji Cuadrada de bondad de ajuste
chisq.test(x = observados, p = esperados)


#Esperanza
yfYL<-function(y,lambda=1/100,d=50){
  ifelse(test = y==0,
         yes=y*pexp(q = d,rate = lambda),
         no=y*dexp(x = y+d, rate=lambda))
}

#Integrando (Esperanza)
integral<-integrate(f = yfYL,lower = 0,upper = Inf)
integral<-integral$value ; integral

#Forma alternativa w=Inf
integral<-integrate(f = function(x){1-pexp(x,rate =1/100)},lower = d,upper = Inf)
integral<-integral$value
##Valor de la esperanza
integral

#Muestral
mean(YL)

#Uniforme
#Generamos datos (accidentes)
set.seed(27)
n<-1000000; a<-100 ; b<-200

X<-runif(n = n,min = a,max = b)

#Fijamos un deducible
d<-150

#Pérdida asegurable (YL)
YL<-ifelse(test = X>d,yes = 1,no = 0)
YL<-YL*(X-d)

#Histograma de los datos
hist(YL[YL>0],probability = T,col="red")

#Función de densidad
fYL<-function(y,a=100,b=200,d=150){
  ifelse(test = y==0,
         yes=punif(q = d,min = a,max = b),
         no=dunif(x = y+d,min = a,max = b))
}

#¿Es de densidad?
integral<-integrate(f = fYL,lower = 0,upper = b-d)
integral<-integral$value

fYL(0)+integral

#Graficamos la parte > 0
##Emp??rica
plot(density(YL),
     col="red",lwd=2,
     main = "Densidades")

plot(fYL,from = 0.0001,to = b-d,
     col="blue",lwd=2,add=T)

#¿Cuántos cayeron en 0?
sum(YL==0)/length(YL)
fYL(0)

#Bondad de ajuste
observados<-c(sum(YL==0),sum(YL>0)) ; observados
esperados<-c(fYL(0),1-fYL(0)) ; esperados

#Ji Cuadrada de bondad de ajuste
chisq.test(x = observados, p = esperados)


#Esperanza
yfYL<-function(y,a=100,b=200,d=150){
  ifelse(test = y==0,
         yes=y*punif(q = d,min = a,max = b),
         no=y*dunif(x = y+d,min = a,max = b))
}


#Integrando (Esperanza)
integral<-integrate(f = yfYL,lower = 0,upper = b-d)
integral<-integral$value ; integral

#Forma alternativa
integral<-integrate(f = function(y){1-punif(y,min = 100,max = 200)},
                    lower = d,upper = b)
integral<-integral$value
##Valor de la esperanza
integral

#Muestral
mean(YL)


# L??mite de Póliza --------------------------------------------------------

#Pareto1

library(actuar)
?dpareto1

shape = 2 ; min = 3

f<-function(x) {
  dpareto1(x = x,shape = 2,min = 3)
}

#Fijamos el l??mite de póliza
U<-3.5

plot(f,col="red",lwd=2,
     from = 3, to = 4, ylim=c(0,0.7))
abline(v = U, col = "blue",lwd=2)

set.seed(27)
X<-rpareto1(n = 1000000,shape = shape,min = min)
Y<-pmin(X,U)

#Histograma de los datos < U
hist(Y[Y<U],probability = T,col="red")

fY<-function(y,u=3.5,min=3,shape=2){
  ifelse(y<u,
         yes=dpareto1(y,shape = shape,min = min),
         no=1-ppareto1(u,shape = shape,min = min))
}

plot(fY,lwd=2,col="blue",from=min,to=4,add=F)
#Histograma
hist(Y,probability = T,col="red",add=T)

#Esperanza teórica
yfYL<-function(y){
  y*fY(y)
}

#Integrando (Esperanza)
integral<-integrate(f = yfYL,lower = min,upper = U)
integral<-integral$value

#Parte continua + parte discreta
integral+U*(1-ppareto1(U,shape = shape,min = min))

#Forma alternativa de la esperanza
S<-function(y,min=3,shape=2){
  1-ppareto1(y,shape = shape,min = min)
}

#Integrando (Esperanza)
integral<-integrate(f = S,lower = min,upper = U)
integral<-integral$value

#Esperanza (alternativa)
min + integral

#Esperanza muestral
mean(Y)


# Deducible y L??mite de Póliza --------------------------------------------------------

#Unfiormes

#Parámetros
min<-1000
max<-2000

#Fijamos el l??mite de póliza
U<-1900
#Fijamos el deducible
D<-1200

f<-function(x) {
  dunif(x,1000,2000)
}

plot(f,col="red",lwd=2,
     from = min-1, to = max+1, ylim=c(0,2/1000))
abline(v = c(D,U), col = c("green","blue"),lwd=2)

set.seed(27)
X<-runif(1000000,min,max)
Y<-pmax(pmin(X,U)-D,0)

#Histograma de los 0 < datos < U - D
hist(Y[0<Y&Y<U-D],probability = T,col="red")

fY<-function(y,d=1200,u=1900,min=1000,max=2000){
  ifelse(y==0,
         yes=punif(d,1000,2000),             #y=0
         no=ifelse(test = y == u-d,
                   yes=1-punif(u,1000,2000), #y=u-d
                   no =dunif(y+d,1000,2000)))#y en (0,u-d)
}

plot(fY,lwd=2,col="blue",from=-1,to=U-D+1,add=F)
#Histograma
hist(Y,probability = T,col="red",add=T)

#Esperanza teórica
yfYL<-function(y){
  y*fY(y)
}

#Integrando (Esperanza)
integral<-integrate(f = yfYL,lower = 0,upper = U-D)
integral<-integral$value

#Parte continua + parte discreta
integral+(U-D)*(1-punif(U,1000,2000))

#Forma alternativa de la esperanza
S<-function(y){
  1-punif(y,1000,2000)
}

#Integrando (Esperanza)
integral<-integrate(f = S,lower = D,upper = U)
integral<-integral$value ; integral

#Esperanza muestral
mean(Y)


# Caso particular (sin deducible) -----------------------------------------

#Pareto1

library(actuar)
?dpareto1

shape = 2 ; min = 3

U<-3.5
D<-min #Según este modelo, D lo menos que vale es el m??nimo de la dist. :(

set.seed(27)
X<-rpareto1(n = 1000000,shape = shape,min = min)
Y<-pmax(pmin(X,U)-D,0)

#¿Media?
mean(Y)

#Empecemos:
f<-function(x) {
  dpareto1(x = x,shape = 2,min = 3)
}

#Donde nos estamos fijando
plot(f,col="red",lwd=2,
     from = 3, to = 4, ylim=c(0,0.7))
abline(v = c(D,U), col = c("green","blue"),lwd=2)

#Lo que realmente debemos ver
plot(f,col="red",lwd=2,
     from = 0, to = 4, ylim=c(0,0.7))
abline(v = c(D,U), col = c("green","blue"),lwd=2)

#Histograma de los 0 < datos < U - D
hist(Y[0<Y&Y<U-D],probability = T,col="red")

#Densidad
fY<-function(y,d=3,u=3.5,min=3,shape=2){
  ifelse(y==0,
         yes=ppareto1(d,shape = shape,min = min),                         #y=0
         no=ifelse(test = y == u-d,
                   yes=1-ppareto1(u,shape = shape,min = min),             #y=u-d
                   no = ifelse(test = 0<y & y<u-d,
                               yes=dpareto1(y+d,shape = shape,min = min), #y en (0,u-d)
                               no=0)))  
}

plot(fY,lwd=2,col="blue",from=-1,to=U-D+1,add=F)
#Histograma
hist(Y,probability = T,col="red",add=T)

#Esperanza teórica
yfYL<-function(y){
  y*fY(y)
}

#Integrando (Esperanza)
integral<-integrate(f = yfYL,lower = 0,upper = U-D)
integral<-integral$value

#Parte continua + parte discreta
integral+(U-D)*(1-ppareto1(U,shape = shape,min = min))

#Forma alternativa de la esperanza
S<-function(y,min=3,shape=2){
  1-ppareto1(y,shape = shape,min = min)
}

#Integrando (Esperanza)
integral<-integrate(f = S,lower = D,upper = U)
integral<-integral$value ; integral

#Media chida
min + mean(Y)

#Esperanza chida (alternativa)
min + integral


# Combinando todo ---------------------------------------------------------

#Exponencial
lambda<-1/100

f<-function(x){
  dexp(x = x,rate = 1/100)
}

#Donde nos estamos fijando
plot(f,col="red",lwd=2,
     from = 0, to = 200)

#Fijamos deducible y l??mite máximo
D<-25 ; U <- 175
abline(v = c(D,U), col = c("green","blue"),lwd=2)

#Tomemos un coeficiente de coaseguro
alpha<-0.25

#Fijemos una tasa de inflación
r<-0.15

#Generemos una muestra de sinietros
X<-rexp(1000000,rate = lambda)

#Calculemos los pagos
Y<-pmax(alpha*(pmin(X*(1+r),U)-D),0)

#Histograma de los 0 < datos < alpha*(U - D)
hist(Y[0<Y&Y<alpha*(U-D)],probability = T,col="red")

#Densidad
fY<-function(y,alpha=0.25,r=0.15,d=25,u=175,lambda=1/100){
  ifelse(y==0,
         yes=pexp(d/(1+r),rate = lambda),                         #y=0
         no=ifelse(test = y == alpha*(u-d),
                   yes=1-pexp(u/(1+r),rate = lambda),             #y=alpha*(u-d)
                   no = ifelse(test = 0<y & y<alpha*(u-d),
                               yes=dexp((y+d*alpha)/((1+r)*alpha),rate = lambda)/((1+r)*alpha), #y en (0,alpha*(u-d))
                               no=0)))  
}

#Histograma
hist(Y,probability = T,col="red",add=F)

#Densidad
plot(fY,lwd=2,col="blue",from=-1,to=alpha*(U-D)+1,add=T)

#¿Están cayendo en donde deber??an?

#Observados
mDeducible <- sum(Y==0)
MMaximo <- sum(Y==alpha*(U-D))
observados<-c(mDeducible,length(Y)-mDeducible-MMaximo,MMaximo)

#Esperados
esperados<-c(fY(0),1-fY(0)-fY(alpha*(U-D)),fY(alpha*(U-D)))

#Prueba Ji cuadrada
chisq.test(x = observados, p = esperados)

#Por lo tanto, NO rechazamos H0 al 95% de confianza.

#Comprobemos la esperanza

#Esperanza teórica
yfYL<-function(y){
  y*fY(y)
}

#Integrando (Esperanza)
integral<-integrate(f = yfYL,lower = 0,upper = alpha*(U-D))
integral<-integral$value

#Parte continua + parte discreta
integral+alpha*(U-D)*(1-pexp(U/(1+r),rate = lambda))

#Forma alternativa de la esperanza
S<-function(y,min=3,shape=2){
  1-pexp(y,rate = lambda)
}

#Integrando (Esperanza)
integral<-integrate(f = S,lower = D/(1+r),upper = U/(1+r))
integral<-integral$value

alpha*(1+r)*integral

#Esperanza muestral
mean(Y)


# actuar ------------------------------------------------------------------

#Fijamos deducible y l??mite máximo
d <-25 ; u <- 175

#Tomemos un coeficiente de coaseguro
alpha<-0.25

#Fijemos una tasa de inflación
r<-0.15

#Exponencial
lambda<-1/100

#Densidad construida a pata:
fY<-function(y,alpha=0.25,r=0.15,d=25,u=175,lambda=1/100){
  ifelse(y==0,
         yes=pexp(d/(1+r),rate = lambda),                         #y=0
         no=ifelse(test = y == alpha*(u-d),
                   yes=1-pexp(u/(1+r),rate = lambda),             #y=alpha*(u-d)
                   no = ifelse(test = 0<y & y<alpha*(u-d),
                               yes=dexp((y+d*alpha)/((1+r)*alpha),rate = lambda)/((1+r)*alpha), #y en (0,alpha*(u-d))
                               no=0)))  
}

library(actuar)
fyL<-coverage(pdf = dexp,cdf = pexp,
              limit=u,inflation=r,deductible=d,coinsurance=alpha,per.loss=TRUE)

#Algunos valores
fY(0)
fyL(x = 0,rate = lambda)

fY(alpha*(U-D))
fyL(alpha*(U-D),rate=lambda)

#Observados
mDeducible <- sum(Y==0)
MMaximo <- sum(Y==alpha*(U-D))
observados<-c(mDeducible,length(Y)-mDeducible-MMaximo,MMaximo)

#Esperados
esperados<-c(fyL(0,rate=lambda),
             1-fyL(0,rate=lambda)-fyL(alpha*(U-D),rate=lambda),
             fyL(alpha*(U-D),rate=lambda))

#Prueba Ji cuadrada
chisq.test(x = observados, p = esperados)

#Programando un poco...
lambda==1/100
f<-function(x,lambda=1/100){fyL(x,lambda)}

#Algunos valores
fY(0)
fyL(x = 0,rate = lambda)
f(0)

fY(alpha*(U-D))
fyL(alpha*(U-D),rate=lambda)
f(alpha*(U-D))
