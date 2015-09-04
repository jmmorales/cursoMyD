#------------------------------------------------------------------------------#
#                        Maximum Likelihood Estimation                         #

# Datos de remoci�n de frutos en bosque continuo y fragmentos en 3 sitios pareados colectados por Mariano Rodriguez-Cabal
http://modelosydatos.googlepages.com/Rodriguez-Cabal_07.pdf

# leer los datos
quintral <- read.table("http://modelosydatos.googlepages.com/quintral.txt", header = T)

# Mirar los datos 
quintral
View(quintral) #otra manera de ver los datos como hoja de c�lculo.
str(quintral)  #para ver la estructura de los datos, qu� variables hay y que tipo de variables son.  

# para que las variables est�n disponibles (los puristas de R objetar�an...) 
attach(quintral)

# log likelihood para una sola observaci�n (la primera del set de datos) asumiendo una probabilidad de remoci�n de p = 0.5
dbinom(Removidos[1],size=Frutos[1],prob=.5,log=TRUE)

# para un rango de valores 
op<-par(mfrow=c(1,2),lwd=2, las=1, bty="l")     # son parametros de graficos
theta <- seq(0,1,length=1000)                   # creo un vector de mil valores entre 0 y 1
plot(theta,dbinom(Removidos[1],size=Frutos[1],prob=theta,log=F), type = "l", ylab="L")    # grafico del log likelihood para distintas probabilidades
plot(theta,-dbinom(Removidos[1],size=Frutos[1],prob=theta,log=T), type = "l", ylab="-logL") # grafico del -log likelihood para distintas probabilidades
par(op)

# Todas las observaciones con probabilidad de remoci�n p = 0.5
-sum(dbinom(Removidos,size=Frutos,prob=.5,log=TRUE))   #la suma del likelihood de todas las observaciones 

# podemos ver que pasa para un rango de valores de p:
p <- seq(0, 1, length=11) # vector de 11 valores que van de 0 a 1
nLL <- array(0,length(p)) # vector de 11 valores iguales a 0
for (i in 1:length(p)) {
   nLL[i] <- -sum(dbinom(Removidos,size=Frutos,prob=p[i],log=TRUE))
}
# graficamos
plot(p,nLL,type="o")

# Probando con una mejor resoluci�n 
p <- seq(0, 1, length=1000) # vector de 1000 valores que van de 0 a 1
nLL <- array(0,length(p))
for (i in 1:length(p)) {
   nLL[i] <- -sum(dbinom(Removidos,size=Frutos,prob=p[i],log=TRUE))
}

# graficamos...
plot(p,nLL,type="l",lwd = 2, ylim = c(200,2000),ylab="Negative Log-Likelihood", xlab="Fruit Removal Probability (p)")

# Veamos c�mo cambia el Likelihood con el n�mero de observaciones
op<-par(mfrow=c(2,3),lwd=2, las=1, bty="l")
theta <- seq(0,1,length=1000)
plot(theta,dbinom(Removidos[1],size=Frutos[1],prob=theta,log=F), type = "l", ylab="L", main="n = 1")

nLL2 <- array(0,length(theta))
for (i in 1:length(theta)) {
   nLL2[i] <- -sum(dbinom(Removidos[1:2],size=Frutos[1:2],prob=theta[i],log=TRUE))
}
plot(theta,exp(-nLL2), type = "l", ylab="L", main="n=2")

nLL10 <- array(0,length(theta))
for (i in 1:length(theta)) {
   nLL10[i] <- -sum(dbinom(Removidos[1:10],size=Frutos[1:10],prob=theta[i],log=TRUE))
}
plot(theta,exp(-nLL10), type = "l", ylab = "L", main="n=10")
plot(theta,-dbinom(Removidos[1],size=Frutos[1],prob=theta,log=T), type = "l", ylab="-logL", main="n=1")
plot(theta,-dbinom(Removidos[1],size=Frutos[1],prob=theta,log=T), type = "l", ylab="-logL", main="n=2")
lines(theta,nLL2, type = "l", lty=5)
plot(theta,-dbinom(Removidos[1],size=Frutos[1],prob=theta,log=T), type = "l", ylab="-logL", main="n=10")
lines(theta,nLL10, type = "l", lty=5)
par(op)
#-------------------------------------------------------------------------------

# Para encontrar el valor de p que maximiza el likelihood
p[which(nLL==min(nLL))]

# �Como comparar este valor con una estimaci�n m�s directa? 
sum(Removidos)/sum(Frutos)

# maximum log-likelihood
-min(nLL)

# maximum likelihood
exp(-min(nLL))

################################################################################
#        B�squedas m�s eficientes usando "optim" y "mle2" de Bolker            #

# Primero tenemos que definir una funci�n para nuestro negative log likelihood

binomNLL1 <- function(p,k,N){
  -sum(dbinom(k, prob = p, size =N, log = TRUE))      #k son los removidos, N= los frutos
}

# Ahora usamos "optim" especificando los par�metros iniciales, los datos y el m�todo de b�squeda
O1 <- optim(fn = binomNLL1, par = c(p = 0.5), N = Frutos, k = Removidos, method = "BFGS")
# Hay algunos "warnings" de NAs producidos en dbinom pero nada de qu� preocuparse

# Veamos qu� paso
O1

# O1 guard�: 
# en $par, el valor �ptimo del par�metro  
# en $value, el valor del log likelihood para $par 
# en $counts, hay algunas cosas cr�pticas acerca de c�mo se desarroll� la b�squeda 
# Hay que asegurarse de que el algoritmo haya convergido ( $convergence = 0 )

#------------

# "optim" en R es una funci�n general de optimizaci�n, pero para MLE podemos usar mle2 desarrollado por Ben Bolker

library(bbmle) # cargar el paquete de MLE 
# Chequear
?mle2

m1 <- mle2(minuslogl = binomNLL1, start = list(p = 0.5), data = list(N = Frutos, k = Removidos))
# en mle2 la funci�n de negative log likelihood se llama "minuslogl" en vez de "fn" (en optim)

# Comparemos m1 con O1
m1
# Devuelve la funci�n usada, valores de los coeficientes y el log likelihood

# Podemos ver m�s detalles con: 
summary(m1)

# Ahora tenemos errores standares y valores de p para los coeficientes y la devianza (-2 log likelihood)
# Despu�s veremos como usar �stas y otras salidas de mle2 para obtener intervalos de confianza y hacer comparaciones entre modelos 

###################################

# Comparaci�n (cruda) con los datos
k <- Removidos
hist(k, breaks = 40, freq=F, ylim = c(0,0.15))
points(0:50,dbinom(0:50,size=35,prob=m1@coef),pch=16,col="darkgray")

# Sin embargo, el n�mero de frutos disponibles es variable y para una mejor comparaci�n podemos usar un bootstrap (remuestreo)
res <- matrix(0,length(Frutos),1000)
for (i in 1:1000){
n <- sample(Frutos,length(Frutos),replace=TRUE)
res[,i] <- rbinom(length(Frutos),n,m1@coef)}
hist(k, breaks = 40, freq=F, ylim = c(0,0.15))
lines(density(res))

# Una exageraci�n...
 hist(k, breaks = 40, freq=F, ylim = c(0,0.15))
 for(i in 1:1000){lines(density(res[,i]))}
 
# �Qu� puede decir sobre el ajuste del modelo a los datos?
# �C�mo cambia la remoci�n de frutos por sitio y tipo de bosque?


########### Algunos trucos de R 
# Podemos evitar muchos loops usando "apply"

# Primero defina una funci�n de p
binom.nll <- function(p, x = Removidos,size = Frutos){
 -sum(dbinom(x,size,prob=p,log=TRUE))}

# Ahora aplicamos la funci�n a un vector (por lo tanto "sapply") de valores de probabilidad p
p <- seq(0, 1, length=100)
nLL <- sapply(p, binom.nll)
###########
#Otro atajo: para funciones est�ndar se puede escribir directamente la funci�n en mle2
m2 <- mle2(k~dbinom(prob=p, size=Frutos), start=list(p=0.5))