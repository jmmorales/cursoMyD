#------------------------------------------------------------------------------#
#                        Maximum Likelihood Estimation                         #

# Datos de remoción de frutos en bosque continuo y fragmentos en 3 sitios pareados colectados por Mariano Rodriguez-Cabal
http://modelosydatos.googlepages.com/Rodriguez-Cabal_07.pdf

# leer los datos
quintral <- read.table("http://modelosydatos.googlepages.com/quintral.txt", header = T)

# Mirar los datos 
quintral
View(quintral) #otra manera de ver los datos como hoja de cálculo.
str(quintral)  #para ver la estructura de los datos, qué variables hay y que tipo de variables son.  

# para que las variables estén disponibles (los puristas de R objetarían...) 
attach(quintral)

# log likelihood para una sola observación (la primera del set de datos) asumiendo una probabilidad de remoción de p = 0.5
dbinom(Removidos[1],size=Frutos[1],prob=.5,log=TRUE)

# para un rango de valores 
op<-par(mfrow=c(1,2),lwd=2, las=1, bty="l")     # son parametros de graficos
theta <- seq(0,1,length=1000)                   # creo un vector de mil valores entre 0 y 1
plot(theta,dbinom(Removidos[1],size=Frutos[1],prob=theta,log=F), type = "l", ylab="L")    # grafico del log likelihood para distintas probabilidades
plot(theta,-dbinom(Removidos[1],size=Frutos[1],prob=theta,log=T), type = "l", ylab="-logL") # grafico del -log likelihood para distintas probabilidades
par(op)

# Todas las observaciones con probabilidad de remoción p = 0.5
-sum(dbinom(Removidos,size=Frutos,prob=.5,log=TRUE))   #la suma del likelihood de todas las observaciones 

# podemos ver que pasa para un rango de valores de p:
p <- seq(0, 1, length=11) # vector de 11 valores que van de 0 a 1
nLL <- array(0,length(p)) # vector de 11 valores iguales a 0
for (i in 1:length(p)) {
   nLL[i] <- -sum(dbinom(Removidos,size=Frutos,prob=p[i],log=TRUE))
}
# graficamos
plot(p,nLL,type="o")

# Probando con una mejor resolución 
p <- seq(0, 1, length=1000) # vector de 1000 valores que van de 0 a 1
nLL <- array(0,length(p))
for (i in 1:length(p)) {
   nLL[i] <- -sum(dbinom(Removidos,size=Frutos,prob=p[i],log=TRUE))
}

# graficamos...
plot(p,nLL,type="l",lwd = 2, ylim = c(200,2000),ylab="Negative Log-Likelihood", xlab="Fruit Removal Probability (p)")

# Veamos cómo cambia el Likelihood con el número de observaciones
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

# ¿Como comparar este valor con una estimación más directa? 
sum(Removidos)/sum(Frutos)

# maximum log-likelihood
-min(nLL)

# maximum likelihood
exp(-min(nLL))

################################################################################
#        Búsquedas más eficientes usando "optim" y "mle2" de Bolker            #

# Primero tenemos que definir una función para nuestro negative log likelihood

binomNLL1 <- function(p,k,N){
  -sum(dbinom(k, prob = p, size =N, log = TRUE))      #k son los removidos, N= los frutos
}

# Ahora usamos "optim" especificando los parámetros iniciales, los datos y el método de búsqueda
O1 <- optim(fn = binomNLL1, par = c(p = 0.5), N = Frutos, k = Removidos, method = "BFGS")
# Hay algunos "warnings" de NAs producidos en dbinom pero nada de qué preocuparse

# Veamos qué paso
O1

# O1 guardó: 
# en $par, el valor óptimo del parámetro  
# en $value, el valor del log likelihood para $par 
# en $counts, hay algunas cosas crípticas acerca de cómo se desarrolló la búsqueda 
# Hay que asegurarse de que el algoritmo haya convergido ( $convergence = 0 )

#------------

# "optim" en R es una función general de optimización, pero para MLE podemos usar mle2 desarrollado por Ben Bolker

library(bbmle) # cargar el paquete de MLE 
# Chequear
?mle2

m1 <- mle2(minuslogl = binomNLL1, start = list(p = 0.5), data = list(N = Frutos, k = Removidos))
# en mle2 la función de negative log likelihood se llama "minuslogl" en vez de "fn" (en optim)

# Comparemos m1 con O1
m1
# Devuelve la función usada, valores de los coeficientes y el log likelihood

# Podemos ver más detalles con: 
summary(m1)

# Ahora tenemos errores standares y valores de p para los coeficientes y la devianza (-2 log likelihood)
# Después veremos como usar éstas y otras salidas de mle2 para obtener intervalos de confianza y hacer comparaciones entre modelos 

###################################

# Comparación (cruda) con los datos
k <- Removidos
hist(k, breaks = 40, freq=F, ylim = c(0,0.15))
points(0:50,dbinom(0:50,size=35,prob=m1@coef),pch=16,col="darkgray")

# Sin embargo, el número de frutos disponibles es variable y para una mejor comparación podemos usar un bootstrap (remuestreo)
res <- matrix(0,length(Frutos),1000)
for (i in 1:1000){
n <- sample(Frutos,length(Frutos),replace=TRUE)
res[,i] <- rbinom(length(Frutos),n,m1@coef)}
hist(k, breaks = 40, freq=F, ylim = c(0,0.15))
lines(density(res))

# Una exageración...
 hist(k, breaks = 40, freq=F, ylim = c(0,0.15))
 for(i in 1:1000){lines(density(res[,i]))}
 
# ¿Qué puede decir sobre el ajuste del modelo a los datos?
# ¿Cómo cambia la remoción de frutos por sitio y tipo de bosque?


########### Algunos trucos de R 
# Podemos evitar muchos loops usando "apply"

# Primero defina una función de p
binom.nll <- function(p, x = Removidos,size = Frutos){
 -sum(dbinom(x,size,prob=p,log=TRUE))}

# Ahora aplicamos la función a un vector (por lo tanto "sapply") de valores de probabilidad p
p <- seq(0, 1, length=100)
nLL <- sapply(p, binom.nll)
###########
#Otro atajo: para funciones estándar se puede escribir directamente la función en mle2
m2 <- mle2(k~dbinom(prob=p, size=Frutos), start=list(p=0.5))