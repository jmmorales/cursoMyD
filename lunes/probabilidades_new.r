
#distribuciones discretas
#Poisson, Binomial, Binomial negativa, Geometrica

#Ejemplo de Binomial negativa
#Numero de renovales en una parcela

dnbinom(x, size, prob, mu, log = FALSE)  #densidad de probabilidad
pnbinom(q, size, prob, mu, lower.tail = TRUE, log.p = FALSE) #CDF el valor de la prob acumulada hasta un valor determinado. Ej x> a un valor determinado.
qnbinom(p, size, prob, mu, lower.tail = TRUE, log.p = FALSE)  #es la inversa de la CDF. Devuelve el valor (o los valores) de la variable que esta asocidado con probabilidades acumuladas. Estos valores son los cuantiles. límites de confianza superior e inferior del 95% de la distribución
rnbinom(n, size, prob, mu)

#[p] (0<p<1) 
#[mu] (real y positivo) número esperado de renovales
#[size] mide sobredispersion (ó agregación, ó heterogeneidad)
#[q]  vector de cuantiles


set.seed(1001)
z <- rnbinom(1000,mu=10,size=0.9)

#Para chequear los primeros valores
head(z)
maxz <- max(z)
#Como no tiene límite superior, grafico sólo hasta el valor máximo del vector

#Si quiero ver estos resultados: 
f <- factor(z,levels=0:maxz)
plot(f)

#Pero para ver las probabilidades en vez de la frecuencia (frec/N)
obsprobs <- table(f)/1000
plot(obsprobs)

#Para comparar con valores teóricos de una distribución binomial negativa (ajuste)
tvals <- dnbinom(0:maxz,size=0.9,mu=10)
points(0:maxz,tvals)     #veo los desvios

#¿Cuál es la probabilidad de que x>30?
pnbinom(30,size=0.9,mu=10,lower.tail=FALSE)     #R por defecto da la probabilidad de que x<= a un valor particular, por eso se usa el lower.tail=FALSE

#¿Cuál es el percentil 95 de esta distribución?
qnbinom(0.95,size=0.9,mu=10)

#Para tener los límites de confianza superior e inferior del 95% de la distribución
qnbinom(c(0.025,0.975) ,size=0.9,mu=10)

#Distribuciones Continuas
#Normal, Lognormal, Beta, Gamma, Exponencial
#Lognormal
z <- rlnorm(1000, meanlog = 2, sdlog = 1)

#Gráficamente
hist(z, breaks = 100, freq = FALSE)
lines(density(z, from = 0), lwd = 2)


#Agregar los valores teóricos de la distribución y graficarlos
curve(dlnorm(x, meanlog = 2, sdlog = 1), add = TRUE, lwd = 2, from = 0, col = "darkgray")

#La probabilidad de que x > 30 y los limites de confianza del 95%
plnorm(30, meanlog = 2, sdlog = 1, lower.tail = FALSE)

#Para tener los límites de confianza superior e inferior del 95% de la distribución
qlnorm(c(0.025, 0.975), meanlog = 2, sdlog = 1)

#A veces es mas simple trabajar con distribuciones normales. Entonces se puede tomar el logaritmo de la distribución log-normal y luego compararla con la normal:
hist(log(z), freq = FALSE, breaks = 100)
curve(dnorm(x, mean = meanlog, sd = sdlog), add = TRUE, lwd = 2)

#Distribuciones mixtas
u1 <- runif(1000)
z <- ifelse(u1 < 0.3, rnorm(1000, mean = 1, sd = 2), rnorm(1000, mean = 5, sd = 1)) 
hist(z, breaks = 100, freq = FALSE)

#Luego se puede superponer una densidad de probabilidad teórica en el histograma
 curve(0.3 * dnorm(x, mean = 1, sd = 2) + 0.7 * dnorm(x, mean = 5, sd = 1), add = TRUE, lwd = 2)


#Distribucion Beta
#parametros 1 y 5 a<<b
x<-seq(from = 0, to = 1, by = 0.01)
curve(dbeta(x,1,5), lwd = 2)
#parametros 1 y 3    a<b
curve(dbeta(x,1,3), add=T, lwd = 2, col="blue")

#parametros 3 y 3    a=b
curve(dbeta(x,3,3), add=T, lwd = 2, col="red")

#parametros 3 y 2    a>b
curve(dbeta(x,3,2), add=T, lwd = 2, col="green")

#parametros 0.5 y 0.7    a>b
curve(dbeta(x,0.5,0.7), add=T, lwd = 2, col="violet")
#Como estimar a partir de los parámetros: la media, la moda y la varianza
a=3
b=2
media<-a/(a+b)
media
varianza<-(a*b)/(a+b)^2*(a+b+1)
varianza
moda<-(a-1)/(a+b-2)
moda

# Reclutamiento de una especie en parcelas. Cuento cuantos individuos encuentro. 
#Para la Poisson, calculo la media a partir de los datos.
#Para la Normal calculo la media (estimador de mu) y el desvío (estimador de la varianza)
b<-c(3, 5, 3, 2, 7, 7, 3, 3, 5 ,5 ,5, 6, 6, 8, 6, 4, 5, 7, 6, 5, 4, 2, 9, 4, 5, 6, 5, 5, 4, 8, 7, 2, 4, 3, 5, 4, 5, 8, 5, 5, 8, 4, 7, 4, 9, 3, 1, 6, 5, 6)
hist(b, prob=T)
mean(b)
sd(b)

#Para ajustar sobre los datos distribuciones teóricas con los parámetros calculados. 
#Necesito crear un vector con datos posibles (como los originales)
x<-(0:10)
lines(x,dpois(x,mean(b)))
lines(x,dnorm(x,mean(b),sd(b)),col=4)
