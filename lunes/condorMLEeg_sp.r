# Aves carro�eras y rutas 
# Lambertucci y colaboradores pusieron cad�veres de ovejas a distintas distancias de rutas y observaron la actividad de aves carro�eras
# Estudian la detecci�n y el uso de carro�a en relaci�n a la distancia a las rutas por c�ndores y otras aves rapaces
#http://modelosydatos.googlepages.com/Lambertucci_09.pdf

library(bbmle) # vamos a usar funciones de este paquete

# Datos Condor 
#condor <- read.table("C:\\cursulo\\condor.txt", header=T)
condor <- read.table("http://modelosydatos.googlepages.com/condor.txt", header=T)
attach(condor)
head(condor) # revisamos los datos
View(condor) # tambi�n podemos usar 

# Necesitamos escribir una funci�n para la regresi�n binomial "inflada" con ceros
# asumimos Poisson con exeso de ceros para el n�mero total de aves (volando y en tierra) detectadas cerca de una carro�a (ver nllp abajo)
# asumimos Binomial para la fracci�n de aves que bajan hasta la carro�a (nllb abajo)

# usamos la funci�n "plogis" para pasar de valores + - a valores entre cero y uno
# para ver como funciona este truco hacemos:
tmp <- seq(from=-5, to=5, by=0.01) # secuencia de valores desde -5 hasta 5 con un incremento de 0.01
plot(tmp,plogis(tmp),type="l", lwd=2)

# ahora s� escribimos la funci�n                                                                                                                 
zibinregc = function(p) {
  av = p[1]
  ad = p[2]
  bd = p[3]
  an = p[4]
  bn = p[5]
  
  v = plogis(av)  #  probabilidad de detecci�n 
  pr = plogis(ad + bd * condor$Dist/1000) # probabilidad de bajar a comer
  lambda = exp(an + bn * condor$Dist/1000) # par�metro de la Poisson en funci�n de la distancia a la ruta
  nllp = -sum(log(ifelse(k+f==0,(1-v) + v*dpois(0,lambda), v*dpois(k+f,lambda))))  # Poisson likelihood inflada en ceros
  nllb = -sum(dbinom(k,k+f,prob=pr,log=T))   # likelihood Binomial
  return(nllp+nllb) # el resultado es la suma de las dos - log likelihoods
}

parnames(zibinregc) = c("av","ad","bd","an","bn")  # definir los nombres de los par�metros para que los use mle2

#Ajustamos la funcion para los c�ndores
fit.C <- mle2(minuslogl=zibinregc, start=c(av=1, ad=2, bd=0, an=2, bn=0), data=list(k=condor$Condor,f=condor$CoFl, Dist=condor$Dist))  

summary(fit.C) # ver los resultados

par_cof <- fit.C@coef # extraer los coeficientes (MLE de los par�metros)
d = 0:6000   # variable para graficar distancia

# gr�fico de los ajustes
op=par(mfrow=c(2,1), lwd=2)                             
plot(condor$Dist,(condor$Condor+condor$CoFl), ylab = "# de C�ndores", xlab="Distancia a la Ruta")                                    
lines(d,exp(par_cof[4]+par_cof[5]*d/1000))
plot(condor$Dist, condor$Condor/(condor$Condor+condor$CoFl),ylab="Proporci�n de C�ndores comiendo", xlab="Distancia a la Ruta")
lines(d,plogis(par_cof[2]+par_cof[3]*d/1000))
par(op)             

# Lo mismo para Jote negro 
fit.J <- mle2(zibinregc, start = c(av=1, ad=2, bd=0, an=2, bn=0), data = list(k=condor$JoteN,f=condor$JNFI, Dist=condor$Dist))  

summary(fit.J)
par_cof <- fit.J@coef

op=par(mfrow=c(2,1), lwd=2)                             
plot(condor$Dist,(condor[,11]+condor[,12]), ylab = "# de Jote Negro", xlab="Distancia a la Ruta")                                    
lines(d,exp(par_cof[4]+par_cof[5]*d/1000))
plot(condor$Dist, condor[,11]/(condor[,11]+condor[,12]),ylab="Proporci�n de Jote Negro comiendo", xlab="Distancia a la Ruta")
lines(d,plogis(par_cof[2]+par_cof[3]*d/1000))
par(op)        
                          
