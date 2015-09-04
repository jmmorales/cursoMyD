# Criterios de Información aplicados a modelos de deposicion de granos de polen en plantas de pomelo 
# Datos de Natacha Chacoff

library(bbmle)

apis<-read.table("http://modelosydatos.googlepages.com/polen_Apis.csv",header=T,sep=",")
View(apis) #numero de visitas (visitas) y granos (número de granos de polen que depositan en estigma de la flor)
attach(apis)

# Graficar los datos (plot) 
plot(visitas,granos,xlab="Number of Visits", ylab="Deposited Polen Grains", pch=19)
title(main= expression(paste("Efficiency of ", italic(Apis~mellifera))))

# Probemos diferentes modelos para descibir esta relación 

# 1 ---------- Modelo lineal con distribución de Poisson 

# definir la función de neg log L 
fnLIN<-function (p) {
 y0 <- p[1]
 a <- p[2]
 lambda <- y0 + a * visitas
 -sum(dpois(granos, lambda, log=TRUE))
}

# Nombrar los parámetros 
parnames(fnLIN) <- c("y0", "a")

# Ajustamos usando mle2 de "bbmle"
mLIN <- mle2(fnLIN, start = c(y0 = 10, a = 3), data = list(visitas = visitas, granos = granos))

# Vemos los resultados
summary(mLIN)

# -- Perfiles y CI 
# calculamos los perfiles y los intervalos de confianza 
p_LIN <- profile(mLIN)   # perfil

# plot perfiles 
# por default el plot esta en sqrt(deviance) entonces que tenga forma de una V es bueno para "lindos" perfiles 
op = par(mfrow = c(1, 2))
plot(p_LIN, plot.confstr = TRUE)
par(op)

# los CI pueden ser calculados de diferentes maneras (todos dan el mismo resultado dado que la superficie del likelihood se comporta bien)
ci_LIN = confint(p_LIN) # default method; invert a spline fitted to the profile
ci_LINq = confint(p_LIN, method="quad") # quadratic approximation at the MLE
ci_LINu = confint(p_LIN, method="uniroot") # root finding of the points at which the profile crosses critical values

# Chequear la matriz de varcovar y las correlaciones
vcov(mLIN) # var-covar aproximado para todos los parámetros 
solve(mLIN@details$hessian) # la matriz de vcov es la inversa de la hessian calculada en el MLE
cov2cor(vcov(mLIN)) # matriz de correlación entre los parámetros 


# 2 ------------- Modelo de Saturación ----------------------------------------- 

# neg log L
fnSAT <- function (p) {
 y0 <- p[1]
 a <- p[2]
 b <- p[3]
 lambda <- y0 + a*(1-exp(-b*visitas))
 -sum(dpois(granos, lambda, log=TRUE))
}

parnames(fnSAT) <- c("y0", "a", "b")
mSAT <- mle2(fnSAT, start = c(y0 = 10, a = 41, b = 0.16), data = list(visitas = visitas, granos = granos))

p_SAT = profile(mSAT)
ci_SAT = confint(mSAT)

op = par(mfrow = c(1, 3))
plot(p_SAT, plot.confstr = TRUE)
par(op)
 