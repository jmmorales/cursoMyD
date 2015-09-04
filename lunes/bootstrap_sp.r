# Bootstrap 

# objetivo: revisar los conceptos básicos de remuestreo de datos

# ejemplo básico
# Estimación de la media y CI de datos que tienen una distribucion Gamma 

datos = rgamma(50,shape = 0.8,rate=1)   # simulo 50 datos
n = length(datos)                       # número de observaciones
B = 2000                                # número de muestras de bootstrap 
theta_b = numeric(B)                    # vector donde se van a guardar los resultados 

for (i in 1:B){                          
    y <- sample(datos,size=n,replace=T) # bootstrap sample
    theta_b[i] <- mean(y)               # en gral hacemos algo más interesante aqui
}

# tipear ?sample para ver lo que hace esta función

# Resumen de los resultados 
mb = mean(theta_b)
se <- sqrt(sum((theta_b-mb)^2)/ (B-1))

#se <- sd(theta_b) #otra manera de calcular el error standart

st = sort(theta_b)        # ordena de menor a mayor los valores 
ci = c(st[round(B*0.05/2)], st[B - round(B*0.05/2)])  # intervalo de confianza

# también se puede hacer con la función "quantile"
ci = quantile(theta_b,probs=c(0.025,0.975))

#-------------------------------------------------------------------------------
#                      Poner a prueba un "Correlated Random Walk"

setwd("C:/Dropbox/modelosydatos2012/datos") # cambiar el directorio a donde tenemos los datos

# cargar los datos de localizaciones de elk reintroducidos
elk <- read.table("elkGPS1.txt", header = T)

# ver trayectoria de movimiento
plot(elk$Easting, elk$Northing, type="o", asp=1)

# calcular y graficar el desplazamiento al cuadrado
R2 <- (elk$Easting/1000 - elk$Easting[1]/1000)^2 + (elk$Northing/1000 - elk$Northing[1]/1000)^2
plot(R2, type="l")
# les parece que esto podría haber sido generado por un CRW?
# para responder a esta pregunta, vamos a obtener los percentiles bajo el supuesto de que
# el movimiento es de hecho un CRW

# definimos los "pasos" y "giros" que ya están calculados en el set de datos
steps <- elk$km_day
turns <- elk$turns

sts <- steps[2:(length(steps)-1)]
trns <- turns[2:(length(steps)-1)]


# simulamos una cantidad de CRWs
B <- 1000                                 
nobs <- length(sts)
R2B <- matrix(NA,B,nobs)                  

for(j in 1:B){
  x <- numeric(nobs)
  y <- numeric(nobs)
  compass <- numeric(nobs)

  # simulamos CRW
  for(i in 2:length(sts)){
    compass[i] <- compass[i-1] + sample(trns, size=1, replace=T)
    steplength <- sample(sts, size=1)
    x[i] <- x[i-1] + steplength*cos(compass[i])
    y[i] <- y[i-1] + steplength*sin(compass[i])
  }                                            
  R2B[j,] <- (x-x[1])^2 + (y-y[1])^2 
}  

# calcular percentiles
per <- matrix(NA, nobs,2)
for(i in 1:nobs){
per[i,] <- quantile(R2B[,i], probs=c(0.025,0.975))
}  

plot(R2, type="o", ylim=c(0, max(R2B)), pch=16, xlab="Pasos", ylab="Desplazamiento al Cuadrado")
for(i in 1:B) lines(R2B[i,], lty=3, col="lightgray")  
lines(per[,1], lwd=2)
lines(per[,2], lwd=2)
lines(R2, type="o", pch=16)

# cómo se interpreta este gráfico?