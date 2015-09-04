#------------------------------------------------------------------------------#
#                        Maximum Likelihood Estimation                         #
# Veremos un ejemplo un poco más "sofisticado" de MLE

# Estos son los datos colectados por Guillermo Amico (http://ecotono.crub.uncoma.edu.ar/amico.htm) sobre el tiempo que tardan las semillas de Quintral en pasar por el tubo digestivo del monito del monte (Dromiciops gliroides) 
# Los animales fueron observados cada 10 minutos y se registró el número de frutos consumidos y el número de semillas defecadas
# Queremos ajustar una distribución de probabilidad para el tiempo de paso por el tubo digestivo 

gutpt <- read.table("http://modelosydatos.googlepages.com/gut.txt", header = T)
attach(gutpt)
View(gutpt) # datos con time, consumed (número de semillas consumidas), defecated(número de semillas defecadas), id (identifica a los diferentes individuos), replica (al mismo individuo se lo sometió al experimento mas de una vez: entre 3 y 5 veces) 


# preliminares (Es necesario que queden semillas en el intestino, es decir que no defequen todo lo que se comieron para que haya un tiempo que de pasaje que estimar)
left = consumed
for (i in 1:max(id)){
for (j in 1:max(replica)){
obs = which(id==i & replica==j)
if (length(obs)>1){
for(k in 2:length(obs)){
left[obs[k]] = left[obs[k-1]] + consumed[obs[k]] - defecated[obs[k-1]]
}}}}

# La Negative log likelihood usando una Gamma acumulada 
nll.CDFgamma<-function(p,left,defecated,time)
{
  scale<-p[1]
  shape<-p[2]
  CDF.before<-pgamma(time[-length(time)],shape=shape,scale=scale)
  CDF.after<-pgamma(time[-1],shape=shape,scale=scale)
  prob<-(CDF.after-CDF.before)/(1-CDF.before)
  nll<-(-sum(dbinom(x=defecated[-1],size=left[-1],prob=prob,log=T)))
  return(nll)}
  

nllGammafit <- function(p,id,replica,left,defecated,time){
tmp=0
for (i in 1:max(id)){
for (j in 1:max(replica)){
obs = which(id==i & replica==j)
if (length(obs)>1){
 tmp = tmp + nll.CDFgamma(p,left[obs],defecated[obs],time[obs])
}}} 
return(tmp)}  

O1 = optim(fn = nllGammafit, p = c(100,1),left=left,defecated=defecated,id=id,replica=replica,time=time)

t = 0:120
plot(t,dgamma(t,shape=O1$par[2],scale=O1$par[1]),type="l", ylab="probability density")

library(bbmle)
parnames(nllGammafit) = c("scale", "shape")
m1 = mle2(minuslogl = nllGammafit, start = c(scale=170,shape=2), data = list(id=id,replica=replica,left=left,defecated=defecated,time=time))

plot(t,dgamma(t,shape=O1$par[2],scale=O1$par[1]),type="l", ylab="probability density")



nll.CDFgammaM<-function(p,consumed,defecated,time)
{
  scale <- p[1]
  shape <- p[2]  
  # Establecemos tiempo para consumos diferentes 
  nll=0
  t <- which(consumed > 0)
  if(length(t)>1){
  tt = array(data=0,dim=c(length(time),length(t)))
  tt[,1] = time
  for(i in 2:length(t)){
  tmp = numeric(length(time))
  tmp[t[i]:length(time)] = time[t[i]:length(time)] - time[t[i]] + (time[t[i]]-time[t[i]-1])/2 
  tt[,i] = tmp
  }  
  CDF.before = array(data=NA,dim=c(length(time)-1,length(t)))
  CDF.after =  CDF.before
  prob = CDF.before
  for (j in 1:length(t)){
    CDF.before[,j] <- pgamma(tt[-length(time),j],shape=shape,scale=scale)
    CDF.after[,j]  <- pgamma(tt[-1,j],shape=shape,scale=scale)
    prob[,j] <- (CDF.after[,j]-CDF.before[,j])/(1-CDF.before[,j])
    }
    # asigamos semillas para diferentes ingestiones
     d = round(defecated[-length(defecated)]*(prob/rowSums(prob)))
     f=t(d)/(colSums(d))
     d=round(consumed[t]*f)
     d=t(d)
     l = array(data=0,dim(d)) # para semillas dejadas en el intestino para cada evento de consumo
     nll = 0
     for (k in 1:length(t)){
     l[t[k],k] = consumed[t[k]]
     l[,k]=consumed[t[k]]-cumsum(d[,k])
     l[which(l<0)]=0
     d[which(l==0)]=0
     d[which(d>l)]=l[which(d>l)]
     nll = nll + (-sum(dbinom(x=d[,k],size=l[,k],prob=prob[,k],log=T)))
     }}
     return(nll)}
     



nllGammafitM <- function(p,id,replica,left,consumed,defecated,time){
tmp=0
for (i in 1:max(id)){
for (j in 1:max(replica)){
obs = which(id==i & replica==j)
if (length(obs)>1){
t <- which(consumed[obs] > 0)
if(length(t)>1){
 tmp = tmp + nll.CDFgammaM(p,consumed=consumed[obs],defecated=defecated[obs],time=time[obs])
}
#else { tmp = tmp + nll.CDFgamma(p,left=left[obs],defecated=defecated[obs],time=time[obs])}
}}} 
return(tmp)} 

     
OM = optim(fn = nllGammafitM, p = c(13,2.7),id=id,replica=replica,left=left,consumed = consumed,defecated=defecated,time=time)

 plot(t,dgamma(t,shape=OM$par[2],scale=OM$par[1]),type="l", ylab="probability density")

#   i = 1
#   j = 1
#   obs = which(id==i & replica==j)
#  tim = time[obs]
#  scale<-p[1]
#  shape<-p[2]
#  CDF.before<-pgamma(tim[-length(tim)],shape=shape,scale=scale)
#  CDF.after<-pgamma(tim[-1],shape=shape,scale=scale)
#  prob<-(CDF.after-CDF.before)/(1-CDF.before)

