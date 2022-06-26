install.packages("actuar")
library(ADGofTest)
library("stats")
library(MASS)
library("stats4", lib.loc="C:/Program Files/R/R-2.15.3/library")
library(compiler)
library("poweRlaw")
library("VGAM")




enableJIT(3)

#################################################
#### Funciones para generar GI0 con L=1  y E(Z)=1
#################################################

pgi0L1<-function(x,alfa,gama)  1-(1+x/gama)^alfa

#######################################
##1-como cociente de gamas
#######################################

generoGI.congama<-function(alfa,gama,n,L)
{
  XG<-rgamma(n,L,L)
  YG<-rgamma(n,-alfa,gama)
  datos<-XG/YG
  return(datos)
}



#######################################
## 2- como cociente de chisq
#######################################

generoGI.conchi<-function(alfa,gama,n)
{
  XC<-rchisq(n,2)
  YC<-rchisq(n,-2*alfa)
  datos<-gama*XC/YC
  return(datos)
}


#######################################
## 3- usando F de Fisher Snedecor
#######################################

generoGI.conf<-function(alfa,gama,n,L)
{
  XF<-rf(n,2,-2*alfa)
  datos<-(-1)*gama*XF/alfa
  return(datos)
}

#######################################
## 4- con pareto
#######################################

generoGI.conpareto<-function(n,alfa,gama)
{
 datos<-rpareto(n,(-1)*alfa,gama)
 return(datos)
}



#################################################################
#n=10^5, alfa=-1.5  gama=0.5 genero 100 muestras 
# y las guardo en una matriz cada fila es una muestra
#############################################################
##para cociente de gamas
#################################

setwd("C:/Users/debie/Dropbox/ILACSC/Colas/Bases")
#setwd("C:/Users/Julia/Dropbox/Procesamiento de imagenes/SEEMI_2015_JuJuDeb/Colas/Bases")

getwd()



fil=1000
col=10000

set.seed(2345678901)
#####################################################
####funciones para la generacion de las bases
#####################################################

gen_mues_gam=function(nfil,ncol,alfa){
base=matrix(0,nfil,ncol)
for(i in 1:nfil){
  base[i,]=generoGI.congama(alfa, -1-alfa,ncol,1)
}

nombre<-paste( "C:/Users/Usuario/Dropbox/ILACSC/Colas/Bases/base10000","alfa",floor(alfa),"gengama",
              sep = "")

write.csv(base, file = nombre)
}

gen_mues_chi=function(nfil,ncol,alfa){
  base=matrix(0,nfil,ncol)
  for(i in 1:nfil){
    base[i,]=generoGI.conchi(alfa, -1-alfa,ncol)
  }
  nombre<-paste( "C:/Users/Usuario/Dropbox/ILACSC/Colas/Bases/base10000","alfa",floor(alfa),"genchi",
                 sep = "")
  
  write.csv(base, file = nombre)
}

gen_mues_f=function(nfil,ncol,alfa){
  base=matrix(0,nfil,ncol)
  for(i in 1:nfil){
    base[i,]=generoGI.conf(alfa, -1-alfa,ncol,1)
  }
  nombre<-paste( "C:/Users/Usuario/Dropbox/ILACSC/Colas/Bases/base10000","alfa",floor(alfa),"genf",
                 sep = "")
  
  write.csv(base, file = nombre)
}

gen_mues_par=function(nfil,ncol,alfa){
  base=matrix(0,nfil,ncol)
  for(i in 1:nfil){
    base[i,]=generoGI.conpareto (ncol,alfa, -1-alfa)
  }
  nombre<-paste( "C:/Users/Usuario/Dropbox/ILACSC/Colas/Bases/base10000","alfa",floor(alfa),"genpareto",
                 sep = "")
  
  write.csv(base, file = nombre)
}

##########################
##generamos las bases
##########################

gen_mues_gam(1000,10000,-1.5)
gen_mues_gam(1000,10000,-3)
gen_mues_gam(1000,10000,-5)
gen_mues_gam(1000,10000,-8)
        

gen_mues_chi(1000,10000,-1.5)
gen_mues_chi(1000,10000,-3)
gen_mues_chi(1000,10000,-5)
gen_mues_chi(1000,10000,-8)

gen_mues_f(1000,10000,-1.5)
gen_mues_f(1000,10000,-3)
gen_mues_f(1000,10000,-5)
gen_mues_f(1000,10000,-8)

gen_mues_par(1000,10000,-1.5)
gen_mues_par(1000,10000,-3)
gen_mues_par(1000,10000,-5)
gen_mues_par(1000,10000,-8)


####################################
#### testeamos el ajuste con KS y AD
####################################

base=read.csv("base10000alfa-3genchi")
base_datos=base[,-1]
vec_ks=rep(0,1000)
vec_ad=rep(0,1000)
for(i in 1:1000){
vec_ks[i]<-ks.test(base_datos[i,],pgi0L1,-3,2)
}
sum(vec_ks<0.05)



muestra=















###################
####tiempos
#######################

tpogam1.5<-system.time(for(i in 1:fil){
  datgam1.5[i,]<-generoGI.congama(-1.5,0.5,col,1)})[[3]]
tpogam1.5

tpochi1.5<-system.time(for(i in 1:fil){
  datchi1.5[i,]<-generoGI.conchi(-1.5,0.5,col)})[[3]]
tpochi1.5

tpof1.5tpof1.5<-system.time(for(i in 1:fil){
  datf1.5[i,]<-generoGI.conf(-1.5,0.5,col,1)})[[3]]
tpof1.5

tpopar1.5<-system.time(for(i in 1:fil){
  datpar1.5[i,]<-generoGI.conpareto(col,-1.5,0.5)})[[3]]
tpopar1.5


################################
##########distribucion del máximo
#################################


pgi0L1max.muestr<-function(x,n,alfa,gama) pgi0L1(x,alfa,gama)^(n)

max1.5.gam<-apply(datgam1.5,1,max)
max1.5.chi<-apply(datchi1.5,1,max)
max1.5.f<-apply(datf1.5,1,max)
max1.5.par<-apply(datpar1.5,1,max)

ks.test(max1.5.gam,pgi0L1max.muestr,100000,-1.5,0.5)
ad.test(max1.5.gam,pgi0L1max.muestr,100000,-1.5,0.5)

ks.test(max1.5.chi,pgi0L1max.muestr,100000,-1.5,0.5)
ad.test(max1.5.chi,pgi0L1max.muestr,100000,-1.5,0.5)

ks.test(max1.5.f,pgi0L1max.muestr,100000,-1.5,0.5)
ad.test(max1.5.f,pgi0L1max.muestr,100000,-1.5,0.5)

ks.test(max1.5.par,pgi0L1max.muestr,100000,-1.5,0.5)
ad.test(max1.5.par,pgi0L1max.muestr,100000,-1.5,0.5)

####################################
############# Vuong
####################################

rm(list = ls())
library("actuar")

fil=1000
dato<-list(fil)
xx<-list(fil)
qq<-rep(0,fil)
est2<-rep(0,fil)
est3<-rep(0,fil)
x<-matrix(0,fil,col)
pval.vuong<-rep(0,fil)
comp<-list(fil)

for(i in 1:fil){
  dato[[i]]<-conpl$new(datgam1.5[i,])
  qq[i]=as.numeric(quantile(datgam1.5[1,],0.75))
  dato[[i]]$setXmin(qq[i])
  est2[i]<-estimate_pars(dato[[i]])$pars
  dato[[i]]$setPars(est2[i])
  x[i,]=rpareto(col,shape=1.5,scale=0.5)
  xx[[i]]=conpl$new(x[i,])
  xx[[i]]$setXmin(qq[i])
  est3[i]<-estimate_pars(xx[[i]])$pars
  xx[[i]]$setPars(est3[i])
  comp[[i]]<-compare_distributions(dato[[i]],xx[[i]])
  pval.vuong[i]<-comp[[i]]$p_two_sided
  }

pval.vuong
View(pval.vuong)
pval.vuong.gama.1.5.df<-data.frame(pval.vuong.1.5=pval.vuong)

write.csv(pval.vuong, file = "C:/Users/Julia/Dropbox/Procesamiento de imagenes/Colas/Bases/pval.vuong.gama.1.5.df")


rm(list = ls())
#################################################################
#n=10^6, alfa=-3  gama=0.5 genero 100 muestras 
# y las guardo en una matriz cada fila es una muestra
#############################################################
##para cociente de gamas
#################################
datgam3=matrix(nrow=fil,ncol=col)
for(i in 1:fil){
  datgam3[i,]<-generoGI.congama(-3,2,col,1)
}
tpogam3<-system.time(for(i in 1:fil){
  datgam3[i,]<-generoGI.congama(-3,2,col,1)})[[3]]
tpogam3
tpogama[2]<-tpogam3
##############################
#para cociente de chisq
##############################
datchi3=matrix(nrow=fil,ncol=col)
for(i in 1:fil){
  datchi3[i,]<-generoGI.conchi(-3,2,col)
}
tpochi3<-system.time(for(i in 1:fil){
  datchi3[i,]<-generoGI.conchi(-3,2,col)})[[3]]
tpochi3
tpochi[2]<-tpochi3
##############################
#para f de fisher
##############################
datf3=matrix(nrow=fil,ncol=col)
for(i in 1:1000){
  datf3[i,]<-generoGI.conf(-3,2,col,1)
}
tpof3<-system.time(for(i in 1:fil){
  datf3[i,]<-generoGI.conf(-3,2,col,1)})[[3]]
tpof3
tpof[2]<-tpof3
##############################
#para pareto
##############################
datpar3=matrix(nrow=fil,ncol=col)
for(i in 1:fil){
  datpar3[i,]<-generoGI.conpareto(col,-3,2)
}
tpopar3<-system.time(for(i in 1:fil){
  datpar3[i,]<-generoGI.conpareto(col,-3,2)})[[3]]
tpopar3
tpopar[2]<-tpopar3
####################################





rm(list = ls())

#################################################################
#n=10^6, alfa=-5  gama=0.5 genero 100 muestras 
# y las guardo en una matriz cada fila es una muestra
#############################################################
##para cociente de gamas
#################################
datgam5=matrix(nrow=fil,ncol=col)
for(i in 1:fil){
  datgam5[i,]<-generoGI.congama(-5,4,col,1)
}
tpogam5<-system.time(for(i in 1:fil){
  datgam5[i,]<-generoGI.congama(-5,4,col,1)})[[3]]
tpogam5
##############################
#para cociente de chisq
##############################
datchi5=matrix(nrow=fil,ncol=col)
for(i in 1:fil){
  datchi5[i,]<-generoGI.conchi(-5,4,col)
}
tpochi5<-system.time(for(i in 1:fil){
  datchi5[i,]<-generoGI.conchi(-5,4,col)})[[3]]
tpochi5
##############################
#para f de fisher
##############################
datf5=matrix(nrow=fil,ncol=col)
for(i in 1:fil){
  datf5[i,]<-generoGI.conf(-5,4,col,1)
}
tpof5<-system.time(for(i in 1:fil){
  datf5[i,]<-generoGI.conf(-5,4,col,1)})[[3]]
tpof5
##############################
#para pareto
##############################
datpar5=matrix(nrow=fil,ncol=col)
for(i in 1:fil){
  datpar5[i,]<-generoGI.conpareto(col,-5,4)
}
tpopar5<-system.time(for(i in 1:fil){
  datpar5[i,]<-generoGI.conpareto(col,-5,4)})[[3]]
tpopar5
####################################






rm(list = ls())
#################################################################
#n=10^6, alfa=-8  gama=0.5 genero 100 muestras 
# y las guardo en una matriz cada fila es una muestra
#############################################################
##para cociente de gamas
#################################
datgam8=matrix(nrow=fil,ncol=col)
for(i in 1:fil){
  datgam8[i,]<-generoGI.congama(-8,7,col,1)
}
tpogam8<-system.time(for(i in 1:fil){
  datgam8[i,]<-generoGI.congama(-8,7,col,1)})[[3]]
tpogam8
##############################
#para cociente de chisq
##############################
datchi8=matrix(nrow=fil,ncol=col)
for(i in 1:fil){
  datchi8[i,]<-generoGI.conchi(-8,7,col)
}
tpochi8<-system.time(for(i in 1:fil){
  datchi8[i,]<-generoGI.conchi(-8,7,col)})[[3]]
tpochi8
##############################
#para f de fisher
##############################
datf8=matrix(nrow=fil,ncol=col)
for(i in 1:fil){
  datf8[i,]<-generoGI.conf(-8,7,col,1)
}
tpof8<-system.time(for(i in 1:fil){
  datf8[i,]<-generoGI.conf(-8,7,col,1)})[[3]]
tpof8
##############################
#para pareto
##############################
datpar8=matrix(nrow=fil,ncol=col)
for(i in 1:fil){
  datpar8[i,]<-generoGI.conpareto(col,-8,7)
}
tpopar8<-system.time(for(i in 1:fil){
  datpar8[i,]<-generoGI.conpareto(col,-8,7)})[[3]]
tpopar8
####################################
  ###############
library(WRS)
install.packages(c("MASS", "akima", "robustbase"))

# second: install suggested packages
install.packages(c("cobs", "robust", "mgcv", "scatterplot3d", "quantreg", "rrcov", 
                   "lars", "pwr", "trimcluster", "parallel", "mc2d", "psych", "Rfit"))

# third: install WRS
install.packages("WRS", repos="http://R-Forge.R-project.org", type="source")


