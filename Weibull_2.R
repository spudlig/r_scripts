install.packages("fBasics")
library("fBasics")
install.packages("fitdistrplus")
library(fitdistrplus)
library(logspline)
install.packages("ismev")
library(ismev)
library(lattice)
install.packages("Lmoments")
library("Lmoments")
install.packages("MASS")
install.packages("stats4")
library(MASS)
library(stats4)
install.packages("rootSolve")
require(rootSolve)
library(rootSolve)


sample1<-read.table("/Users/oliverw/Studium/Watermanagement//2014 WS//UncertainHydr/FD.csv",header=T,sep=";",dec=",")
sa<-sample1$Abfluss[1:30]

f.wei<-function(x,c,a,d){
  z2<-(-(x-c)^a/d)
  z1<-1-exp(z2)
  return(z1)
  
}
f.wei(1 2,22,3,4)

#### Estimation
### MLE
## scale (zwei unterschiedliche Arten, einmal via fitdist; einmal über MLE estimation; a ist k, d ist lambda)
n<-NROW(sa)
x<-sa
# scale via fitdist
lam_e<-function(x,n){
  z1<-(1/n)
  z2<-sum(x)
  z3<-z1*z2
  return(as.numeric(z3))
}
lam_e_cal<-as.numeric(lam_e(x,n))
# scale via MLE estimation (wiki)
func.wei<-fitdist(sa,"weibull",discrete = F)
lam_e_func.wei<-as.numeric(func.wei$estimate[2:2])
lam_e_cal==lam_e_func.wei
## scale via DWA S.74 d ist lambda
#Schiefekoeffizient q3 
n=as.numeric(nrow(sa))
m=mean(sa)
s<-sd(sa)
q3<-function(x, n, m, s){
  z1=n*sum(x-m)^3
  z2=(n-1)*(n-2)*s^3
  z3=z1/z2
  return(as.numeric(z3))
}
q3_norm<-q3(sa,n,m,s)
# Iterative Bestimmung von a
q4<-function(a){
  z1<-as.numeric(3*a^2*gamma(3/a) - 6*a*gamma(1/a)*gamma(2/a))
  z2<-as.numeric(2*gamma(1/a)^3)
  z9<-as.numeric(z1+z2)
  z4<-as.numeric(2*a*gamma(2/a))
  z5<-as.numeric(gamma(1/a)^2)
  z7<-as.numeric((z4-z5)^(2/3))
  z8<-as.numeric((z9)/z7)
  z10<-z8/5.230445e-46
  return(z10)
}
q4(233400)
q4(10000000000+4)
10^10
## shape
