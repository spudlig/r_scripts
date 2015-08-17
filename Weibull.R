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
install.packages("stats4")
install.packages("VarianceGamma")
library(VarianceGamma) 

### Data
sample1<-read.table("/Users/oliverw/Studium/Watermanagement/Masterarbeit/data/Q-Tagesmittel-203398.csv",header=F,sep=";",dec=",",skip=22,na.strings = "Lücke",stringsAsFactors = F)
#histogram(sample1$Abfluss,breaks=12,ylab="rel. frequ",ylim=c(-5:25),xlim=c(-10:300))
data<-as.matrix(sample1[,2])
data2<-as.numeric(sample1[,2])



Date<-sample1[,1]
date<-strptime(Date, format="%d.%m.%Y %H:%M")
year<-format(date,format="%Y")
month<-format(date,format="%m")
y<-as.numeric(year)
ma<-as.numeric(NULL)
mat<-cbind(as.numeric(year),as.numeric(month),data)

ma<-matrix(cbind(c(1:10958),NA))

a<-year<1981
summary(a)

b<-(year>1981)
summary(b)

for (i in ma[,1]){
  ma[i,1]<-mat[mat[,1]<1981]
  
}

for (i in seq(along=matt[,1])){
  matt[i,2]<-matt[matt[,1]==ma]
  
}

tail(ma)
str(ma)
Data<-(mat[,3])
Data<-Data[10958]
dat<-as.numeric(Data)
x.dat<-(dat[,2])


m<-mean(x.dat)
va<-var(dat)
sk<-skewness(dat)
ku<-kurtosis(dat)
#Weibull
x.wei<-rweibull(n = length(dat),shape = m, scale = va)

#Poisson
x.pois<-rpois(n=30,lambda=m)
x.log.pois<-log(rpois(n=30,lambda = m))
hist(x.log.pois)
plot(ecdf(x.log.pois))

plot(sa,type="l",col="red")
lines(x.wei,col="blue")
qqplot(sa,x.wei)
abline(2,5)

### Parameter estimation
## Moments

func.wei<-fitdist(sa,"weibull",discrete = F)
str(func.wei)
plot(func.wei)
summary(func.wei)
func.wei$aic

x.wei_fitdis<-rweibull(func.wei,shape=func.wei$estimate[1:1],scale=func.wei$estimate[2:2])
plot(x.wei_fitdis,type="l")
plot(x.wei_LM,type="l")

func.wei2<-fitdist(sa2,"weibull",discrete=F)
plot(func.wei2)
func.wei2$aic
plot(func.wei2$estimate,func.wei$estimate)

x.wei_LM<-rweibull(func.wei,shape = LM_mean,scale = LM_scale)
x.wei_MASS<-rweibull(func.wei2,shape =k ,scale = lam)
str(x.wei_LM)
plot(x.wei_LM,type="l")
plot.new()

qqplot(x.wei_LM,x.wei_MASS)
abline(lm(x.wei_LM ~ x.wei_MASS))

summary(x.wei_LM)
summary(x.wei_MASS)
summary(x.wei_fitdis)
abline(0,1)
## L-Moments
LM_E<-Lmoments(func.wei,returnobject=T)
str(LM_E)
LM_mean<-LM_E$ratios[1:1]
LM_scale<-LM_E$ratios[2:2]
## MLE
# mle

# MASS
MLE_M<-fitdistr(sa,"weibull")
MLE_M$estimate[1:1]
str(MLE_M)
MLE_M
k<-as.numeric(MLE_M$estimate[1:1])
lam<-as.numeric(MLE_M$estimate[2:2])
