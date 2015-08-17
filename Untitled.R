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
sample1<-read.table("/Users/oliverw/Studium/Watermanagement/Masterarbeit/data/Q-Tagesmittel-203398.csv",header=F,sep=";",dec=",",skip=22)
sam<-as.matrix(sample1[,2])
Date<-sample1[,1]
date<-strptime(Date,format="%d.%m.%Y %H:%M")
month<-format(date,format="%m")
year<-format(date,format="%Y")
t<-as.numeric(year)
tt<-as.matrix(t["1951":"2000"])
tt
tail(sample1)
m<-as.numeric(c(1981:2012))
sample<-matrix(year,sam)
while ((sample1[,1])<(1981)){
  sample[,1]<-sample1[,1]
}
summary(sample)
while i in(sample1[,1]){
  sample[,1]<-sample1[,1]<1981
}

ma.51.80<-matrix()
ta<-seq(1951,2010,1)
da<-rnorm(60,40,12)

la<-lenghts()

sa<-sam[1:30]
sa2<-sam[31:60]
summary(sample1)
sample<-matrix(2,)
str(sample1)

m<-mean(sa)
va<-var(sa)
sk<-skewness(sa)
ku<-kurtosis(sa)
###Weibull
x.wei<-pweibull(sa,shape=sh,scale=sc)
plot(x.wei,type="l")
lines(sa)
?rweibull
##MLE
mle.wei<-fitdistr(sa,densfun=dweibull,start = list(scale=1,shape=1))
mle.wei2<-fitdistr(sa,"weibull")
mle.wei
mle.wei2
sc<-as.numeric(mle.wei$estimate[1:1])
sh<-as.numeric(mle.wei$estimate[2:2])
x.cdf<-(ecdf(mle.wei2))
quantile(x.cdf)
plot(x.cdf,type="l")
x.cdf
plot(x.cdf)
?ecdf()
##Dist

