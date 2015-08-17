install.packages("lmom")
library("lmom")
install.packages("fBasics")
library("fBasics")
install.packages("fitdistrplus")
library(fitdistrplus)
library(logspline)
install.packages("gumbel")
library(gumbel)
install.packages("ismev")
library(ismev)
library(lattice)


sample1<-read.table("/Users/oliverw/Studium/Watermanagement//2014 WS//UncertainHydr/FD.csv",header=T,sep=";",dec=",")
plot(sample1,type="h")
histogram(sample1$Abfluss,breaks=12,ylab="rel. frequ",ylim=c(-5:25),xlim=c(-10:300))
sa<-sample1$Abfluss[1:30]

summary(sample1$Abfluss)
s<-sd(sa)
m<-mean(sa)

plot(density(sample1$Abfluss),main="Density estimate of Data")
plot(ecdf(sample1$Abfluss))
?qqplot()
qqnorm(sample1$Abfluss)
qqline(sample1$Abfluss,qtype = 8)
z.norm<-((sample1$Abfluss-mean(sample1$Abfluss))/sd(sample1$Abfluss))
qqnorm(z.norm)
abline(0,1,col="red")

t.test(sample1$Abfluss)$conf.int
hist(rpois(sample1$Abfluss,lambda=87.54324))
curve(dnorm(x,mean = m,sd = s),from=0,to=300)
abline(curve(dgamma(x,shape = )))

descdist(sa,discrete=F)
fit.gumbel<-fitdist(sa,"gumbel")
fit.lnorm<-fitdist(sa,"lnorm")
fit.lnorm$aic
fit.weibull<-fitdist(sa,"weibull")
plot(fit.weibull)
plot(fit.lnorm)
fit.lnorm$aic
fit.weibull$aic
?fitdist()


