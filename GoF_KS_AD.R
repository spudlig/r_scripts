#### Anderson Darling und Kolmogorov Smirnov Test als GoF
### KS
## GOF
x<-maxOL
## AIC BIC
gofd<-matrix(NA,30,2)
descdist(x)
for (i in seq(1:30)){
  x<-maxOL[i:(29+i)]
  gofd[i,]<-append(descdist(x)$skewness,descdist(x)$kurtosis)
}
for (i in seq(1:30)){
  points(x=c(gofd[i,1])^2,y=-(gofd[i,2]-10),col="darkblue",cex=1,pch=21,bg="darkblue")
}

gof<-matrix(NA,30,6)


for (i in seq(1:30)){
  x<-maxOL[i:(29+i)]
  wei_fitd<-fitdist(x,"weibull")
pe3_fitd<-fitdist(x,"gamma")
ln3_fitd<-fitdist(x,"lnorm")
gof[i,]<-append(gofstat(list(wei_fitd,pe3_fitd,ln3_fitd))$aic,gofstat(list(wei_fitd,pe3_fitd,ln3_fitd))$bic)
}

plot(gof[,1],type="l",col="red")
lines(gof[,2],col="green")
lines(gof[,3],col="blue")
lines(gof[,4],col="orange")
lines(gof[,5],col="lightgreen")
lines(gof[,6],col="purple")

kk<-gofstat(wei_fitd)
kk$
gof
