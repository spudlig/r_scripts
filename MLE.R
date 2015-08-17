
### Gumbel MLE (done)
gum.fit(df[[4]])

mle.results.gum0<-gum.fit(yy0)
mle.results.gum1<-gum.fit(yy1)
mle.results.gum2<-gum.fit(yy2)
mle.results.gum3<-gum.fit(yy3)
mle.results.gum4<-gum.fit(yy4)
mle.results.gum5<-gum.fit(yy5)
mle.results.gum6<-gum.fit(yy6)
mle.results.gum7<-gum.fit(yy7)
mle.results.gum8<-gum.fit(yy8)
mle.results.gum9<-gum.fit(yy9)
mle.results.gum10<-gum.fit(yy10)
mle.results.gum11<-gum.fit(yy11)
mle.results.gum12<-gum.fit(yy12)
mle.results.gum13<-gum.fit(yy13)
mle.results.gum14<-gum.fit(yy14)
mle.results.gum15<-gum.fit(yy15)
mle.results.gum16<-gum.fit(yy16)
mle.results.gum17<-gum.fit(yy17)
mle.results.gum18<-gum.fit(yy18)
mle.results.gum19<-gum.fit(yy19)
mle.results.gum20<-gum.fit(yy20)
mle.results.gum21<-gum.fit(yy21)
mle.results.gum22<-gum.fit(yy22)
mle.results.gum23<-gum.fit(yy23)
mle.results.gum24<-gum.fit(yy24)
mle.results.gum25<-gum.fit(yy25)
mle.results.gum26<-gum.fit(yy26)
mle.results.gum27<-gum.fit(yy27)
mle.results.gum28<-gum.fit(yy28)
mle.results.gum29<-gum.fit(yy29)
mle.results.gum30<-gum.fit(yy30)

y0.gum<-quagum(f=quantile, para=c(mle.results.gum0$mle[1],mle.results.gum0$mle[2]))
y1.gum<-quagum(f=quantile, para=c(mle.results.gum1$mle[1],mle.results.gum1$mle[2]))
y2.gum<-quagum(f=quantile, para=c(mle.results.gum2$mle[1],mle.results.gum2$mle[2]))
y3.gum<-quagum(f=quantile, para=c(mle.results.gum3$mle[1],mle.results.gum3$mle[2]))
y4.gum<-quagum(f=quantile, para=c(mle.results.gum4$mle[1],mle.results.gum4$mle[2]))
y5.gum<-quagum(f=quantile, para=c(mle.results.gum5$mle[1],mle.results.gum5$mle[2]))
y6.gum<-quagum(f=quantile, para=c(mle.results.gum6$mle[1],mle.results.gum6$mle[2]))
y7.gum<-quagum(f=quantile, para=c(mle.results.gum7$mle[1],mle.results.gum7$mle[2]))
y8.gum<-quagum(f=quantile, para=c(mle.results.gum8$mle[1],mle.results.gum8$mle[2]))
y9.gum<-quagum(f=quantile, para=c(mle.results.gum9$mle[1],mle.results.gum9$mle[2]))
y10.gum<-quagum(f=quantile, para=c(mle.results.gum10$mle[1],mle.results.gum10$mle[2]))
y11.gum<-quagum(f=quantile, para=c(mle.results.gum11$mle[1],mle.results.gum11$mle[2]))
y12.gum<-quagum(f=quantile, para=c(mle.results.gum12$mle[1],mle.results.gum12$mle[2]))
y13.gum<-quagum(f=quantile, para=c(mle.results.gum13$mle[1],mle.results.gum13$mle[2]))
y14.gum<-quagum(f=quantile, para=c(mle.results.gum14$mle[1],mle.results.gum14$mle[2]))
y15.gum<-quagum(f=quantile, para=c(mle.results.gum15$mle[1],mle.results.gum15$mle[2]))
y16.gum<-quagum(f=quantile, para=c(mle.results.gum16$mle[1],mle.results.gum16$mle[2]))
y17.gum<-quagum(f=quantile, para=c(mle.results.gum17$mle[1],mle.results.gum17$mle[2]))
y18.gum<-quagum(f=quantile, para=c(mle.results.gum18$mle[1],mle.results.gum18$mle[2]))
y19.gum<-quagum(f=quantile, para=c(mle.results.gum19$mle[1],mle.results.gum19$mle[2]))
y20.gum<-quagum(f=quantile, para=c(mle.results.gum20$mle[1],mle.results.gum20$mle[2]))
y21.gum<-quagum(f=quantile, para=c(mle.results.gum21$mle[1],mle.results.gum21$mle[2]))
y22.gum<-quagum(f=quantile, para=c(mle.results.gum22$mle[1],mle.results.gum22$mle[2]))
y23.gum<-quagum(f=quantile, para=c(mle.results.gum23$mle[1],mle.results.gum23$mle[2]))
y24.gum<-quagum(f=quantile, para=c(mle.results.gum24$mle[1],mle.results.gum24$mle[2]))
y25.gum<-quagum(f=quantile, para=c(mle.results.gum25$mle[1],mle.results.gum25$mle[2]))
y26.gum<-quagum(f=quantile, para=c(mle.results.gum26$mle[1],mle.results.gum26$mle[2]))
y27.gum<-quagum(f=quantile, para=c(mle.results.gum27$mle[1],mle.results.gum27$mle[2]))
y28.gum<-quagum(f=quantile, para=c(mle.results.gum28$mle[1],mle.results.gum28$mle[2]))
y29.gum<-quagum(f=quantile, para=c(mle.results.gum29$mle[1],mle.results.gum29$mle[2]))
y30.gum<-quagum(f=quantile, para=c(mle.results.gum30$mle[1],mle.results.gum30$mle[2]))


(pelgum(samlmu(listko[1][[1]][[4]])))

mimi<-param.gum.ml(listko[1][[1]],year=80,quantile = 0.90)
mimi

gum.fitx<-function(scale,location) { 
  -sum(dgumbel(listko[1][[1]][[4]],scale,location,log=T)) }
mle.results.wei<-mle2(wei.fitx,start=list(scale=1600,location=750),data=list(listko[5][[1]][[4]][1:9000]))
mle.results.gum@details$par[1]
y0.gum<-quagum(f=0.99, para=c(1660.5935,684.0752  ))
y0.wei<-quawei(f=0.99, para=c(mle.results.wei0@details$par))


?quawei()
# Abschätzen
gum.est<-pelgum(samlmu(listko[2][[1]][[4]]))
mle.results.gum<-mle2(gum.fitx,start=list(scale=gum.est[1],location=gum.est[2]),data=list(listko[2][[1]][[4]]))

wei.est<-pelwei(samlmu(listko[1][[1]][[4]]))
mle.results.wei0<-mle2(wei.fitx,start=list(shape=wei.est[3],scale=wei.est[2],thres=wei.est[1]),data=list(listko[1][[1]][[4]]))
mle.results.wei0<-mle2(wei3.fit0,start=list(shape=1,scale=1,thres=1),data=list(listko[1][[1]][[4]]))
mle.results.wei0

par(mfrow=c(2,2))
plot(profile(mle.results.gum))
plot(p2,which="theta",plot.confstr=TRUE)

  dev.off()



gum.est<-samlmu(df[[4]])
mle.results.wei0<-mle2(wei3.fit0,start=list(shape=1.7,scale=1900,thres=400),data=list(yy0))
mle.results.gum0<-mle2(gum.fitx,start=list(scale=gum.est[1],location=gum.est[2]),data=list(listko[1][[1]][[4]]))
mle.results.gum0



### Weibull
#fitting a normal distribution requires two parameters, mean (mu) and standard deviation (sigma)
wei.fitx<-function(shape,scale,thres) { 
  -sum(dweibull3(listko[1][[1]][[4]],shape,scale,thres,log=T)) }
ln3.fitx<-function(shape,scale,thres) { 
  -sum(dlnorm3(listko[1][[1]][[4]],shape,scale,thres,log=T)) }
gum.fit0<-function(scale,location) { 
  -sum(dgumbel(listko[1][[1]][[4]][ys0],scale,location,log=T)) }
gum.fit0<-function(scale,location) { 
  -sum(dgumbel(listko[1][[1]][[4]][ys0],scale,location,log=T)) }

gum.fit15<-function(scale,location) { 
  -sum(dgumbel(listko[1][[1]][[4]][ys15],scale,location,log=T)) }

pe3.fit<-function(shape,scale,thres) { 
  -sum(dgamma3(listko[1][[1]][[4]],shape,scale,thres,log=T)) }

mle.results.wei3<-mle2(wei.fitx,start=list(shape=1.7,scale=1900,thres=400),data=list(listko[1][[1]][[4]]))
mle.results.wei3[3,2,1]
mle.results.ln3<-mle2(ln3.fitx,start=list(shape=0.5,scale=7,thres=310),data=list(listko[1][[1]][[4]]))
mle.results.pe3<-mle2(pe3.fit,start=list(thres =300 ,shape=3,scale=500),data=list(listko[1][[1]][[4]]))
summary(mle.results.wei3)
summary(mle.results.ln3)
summary(mle.results.gum)
summary(mle.results.pe3)
?qualn3()
mle.results.ln3@details$par[2],log(mle.results.ln3@details$par[2]),log(mle.results.ln3@details$par[3])
?qualn3()

?quagum()
cc<-listko[1][[1]][[4]]
a<-listko[1][[1]][[4]][ys15]
b<-listko[1][[1]][[4]][ys7]
c<-listko[1][[1]][[4]][ys1]

mle.results.gum<-mle2(gum.fit15,start=list(scale=1600,location=750),data=list(a))
mle.results.gum<-mle2(gum.fit7,start=list(scale=1600,location=750),data=list(b))
mle.results.gum<-mle2(gum.fit0,start=list(scale=1600,location=750),data=list(c))
y00.gum<-quagum(f=0.9, para=c(mle.results.gum@details$par))
y01.gum<-quagum(f=0.9, para=c(mle.results.gum1@details$par))
y02.gum<-quagum(f=0.9, para=c(mle.results.gum2@details$par))


y0.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys0])))
y0.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys0])))
y00.gum #<- Am Montag weiter machen, für yy0 

?evplot()

pelwei(samlmu(listko[1][[1]][[4]]))
pelln3(samlmu(listko[1][[1]][[4]]))
pelgum(samlmu(listko[1][[1]][[4]]))
pelpe3(samlmu(listko[1][[1]][[4]]))
(cdfpe3(listko[1][[1]][[4]],para=(shape=2087,scale=1006,thres=1.6,log=FALSE)))

AICctab(mle.results.ln3,mle.results.gum,mle.results.pe3,weights=T)
?AICctab()
fitdistr(listko[1][[1]][[4]],"weibull")
pelwei(samlmu(listko[1][[1]][[4]]))

fitdistr(listko[1][[1]][[4]],"log-normal")
pelln3(samlmu(listko[1][[1]][[4]]))
### Pearson 3 --> Gamma Dist
# MLE

fitdistr(listko[1][[1]][[4]],"log-normal")


fitdistr(listko[1][[1]][[4]], dgamma,start=list(shape=2.083688, scale=80.469296 ))
fitdistr(listko[1][[1]][[4]],"Gamma")
?dgamma()
### <-------!!!!!

pe3.fitx<-function(scale,rate,shape) { 
  -sum(dgamma(listko[1][[1]][[4]],scale,rate,shape,log=T)) }

mle.results.pe322<-mle2(pe3.fitx,start=list(shape=3,scale=500),data=list(listko[1][[1]][[4]]))

summary(mle.results.pe322)
mle.results.pe322@coef[2]
?quagam()
pelpe3(samlmu(listko[1][[1]][[4]]))
pe3.ml<-pearsonFitML(listko[1][[1]][[4]])
pe3.ml$m
y0.pe3<-quagam(f=0.90, para=c(mle.results.pe322@coef[1],mle.results.pe322@coef[2]))
y0.pe31<-quagam(f=0.90, para=c(mle.results.pe322@coef[1],mle.results.pe322@coef[2]))
cbind(as.numeric(y0.pe3,y0.pe31))

gum.ext<-pelgum(listko[1][[1]][[4]])
gum.ext[2]
?mle2()

pe3.fit0<-function(scale,location) { 
  -sum(dgamma(yy0,scale,location,log=T)) }

dd2<-pearsonMSC(listko[1][[1]][[4]])
dd<-pearsonFitML(listko[1][[1]][[4]])
(samlmu(listko[1][[1]][[4]],trim=0))
llm<-Lmoments(listko[1][[1]][[4]])
llm
parameters<-par.gamma(llm[1],llm[2],llm[4])
parameters
moments <- par2mom.gamma(parameters$alfa,parameters$beta,parameters$xi); moments
mom2par.gamma(moments$mu,moments$sigma,moments$gamm)


unlist(pearsonFitM(moments=empMoments(listko[1][[1]][[4]])))
xlm.w<-cdfpe3(listko[1][[1]][[4]],para=c(location=400.529234,shape=3.249965,scale=518.947625 ))
xlm.ww<-(400.529234,3.249965,518.947625 )
plot(ahj)
?cdfpe3()
dd
evplot(listko[1][[1]][[4]],type="l")

evdistq(quape3, xlm.ww,type="l",col=2)
weiqu0<-quawei(f=quantile, para=xlm.ww)

# MLE as Gamma
(fitdistr(listko[1][[1]][[4]], densfun="gamma",lower=0))
pelpe3(samlmu(listko[1][[1]][[4]]))
pelpe3(empMoments(listko[1][[1]][[4]]))
pearsonMoments(listko[1][[1]][[4]])
sd(listko[1][[1]][[4]])
?empMoments()

# Moments via PearsonDS
matt<-matrix(NA,length(dfStatKenn),ncol = 4)
matt[,2]
l<-nrow(matt)
list<-list(NULL)
matt[l,]
c(1:l)

mean.list<-matrix(unlist(lapply(dfStatKenn[1:length(dfStatKenn)],FUN=pearDSIn)))
mean.list

allPearDS<-function(df){
  l<-nrow(matt)
  mean1<-matrix(unlist(lapply(df[c(1:l)],FUN=pearDSInMean)))
  sd1<-matrix(unlist(lapply(df[c(1:l)],FUN=pearDSInsd)))
  skew1<-matrix(unlist(lapply(df[c(1:l)],FUN=pearDSInskew)))
  kurt1<-matrix(unlist(lapply(df[c(1:l)],FUN=pearDSInkurt)))
  return(cbind(mean1,sd1,skew1,kurt1))
}

pearDSInMean<-function(df){
  mean<-(df[[1]])
  return(as.numeric(mean))
}
pearDSInsd<-function(df){
  sd<-(df[[2]])
  return(as.numeric(sd))
}
pearDSInskew<-function(df){
  skew<-(df[[3]])
  return(as.numeric(skew))
}
pearDSInkurt<-function(df){
  kurt<-(df[[4]])
  return(as.numeric(kurt))
}

cv<-allPearDS(dfStatKenn[[1]])


### Log-Normal (zeta fehlt)
qu.ml.lg<-(fitdistr(listko[1][[1]][[4]], densfun="lognormal"))
pelln3(samlmu(listko[1][[1]][[4]]))
?qualn3()


y0.ln3<-qlnorm(qu.ml.lg)
qu.ml.lg$estimate[[1]]
?qualn3()
sh<-as.numeric(mean(listko[1][[1]][[4]]))
sc<-sd(listko[1][[1]][[4]])
ku<-as.numeric(skewness(listko[1][[1]][[4]]))
ku[1]
sh
yx<-listko[1][[1]][[4]]
X <- rlnorm3(n=100, shape = 2, scale = 1.5, thres = 1)
fitdistr(X,dlnorm3,start=list(shape = 2, scale = 1.5, thres = 1))
fitdistr(yx,dlnorm3,start=list(shape = sh, scale = sc, thres = ku))
shape(listko[1][[1]][[4]])
fitdistr()
?gev.fit()
?ismev
yx<-(fitdistr(listko[1][[1]][[4]], densfun="weibull",lower=0))
pelln3(samlmu(listko[1][[1]][[4]]))
pelwei(samlmu(listko[1][[1]][[4]]))

yx[1][[1]][[3]]
yx
mle.results.ln330<-mle2(ln3.fit30,start=list(shape=0.5,scale=7,thres=310),data=list(yy30))

1600*0.96

ln3.fitx<-function(shape,scale,thres) { 
  -sum(dlnorm3(listko[1][[1]][[4]],shape,scale,thres,log=T)) }
mle.results.ln3<-mle2(ln3.fitx,start=list(shape=0.5,scale=7,thres=310),data=list(listko[1][[1]][[4]]))
mle.results.ln3@coef[3]
quape3(f=0.9, para=c(pelln3(samlmu(listko[1][[1]][[4]]))))
qualn3(f=0.9,para=c())

ln3.fit19<-function(shape,scale,thres) { 
  -sum(dlnorm3(yy19,shape,scale,thres,log=T)) }
mle.results.ln319<-mle2(ln3.fit19,start=list(shape=0.5,scale=7,thres=310),data=list(yy19))
y18.ln3<-qualn3(f=0.3, para=c(mle.results.ln319@details$par[1],log(mle.results.ln319@details$par[2]),log(mle.results.ln319@details$par[3])))
y18.ln3

y19.ln3<-qualn3(f=quantile, para=c(mle.results.ln319@details$par[1],log(mle.results.ln319@details$par[2]),log(mle.results.ln319@details$par[3])))



