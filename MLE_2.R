### Parameter für MLE (ln3 funktioniert noch nicht gut <- überarbeiten)
param.mle<-function(df,quantile,year){
  ys0<-seq(1,year*365+11,1)
  ys1<-seq(1*365,(year*365+10+1*365),1)
  ys2<-seq(2*365,(year*365+10+2*365),1)
  ys3<-seq(3*365,(year*365+10+3*365),1)
  ys4<-seq(4*365,(year*365+10+4*365),1)
  ys5<-seq(5*365,(year*365+10+5*365),1)
  ys6<-seq(6*365,(year*365+10+6*365),1)
  ys7<-seq(7*365,(year*365+10+7*365),1)
  ys8<-seq(8*365,(year*365+10+8*365),1)
  ys9<-seq(9*365,(year*365+10+9*365),1)
  ys10<-seq(10*365,(year*365+10+10*365),1)
  ys11<-seq(11*365,(year*365+10+11*365),1)
  ys12<-seq(12*365,(year*365+10+12*365),1)
  ys13<-seq(13*365,(year*365+10+13*365),1)
  ys14<-seq(14*365,(year*365+10+14*365),1)
  ys15<-seq(15*365,(year*365+10+15*365),1)
  ys16<-seq(16*365,(year*365+10+16*365),1)
  ys17<-seq(17*365,(year*365+10+17*365),1)
  ys18<-seq(18*365,(year*365+10+18*365),1)
  ys19<-seq(19*365,(year*365+10+19*365),1)
  ys20<-seq(20*365,(year*365+10+20*365),1)
  ys21<-seq(21*365,(year*365+10+21*365),1)
  ys22<-seq(22*365,(year*365+10+22*365),1)
  ys23<-seq(23*365,(year*365+10+23*365),1)
  ys24<-seq(24*365,(year*365+10+24*365),1)
  ys25<-seq(25*365,(year*365+10+25*365),1)
  ys26<-seq(26*365,(year*365+10+26*365),1)
  ys27<-seq(27*365,(year*365+10+27*365),1)
  ys28<-seq(28*365,(year*365+10+28*365),1)
  ys29<-seq(29*365,(year*365+10+29*365),1)
  ys30<-seq(30*365,(year*365+10+30*365),1)
  yy0<-df[[4]][ys0]
  yy1<-df[[4]][ys1]
  yy2<-df[[4]][ys2]
  yy3<-df[[4]][ys3]
  yy4<-df[[4]][ys4]
  yy5<-df[[4]][ys5]
  yy6<-df[[4]][ys6]
  yy7<-df[[4]][ys7]
  yy8<-df[[4]][ys8]
  yy9<-df[[4]][ys9]
  yy10<-df[[4]][ys10]
  yy11<-df[[4]][ys11]
  yy12<-df[[4]][ys12]
  yy13<-df[[4]][ys13]
  yy14<-df[[4]][ys14]
  yy15<-df[[4]][ys15]
  yy16<-df[[4]][ys16]
  yy17<-df[[4]][ys17]
  yy18<-df[[4]][ys18]
  yy19<-df[[4]][ys19]
  yy20<-df[[4]][ys20]
  yy21<-df[[4]][ys21]
  yy22<-df[[4]][ys22]
  yy23<-df[[4]][ys23]
  yy24<-df[[4]][ys24]
  yy25<-df[[4]][ys25]
  yy26<-df[[4]][ys26]
  yy27<-df[[4]][ys27]
  yy28<-df[[4]][ys28]
  yy29<-df[[4]][ys29]
  yy30<-df[[4]][ys30]
  
  wei3.fit0<-function(shape,scale,thres) { 
    -sum(dweibull3(yy0,shape,scale,thres,log=T)) }
  ln3.fit0<-function(shape,scale,thres) { 
    -sum(dlnorm3(yy0,shape,scale,thres,log=T)) }
  pe3.fit0<-function(scale,rate,shape) { 
    -sum(dgamma(yy0,scale,rate,shape,log=T)) }
  gum.fit0<-function(scale,location) { 
    -sum(dgumbel(yy0,scale,location,log=T)) }
  wei3.fit1<-function(shape,scale,thres) { 
    -sum(dweibull3(yy1,shape,scale,thres,log=T)) }
  ln3.fit1<-function(shape,scale,thres) { 
    -sum(dlnorm3(yy1,shape,scale,thres,log=T)) }
  pe3.fit1<-function(scale,rate,shape) { 
    -sum(dgamma(yy1,scale,rate,shape,log=T)) }
  gum.fit1<-function(scale,location) { 
    -sum(dgumbel(yy1,scale,location,log=T)) }
  wei3.fit2<-function(shape,scale,thres) { 
    -sum(dweibull3(yy2,shape,scale,thres,log=T)) }
  ln3.fit2<-function(shape,scale,thres) { 
    -sum(dlnorm3(yy2,shape,scale,thres,log=T)) }
  pe3.fit2<-function(scale,rate,shape) { 
    -sum(dgamma(yy2,scale,rate,shape,log=T)) }
  gum.fit2<-function(scale,location) { 
    -sum(dgumbel(yy2,scale,location,log=T)) }
  wei3.fit3<-function(shape,scale,thres) { 
    -sum(dweibull3(yy3,shape,scale,thres,log=T)) }
  ln3.fit3<-function(shape,scale,thres) { 
    -sum(dlnorm3(yy3,shape,scale,thres,log=T)) }
  pe3.fit3<-function(scale,rate,shape) { 
    -sum(dgamma(yy3,scale,rate,shape,log=T)) }
  gum.fit3<-function(scale,location) { 
    -sum(dgumbel(yy3,scale,location,log=T)) }
  wei3.fit4<-function(shape,scale,thres) { 
    -sum(dweibull3(yy4,shape,scale,thres,log=T)) }
  ln3.fit4<-function(shape,scale,thres) { 
    -sum(dlnorm3(yy4,shape,scale,thres,log=T)) }
  pe3.fit4<-function(scale,rate,shape) { 
    -sum(dgamma(yy4,scale,rate,shape,log=T)) }
  gum.fit4<-function(scale,location) { 
    -sum(dgumbel(yy4,scale,location,log=T)) }
  wei3.fit5<-function(shape,scale,thres) { 
    -sum(dweibull3(yy5,shape,scale,thres,log=T)) }
  ln3.fit5<-function(shape,scale,thres) { 
    -sum(dlnorm3(yy5,shape,scale,thres,log=T)) }
  pe3.fit5<-function(scale,rate,shape) { 
    -sum(dgamma(yy5,scale,rate,shape,log=T)) }
  gum.fit5<-function(scale,location) { 
    -sum(dgumbel(yy5,scale,location,log=T)) }
  wei3.fit6<-function(shape,scale,thres) { 
    -sum(dweibull3(yy6,shape,scale,thres,log=T)) }
  ln3.fit6<-function(shape,scale,thres) { 
    -sum(dlnorm3(yy6,shape,scale,thres,log=T)) }
  pe3.fit6<-function(scale,rate,shape) { 
    -sum(dgamma(yy6,scale,rate,shape,log=T)) }
  gum.fit6<-function(scale,location) { 
    -sum(dgumbel(yy6,scale,location,log=T)) }
  wei3.fit7<-function(shape,scale,thres) { 
    -sum(dweibull3(yy7,shape,scale,thres,log=T)) }
  ln3.fit7<-function(shape,scale,thres) { 
    -sum(dlnorm3(yy7,shape,scale,thres,log=T)) }
  pe3.fit7<-function(scale,rate,shape) { 
    -sum(dgamma(yy7,scale,rate,shape,log=T)) }
  gum.fit7<-function(scale,location) { 
    -sum(dgumbel(yy7,scale,location,log=T)) }
  wei3.fit8<-function(shape,scale,thres) { 
    -sum(dweibull3(yy8,shape,scale,thres,log=T)) }
  ln3.fit8<-function(shape,scale,thres) { 
    -sum(dlnorm3(yy8,shape,scale,thres,log=T)) }
  pe3.fit8<-function(scale,rate,shape) { 
    -sum(dgamma(yy8,scale,rate,shape,log=T)) }
  gum.fit8<-function(scale,location) { 
    -sum(dgumbel(yy8,scale,location,log=T)) }
  wei3.fit9<-function(shape,scale,thres) { 
    -sum(dweibull3(yy9,shape,scale,thres,log=T)) }
  ln3.fit9<-function(shape,scale,thres) { 
    -sum(dlnorm3(yy9,shape,scale,thres,log=T)) }
  pe3.fit9<-function(scale,rate,shape) { 
    -sum(dgamma(yy9,scale,rate,shape,log=T)) }
  gum.fit9<-function(scale,location) { 
    -sum(dgumbel(yy9,scale,location,log=T)) }
  wei3.fit10<-function(shape,scale,thres) { 
    -sum(dweibull3(yy10,shape,scale,thres,log=T)) }
  ln3.fit10<-function(shape,scale,thres) { 
    -sum(dlnorm3(yy10,shape,scale,thres,log=T)) }
  pe3.fit10<-function(scale,rate,shape) { 
    -sum(dgamma(yy10,scale,rate,shape,log=T)) }
  gum.fit10<-function(scale,location) { 
    -sum(dgumbel(yy10,scale,location,log=T)) }
  wei3.fit11<-function(shape,scale,thres) { 
    -sum(dweibull3(yy11,shape,scale,thres,log=T)) }
  ln3.fit11<-function(shape,scale,thres) { 
    -sum(dlnorm3(yy11,shape,scale,thres,log=T)) }
  pe3.fit11<-function(scale,rate,shape) { 
    -sum(dgamma(yy11,scale,rate,shape,log=T)) }
  gum.fit11<-function(scale,location) { 
    -sum(dgumbel(yy11,scale,location,log=T)) }
  wei3.fit12<-function(shape,scale,thres) { 
    -sum(dweibull3(yy12,shape,scale,thres,log=T)) }
  ln3.fit12<-function(shape,scale,thres) { 
    -sum(dlnorm3(yy12,shape,scale,thres,log=T)) }
  pe3.fit12<-function(scale,rate,shape) { 
    -sum(dgamma(yy12,scale,rate,shape,log=T)) }
  gum.fit12<-function(scale,location) { 
    -sum(dgumbel(yy12,scale,location,log=T)) }
  wei3.fit13<-function(shape,scale,thres) { 
    -sum(dweibull3(yy13,shape,scale,thres,log=T)) }
  ln3.fit13<-function(shape,scale,thres) { 
    -sum(dlnorm3(yy13,shape,scale,thres,log=T)) }
  pe3.fit13<-function(scale,rate,shape) { 
    -sum(dgamma(yy13,scale,rate,shape,log=T)) }
  gum.fit13<-function(scale,location) { 
    -sum(dgumbel(yy13,scale,location,log=T)) }
  wei3.fit14<-function(shape,scale,thres) { 
    -sum(dweibull3(yy14,shape,scale,thres,log=T)) }
  ln3.fit14<-function(shape,scale,thres) { 
    -sum(dlnorm3(yy14,shape,scale,thres,log=T)) }
  pe3.fit14<-function(scale,rate,shape) { 
    -sum(dgamma(yy14,scale,rate,shape,log=T)) }
  gum.fit14<-function(scale,location) { 
    -sum(dgumbel(yy14,scale,location,log=T)) }
  wei3.fit15<-function(shape,scale,thres) { 
    -sum(dweibull3(yy15,shape,scale,thres,log=T)) }
  ln3.fit15<-function(shape,scale,thres) { 
    -sum(dlnorm3(yy15,shape,scale,thres,log=T)) }
  pe3.fit15<-function(scale,rate,shape) { 
    -sum(dgamma(yy15,scale,rate,shape,log=T)) }
  gum.fit15<-function(scale,location) { 
    -sum(dgumbel(yy15,scale,location,log=T)) }
  wei3.fit16<-function(shape,scale,thres) { 
    -sum(dweibull3(yy16,shape,scale,thres,log=T)) }
  ln3.fit16<-function(shape,scale,thres) { 
    -sum(dlnorm3(yy16,shape,scale,thres,log=T)) }
  pe3.fit16<-function(scale,rate,shape) { 
    -sum(dgamma(yy16,scale,rate,shape,log=T)) }
  gum.fit16<-function(scale,location) { 
    -sum(dgumbel(yy16,scale,location,log=T)) }
  wei3.fit17<-function(shape,scale,thres) { 
    -sum(dweibull3(yy17,shape,scale,thres,log=T)) }
  ln3.fit17<-function(shape,scale,thres) { 
    -sum(dlnorm3(yy17,shape,scale,thres,log=T)) }
  pe3.fit17<-function(scale,rate,shape) { 
    -sum(dgamma(yy17,scale,rate,shape,log=T)) }
  gum.fit17<-function(scale,location) { 
    -sum(dgumbel(yy17,scale,location,log=T)) }
  wei3.fit18<-function(shape,scale,thres) { 
    -sum(dweibull3(yy18,shape,scale,thres,log=T)) }
  ln3.fit18<-function(shape,scale,thres) { 
    -sum(dlnorm3(yy18,shape,scale,thres,log=T)) }
  pe3.fit18<-function(scale,rate,shape) { 
    -sum(dgamma(yy18,scale,rate,shape,log=T)) }
  gum.fit18<-function(scale,location) { 
    -sum(dgumbel(yy18,scale,location,log=T)) }
  wei3.fit19<-function(shape,scale,thres) { 
    -sum(dweibull3(yy19,shape,scale,thres,log=T)) }
  ln3.fit19<-function(shape,scale,thres) { 
    -sum(dlnorm3(yy19,shape,scale,thres,log=T)) }
  pe3.fit19<-function(scale,rate,shape) { 
    -sum(dgamma(yy19,scale,rate,shape,log=T)) }
  gum.fit19<-function(scale,location) { 
    -sum(dgumbel(yy19,scale,location,log=T)) }
  wei3.fit20<-function(shape,scale,thres) { 
    -sum(dweibull3(yy20,shape,scale,thres,log=T)) }
  ln3.fit20<-function(shape,scale,thres) { 
    -sum(dlnorm3(yy20,shape,scale,thres,log=T)) }
  pe3.fit20<-function(scale,rate,shape) { 
    -sum(dgamma(yy20,scale,rate,shape,log=T)) }
  gum.fit20<-function(scale,location) { 
    -sum(dgumbel(yy20,scale,location,log=T)) }
  wei3.fit21<-function(shape,scale,thres) { 
    -sum(dweibull3(yy21,shape,scale,thres,log=T)) }
  ln3.fit21<-function(shape,scale,thres) { 
    -sum(dlnorm3(yy21,shape,scale,thres,log=T)) }
  pe3.fit21<-function(scale,rate,shape) { 
    -sum(dgamma(yy21,scale,rate,shape,log=T)) }
  gum.fit21<-function(scale,location) { 
    -sum(dgumbel(yy21,scale,location,log=T)) }
  wei3.fit22<-function(shape,scale,thres) { 
    -sum(dweibull3(yy22,shape,scale,thres,log=T)) }
  ln3.fit22<-function(shape,scale,thres) { 
    -sum(dlnorm3(yy22,shape,scale,thres,log=T)) }
  pe3.fit22<-function(scale,rate,shape) { 
    -sum(dgamma(yy22,scale,rate,shape,log=T)) }
  gum.fit22<-function(scale,location) { 
    -sum(dgumbel(yy22,scale,location,log=T)) }
  wei3.fit23<-function(shape,scale,thres) { 
    -sum(dweibull3(yy23,shape,scale,thres,log=T)) }
  ln3.fit23<-function(shape,scale,thres) { 
    -sum(dlnorm3(yy23,shape,scale,thres,log=T)) }
  pe3.fit23<-function(scale,rate,shape) { 
    -sum(dgamma(yy23,scale,rate,shape,log=T)) }
  gum.fit23<-function(scale,location) { 
    -sum(dgumbel(yy23,scale,location,log=T)) }
  wei3.fit24<-function(shape,scale,thres) { 
    -sum(dweibull3(yy24,shape,scale,thres,log=T)) }
  ln3.fit24<-function(shape,scale,thres) { 
    -sum(dlnorm3(yy24,shape,scale,thres,log=T)) }
  pe3.fit24<-function(scale,rate,shape) { 
    -sum(dgamma(yy24,scale,rate,shape,log=T)) }
  gum.fit24<-function(scale,location) { 
    -sum(dgumbel(yy24,scale,location,log=T)) }
  wei3.fit25<-function(shape,scale,thres) { 
    -sum(dweibull3(yy25,shape,scale,thres,log=T)) }
  ln3.fit25<-function(shape,scale,thres) { 
    -sum(dlnorm3(yy25,shape,scale,thres,log=T)) }
  pe3.fit25<-function(scale,rate,shape) { 
    -sum(dgamma(yy25,scale,rate,shape,log=T)) }
  gum.fit25<-function(scale,location) { 
    -sum(dgumbel(yy25,scale,location,log=T)) }
  wei3.fit26<-function(shape,scale,thres) { 
    -sum(dweibull3(yy26,shape,scale,thres,log=T)) }
  ln3.fit26<-function(shape,scale,thres) { 
    -sum(dlnorm3(yy26,shape,scale,thres,log=T)) }
  pe3.fit26<-function(scale,rate,shape) { 
    -sum(dgamma(yy26,scale,rate,shape,log=T)) }
  gum.fit26<-function(scale,location) { 
    -sum(dgumbel(yy26,scale,location,log=T)) }
  wei3.fit27<-function(shape,scale,thres) { 
    -sum(dweibull3(yy27,shape,scale,thres,log=T)) }
  ln3.fit27<-function(shape,scale,thres) { 
    -sum(dlnorm3(yy27,shape,scale,thres,log=T)) }
  pe3.fit27<-function(scale,rate,shape) { 
    -sum(dgamma(yy27,scale,rate,shape,log=T)) }
  gum.fit27<-function(scale,location) { 
    -sum(dgumbel(yy27,scale,location,log=T)) }
  wei3.fit28<-function(shape,scale,thres) { 
    -sum(dweibull3(yy28,shape,scale,thres,log=T)) }
  ln3.fit28<-function(shape,scale,thres) { 
    -sum(dlnorm3(yy28,shape,scale,thres,log=T)) }
  pe3.fit28<-function(scale,rate,shape) { 
    -sum(dgamma(yy28,scale,rate,shape,log=T)) }
  gum.fit28<-function(scale,location) { 
    -sum(dgumbel(yy28,scale,location,log=T)) }
  wei3.fit29<-function(shape,scale,thres) { 
    -sum(dweibull3(yy29,shape,scale,thres,log=T)) }
  ln3.fit29<-function(shape,scale,thres) { 
    -sum(dlnorm3(yy29,shape,scale,thres,log=T)) }
  pe3.fit29<-function(scale,rate,shape) { 
    -sum(dgamma(yy29,scale,rate,shape,log=T)) }
  gum.fit29<-function(scale,location) { 
    -sum(dgumbel(yy29,scale,location,log=T)) }
  wei3.fit30<-function(shape,scale,thres) { 
    -sum(dweibull3(yy30,shape,scale,thres,log=T)) }
  ln3.fit30<-function(shape,scale,thres) { 
    -sum(dlnorm3(yy30,shape,scale,thres,log=T)) }
  pe3.fit30<-function(scale,rate,shape) { 
    -sum(dgamma(yy30,scale,rate,shape,log=T)) }
  gum.fit30<-function(scale,location) { 
    -sum(dgumbel(yy30,scale,location,log=T)) }
  
  
  ## wei
  wei.est<-pelwei(samlmu(listko[1][[1]][[4]]))
  
  mle.results.wei0<-mle2(wei3.fit0,start=list(shape=1.7,scale=1900,thres=400),data=list(yy0))
  mle.results.wei1<-mle2(wei3.fit1,start=list(shape=1.7,scale=1900,thres=400),data=list(yy1))
  mle.results.wei2<-mle2(wei3.fit2,start=list(shape=1.7,scale=1900,thres=400),data=list(yy2))
  mle.results.wei3<-mle2(wei3.fit3,start=list(shape=1.7,scale=1900,thres=400),data=list(yy3))
  mle.results.wei4<-mle2(wei3.fit4,start=list(shape=1.7,scale=1900,thres=400),data=list(yy4))
  mle.results.wei5<-mle2(wei3.fit5,start=list(shape=1.7,scale=1900,thres=400),data=list(yy5))
  mle.results.wei6<-mle2(wei3.fit6,start=list(shape=1.7,scale=1900,thres=400),data=list(yy6))
  mle.results.wei7<-mle2(wei3.fit7,start=list(shape=1.7,scale=1900,thres=400),data=list(yy7))
  mle.results.wei8<-mle2(wei3.fit8,start=list(shape=1.7,scale=1900,thres=400),data=list(yy8))
  mle.results.wei9<-mle2(wei3.fit9,start=list(shape=1.7,scale=1900,thres=400),data=list(yy9))
  mle.results.wei10<-mle2(wei3.fit10,start=list(shape=1.7,scale=1900,thres=400),data=list(yy10))
  mle.results.wei11<-mle2(wei3.fit11,start=list(shape=1.7,scale=1900,thres=400),data=list(yy11))
  mle.results.wei12<-mle2(wei3.fit12,start=list(shape=1.7,scale=1900,thres=400),data=list(yy12))
  mle.results.wei13<-mle2(wei3.fit13,start=list(shape=1.7,scale=1900,thres=400),data=list(yy13))
  mle.results.wei14<-mle2(wei3.fit14,start=list(shape=1.7,scale=1900,thres=400),data=list(yy14))
  mle.results.wei15<-mle2(wei3.fit15,start=list(shape=1.7,scale=1900,thres=400),data=list(yy15))
  mle.results.wei16<-mle2(wei3.fit16,start=list(shape=1.7,scale=1900,thres=400),data=list(yy16))
  mle.results.wei17<-mle2(wei3.fit17,start=list(shape=1.7,scale=1900,thres=400),data=list(yy17))
  mle.results.wei18<-mle2(wei3.fit18,start=list(shape=1.7,scale=1900,thres=400),data=list(yy18))
  mle.results.wei19<-mle2(wei3.fit19,start=list(shape=1.7,scale=1900,thres=400),data=list(yy19))
  mle.results.wei20<-mle2(wei3.fit20,start=list(shape=1.7,scale=1900,thres=400),data=list(yy20))
  mle.results.wei21<-mle2(wei3.fit21,start=list(shape=1.7,scale=1900,thres=400),data=list(yy21))
  mle.results.wei22<-mle2(wei3.fit22,start=list(shape=1.7,scale=1900,thres=400),data=list(yy22))
  mle.results.wei23<-mle2(wei3.fit23,start=list(shape=1.7,scale=1900,thres=400),data=list(yy23))
  mle.results.wei24<-mle2(wei3.fit24,start=list(shape=1.7,scale=1900,thres=400),data=list(yy24))
  mle.results.wei25<-mle2(wei3.fit25,start=list(shape=1.7,scale=1900,thres=400),data=list(yy25))
  mle.results.wei26<-mle2(wei3.fit26,start=list(shape=1.7,scale=1900,thres=400),data=list(yy26))
  mle.results.wei27<-mle2(wei3.fit27,start=list(shape=1.7,scale=1900,thres=400),data=list(yy27))
  mle.results.wei28<-mle2(wei3.fit28,start=list(shape=1.7,scale=1900,thres=400),data=list(yy28))
  mle.results.wei29<-mle2(wei3.fit29,start=list(shape=1.7,scale=1900,thres=400),data=list(yy29))
  mle.results.wei30<-mle2(wei3.fit30,start=list(shape=1.7,scale=1900,thres=400),data=list(yy30))
  
  y0.wei<-quawei(f=quantile, para=c(mle.results.wei0@details$par[3],mle.results.wei0@details$par[2],mle.results.wei0@details$par[1]))
  y1.wei<-quawei(f=quantile, para=c(mle.results.wei1@details$par[3],mle.results.wei1@details$par[2],mle.results.wei1@details$par[1]))
  y2.wei<-quawei(f=quantile, para=c(mle.results.wei2@details$par[3],mle.results.wei2@details$par[2],mle.results.wei2@details$par[1]))
  y3.wei<-quawei(f=quantile, para=c(mle.results.wei3@details$par[3],mle.results.wei3@details$par[2],mle.results.wei3@details$par[1]))
  y4.wei<-quawei(f=quantile, para=c(mle.results.wei4@details$par[3],mle.results.wei4@details$par[2],mle.results.wei4@details$par[1]))
  y5.wei<-quawei(f=quantile, para=c(mle.results.wei5@details$par[3],mle.results.wei5@details$par[2],mle.results.wei5@details$par[1]))
  y6.wei<-quawei(f=quantile, para=c(mle.results.wei6@details$par[3],mle.results.wei6@details$par[2],mle.results.wei6@details$par[1]))
  y7.wei<-quawei(f=quantile, para=c(mle.results.wei7@details$par[3],mle.results.wei7@details$par[2],mle.results.wei7@details$par[1]))
  y8.wei<-quawei(f=quantile, para=c(mle.results.wei8@details$par[3],mle.results.wei8@details$par[2],mle.results.wei8@details$par[1]))
  y9.wei<-quawei(f=quantile, para=c(mle.results.wei9@details$par[3],mle.results.wei9@details$par[2],mle.results.wei9@details$par[1]))
  y10.wei<-quawei(f=quantile, para=c(mle.results.wei10@details$par[3],mle.results.wei10@details$par[2],mle.results.wei10@details$par[1]))
  y11.wei<-quawei(f=quantile, para=c(mle.results.wei11@details$par[3],mle.results.wei11@details$par[2],mle.results.wei11@details$par[1]))
  y12.wei<-quawei(f=quantile, para=c(mle.results.wei12@details$par[3],mle.results.wei12@details$par[2],mle.results.wei12@details$par[1]))
  y13.wei<-quawei(f=quantile, para=c(mle.results.wei13@details$par[3],mle.results.wei13@details$par[2],mle.results.wei13@details$par[1]))
  y14.wei<-quawei(f=quantile, para=c(mle.results.wei14@details$par[3],mle.results.wei14@details$par[2],mle.results.wei14@details$par[1]))
  y15.wei<-quawei(f=quantile, para=c(mle.results.wei15@details$par[3],mle.results.wei15@details$par[2],mle.results.wei15@details$par[1]))
  y16.wei<-quawei(f=quantile, para=c(mle.results.wei16@details$par[3],mle.results.wei16@details$par[2],mle.results.wei16@details$par[1]))
  y17.wei<-quawei(f=quantile, para=c(mle.results.wei17@details$par[3],mle.results.wei17@details$par[2],mle.results.wei17@details$par[1]))
  y18.wei<-quawei(f=quantile, para=c(mle.results.wei18@details$par[3],mle.results.wei18@details$par[2],mle.results.wei18@details$par[1]))
  y19.wei<-quawei(f=quantile, para=c(mle.results.wei19@details$par[3],mle.results.wei19@details$par[2],mle.results.wei19@details$par[1]))
  y20.wei<-quawei(f=quantile, para=c(mle.results.wei20@details$par[3],mle.results.wei20@details$par[2],mle.results.wei20@details$par[1]))
  y21.wei<-quawei(f=quantile, para=c(mle.results.wei21@details$par[3],mle.results.wei21@details$par[2],mle.results.wei21@details$par[1]))
  y22.wei<-quawei(f=quantile, para=c(mle.results.wei22@details$par[3],mle.results.wei22@details$par[2],mle.results.wei22@details$par[1]))
  y23.wei<-quawei(f=quantile, para=c(mle.results.wei23@details$par[3],mle.results.wei23@details$par[2],mle.results.wei23@details$par[1]))
  y24.wei<-quawei(f=quantile, para=c(mle.results.wei24@details$par[3],mle.results.wei24@details$par[2],mle.results.wei24@details$par[1]))
  y25.wei<-quawei(f=quantile, para=c(mle.results.wei25@details$par[3],mle.results.wei25@details$par[2],mle.results.wei25@details$par[1]))
  y26.wei<-quawei(f=quantile, para=c(mle.results.wei26@details$par[3],mle.results.wei26@details$par[2],mle.results.wei26@details$par[1]))
  y27.wei<-quawei(f=quantile, para=c(mle.results.wei27@details$par[3],mle.results.wei27@details$par[2],mle.results.wei27@details$par[1]))
  y28.wei<-quawei(f=quantile, para=c(mle.results.wei28@details$par[3],mle.results.wei28@details$par[2],mle.results.wei28@details$par[1]))
  y29.wei<-quawei(f=quantile, para=c(mle.results.wei29@details$par[3],mle.results.wei29@details$par[2],mle.results.wei29@details$par[1]))
  y30.wei<-quawei(f=quantile, para=c(mle.results.wei30@details$par[3],mle.results.wei30@details$par[2],mle.results.wei30@details$par[1]))
  
  ## gum
  gum.ext<-pelgum(samlmu(df[[4]]))
  
  
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
  
  
  
  ## log-norm 3 
  mle.results.ln30<-mle2(ln3.fit0,start=list(shape=0.5,scale=7,thres=310),data=list(yy0))
  mle.results.ln31<-mle2(ln3.fit1,start=list(shape=0.5,scale=7,thres=310),data=list(yy1))
  mle.results.ln32<-mle2(ln3.fit2,start=list(shape=0.5,scale=7,thres=310),data=list(yy2))
  mle.results.ln33<-mle2(ln3.fit3,start=list(shape=0.5,scale=7,thres=310),data=list(yy3))
  mle.results.ln34<-mle2(ln3.fit4,start=list(shape=0.5,scale=7,thres=310),data=list(yy4))
  mle.results.ln35<-mle2(ln3.fit5,start=list(shape=0.5,scale=7,thres=310),data=list(yy5))
  mle.results.ln36<-mle2(ln3.fit6,start=list(shape=0.5,scale=7,thres=310),data=list(yy6))
  mle.results.ln37<-mle2(ln3.fit7,start=list(shape=0.5,scale=7,thres=310),data=list(yy7))
  mle.results.ln38<-mle2(ln3.fit8,start=list(shape=0.5,scale=7,thres=310),data=list(yy8))
  mle.results.ln39<-mle2(ln3.fit9,start=list(shape=0.5,scale=7,thres=310),data=list(yy9))
  mle.results.ln310<-mle2(ln3.fit10,start=list(shape=0.5,scale=7,thres=310),data=list(yy10))
  mle.results.ln311<-mle2(ln3.fit11,start=list(shape=0.5,scale=7,thres=310),data=list(yy11))
  mle.results.ln312<-mle2(ln3.fit12,start=list(shape=0.5,scale=7,thres=310),data=list(yy12))
  mle.results.ln313<-mle2(ln3.fit13,start=list(shape=0.5,scale=7,thres=310),data=list(yy13))
  mle.results.ln314<-mle2(ln3.fit14,start=list(shape=0.5,scale=7,thres=310),data=list(yy14))
  mle.results.ln315<-mle2(ln3.fit15,start=list(shape=0.5,scale=7,thres=310),data=list(yy15))
  mle.results.ln316<-mle2(ln3.fit16,start=list(shape=0.5,scale=7,thres=310),data=list(yy16))
  mle.results.ln317<-mle2(ln3.fit17,start=list(shape=0.5,scale=7,thres=310),data=list(yy17))
  mle.results.ln318<-mle2(ln3.fit18,start=list(shape=0.5,scale=7,thres=310),data=list(yy18))
  mle.results.ln319<-mle2(ln3.fit19,start=list(shape=0.5,scale=7,thres=310),data=list(yy19))
  mle.results.ln320<-mle2(ln3.fit20,start=list(shape=0.5,scale=7,thres=310),data=list(yy20))
  mle.results.ln321<-mle2(ln3.fit21,start=list(shape=0.5,scale=7,thres=310),data=list(yy21))
  mle.results.ln322<-mle2(ln3.fit22,start=list(shape=0.5,scale=7,thres=310),data=list(yy22))
  mle.results.ln323<-mle2(ln3.fit23,start=list(shape=0.5,scale=7,thres=310),data=list(yy23))
  mle.results.ln324<-mle2(ln3.fit24,start=list(shape=0.5,scale=7,thres=310),data=list(yy24))
  mle.results.ln325<-mle2(ln3.fit25,start=list(shape=0.5,scale=7,thres=310),data=list(yy25))
  mle.results.ln326<-mle2(ln3.fit26,start=list(shape=0.5,scale=7,thres=310),data=list(yy26))
  mle.results.ln327<-mle2(ln3.fit27,start=list(shape=0.5,scale=7,thres=310),data=list(yy27))
  mle.results.ln328<-mle2(ln3.fit28,start=list(shape=0.5,scale=7,thres=310),data=list(yy28))
  mle.results.ln329<-mle2(ln3.fit29,start=list(shape=0.5,scale=7,thres=310),data=list(yy29))
  mle.results.ln330<-mle2(ln3.fit30,start=list(shape=0.5,scale=7,thres=310),data=list(yy30))
  
  y0.ln3<-qualn3(f=quantile, para=c(mle.results.ln30@details$par[1],log(mle.results.ln30@details$par[3]),log(mle.results.ln30@details$par[2])))
  y1.ln3<-qualn3(f=quantile, para=c(mle.results.ln31@details$par[1],log(mle.results.ln31@details$par[3]),log(mle.results.ln31@details$par[2])))
  y2.ln3<-qualn3(f=quantile, para=c(mle.results.ln32@details$par[1],log(mle.results.ln32@details$par[3]),log(mle.results.ln32@details$par[2])))
  y3.ln3<-qualn3(f=quantile, para=c(mle.results.ln33@details$par[1],log(mle.results.ln33@details$par[3]),log(mle.results.ln33@details$par[2])))
  y4.ln3<-qualn3(f=quantile, para=c(mle.results.ln34@details$par[1],log(mle.results.ln34@details$par[3]),log(mle.results.ln34@details$par[2])))
  y5.ln3<-qualn3(f=quantile, para=c(mle.results.ln35@details$par[1],log(mle.results.ln35@details$par[3]),log(mle.results.ln35@details$par[2])))
  y6.ln3<-qualn3(f=quantile, para=c(mle.results.ln36@details$par[1],log(mle.results.ln36@details$par[3]),log(mle.results.ln36@details$par[2])))
  y7.ln3<-qualn3(f=quantile, para=c(mle.results.ln37@details$par[1],log(mle.results.ln37@details$par[3]),log(mle.results.ln37@details$par[2])))
  y8.ln3<-qualn3(f=quantile, para=c(mle.results.ln38@details$par[1],log(mle.results.ln38@details$par[3]),log(mle.results.ln38@details$par[2])))
  y9.ln3<-qualn3(f=quantile, para=c(mle.results.ln39@details$par[1],log(mle.results.ln39@details$par[3]),log(mle.results.ln39@details$par[2])))
  y10.ln3<-qualn3(f=quantile, para=c(mle.results.ln310@details$par[1],log(mle.results.ln310@details$par[3]),log(mle.results.ln310@details$par[2])))
  y11.ln3<-qualn3(f=quantile, para=c(mle.results.ln311@details$par[1],log(mle.results.ln311@details$par[3]),log(mle.results.ln311@details$par[2])))
  y12.ln3<-qualn3(f=quantile, para=c(mle.results.ln312@details$par[1],log(mle.results.ln312@details$par[3]),log(mle.results.ln312@details$par[2])))
  y13.ln3<-qualn3(f=quantile, para=c(mle.results.ln313@details$par[1],log(mle.results.ln313@details$par[3]),log(mle.results.ln313@details$par[2])))
  y14.ln3<-qualn3(f=quantile, para=c(mle.results.ln314@details$par[1],log(mle.results.ln314@details$par[3]),log(mle.results.ln314@details$par[2])))
  y15.ln3<-qualn3(f=quantile, para=c(mle.results.ln315@details$par[1],log(mle.results.ln315@details$par[3]),log(mle.results.ln315@details$par[2])))
  y16.ln3<-qualn3(f=quantile, para=c(mle.results.ln316@details$par[1],log(mle.results.ln316@details$par[3]),log(mle.results.ln316@details$par[2])))
  y17.ln3<-qualn3(f=quantile, para=c(mle.results.ln317@details$par[1],log(mle.results.ln317@details$par[3]),log(mle.results.ln317@details$par[2])))
  y18.ln3<-qualn3(f=quantile, para=c(mle.results.ln318@details$par[1],log(mle.results.ln318@details$par[3]),log(mle.results.ln318@details$par[2])))
  y19.ln3<-qualn3(f=quantile, para=c(mle.results.ln319@details$par[1],log(mle.results.ln319@details$par[3]),log(mle.results.ln319@details$par[2])))
  y20.ln3<-qualn3(f=quantile, para=c(mle.results.ln320@details$par[1],log(mle.results.ln320@details$par[3]),log(mle.results.ln320@details$par[2])))
  y21.ln3<-qualn3(f=quantile, para=c(mle.results.ln321@details$par[1],log(mle.results.ln321@details$par[3]),log(mle.results.ln321@details$par[2])))
  y22.ln3<-qualn3(f=quantile, para=c(mle.results.ln322@details$par[1],log(mle.results.ln322@details$par[3]),log(mle.results.ln322@details$par[2])))
  y23.ln3<-qualn3(f=quantile, para=c(mle.results.ln323@details$par[1],log(mle.results.ln323@details$par[3]),log(mle.results.ln323@details$par[2])))
  y24.ln3<-qualn3(f=quantile, para=c(mle.results.ln324@details$par[1],log(mle.results.ln324@details$par[3]),log(mle.results.ln324@details$par[2])))
  y25.ln3<-qualn3(f=quantile, para=c(mle.results.ln325@details$par[1],log(mle.results.ln325@details$par[3]),log(mle.results.ln325@details$par[2])))
  y26.ln3<-qualn3(f=quantile, para=c(mle.results.ln326@details$par[1],log(mle.results.ln326@details$par[3]),log(mle.results.ln326@details$par[2])))
  y27.ln3<-qualn3(f=quantile, para=c(mle.results.ln327@details$par[1],log(mle.results.ln327@details$par[3]),log(mle.results.ln327@details$par[2])))
  y28.ln3<-qualn3(f=quantile, para=c(mle.results.ln328@details$par[1],log(mle.results.ln328@details$par[3]),log(mle.results.ln328@details$par[2])))
  y29.ln3<-qualn3(f=quantile, para=c(mle.results.ln329@details$par[1],log(mle.results.ln329@details$par[3]),log(mle.results.ln329@details$par[2])))
  y30.ln3<-qualn3(f=quantile, para=c(mle.results.ln330@details$par[1],log(mle.results.ln330@details$par[3]),log(mle.results.ln320@details$par[2])))
  
  ## Pearson3 (via gamma)
  
  mle.results.pe30<-mle2(pe3.fit0,start=list(shape=3,scale=500),data=list(yy0))
  mle.results.pe31<-mle2(pe3.fit1,start=list(shape=3,scale=500),data=list(yy1))
  mle.results.pe32<-mle2(pe3.fit2,start=list(shape=3,scale=500),data=list(yy2))
  mle.results.pe33<-mle2(pe3.fit3,start=list(shape=3,scale=500),data=list(yy3))
  mle.results.pe34<-mle2(pe3.fit4,start=list(shape=3,scale=500),data=list(yy4))
  mle.results.pe35<-mle2(pe3.fit5,start=list(shape=3,scale=500),data=list(yy5))
  mle.results.pe36<-mle2(pe3.fit6,start=list(shape=3,scale=500),data=list(yy6))
  mle.results.pe37<-mle2(pe3.fit7,start=list(shape=3,scale=500),data=list(yy7))
  mle.results.pe38<-mle2(pe3.fit8,start=list(shape=3,scale=500),data=list(yy8))
  mle.results.pe39<-mle2(pe3.fit9,start=list(shape=3,scale=500),data=list(yy9))
  mle.results.pe310<-mle2(pe3.fit10,start=list(shape=3,scale=500),data=list(yy10))
  mle.results.pe311<-mle2(pe3.fit11,start=list(shape=3,scale=500),data=list(yy11))
  mle.results.pe312<-mle2(pe3.fit12,start=list(shape=3,scale=500),data=list(yy12))
  mle.results.pe313<-mle2(pe3.fit13,start=list(shape=3,scale=500),data=list(yy13))
  mle.results.pe314<-mle2(pe3.fit14,start=list(shape=3,scale=500),data=list(yy14))
  mle.results.pe315<-mle2(pe3.fit15,start=list(shape=3,scale=500),data=list(yy15))
  mle.results.pe316<-mle2(pe3.fit16,start=list(shape=3,scale=500),data=list(yy16))
  mle.results.pe317<-mle2(pe3.fit17,start=list(shape=3,scale=500),data=list(yy17))
  mle.results.pe318<-mle2(pe3.fit18,start=list(shape=3,scale=500),data=list(yy18))
  mle.results.pe319<-mle2(pe3.fit19,start=list(shape=3,scale=500),data=list(yy19))
  mle.results.pe320<-mle2(pe3.fit20,start=list(shape=3,scale=500),data=list(yy20))
  mle.results.pe321<-mle2(pe3.fit21,start=list(shape=3,scale=500),data=list(yy21))
  mle.results.pe322<-mle2(pe3.fit22,start=list(shape=3,scale=500),data=list(yy22))
  mle.results.pe323<-mle2(pe3.fit23,start=list(shape=3,scale=500),data=list(yy23))
  mle.results.pe324<-mle2(pe3.fit24,start=list(shape=3,scale=500),data=list(yy24))
  mle.results.pe325<-mle2(pe3.fit25,start=list(shape=3,scale=500),data=list(yy25))
  mle.results.pe326<-mle2(pe3.fit26,start=list(shape=3,scale=500),data=list(yy26))
  mle.results.pe327<-mle2(pe3.fit27,start=list(shape=3,scale=500),data=list(yy27))
  mle.results.pe328<-mle2(pe3.fit28,start=list(shape=3,scale=500),data=list(yy28))
  mle.results.pe329<-mle2(pe3.fit29,start=list(shape=3,scale=500),data=list(yy29))
  mle.results.pe330<-mle2(pe3.fit30,start=list(shape=3,scale=500),data=list(yy30))
  
  y0.pe3<-quagam(f=quantile, para=c(mle.results.pe30@coef[1],mle.results.pe30@coef[2]))
  y1.pe3<-quagam(f=quantile, para=c(mle.results.pe31@coef[1],mle.results.pe31@coef[2]))
  y2.pe3<-quagam(f=quantile, para=c(mle.results.pe32@coef[1],mle.results.pe32@coef[2]))
  y3.pe3<-quagam(f=quantile, para=c(mle.results.pe33@coef[1],mle.results.pe33@coef[2]))
  y4.pe3<-quagam(f=quantile, para=c(mle.results.pe34@coef[1],mle.results.pe34@coef[2]))
  y5.pe3<-quagam(f=quantile, para=c(mle.results.pe35@coef[1],mle.results.pe35@coef[2]))
  y6.pe3<-quagam(f=quantile, para=c(mle.results.pe36@coef[1],mle.results.pe36@coef[2]))
  y7.pe3<-quagam(f=quantile, para=c(mle.results.pe37@coef[1],mle.results.pe37@coef[2]))
  y8.pe3<-quagam(f=quantile, para=c(mle.results.pe38@coef[1],mle.results.pe38@coef[2]))
  y9.pe3<-quagam(f=quantile, para=c(mle.results.pe39@coef[1],mle.results.pe39@coef[2]))
  y10.pe3<-quagam(f=quantile, para=c(mle.results.pe310@coef[1],mle.results.pe310@coef[2]))
  y11.pe3<-quagam(f=quantile, para=c(mle.results.pe311@coef[1],mle.results.pe311@coef[2]))
  y12.pe3<-quagam(f=quantile, para=c(mle.results.pe312@coef[1],mle.results.pe312@coef[2]))
  y13.pe3<-quagam(f=quantile, para=c(mle.results.pe313@coef[1],mle.results.pe313@coef[2]))
  y14.pe3<-quagam(f=quantile, para=c(mle.results.pe314@coef[1],mle.results.pe314@coef[2]))
  y15.pe3<-quagam(f=quantile, para=c(mle.results.pe315@coef[1],mle.results.pe315@coef[2]))
  y16.pe3<-quagam(f=quantile, para=c(mle.results.pe316@coef[1],mle.results.pe316@coef[2]))
  y17.pe3<-quagam(f=quantile, para=c(mle.results.pe317@coef[1],mle.results.pe317@coef[2]))
  y18.pe3<-quagam(f=quantile, para=c(mle.results.pe318@coef[1],mle.results.pe318@coef[2]))
  y19.pe3<-quagam(f=quantile, para=c(mle.results.pe319@coef[1],mle.results.pe319@coef[2]))
  y20.pe3<-quagam(f=quantile, para=c(mle.results.pe320@coef[1],mle.results.pe320@coef[2]))
  y21.pe3<-quagam(f=quantile, para=c(mle.results.pe321@coef[1],mle.results.pe321@coef[2]))
  y22.pe3<-quagam(f=quantile, para=c(mle.results.pe322@coef[1],mle.results.pe322@coef[2]))
  y23.pe3<-quagam(f=quantile, para=c(mle.results.pe323@coef[1],mle.results.pe323@coef[2]))
  y24.pe3<-quagam(f=quantile, para=c(mle.results.pe324@coef[1],mle.results.pe324@coef[2]))
  y25.pe3<-quagam(f=quantile, para=c(mle.results.pe325@coef[1],mle.results.pe325@coef[2]))
  y26.pe3<-quagam(f=quantile, para=c(mle.results.pe326@coef[1],mle.results.pe326@coef[2]))
  y27.pe3<-quagam(f=quantile, para=c(mle.results.pe327@coef[1],mle.results.pe327@coef[2]))
  y28.pe3<-quagam(f=quantile, para=c(mle.results.pe328@coef[1],mle.results.pe328@coef[2]))
  y29.pe3<-quagam(f=quantile, para=c(mle.results.pe329@coef[1],mle.results.pe329@coef[2]))
  y30.pe3<-quagam(f=quantile, para=c(mle.results.pe330@coef[1],mle.results.pe330@coef[2]))
  
  
  
  wei<-cbind(y0.wei,y1.wei,y2.wei,y3.wei,y4.wei,y5.wei,y6.wei,y7.wei,y8.wei,y9.wei,y10.wei,y11.wei,y12.wei,y13.wei,y14.wei,y15.wei,y16.wei,y17.wei,y18.wei,y19.wei,y20.wei,y21.wei,y22.wei,y23.wei,y24.wei,y25.wei,y26.wei,y27.wei,y28.wei,y29.wei,y30.wei)  
  gum<-cbind(y0.gum,y1.gum,y2.gum,y3.gum,y4.gum,y5.gum,y6.gum,y7.gum,y8.gum,y9.gum,y10.gum,y11.gum,y12.gum,y13.gum,y14.gum,y15.gum,y16.gum,y17.gum,y18.gum,y19.gum,y20.gum,y21.gum,y22.gum,y23.gum,y24.gum,y25.gum,y26.gum,y27.gum,y28.gum,y29.gum,y30.gum)
  ln3<-cbind(y0.ln3,y1.ln3,y2.ln3,y3.ln3,y4.ln3,y5.ln3,y6.ln3,y7.ln3,y8.ln3,y9.ln3,y10.ln3,y11.ln3,y12.ln3,y13.ln3,y14.ln3,y15.ln3,y16.ln3,y17.ln3,y18.ln3,y19.ln3,y20.ln3,y21.ln3,y22.ln3,y23.ln3,y24.ln3,y25.ln3,y26.ln3,y27.ln3,y28.ln3,y29.ln3,y30.ln3)
  pe3<-cbind(y0.pe3,y1.pe3,y2.pe3,y3.pe3,y4.pe3,y5.pe3,y6.pe3,y7.pe3,y8.pe3,y9.pe3,y10.pe3,y11.pe3,y12.pe3,y13.pe3,y14.pe3,y15.pe3,y16.pe3,y17.pe3,y18.pe3,y19.pe3,y20.pe3,y21.pe3,y22.pe3,y23.pe3,y24.pe3,y25.pe3,y26.pe3,y27.pe3,y28.pe3,y29.pe3,y30.pe3)
  return(list(wei,gum,ln3,pe3))
}


### MLE -> Leider zu wenig n (bei einem Shift von 30 Jahre viel zu wenig)
for (i in seq_len(nSubsets)){
    rowsToGrab<-seq(i*365.25-365.25,(365.25*i),1)
    outList[[i]]<-df[[4]][rowsToGrab]
    maxOL[i]<-max(outList[[i]],na.rm=T)
  } 

### Funktioniert mit MLE, start Wert sind immer etwas fragwürdig, aber, wie auch im "Leitfade" angesrochen, bei nur 60-Werten ist der
### Fehler massiv hoch. Nicht brauchbar.

ll.wei<-function(shape,scale,thres){
  R=suppressWarnings(dweibull3(maxOL,shape,scale,thres))
  -sum(log(R))
}
mle.wei<-mle(ll.wei,start=list(shape=1,scale=10,thres=1))

ll.gum<-function(scale,location){
  R=suppressWarnings(dgumbel(maxOL,scale,location))
  -sum(log(R))
}
mle.gum<-mle(ll.gum,start=list(scale=10,location=1))

ll.pe3<-function(shape,location,scale){
  R=suppressWarnings(dpearsonIII(maxOL,shape,location,scale))
  -sum(log(R))
}
mle.pe3<-mle(ll.pe3,start=list(shape=1,location=1,scale=10))
    
ll.ln3<-function(shape,scale,thres){
  R=suppressWarnings(dlnorm3(maxOL,shape,scale,thres))
  -sum(log(R))
}
mle.ln3<-mle(ll.ln3,start=list(shape=1,scale=1,thres=1))


quawei(f = 0.99,para = c(location=mle.wei@coef[3],scale=mle.wei@coef[2],shape=mle.wei@coef[1]))
quawei(f = 0.99,para = c(pelwei(lmom = samlmu(maxOL))))

quagum(f = 0.99,para = c(location=mle.gum@coef[2],scale=mle.gum@coef[1]))
quagum(f = 0.99,para = c(pelgum(lmom = samlmu(maxOL))))

quape3(f = 0.99,para = c(location=mle.pe3@coef[2],scale=mle.pe3@coef[3],shape=mle.pe3@coef[1]))
quape3(f = 0.99,para = c(pelpe3(lmom = samlmu(maxOL))))

qualn3(f = 0.99,para = c(zeta=log(mle.ln3@coef[1]),mu=log(mle.ln3@coef[2]),sigma=log(mle.ln3@coef[3])))
qualn3(f = 0.99,para = c(pelln3(lmom = samlmu(maxOL))))
