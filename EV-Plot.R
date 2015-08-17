#### EV Plot Function

par(mfrow=(c(2,2)))




# ys0
evplot(yy0)
xlm.wei<-pelwei(samlmu(yy0))
evdistq(quawei, xlm.wei,type="l",col="blue")
quawei(f=0.99, para=xlm.wei)
xlm.gum<-pelgum(samlmu(yy0))
evdistq(quagum, xlm.gum,type="l",col="green")
quagum(f=0.99, para=xlm.gum)
xlm.pe3<-pelpe3(samlmu(yy0))
evdistq(quape3, xlm.pe3,type="l",col="red")
quape3(f=0.99, para=xlm.pe3)
xlm.ln3<-pelln3(samlmu(yy0))
evdistq(qualn3, xlm.ln3,type="l",col="gray")
qualn3(f=0.99, para=xlm.ln3)
# ys1
evplot(yy1)
xlm.wei<-pelwei(samlmu(yy1))
evdistq(quawei, xlm.wei,type="l",col="blue")
quawei(f=0.99, para=xlm.wei)
xlm.gum<-pelgum(samlmu(yy1))
evdistq(quagum, xlm.gum,type="l",col="green")
quagum(f=0.99, para=xlm.gum)
xlm.pe3<-pelpe3(samlmu(yy1))
evdistq(quape3, xlm.pe3,type="l",col="red")
quape3(f=0.99, para=xlm.pe3)
xlm.ln3<-pelln3(samlmu(yy1))
evdistq(qualn3, xlm.ln3,type="l",col="gray")
qualn3(f=0.99, para=xlm.ln3)
# ys2
evplot(yy2)
xlm.wei<-pelwei(samlmu(yy2))
evdistq(quawei, xlm.wei,type="l",col="blue")
quawei(f=0.99, para=xlm.wei)
xlm.gum<-pelgum(samlmu(yy2))
evdistq(quagum, xlm.gum,type="l",col="green")
quagum(f=0.99, para=xlm.gum)
xlm.pe3<-pelpe3(samlmu(yy2))
evdistq(quape3, xlm.pe3,type="l",col="red")
quape3(f=0.99, para=xlm.pe3)
xlm.ln3<-pelln3(samlmu(yy2))
evdistq(qualn3, xlm.ln3,type="l",col="gray")
qualn3(f=0.99, para=xlm.ln3)
# ys3
evplot(yy3)
xlm.wei<-pelwei(samlmu(yy3))
evdistq(quawei, xlm.wei,type="l",col="blue")
quawei(f=0.99, para=xlm.wei)
xlm.gum<-pelgum(samlmu(yy3))
evdistq(quagum, xlm.gum,type="l",col="green")
quagum(f=0.99, para=xlm.gum)
xlm.pe3<-pelpe3(samlmu(yy3))
evdistq(quape3, xlm.pe3,type="l",col="red")
quape3(f=0.99, para=xlm.pe3)
xlm.ln3<-pelln3(samlmu(yy3))
evdistq(qualn3, xlm.ln3,type="l",col="gray")
qualn3(f=0.99, para=xlm.ln3)
dev.off()
### ys9

evplot(listko[2][[1]][[4]][ys9])

# weibull y9
xlm.wei<-pelwei(samlmu(listko[2][[1]][[4]][ys9]))
evdistq(quawei, xlm.wei,type="l",col="blue")
quawei(f=0.99, para=pelwei(samlmu(listko[2][[1]][[4]][ys9])))
#wei.sample <- quawei(prob<-runif(5000), para = xlm.wei)
#evpoints(wei.sample, col="green", pch="*")
# gumbel y9
xlm.gum<-pelgum(samlmu(listko[2][[1]][[4]][ys9]))
evdistq(quagum, xlm.gum,type="l",col="green")
quagum(f=0.99, para=pelgum(samlmu(listko[2][[1]][[4]][ys9])))
#gum.sample <- quagum(prob<-runif(5000), para = xlm.gum)
#evpoints(gum.sample, col="green", pch="*")
# pearson y9
xlm.pe3<-pelpe3(samlmu(listko[2][[1]][[4]][ys9]))
evdistq(quape3, xlm.pe3,type="l",col="red")
quape3(f=0.99, para=pelpe3(samlmu(listko[2][[1]][[4]][ys9])))
#pe3.sample <- quape3(prob<-runif(5000), para = xlm.pe3)
#evpoints(pe3.sample, col="green", pch="*")
# pearson y9
xlm.ln3<-pelln3(samlmu(listko[2][[1]][[4]][ys9]))
evdistq(qualn3, xlm.ln3,type="l",col="gray")
qualn3(f=0.99, para=pelln3(samlmu(listko[2][[1]][[4]][ys9])))
#ln3.sample <- qualn3(prob<-runif(5000), para = xlm.ln3)
#evpoints(ln3.sample, col="green", pch="*")

evWeiPl<-function(df,year,quantile,names,ally,allyn,func){
  xy<-seq15y(df,year)
  evplot(ally,main=paste("EV-Plot, Quantiles (",quantile,")",",",year,"yrs"),lwd=2,lty=2,type="l",col=1)
  #legend("topleft",inset=.005,title="Functions",c("EV","Wei","Gum","Pe3","Ln3"),fill=c(1:5),horiz=T)
  mtext(names, side=3, line=3,las=0, col="seagreen")
  
  if(func=="weibull")
    {
    xlm.wei<-pelwei(samlmu(xy$yy0))
    evdistq(quawei, xlm.wei,type="l",col=2)
    quawei(f=quantile, para=xlm.wei)
    xlm.wei<-pelwei(samlmu(xy$yy3))
    evdistq(quawei, xlm.wei,type="l",col=3)
    quawei(f=quantile, para=xlm.wei)
    xlm.wei<-pelwei(samlmu(xy$yy6))
    evdistq(quawei, xlm.wei,type="l",col=4)
    quawei(f=quantile, para=xlm.wei)
    xlm.wei<-pelwei(samlmu(xy$yy9))
    evdistq(quawei, xlm.wei,type="l",col=5)
    quawei(f=quantile, para=xlm.wei)
    xlm.wei<-pelwei(samlmu(xy$yy12))
    evdistq(quawei, xlm.wei,type="l",col=6)
    quawei(f=quantile, para=xlm.wei)
    xlm.wei<-pelwei(samlmu(xy$yy15))
    evdistq(quawei, xlm.wei,type="l",col=7)
    quawei(f=quantile, para=xlm.wei)
    legend("topleft",inset=.005,title="Year-Shift",c("yy0","yy3","yy6","yy9","yy12","yy15"),fill=c(2:7),horiz=T)
  } 
  else 
    { 
      if(func=="gumbel")
        {
    xlm.gum<-pelgum(samlmu(xy$yy0))
    evdistq(quagum, xlm.gum,type="l",col=2)
    quagum(f=quantile, para=xlm.gum)
    xlm.gum<-pelgum(samlmu(xy$yy3))
    evdistq(quagum, xlm.gum,type="l",col=3)
    quagum(f=quantile, para=xlm.gum)
    xlm.gum<-pelgum(samlmu(xy$yy6))
    evdistq(quagum, xlm.gum,type="l",col=4)
    quagum(f=quantile, para=xlm.gum)
    xlm.gum<-pelgum(samlmu(xy$yy9))
    evdistq(quagum, xlm.gum,type="l",col=5)
    quagum(f=quantile, para=xlm.gum)
    xlm.gum<-pelgum(samlmu(xy$yy12))
    evdistq(quagum, xlm.gum,type="l",col=6)
    quagum(f=quantile, para=xlm.gum)
    xlm.gum<-pelgum(samlmu(xy$yy15))
    evdistq(quagum, xlm.gum,type="l",col=6)
    quagum(f=quantile, para=xlm.gum)
    legend("topleft",inset=.005,title="Year-Shift",c("yy0","yy3","yy6","yy9","yy12","yy15"),fill=c(2:7),horiz=T)
        }
   
  else 
    {
    if(func=="person")
      {
      xlm.pe3<-pelpe3(samlmu(xy$yy0))
      evdistq(quape3, xlm.pe3,type="l",col=2)
      quape3(f=quantile, para=xlm.pe3)
      xlm.pe3<-pelpe3(samlmu(xy$yy3))
      evdistq(quape3, xlm.pe3,type="l",col=3)
      quape3(f=quantile, para=xlm.pe3)
      xlm.pe3<-pelpe3(samlmu(xy$yy6))
      evdistq(quape3, xlm.pe3,type="l",col=4)
      quape3(f=quantile, para=xlm.pe3)
      xlm.pe3<-pelpe3(samlmu(xy$yy9))
      evdistq(quape3, xlm.pe3,type="l",col=5)
      quape3(f=quantile, para=xlm.pe3)
      xlm.pe3<-pelpe3(samlmu(xy$yy12))
      evdistq(quape3, xlm.pe3,type="l",col=6)
      quape3(f=quantile, para=xlm.pe3)
      xlm.pe3<-pelpe3(samlmu(xy$yy15))
      evdistq(quape3, xlm.pe3,type="l",col=6)
      quape3(f=quantile, para=xlm.pe3)
      legend("topleft",inset=.005,title="Year-Shift",c("yy0","yy3","yy6","yy9","yy12","yy15"),fill=c(2:7),horiz=T)
      }
      
   else {
      if(func=="lognorm")
        {
        xlm.ln3<-pelln3(samlmu(xy$yy0))
        evdistq(qualn3, xlm.ln3,type="l",col=2)
        qualn3(f=quantile, para=xlm.ln3)
        xlm.ln3<-pelln3(samlmu(xy$yy3))
        evdistq(qualn3, xlm.ln3,type="l",col=3)
        qualn3(f=quantile, para=xlm.ln3)
        xlm.ln3<-pelln3(samlmu(xy$yy6))
        evdistq(qualn3, xlm.ln3,type="l",col=4)
        qualn3(f=quantile, para=xlm.ln3)
        xlm.ln3<-pelln3(samlmu(xy$yy9))
        evdistq(qualn3, xlm.ln3,type="l",col=5)
        qualn3(f=quantile, para=xlm.ln3)
        xlm.ln3<-pelln3(samlmu(xy$yy12))
        evdistq(qualn3, xlm.ln3,type="l",col=6)
        qualn3(f=quantile, para=xlm.ln3)
        xlm.ln3<-pelln3(samlmu(xy$yy15))
        evdistq(qualn3, xlm.ln3,type="l",col=7)
        qualn3(f=quantile, para=xlm.ln3)
        legend("topleft",inset=.005,title="Year-Shift",c("yy0","yy3","yy6","yy9","yy12","yy15"),fill=c(2:7),horiz=T)
      }
    else {
     if(func=="all")
     {
      xlm.wei<-pelwei(samlmu(ally))
      evdistq(quawei, xlm.wei,type="l",col=2)
      quawei(f=quantile, para=xlm.wei)
      
      xlm.gum<-pelgum(samlmu(ally))
      evdistq(quagum, xlm.gum,type="l",col=3)
      quagum(f=quantile, para=xlm.gum)
      
      xlm.pe3<-pelpe3(samlmu(ally))
      evdistq(quape3, xlm.pe3,type="l",col=4)
      quape3(f=quantile, para=xlm.pe3)
      
      xlm.ln3<-pelln3(samlmu(ally))
      evdistq(qualn3, xlm.ln3,type="l",col=5)
      qualn3(f=quantile, para=xlm.ln3)
      legend("topleft",inset=.005,title=paste("Functions",",",allyn),c("EV","Wei","Gum","Pe3","Ln3"),fill=c(1:5),horiz=T)
      }
    }
    }  
    } 
  }
}

f<-lapply(listko[1],year=30,quantile=0.90,names=list(names(la)[1:4]),func="all",ally=(yyall$yy13),allyn="yy13",evWeiPl)
par(mfrow=(c(2,2)))
dev.off()


yy0<-listko[1][[1]][[4]][ys0]
yy1<-listko[1][[1]][[4]][ys1]
yy2<-listko[1][[1]][[4]][ys2]
yy3<-listko[1][[1]][[4]][ys3]
yy4<-listko[1][[1]][[4]][ys4]
yy5<-listko[1][[1]][[4]][ys5]
yy6<-listko[1][[1]][[4]][ys6]
yy7<-listko[1][[1]][[4]][ys7]
yy8<-listko[1][[1]][[4]][ys8]
yy9<-listko[1][[1]][[4]][ys9]
yy10<-listko[1][[1]][[4]][ys10]
yy11<-listko[1][[1]][[4]][ys11]
yy12<-listko[1][[1]][[4]][ys12]
yy13<-listko[1][[1]][[4]][ys13]
yy14<-listko[1][[1]][[4]][ys14]
yy15<-listko[1][[1]][[4]][ys15]
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
yy15<-df[[4]][ys15] 
