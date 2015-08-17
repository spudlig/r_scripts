@@ -1,1863 +0,0 @@
install.packages("fBasics")
install.packages("dplyr")
install.packages("lmom")
install.packages("ismev")
install.packages("survival")
install.packages("MASS")
library(ismev) 
library(fBasics)
library(dplyr)
library(lmom)
library(survival)
library(gmm)
library(MASS)
### Einlesen, und in eine formatierte Liste bringen
setwd("./Studium/Watermanagement/Masterarbeit/data/y120/")
setwd("./Studium/Watermanagement/Masterarbeit/data/y120/")

# Vom Directory in eine Liste
path<-"./"
file_dir<-dir(path,pattern=".day.txt")
readt<-function(path){
  tab<-read.table(path,sep=";",dec=".",header=F)
  return(tab)
}
readTable<-function(file_dir){
  table<-lapply(file_dir,readt)
  return(table)
}

??xpand.grid()
expand.grid( year= seq(1, 365, 1), weight = seq(100, 300, 50),
            sex = c("Male","Female"))


largeList<-readTable(file_dir)

# Rename 
nam<-list.files("./",pattern=".day.txt")
nl<-sub(".day.txt","", nam,ignore.case = T)
namList<-grep("[0-9]{7}",nl,value=T)
namList<-paste("a",namList,sep="")
names(largeList)[c(1:length(file_dir))]<-namList


# Die DF in der Liste rechentauglich machen
get.matrix<-function(table){
  data<-table[[4]]
  date<-as.Date(table[[1]], "%Y-%m-%d")
  year<-format(date,format="%Y")
  month<-format(date,format="%m")
  day<-format(date,format="%d")
  mat<-data.frame(year,month,day,data)
  return(mat)
}
formList<-lapply(largeList[c(1:length(file_dir))],get.matrix)

### nur noch ohne -999 -> sind genau 9 (bei y120)
kickoutnine<-function(df){
  if(min(df[[4]])=="-999"){
    df=NULL
  } 
  return(df)
}
lltry<-lapply(formList[1:length(file_dir)],kickoutnine)
listko<-lltry[!sapply(lltry, is.null)]

### Statisitsche Kenngrößen

stat.kenn<-function(df){
  mean<-mean(as.numeric(df[[4]]))
  std<-sd(as.numeric(df[[4]]))
  vari<-var(as.numeric(df[[4]]))
  skew<-skewness(as.numeric(df[[4]]))
  kurt<-kurtosis(as.numeric(df[[4]]))
  matr<-data.frame(mean,std,vari,skew,kurt)
  return(matr)
}
dfStatKenn<-lapply(formList[c(1:length(file_dir))],stat.kenn)

## l-moments
# y10 Sequenzen
y10Fun<-function()
y0<-365*40+11
y1<-1*365+365*40+10
y2<-2*365+365*40+10
y3<-3*365+365*40+10
y4<-4*365+365*40+10
y5<-5*365+365*40+10
y6<-6*365+365*40+10
y7<-7*365+365*40+10
y8<-8*365+365*40+10
y9<-9*365+365*40+10
ys0<-seq(1,y0,1)
ys1<-seq(1*365,y1,1)
ys2<-seq(2*365,y2,1)
ys3<-seq(3*365,y3,1)
ys4<-seq(4*365,y4,1)
ys5<-seq(5*365,y5,1)
ys6<-seq(6*365,y6,1)
ys7<-seq(7*365,y7,1)
ys8<-seq(8*365,y8,1)
ys9<-seq(9*365,y9,1)

# gumbel
lgum.y10<-function(df){
  cg0<-(cdfgum(df[[4]][ys0],para=(pelgum(samlmu(df[[4]][ys0])))))
  cg1<-(cdfgum(df[[4]][ys1],para=(pelgum(samlmu(df[[4]][ys1])))))
  cg2<-(cdfgum(df[[4]][ys2],para=(pelgum(samlmu(df[[4]][ys2])))))
  cg3<-(cdfgum(df[[4]][ys3],para=(pelgum(samlmu(df[[4]][ys3])))))
  cg4<-(cdfgum(df[[4]][ys4],para=(pelgum(samlmu(df[[4]][ys4])))))
  cg5<-(cdfgum(df[[4]][ys5],para=(pelgum(samlmu(df[[4]][ys5])))))
  cg6<-(cdfgum(df[[4]][ys6],para=(pelgum(samlmu(df[[4]][ys6])))))
  cg7<-(cdfgum(df[[4]][ys7],para=(pelgum(samlmu(df[[4]][ys7])))))
  cg8<-(cdfgum(df[[4]][ys8],para=(pelgum(samlmu(df[[4]][ys8])))))
  cg9<-(cdfgum(df[[4]][ys9],para=(pelgum(samlmu(df[[4]][ys9])))))
  return(list(cg0,cg1,cg2,cg3,cg4,cg5,cg6,cg7,cg8,cg9))
}
lgum<-lapply(listko[1:9],lgum.y10)
# weibull
lwei.y10<-function(df){
  cg0<-(cdfwei(df[[4]][ys0],para=(pelwei(samlmu(df[[4]][ys0])))))
  cg1<-(cdfwei(df[[4]][ys1],para=(pelwei(samlmu(df[[4]][ys1])))))
  cg2<-(cdfwei(df[[4]][ys2],para=(pelwei(samlmu(df[[4]][ys2])))))
  cg3<-(cdfwei(df[[4]][ys3],para=(pelwei(samlmu(df[[4]][ys3])))))
  cg4<-(cdfwei(df[[4]][ys4],para=(pelwei(samlmu(df[[4]][ys4])))))
  cg5<-(cdfwei(df[[4]][ys5],para=(pelwei(samlmu(df[[4]][ys5])))))
  cg6<-(cdfwei(df[[4]][ys6],para=(pelwei(samlmu(df[[4]][ys6])))))
  cg7<-(cdfwei(df[[4]][ys7],para=(pelwei(samlmu(df[[4]][ys7])))))
  cg8<-(cdfwei(df[[4]][ys8],para=(pelwei(samlmu(df[[4]][ys8])))))
  cg9<-(cdfwei(df[[4]][ys9],para=(pelwei(samlmu(df[[4]][ys9])))))
  return(list(cg0,cg1,cg2,cg3,cg4,cg5,cg6,cg7,cg8,cg9))
}
lwei<-lapply(listko[1:9],year=30,quantile=0.99,lwei.y10)
listko

yy0<-listko[1][[1]][[4]][ys0]
yy1<-listko[1][[1]][[4]][ys1]
yy2<-listko[1][[1]][[4]][ys2]
yy3<-listko[1][[1]][[4]][ys3]
yy4<-listko[1][[1]][[4]][ys4]
yy5<-listko[1][[1]][[4]][ys5]
yy6<-listko[1][[1]][[4]][ys6]
yy7<-listko[1][[1]][[4]][ys7]
yy8<-listko[1][[1]][[4]][ys8]

# Show sampling uncertainty
wei.sample <- quawei(prob<-runif(5000), para = xlm.wei)
evpoints(wei.sample, col="green", pch="*")

###### Parameters EV

param<-function(df,quantile,year){
  y0<-365*year+11
  y1<-1*365+365*year+10
  y2<-2*365+365*year+10
  y3<-3*365+365*year+10
  y4<-4*365+365*year+10
  y5<-5*365+365*year+10
  y6<-6*365+365*year+10
  y7<-7*365+365*year+10
  y8<-8*365+365*year+10
  y9<-9*365+365*year+10
  y10<-10*365+365*year+10
  y11<-11*365+365*year+10
  y12<-12*365+365*year+10
  y13<-13*365+365*year+10
  y14<-14*365+365*year+10
  y15<-15*365+365*year+10
  ys0<-seq(1,y0,1)
  ys1<-seq(1*365,y1,1)
  ys2<-seq(2*365,y2,1)
  ys3<-seq(3*365,y3,1)
  ys4<-seq(4*365,y4,1)
  ys5<-seq(5*365,y5,1)
  ys6<-seq(6*365,y6,1)
  ys7<-seq(7*365,y7,1)
  ys8<-seq(8*365,y8,1)
  ys9<-seq(9*365,y9,1)
  ys10<-seq(10*365,y10,1)
  ys11<-seq(11*365,y11,1)
  ys12<-seq(12*365,y12,1)
  ys13<-seq(13*365,y13,1)
  ys14<-seq(14*365,y14,1)
  ys15<-seq(15*365,y15,1)
  
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

y0.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys0])))
y0.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys0])))
y0.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys0])))
y0.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys0])))

y1.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys1])))
y1.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys1])))
y1.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys1])))
y1.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys1])))

y2.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys2])))
y2.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys2])))
y2.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys2])))
y2.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys2])))

y3.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys3])))
y3.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys3])))
y3.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys3])))
y3.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys3])))

y4.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys4])))
y4.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys4])))
y4.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys4])))
y4.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys4])))

y5.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys5])))
y5.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys5])))
y5.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys5])))
y5.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys5])))

y6.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys6])))
y6.gum<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys6])))
y6.pe3<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys6])))
y6.ln3<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys6])))

y7.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys7])))
y7.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys7])))
y7.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys7])))
y7.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys7])))

y8.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys8])))
y8.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys8])))
y8.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys8])))
y8.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys8])))

y9.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys0])))
y9.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys0])))
y9.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys0])))
y9.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys0])))

y10.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys0])))
y10.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys0])))
y10.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys0])))
y10.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys0])))

y11.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys0])))
y11.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys0])))
y11.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys0])))
y11.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys0])))

y12.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys0])))
y12.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys0])))
y12.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys0])))
y12.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys0])))

y13.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys0])))
y13.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys0])))
y13.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys0])))
y13.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys0])))

y14.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys0])))
y14.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys0])))
y14.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys0])))
y14.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys0])))

y15.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys0])))
y15.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys0])))
y15.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys0])))
y15.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys0])))

wei<-cbind(y0.wei,y1.wei,y2.wei,y3.wei,y4.wei,y5.wei,y6.wei,y7.wei,y8.wei,y9.wei,y10.wei,y11.wei,y12.wei,y13.wei,y14.wei,y14.wei)
gum<-cbind(y0.gum,y1.gum,y2.gum,y3.gum,y4.gum,y5.gum,y6.gum,y7.gum,y8.gum,y9.gum,y10.gum,y11.gum,y12.gum,y13.gum,y14.gum,y15.gum)
pe3<-cbind(y0.pe3,y1.pe3,y2.pe3,y3.pe3,y4.pe3,y5.pe3,y6.pe3,y7.pe3,y8.pe3,y9.pe3,y10.pe3,y11.pe3,y12.pe3,y13.pe3,y14.pe3,y15.pe3)
ln3<-cbind(y0.ln3,y1.ln3,y2.ln3,y3.ln3,y4.ln3,y5.ln3,y6.ln3,y7.ln3,y8.ln3,y9.ln3,y10.ln3,y11.ln3,y12.ln3,y13.ln3,y14.ln3,y15.ln3)

return(list(wei,gum,pe3,ln3))

}
la<-lapply(listko[c(1:9)],quantile=0.99,param,year=20)

ploti<-function(df){
  ylim.max<-max(cbind(df[[1]],df[[2]],df[[3]],df[[4]]))*1.15
  ylim.min<-min(cbind(df[[1]],df[[2]],df[[3]],df[[4]]))
  wei<-as.numeric(df[[1]])
  gum<-as.numeric(df[[2]])
  pe3<-as.numeric(df[[3]])
  ln3<-as.numeric(df[[4]])
  plot(wei,type="l",ylim=c(ylim.min,ylim.max),lty=2,col=1,xlab = "Year-Shift",
       ylab="Discharge",main=dput(names(df)))
  legend("topright",inset=.005,title="Functions",c("Wei","Gum","Pe3","Ln3"),fill=c(1:4),horiz=T)
  lines(gum,lty=3,col=2m)
  lines(pe3,lty=4,col=3)
  lines(ln3,lty=5,col=4)
  abline(h = seq(1,10000,100),v=c(1:15),lwd=0.5)
  points()
}
names<-as.list(as.list(dput(names(la))))

par(mfrow=c(2,2))
lapply(la[c(1:4)],ploti)
dev.off()
ylim.max<-max(cbind(la[[1]][[1]],la[[1]][[2]],la[[1]][[3]],la[[1]][[4]]))*1.05
ylim.min<-min(cbind(la[[1]][[1]],la[[1]][[2]],la[[1]][[3]],la[[1]][[4]]))*0.95



wei<-as.numeric(la[[1]][[1]])
gum<-as.numeric(la[[1]][[2]])
pe3<-as.numeric(la[[1]][[3]])
ln3<-as.numeric(la[[1]][[4]])
plot(wei,type="l",ylim=c(ylim.min,ylim.max),lty=2,col=1,xlab = "Year-Shift",
     ylab="Discharge",main=dput(names(la))[1])
lines(gum,lty=3,col=2)
lines(pe3,lty=4,col=3)
lines(ln3,lty=5,col=4)
legend("topright",inset=.05,title="Functions",c("Wei","Gum","Pe3","Ln3"),fill=c(1:4),horiz=T)
abline(h = seq(1,10000,100),v=c(1:15),lwd=0.5)

#### MXL
fm.gev <- gum.fit(listko[1][[1]][[4]])
pelwei(samlmu(listko[1][[1]][[4]]))
fm.gev
?gev.fit()
?ismev
yx<-(fitdistr(listko[1][[1]][[4]], densfun="gumbel", lower = 0))
yx[1][[1]][[1]]
yx
###### EV Plot

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

evplot(listko[1][[1]][[4]][ys2])
# weibull y9
xlm.wei<-pelwei(samlmu(listko[3][[1]][[4]][ys9]))
evdistq(quawei, xlm.wei,type="l",col="blue")
quawei(f=0.90, para=pelwei(samlmu(listko[3][[1]][[4]][ys1])))
#wei.sample <- quawei(prob<-runif(5000), para = xlm.wei)
#evpoints(wei.sample, col="green", pch="*")
# gumbel y9
xlm.gum<-pelgum(samlmu(listko[2][[1]][[4]][ys9]))
evdistq(quagum, xlm.gum,type="l",col="green")
quagum(f=0.90, para=pelgum(samlmu(listko[3][[1]][[4]][ys1])))
#gum.sample <- quagum(prob<-runif(5000), para = xlm.gum)
#evpoints(gum.sample, col="green", pch="*")
# pearson y9
xlm.pe3<-pelpe3(samlmu(listko[2][[1]][[4]][ys9]))
evdistq(quape3, xlm.pe3,type="l",col="red")
quape3(f=0.90, para=pelpe3(samlmu(listko[3][[1]][[4]][ys1])))
#pe3.sample <- quape3(prob<-runif(5000), para = xlm.pe3)
#evpoints(pe3.sample, col="green", pch="*")
# pearson y9
xlm.ln3<-pelln3(samlmu(listko[2][[1]][[4]][ys9]))
evdistq(qualn3, xlm.ln3,type="l",col="gray")
qualn3(f=0.99, para=pelln3(samlmu(listko[2][[1]][[4]][ys9])))
#ln3.sample <- qualn3(prob<-runif(5000), para = xlm.ln3)
#evpoints(ln3.sample, col="green", pch="*")

plot9<-function(df){
  evplot(listko[[4]][ys1])
  yy0<-listko[[4]][ys0]
  yy1<-listko[[4]][ys1]
  yy2<-listko[[4]][ys2]
  yy3<-listko[[4]][ys3]
  yy4<-listko[[4]][ys4]
  yy5<-listko[[4]][ys5]
  yy6<-listko[[4]][ys6]
  yy7<-listko[[4]][ys7]
  yy8<-listko[[4]][ys8]
  yy9<-listko[[4]][ys9]
  # weibull y0
  xlm.wei<-pelwei(samlmu(yy0))
  a<-evdistq(quawei, xlm.wei,type="l",col="blue")
  quawei(f=0.99, para=xlm.wei)
  
  xlm.gum<-pelgum(samlmu(yy0))
  b<-evdistq(quagum, xlm.gum,type="l",col="green")
  quagum(f=0.99, para=xlm.gum)
  
  xlm.pe3<-pelpe3(samlmu(yy0))
  c<-evdistq(quape3, xlm.pe3,type="l",col="red")
  quape3(f=0.99, para=xlm.pe3)
  
  xlm.ln3<-pelln3(samlmu(yy0)
  d<-evdistq(qualn3, xlm.ln3,type="l",col="gray")
  qualn3(f=0.99, para=xlm.ln3)
}
f<-lapply(listko[1],plot)

# mean als matrix (funktioniert)
trytry<-function(df){
  mean<-mean(as.numeric(df[[4]]))
  matr<-mean
  return(matr)
}
dfS<-(lapply(formList[c(1:length(file_dir))],trytry))
a<-as.matrix(dfS[c(1:14)])
plot(a,type = "lines")


y0<-365*1+11
y1<-1*365+365*10+10
y2<-2*365+365*30+10
y3<-3*365+365*30+10
y4<-4*365+365*30+10
y5<-5*365+365*30+10
y6<-6*365+365*6+10
y7<-7*365+365*7+10
y8<-8*365+365*8+10
y9<-9*365+365*9+10
y10<-10*365+365*year+10
y11<-11*365+365*year+10
y12<-12*365+365*year+10
y13<-13*365+365*30+10
y14<-14*365+365*30+10
y15<-15*365+365*30+10
ys0<-seq(1,y0,1)
ys1<-seq(1*365,y1,1)
ys2<-seq(3*365,y2,1)
ys3<-seq(5*365,y3,1)
ys4<-seq(7*365,y4,1)
ys5<-seq(9*365,y5,1)
ys6<-seq(6*365,y6,1)
ys7<-seq(7*365,y7,1)
ys8<-seq(8*365,y8,1)
ys9<-seq(9*365,y9,1)
ys10<-seq(10*365,y10,1)
ys11<-seq(11*365,y11,1)
ys12<-seq(12*365,y12,1)
ys13<-seq(13*365,y13,1)
ys14<-seq(y13,y14,1)
ys15<-seq(y14,y15,1)

year=20
y0<-seq(1,30*365+11,1)
ys1<-seq(1*365,(30*365+10+1*365),1)
ys2<-seq(2*365,(30*365+10+2*365),1)
ys3<-seq(3*365,(year*365+10+3*365),1)
ys4<-seq(4*365,(year*365+10+4*365),1)
ys5<-seq(5*365,(year*365+10+5*365),1)
ys6<-seq(6*365,(year*365+10+6*365),1)
ys7<-seq(7*365,(year*365+10+7*365),1)
ys8<-seq(8*365,(year*365+10+8*365),1)
ys9<-seq(9*365,(year*365+10+9*365),1)
ys10<-seq(10*365,(30*365+10+10*365),1)
ys11<-seq(11*1365,(year*365+10+11*365),1)
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
ys30<-seq(30*365,(30*365+10+30*365),1)

??mle2()

y0.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys0])))
y0.gum<-quagum(f=quantile, para=q.ml.gum0)
y0.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys0])))
y0.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys0])))

y1.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys1])))
y1.gum<-quagum(f=quantile, para=q.ml.gum1)
y1.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys1])))
y1.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys1])))

y2.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys2])))
y2.gum<-quagum(f=quantile, para=q.ml.gum2)
y2.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys2])))
y2.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys2])))

y3.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys3])))
y3.gum<-quagum(f=quantile, para=q.ml.gum3)
y3.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys3])))
y3.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys3])))

y4.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys4])))
y4.gum<-quagum(f=quantile, para=q.ml.gum4)
y4.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys4])))
y4.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys4])))

y5.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys5])))
y5.gum<-quagum(f=quantile, para=q.ml.gum5)
y5.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys5])))
y5.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys5])))

y6.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys6])))
y6.gum<-quawei(f=quantile, para=q.ml.gum6)
y6.pe3<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys6])))
y6.ln3<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys6])))

y7.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys7])))
y7.gum<-quagum(f=quantile, para=q.ml.gum7)
y7.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys7])))
y7.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys7])))

y8.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys8])))
y8.gum<-quagum(f=quantile, para=q.ml.gum8)
y8.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys8])))
y8.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys8])))

y9.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys9])))
y9.gum<-quagum(f=quantile, para=q.ml.gum9)
y9.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys9])))
y9.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys9])))

y10.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys10])))
y10.gum<-quagum(f=quantile, para=q.ml.gum10)
y10.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys10])))
y10.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys10])))

y11.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys11])))
y11.gum<-quagum(f=quantile, para=q.ml.gum11)
y11.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys11])))
y11.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys11])))

y12.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys12])))
y12.gum<-quagum(f=quantile, para=q.ml.gum12)
y12.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys12])))
y12.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys12])))

y13.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys13])))
y13.gum<-quagum(f=quantile, para=q.ml.gum13)
y13.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys13])))
y13.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys13])))

y14.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys14])))
y14.gum<-quagum(f=quantile, para=q.ml.gum14)
y14.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys14])))
y14.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys14])))

y15.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys15])))
y15.gum<-quagum(f=quantile, para=q.ml.gum15)
y15.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys15])))
y15.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys15])))

y16.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys16])))
y16.gum<-quagum(f=quantile, para=q.ml.gum16)
y16.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys16])))
y16.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys16])))

y17.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys17])))
y17.gum<-quagum(f=quantile, para=q.ml.gum17)
y17.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys17])))
y17.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys17])))

y18.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys18])))
y18.gum<-quagum(f=quantile, para=q.ml.gum18)
y18.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys18])))
y18.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys18])))

y19.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys19])))
y19.gum<-quagum(f=quantile, para=q.ml.gum19)
y19.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys19])))
y19.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys19])))

y20.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys20])))
y20.gum<-quagum(f=quantile, para=q.ml.gum20)
y20.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys20])))
y20.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys20])))

y21.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys21])))
y21.gum<-quagum(f=quantile, para=q.ml.gum21)
y21.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys21])))
y21.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys21])))

y22.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys22])))
y22.gum<-quagum(f=quantile, para=q.ml.gum22)
y22.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys22])))
y22.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys22])))

y23.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys23])))
y23.gum<-quagum(f=quantile, para=q.ml.gum23)
y23.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys23])))
y23.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys23])))

y24.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys24])))
y24.gum<-quagum(f=quantile, para=q.ml.gum24)
y24.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys24])))
y24.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys24])))

y25.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys25])))
y25.gum<-quagum(f=quantile, para=q.ml.gum25)
y25.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys25])))
y25.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys25])))

y26.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys26])))
y26.gum<-quagum(f=quantile, para=q.ml.gum26)
y26.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys26])))
y26.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys26])))

y27.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys27])))
y27.gum<-quagum(f=quantile, para=q.ml.gum27)
y27.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys27])))
y27.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys27])))

y28.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys28])))
y28.gum<-quagum(f=quantile, para=q.ml.gum28)
y28.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys28])))
y28.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys28])))

y29.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys29])))
y29.gum<-quagum(f=quantile, para=q.ml.gum29)
y29.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys29])))
y29.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys29])))

y30.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys30])))
y30.gum<-quagum(f=quantile, para=q.ml.gum30)
y30.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys30])))
y30.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys30])))

wei<-cbind(y0.wei,y1.wei,y2.wei,y3.wei,y4.wei,y5.wei,y6.wei,y7.wei,y8.wei,y9.wei,y10.wei,y11.wei,y12.wei,y13.wei,y14.wei,y15.wei,y16.wei,y17.wei,y18.wei,y19.wei,y20.wei,y21.wei,y22.wei,y23.wei,y24.wei,y25.wei,y26.wei,y27.wei,y28.wei,y29.wei,y30.wei)
gum<-cbind(y0.gum,y1.gum,y2.gum,y3.gum,y4.gum,y5.gum,y6.gum,y7.gum,y8.gum,y9.gum,y10.gum,y11.gum,y12.gum,y13.gum,y14.gum,y15.gum,y16.gum,y17.gum,y18.gum,y19.gum,y20.gum,y21.gum,y22.gum,y23.gum,y24.gum,y25.gum,y26.gum,y27.gum,y28.gum,y29.gum,y30.gum)
pe3<-cbind(y0.pe3,y1.pe3,y2.pe3,y3.pe3,y4.pe3,y5.pe3,y6.pe3,y7.pe3,y8.pe3,y9.pe3,y10.pe3,y11.pe3,y12.pe3,y13.pe3,y14.pe3,y15.pe3,y16.pe3,y17.pe3,y18.pe3,y19.pe3,y20.pe3,y21.pe3,y22.pe3,y23.pe3,y24.pe3,y25.pe3,y26.pe3,y27.pe3,y28.pe3,y29.pe3,y30.pe3)
ln3<-cbind(y0.ln3,y1.ln3,y2.ln3,y3.ln3,y4.ln3,y5.ln3,y6.ln3,y7.ln3,y8.ln3,y9.ln3,y10.ln3,y11.ln3,y12.ln3,y13.ln3,y14.ln3,y15.ln3,y16.ln3,y17.ln3,y18.ln3,y19.ln3,y20.ln3,y21.ln3,y22.ln3,y23.ln3,y24.ln3,y25.ln3,y26.ln3,y27.ln3,y28.ln3,y29.ln3,y30.ln3)

return(list(wei,gum,pe3,ln3))

}

q.ml.gum0<-gum.fit(yy0)
q.ml.gum1<-gum.fit(yy1)
q.ml.gum2<-gum.fit(yy2)
q.ml.gum3<-gum.fit(yy3)
q.ml.gum4<-gum.fit(yy4)
q.ml.gum5<-gum.fit(yy5)
q.ml.gum6<-gum.fit(yy6)
q.ml.gum7<-gum.fit(yy7)
q.ml.gum8<-gum.fit(yy8)
q.ml.gum9<-gum.fit(yy9)
q.ml.gum10<-gum.fit(yy10)
q.ml.gum11<-gum.fit(yy11)
q.ml.gum12<-gum.fit(yy12)
q.ml.gum13<-gum.fit(yy13)
q.ml.gum14<-gum.fit(yy14)
q.ml.gum15<-gum.fit(yy15)
q.ml.gum16<-gum.fit(yy16)
q.ml.gum17<-gum.fit(yy17)
q.ml.gum18<-gum.fit(yy18)
q.ml.gum19<-gum.fit(yy19)
q.ml.gum20<-gum.fit(yy20)
q.ml.gum21<-gum.fit(yy21)
q.ml.gum22<-gum.fit(yy22)
q.ml.gum23<-gum.fit(yy23)
q.ml.gum24<-gum.fit(yy24)
q.ml.gum25<-gum.fit(yy25)
q.ml.gum26<-gum.fit(yy26)
q.ml.gum27<-gum.fit(yy27)
q.ml.gum28<-gum.fit(yy28)
q.ml.gum29<-gum.fit(yy29)
q.ml.gum30<-gum.fit(yy30)





evplot(listko[1][[1]][[4]],main=paste("EV-Plot"),lwd=3,type="l",lty=2,col=1)

gum.mle<-gum.fit(listko[1][[1]][[4]])
gu<-c(gum.mle$mle[1],gum.mle$mle[2])
evdistq(quagum, gu,type="l",col=2)

wei3.fit0<-function(shape,scale,thres) { 
  -sum(dweibull3(listko[1][[1]][[4]],shape,scale,thres,log=T)) }
wei.mle<-mle2(wei3.fit0,start=list(shape=1.7,scale=1900,thres=400),data=list(listko[1][[1]][[4]]))
w<-c(wei.mle@coef[3],wei.mle@coef[2],wei.mle@coef[1])
evdistq(quawei, w,type="l",col=1)

ln3.fit0<-function(shape,scale,thres) { 
  -sum(dlnorm3(listko[1][[1]][[4]],shape,scale,thres,log=T)) }
ln3.mle<-mle2(ln3.fit0,start=list(shape=0.5,scale=7,thres=310),data=list(listko[1][[1]][[4]]))
ln<-c(ln3.mle@coef[3],ln3.mle@coef[2],ln3.mle@coef[1])
evdistq(qualn3, ln,type="l",col=4)

pe3.fit0<-function(scale,rate,shape) { 
  -sum(dgamma(listko[1][[1]][[4]],scale,rate,shape,log=T)) }
pe3.mle<-mle2(pe3.fit0,start=list(shape=3,scale=500),data=list(listko[1][[1]][[4]]))
pe<-c(1,pe3.mle@coef[2],pe3.mle@coef[1])
evdistq(quape3, para=pe,type="l",col=3)

gum.ext<-pelgum(samlmu(df[[4]]))


mle.results.gum0<-mle2(gum.fit0,start=list(scale=gum.ext[1]*0.96,location=gum.ext[2]*0.96),data=list(yy0))
mle.results.gum1<-mle2(gum.fit1,start=list(scale=gum.ext[1]*0.96,location=gum.ext[2]*0.96),data=list(yy1))
mle.results.gum2<-mle2(gum.fit2,start=list(scale=gum.ext[1]*0.96,location=gum.ext[2]*0.96),data=list(yy2))
mle.results.gum3<-mle2(gum.fit3,start=list(scale=gum.ext[1]*0.96,location=gum.ext[2]*0.96),data=list(yy3))
mle.results.gum4<-mle2(gum.fit4,start=list(scale=gum.ext[1]*0.96,location=gum.ext[2]*0.96),data=list(yy4))
mle.results.gum5<-mle2(gum.fit5,start=list(scale=gum.ext[1]*0.96,location=gum.ext[2]*0.96),data=list(yy5))
mle.results.gum6<-mle2(gum.fit6,start=list(scale=gum.ext[1]*0.96,location=gum.ext[2]*0.96),data=list(yy6))
mle.results.gum7<-mle2(gum.fit7,start=list(scale=gum.ext[1]*0.96,location=gum.ext[2]*0.96),data=list(yy7))
mle.results.gum8<-mle2(gum.fit8,start=list(scale=gum.ext[1]*0.96,location=gum.ext[2]*0.96),data=list(yy8))
mle.results.gum9<-mle2(gum.fit9,start=list(scale=gum.ext[1]*0.96,location=gum.ext[2]*0.96),data=list(yy9))
mle.results.gum10<-mle2(gum.fit10,start=list(scale=gum.ext[1]*0.96,location=gum.ext[2]*0.96),data=list(yy10))
mle.results.gum11<-mle2(gum.fit11,start=list(scale=gum.ext[1]*0.96,location=gum.ext[2]*0.96),data=list(yy11))
mle.results.gum12<-mle2(gum.fit12,start=list(scale=gum.ext[1]*0.96,location=gum.ext[2]*0.96),data=list(yy12))
mle.results.gum13<-mle2(gum.fit13,start=list(scale=gum.ext[1]*0.96,location=gum.ext[2]*0.96),data=list(yy13))
mle.results.gum14<-mle2(gum.fit14,start=list(scale=gum.ext[1]*0.96,location=gum.ext[2]*0.96),data=list(yy14))
mle.results.gum15<-mle2(gum.fit15,start=list(scale=gum.ext[1]*0.96,location=gum.ext[2]*0.96),data=list(yy15))
mle.results.gum16<-mle2(gum.fit16,start=list(scale=gum.ext[1]*0.96,location=gum.ext[2]*0.96),data=list(yy16))
mle.results.gum17<-mle2(gum.fit17,start=list(scale=gum.ext[1]*0.96,location=gum.ext[2]*0.96),data=list(yy17))
mle.results.gum18<-mle2(gum.fit18,start=list(scale=gum.ext[1]*0.96,location=gum.ext[2]*0.96),data=list(yy18))
mle.results.gum19<-mle2(gum.fit19,start=list(scale=gum.ext[1]*0.96,location=gum.ext[2]*0.96),data=list(yy19))
mle.results.gum20<-mle2(gum.fit20,start=list(scale=gum.ext[1]*0.96,location=gum.ext[2]*0.96),data=list(yy20))
mle.results.gum21<-mle2(gum.fit21,start=list(scale=gum.ext[1]*0.96,location=gum.ext[2]*0.96),data=list(yy21))
mle.results.gum22<-mle2(gum.fit22,start=list(scale=gum.ext[1]*0.96,location=gum.ext[2]*0.96),data=list(yy22))
mle.results.gum23<-mle2(gum.fit23,start=list(scale=gum.ext[1]*0.96,location=gum.ext[2]*0.96),data=list(yy23))
mle.results.gum24<-mle2(gum.fit24,start=list(scale=gum.ext[1]*0.96,location=gum.ext[2]*0.96),data=list(yy24))
mle.results.gum25<-mle2(gum.fit25,start=list(scale=gum.ext[1]*0.96,location=gum.ext[2]*0.96),data=list(yy25))
mle.results.gum26<-mle2(gum.fit26,start=list(scale=gum.ext[1]*0.96,location=gum.ext[2]*0.96),data=list(yy26))
mle.results.gum27<-mle2(gum.fit27,start=list(scale=gum.ext[1]*0.96,location=gum.ext[2]*0.96),data=list(yy27))
mle.results.gum28<-mle2(gum.fit28,start=list(scale=gum.ext[1]*0.96,location=gum.ext[2]*0.96),data=list(yy28))
mle.results.gum29<-mle2(gum.fit29,start=list(scale=gum.ext[1]*0.96,location=gum.ext[2]*0.96),data=list(yy29))
mle.results.gum30<-mle2(gum.fit30,start=list(scale=gum.ext[1]*0.96,location=gum.ext[2]*0.96),data=list(yy30))

y0.gum<-quagum(f=quantile, para=c(mle.results.gum0@details$par[2],mle.results.gum0@details$par[1]))
y1.gum<-quagum(f=quantile, para=c(mle.results.gum1@details$par[2],mle.results.gum1@details$par[1]))
y2.gum<-quagum(f=quantile, para=c(mle.results.gum2@details$par[2],mle.results.gum2@details$par[1]))
y3.gum<-quagum(f=quantile, para=c(mle.results.gum3@details$par[2],mle.results.gum3@details$par[1]))
y4.gum<-quagum(f=quantile, para=c(mle.results.gum4@details$par[2],mle.results.gum4@details$par[1]))
y5.gum<-quagum(f=quantile, para=c(mle.results.gum5@details$par[2],mle.results.gum5@details$par[1]))
y6.gum<-quagum(f=quantile, para=c(mle.results.gum6@details$par[2],mle.results.gum6@details$par[1]))
y7.gum<-quagum(f=quantile, para=c(mle.results.gum7@details$par[2],mle.results.gum7@details$par[1]))
y8.gum<-quagum(f=quantile, para=c(mle.results.gum8@details$par[2],mle.results.gum8@details$par[1]))
y9.gum<-quagum(f=quantile, para=c(mle.results.gum9@details$par[2],mle.results.gum9@details$par[1]))
y10.gum<-quagum(f=quantile, para=c(mle.results.gum10@details$par[2],mle.results.gum10@details$par[1]))
y11.gum<-quagum(f=quantile, para=c(mle.results.gum11@details$par[2],mle.results.gum11@details$par[1]))
y12.gum<-quagum(f=quantile, para=c(mle.results.gum12@details$par[2],mle.results.gum12@details$par[1]))
y13.gum<-quagum(f=quantile, para=c(mle.results.gum13@details$par[2],mle.results.gum13@details$par[1]))
y14.gum<-quagum(f=quantile, para=c(mle.results.gum14@details$par[2],mle.results.gum14@details$par[1]))
y15.gum<-quagum(f=quantile, para=c(mle.results.gum15@details$par[2],mle.results.gum15@details$par[1]))
y16.gum<-quagum(f=quantile, para=c(mle.results.gum16@details$par[2],mle.results.gum16@details$par[1]))
y17.gum<-quagum(f=quantile, para=c(mle.results.gum17@details$par[2],mle.results.gum17@details$par[1]))
y18.gum<-quagum(f=quantile, para=c(mle.results.gum18@details$par[2],mle.results.gum18@details$par[1]))
y19.gum<-quagum(f=quantile, para=c(mle.results.gum19@details$par[2],mle.results.gum19@details$par[1]))
y20.gum<-quagum(f=quantile, para=c(mle.results.gum20@details$par[2],mle.results.gum20@details$par[1]))
y21.gum<-quagum(f=quantile, para=c(mle.results.gum21@details$par[2],mle.results.gum21@details$par[1]))
y22.gum<-quagum(f=quantile, para=c(mle.results.gum22@details$par[2],mle.results.gum22@details$par[1]))
y23.gum<-quagum(f=quantile, para=c(mle.results.gum23@details$par[2],mle.results.gum23@details$par[1]))
y24.gum<-quagum(f=quantile, para=c(mle.results.gum24@details$par[2],mle.results.gum24@details$par[1]))
y25.gum<-quagum(f=quantile, para=c(mle.results.gum25@details$par[2],mle.results.gum25@details$par[1]))
y26.gum<-quagum(f=quantile, para=c(mle.results.gum26@details$par[2],mle.results.gum26@details$par[1]))
y27.gum<-quagum(f=quantile, para=c(mle.results.gum27@details$par[2],mle.results.gum27@details$par[1]))
y28.gum<-quagum(f=quantile, para=c(mle.results.gum28@details$par[2],mle.results.gum28@details$par[1]))
y29.gum<-quagum(f=quantile, para=c(mle.results.gum29@details$par[2],mle.results.gum29@details$par[1]))
y30.gum<-quagum(f=quantile, para=c(mle.results.gum30@details$par[2],mle.results.gum30@details$par[1]))

#### Versuch Punkte in Plot

lapply(la[1],names=list(names(la)[1]),quantile=0.90,years=30,ploti)
points(df,quantile,tolerance = 0.005)

la[[1]]==quantile(listko[1][[1]][[4]],c(0.9))



dev.off()
  evplot(listko[1][[1]][[4]],main=paste("EV-Plot", "MLE"),lwd=3,lty=2,type="l",col=1)
  mtext(names(listko[1]), side=3, line=3,las=0, col="seagreen")
  
  gum.mle<-gum.fit(listko[1][[1]][[4]][yyall$yy0])
  gu<-c(gum.mle$mle[1],gum.mle$mle[2])
  evdistq(quagum, gu,type="l",col=2)
  
  wei3.fit0<-function(shape,scale,thres) { 
    -sum(dweibull3(listko[1][[1]][[4]][yyall$yy0],shape,scale,thres,log=T)) }
  wei.mle<-mle2(wei3.fit0,start=list(shape=1.7,scale=1900,thres=400),data=list(listko[1][[1]][[4]][yyall$yy0]))
  w<-c(wei.mle@coef[3],wei.mle@coef[2],wei.mle@coef[1])
  evdistq(quawei, w,type="l",col=1)

  ln3.fit0<-function(shape,scale,thres) { 
    -sum(dlnorm3(listko[1][[1]][[4]][yyall$yy0],shape,scale,thres,log=T)) }
  ln3.mle<-mle2(ln3.fit0,start=list(shape=0.5,scale=7,thres=310),data=list(listko[1][[1]][[4]][yyall$yy0]))
  ln<-c(ln3.mle@coef[3],ln3.mle@coef[2],ln3.mle@coef[1])
  evdistq(qualn3, ln,type="l",col=4)
  ln3.mle@coef[3]
  ?qualn3()
  pe3.fit0<-function(scale,rate,shape) { 
    -sum(dgamma(listko[1][[1]][[4]][yyall$yy0],scale,rate,shape,log=T)) }
  pe3.mle<-mle2(pe3.fit0,start=list(shape=2,scale=300),data=list(listko[1][[1]][[4]][yyall$yy0]))
  pe<-c(shape=pe3.mle@coef[2],scale=pe3.mle@coef[1])
  evdistq(quagam,pe,type="l",col=3)
  ?quagam()
(pe3.mle)
  legend("topleft",inset=.005,title=paste("Functions"),c("EV","Wei","Gum","Pe3","Ln3"),fill=c(1,1,2,3,4),horiz=T)
}

a<-expand.grid(Function = c("Wei", "Gum", "Pe3","Ln3"), Year = seq(1,30,1),Value_Cal = "value",Value_Obs="V")


beer2 <- window(listko[1][[1]][[4]],start=1992,end=2006-.1)
beer2
meanf(beer2,h1l)
cor(listi$LN3,listi$Obs)



y30.wei<-quawei(f=0.9, para=pelwei(samlmu(listko[1][[1]][[4]])))
y30.gum<-quagum(f=0.9, para=pelgum(samlmu(listko[1][[1]][[4]])))
y30.pe3<-quape3(f=0.9, para=pelpe3(samlmu(listko[1][[1]][[4]])))
y30.ln3<-qualn3(f=0.9, para=pelln3(samlmu(listko[1][[1]][[4]])))

#### Vioplot (Verwerten!!!)
wei.bp <- listi$Wei
gum.bp <- listi$Gum
ln3.bp <- listi$LN3
pe3.bp <- listi$PE3
obs.bp<-listi$Obs
vioplot(wei.bp,gum.bp,pe3.bp,ln3.bp,obs.bp, names=c("wei", "gum", "pe3","ln3","Obs"), col=2)
cor(listi$LN3,listi$Obs)
boxplot(listi[1:5])

####
boxplot(Wei~Obs, data=mimi, boxwex=0.25, col = "green") 

boxplot(Gum~Obs, data=mimi, add=TRUE, at = 1:12 - 0.1, boxwex = 0.25, subset=Roof == "TT13", col = "darkgreen") 
boxplot(LN3~Obs, data=mimi, add=TRUE, boxwex=0.25, at = 1:12 + 0.2, subset=Roof == "BARE", col = "red") 
smartlegend(x="left",y="top", inset = 0, c("green 1", "green 2", "bare"), fill = c("green", "darkgreen", "red")) 






dev.off()
wei.bp <- listi$Wei
gum.bp <- listi$Gum
ln3.bp <- listi$LN3
pe3.bp <- listi$PE3
obs.bp<-listi$Obs
vioplot(wei.bp,gum.bp,ln3.bp,pe3.bp,obs.bp, names=c("wei", "gum", "ln3","pe3","Obs"), col=2)
adj.wei<-signif((sum(obs.bp-wei.bp)),4)
(sum(obs.bp)/
   
   sum(obs.bp-pe3.bp)
 
 qqplot(listko[1][[1]][[4]], pch = 20, xlab = "x Quantiles", ylab = "y Quantiles", regress = TRUE,make.plot = TRUE)
 
 summary((a))
 fit.all<-lm(formula = listi$Obs~listi$Wei+listi$Gum+listi$LN3+listi$PE3)
 fit.wei<-lm(formula = listi2$Obs~listi2$Wei)
 fit.gum<-lm(formula = listi$Obs~listi$Gum)
 fit.ln3<-lm(formula = listi$Obs~listi$LN3)
 fit.pe3<-lm(formula = listi$Obs~listi$PE3)
 summary.lm(fit.wei)
 
 library(Hmisc)
 rcorr(listi2$Obs,listi2$PE3, type="pearson")
 summary(fit.all,correlation=T)
 
 
 anova(fit.ln3, fit.gum,fit.wei,fit.pe3,fit.all)
 anova(fit.)
 plot(fit.all)
 summary(fit.ln3)
 summary(fit.pe3)$adj.r.squared
 a$adj.r.squared
 library(relaimpo)
 calc.relimp(fit.all,type=c("Wei","last","first","pratt"),rela=TRUE)
 boot <- boot.relimp(fit.all, b = 1000, type = c("lmg","last", "first", "pratt"), rank = TRUE,diff = TRUE, rela = TRUE)
 booteval.relimp(boot) # print result
 plot(booteval.relimp(boot,sort=TRUE)) # plot result
 
 plot(cooks.distance(fit.all))
 par(mfrow=c(2,2))
 plot(residuals(fit.all))
 plot(fit.all)
 summary(fit.ln3)
 abline(fit)
 
 list.compare<-function(dfal,yearall,quall){
   dfall<-dfal
   lmom<-param(df=dfall,quantile=quall,year=yearall)
   qua<-quantc(df=dfall,year=yearall,qu=quall)
   li.wei<-as.vector(unlist(lmom[[1]]))
   li.gum<-as.vector(unlist(lmom[[2]]))
   li.ln3<-as.vector(unlist(lmom[[4]]))
   li.pe3<-as.vector(unlist(lmom[[3]]))
   quanti<-as.vector(unlist(qua@.Data))
   n<-c(dput(names(dfal[1])),yearall,quall)
   listi<-(list(li.wei,li.gum,li.ln3,li.pe3,quanti,n))
   names(listi)<-c("Wei","Gum","LN3","PE3","Obs","Names")
   wei.bp <- listi$Wei
   gum.bp <- listi$Gum
   ln3.bp <- listi$LN3
   pe3.bp <- listi$PE3
   obs.bp<-listi$Obs
   # minus
   adj.wei<-signif((sum(obs.bp-wei.bp)),4)
   adj.gum<-signif((sum(obs.bp-gum.bp)),4)
   adj.ln3<-signif((sum(obs.bp-ln3.bp)),4)
   adj.pe3<-signif((sum(obs.bp-pe3.bp)),4)
   adj.wei.p<-signif((adj.wei/sum(obs.bp)*100),4)
   adj.gum.p<-signif((adj.gum/sum(obs.bp)*100),4)
   adj.ln3.p<-signif((adj.ln3/sum(obs.bp)*100),4)
   adj.pe3.p<-signif((adj.pe3/sum(obs.bp)*100),4)
   df<-data.frame(adj.wei,adj.gum,adj.ln3,adj.pe3,adj.wei.p,adj.gum.p,adj.ln3.p,adj.pe3.p)
   
   return(df)
 }
 compList<-lapply(listko[1:9],yearall=30,quall=0.99,FUN = list.compare)
 str(listko[1])
 compList
 
SumWei<-function(complist){
   wei.p<-(unlist(complist)[[5]])
   return(wei.p)
 }
wei.p<-lapply(compList[1:9],FUN=SumWei)
sum.wei.p<-sum(unlist(wei.p))/length(compList)


 v<-compList[1]
 v[[1]][[5]]

n<-as.numeric(1:length(compList))
matrix(NA,length(n),4)
f<-unlist(compList[2])[[5]]
f

matrix[n,1]<-compList[n][[5]]
matrix[,1]<-(sum(wei.ma.p[1])/length(n))
y10.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys10])))
y10.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys10])))
y10.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys10])))
y10.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys10])))

y11.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys11])))
y11.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys11])))
y11.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys11])))
y11.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys11])))

y12.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys12])))
y12.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys12])))
y12.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys12])))
y12.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys12])))

y13.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys13])))
y13.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys13])))
y13.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys13])))
y13.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys13])))

y14.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys14])))
y14.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys14])))
y14.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys14])))
y14.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys14])))

y15.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys15])))
y15.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys15])))
y15.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys15])))
y15.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys15])))

y16.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys16])))
y16.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys16])))
y16.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys16])))
y16.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys16])))

y17.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys17])))
y17.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys17])))
y17.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys17])))
y17.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys17])))

y18.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys18])))
y18.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys18])))
y18.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys18])))
y18.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys18])))

y19.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys19])))
y19.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys19])))
y19.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys19])))
y19.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys19])))

y20.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys20])))
y20.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys20])))
y20.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys20])))
y20.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys20])))

y21.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys21])))
y21.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys21])))
y21.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys21])))
y21.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys21])))

y22.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys22])))
y22.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys22])))
y22.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys22])))
y22.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys22])))

y23.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys23])))
y23.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys23])))
y23.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys23])))
y23.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys23])))

y24.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys24])))
y24.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys24])))
y24.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys24])))
y24.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys24])))

y25.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys25])))
y25.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys25])))
y25.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys25])))
y25.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys25])))

y26.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys26])))
y26.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys26])))
y26.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys26])))
y26.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys26])))

y27.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys27])))
y27.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys27])))
y27.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys27])))
y27.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys27])))

y28.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys28])))
y28.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys28])))
y28.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys28])))
y28.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys28])))

y29.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys29])))
y29.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys29])))
y29.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys29])))
y29.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys29])))

y30.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys30])))
y30.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys30])))
y30.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys30])))
y30.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys30])))

y31.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys1])))
y31.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys1])))
y31.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys1])))
y31.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys1])))

y32.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys2])))
y32.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys2])))
y32.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys2])))
y32.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys2])))

y33.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys3])))
y33.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys3])))
y33.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys3])))
y33.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys3])))

y34.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys4])))
y34.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys4])))
y34.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys4])))
y34.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys4])))

y35.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys5])))
y35.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys5])))
y35.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys5])))
y35.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys5])))

y36.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys6])))
y36.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys6])))
y36.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys6])))
y36.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys6])))

y37.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys7])))
y37.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys7])))
y37.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys7])))
y37.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys7])))

y38.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys8])))
y38.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys8])))
y38.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys8])))
y38.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys8])))

y39.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys9])))
y39.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys9])))
y39.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys9])))
y39.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys9])))

y40.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys0])))
y40.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys0])))
y40.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys0])))
y40.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys0])))

y41.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys1])))
y41.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys1])))
y41.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys1])))
y41.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys1])))

y42.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys2])))
y42.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys2])))
y42.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys2])))
y42.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys2])))

y43.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys3])))
y43.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys3])))
y43.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys3])))
y43.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys3])))

y44.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys4])))
y44.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys4])))
y44.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys4])))
y44.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys4])))

y45.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys5])))
y45.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys5])))
y45.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys5])))
y45.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys5])))

y46.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys6])))
y46.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys6])))
y46.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys6])))
y46.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys6])))

y47.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys7])))
y47.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys7])))
y47.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys7])))
y47.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys7])))

y48.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys8])))
y48.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys8])))
y48.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys8])))
y48.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys8])))

y49.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys9])))
y49.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys9])))
y49.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys9])))
y49.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys9])))

y50.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys0])))
y50.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys0])))
y50.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys0])))
y50.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys0])))

y51.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys1])))
y51.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys1])))
y51.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys1])))
y51.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys1])))

y52.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys2])))
y52.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys2])))
y52.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys2])))
y52.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys2])))

y53.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys3])))
y53.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys3])))
y53.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys3])))
y53.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys3])))

y54.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys4])))
y54.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys4])))
y54.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys4])))
y54.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys4])))

y55.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys5])))
y55.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys5])))
y55.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys5])))
y55.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys5])))

y56.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys6])))
y56.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys6])))
y56.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys6])))
y56.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys6])))

y57.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys7])))
y57.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys7])))
y57.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys7])))
y57.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys7])))

y58.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys8])))
y58.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys8])))
y58.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys8])))
y58.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys8])))

y59.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys9])))
y59.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys9])))
y59.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys9])))
y59.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys9])))

y60.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys0])))
y60.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys0])))
y60.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys0])))
y60.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys0])))

y61.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys1])))
y61.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys1])))
y61.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys1])))
y61.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys1])))

y62.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys2])))
y62.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys2])))
y62.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys2])))
y62.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys2])))

y63.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys3])))
y63.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys3])))
y63.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys3])))
y63.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys3])))

y64.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys4])))
y64.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys4])))
y64.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys4])))
y64.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys4])))

y65.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys5])))
y65.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys5])))
y65.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys5])))
y65.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys5])))

y66.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys6])))
y66.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys6])))
y66.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys6])))
y66.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys6])))

y67.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys7])))
y67.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys7])))
y67.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys7])))
y67.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys7])))

y68.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys8])))
y68.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys8])))
y68.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys8])))
y68.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys8])))

y69.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys9])))
y69.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys9])))
y69.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys9])))
y69.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys9])))

y70.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys0])))
y70.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys0])))
y70.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys0])))
y70.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys0])))

y71.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys1])))
y71.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys1])))
y71.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys1])))
y71.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys1])))

y72.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys2])))
y72.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys2])))
y72.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys2])))
y72.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys2])))

y73.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys3])))
y73.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys3])))
y73.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys3])))
y73.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys3])))

y74.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys4])))
y74.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys4])))
y74.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys4])))
y74.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys4])))

y75.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys5])))
y75.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys5])))
y75.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys5])))
y75.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys5])))

y76.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys6])))
y76.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys6])))
y76.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys6])))
y76.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys6])))

y77.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys7])))
y77.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys7])))
y77.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys7])))
y77.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys7])))

y78.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys8])))
y78.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys8])))
y78.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys8])))
y78.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys8])))

y79.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys9])))
y79.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys9])))
y79.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys9])))
y79.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys9])))

y80.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys80])))
y80.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys80])))
y80.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys80])))
y80.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys80])))



df90.0<-rbind(a90[1][[1]][[1]][1],yy0)
df90.1<-rbind(a90[1][[1]][2],yy1)
df90.2<-rbind(a90[1][[1]][[1]][3],yy2)
df90.3<-rbind(a90[1][[1]][[1]][4],yy3)
df90.4<-rbind(a90[1][[1]][[1]][5],yy4)
df90.5<-rbind(a90[1][[1]][[1]][6],yy5)
df90.6<-rbind(a90[1][[1]][[1]][7],yy6)
df90.7<-rbind(a90[1][[1]][[1]][8],yy7)
df90.8<-rbind(a90[1][[1]][[1]][9],yy8)
df90.9<-rbind(a90[1][[1]][[1]][10],yy9)
df90.10<-rbind(a90[1][[1]][[1]][11],yy10)
df90.11<-rbind(a90[1][[1]][[1]][12],yy11)
df90.12<-rbind(a90[1][[1]][[1]][13],yy12)
df90.13<-rbind(a90[1][[1]][[1]][14],yy13)
df90.14<-rbind(a90[1][[1]][[1]][15],yy14)
df90.15<-rbind(a90[1][[1]][[1]][16],yy15)
df90.16<-rbind(a90[1][[1]][[1]][17],yy16)
df90.17<-rbind(a90[1][[1]][[1]][18],yy17)
df90.18<-rbind(a90[1][[1]][[1]][19],yy18)
df90.19<-rbind(a90[1][[1]][[1]][20],yy19)
df90.20<-rbind(a90[1][[1]][[1]][21],yy20)
df90.21<-rbind(a90[1][[1]][[1]][22],yy21)
df90.22<-rbind(a90[1][[1]][[1]][23],yy22)
df90.23<-rbind(a90[1][[1]][[1]][24],yy23)
df90.24<-rbind(a90[1][[1]][[1]][25],yy24)
df90.25<-rbind(a90[1][[1]][[1]][26],yy25)
df90.26<-rbind(a90[1][[1]][[1]][27],yy26)
df90.27<-rbind(a90[1][[1]][[1]][28],yy27)
df90.28<-rbind(a90[1][[1]][[1]][29],yy28)
df90.29<-rbind(a90[1][[1]][[1]][30],yy29)
df90.30<-rbind(a90[1][[1]][[1]][31],yy30)
df90.31<-rbind(a90[1][[1]][[1]][32],yy31)
df90.32<-rbind(a90[1][[1]][[1]][33],yy32)
df90.33<-rbind(a90[1][[1]][[1]][34],yy33)
df90.34<-rbind(a90[1][[1]][[1]][35],yy34)
df90.35<-rbind(a90[1][[1]][[1]][36],yy35)
df90.36<-rbind(a90[1][[1]][[1]][37],yy36)
df90.37<-rbind(a90[1][[1]][[1]][38],yy37)
df90.38<-rbind(a90[1][[1]][[1]][39],yy38)
df90.39<-rbind(a90[1][[1]][[1]][40],yy39)
df90.40<-rbind(a90[1][[1]][[1]][41],yy40)
df90.41<-rbind(a90[1][[1]][[1]][42],yy41)
df90.42<-rbind(a90[1][[1]][[1]][43],yy42)
df90.43<-rbind(a90[1][[1]][[1]][44],yy43)
df90.44<-rbind(a90[1][[1]][[1]][45],yy44)
df90.45<-rbind(a90[1][[1]][[1]][46],yy45)
df90.46<-rbind(a90[1][[1]][[1]][47],yy46)
df90.47<-rbind(a90[1][[1]][[1]][48],yy47)
df90.48<-rbind(a90[1][[1]][[1]][49],yy48)
df90.49<-rbind(a90[1][[1]][[1]][50],yy49)
df90<-list(df90.0,df90.1,df90.2,df90.3,df90.4,df90.5,df90.6,df90.7,df90.8,df90.9,df90.10,df90.11,df90.12,df90.13,df90.14,df90.15,df90.16,df90.17,df90.18,df90.19,df90.20,df90.21,df90.22,df90.23,df90.24,df90.25,df90.26,df90.27,df90.28,df90.29,df90.30,df90.31,df90.32,df90.33,df90.34,df90.35,df90.36,df90.37,df90.38,df90.39,df90.40,df90.41,df90.42,df90.43,df90.44,df90.45,df90.46,df90.47,df90.48,df90.49)
return(df90)


### Momenten Methode und MLE
plot(listko[3][[1]][[4]],type="l")
abline(h=quawei(f=0.99999, para = pelwei(samlmu(listko[3][[1]][[4]]))))
str(listko[3][[1]][[4]])
length(listko[3][[1]][[4]])/365
moments(listko[3][[1]][[4]])
samlmu(listko[3][[1]][[4]])
pelwei(samlmu(listko[3][[1]][[4]]))
quawei(f=0.99999999999, para = pelwei(samlmu(listko[3][[1]][[4]])))
?quawei()
lo<-moments(listko[3][[1]][[4]])[[1]]
sc<-moments(listko[3][[1]][[4]])[2]
sh<-moments(listko[3][[1]][[4]])[3]
pelwei(samlmu(listko[3][[1]][[4]]))
quawei(f=0.999,pelwei(samlmu(listko[3][[1]][[4]])))
quawei(f=0.90, para = c(lo,sc,sh))

rlevd(100, loc =  2050.8555621  , scale = 509.3631332, shape = -0.1447070 ,
      type = "gamma",
      npy = 365.25, rate = 0.01)
rlevd(100, loc =  2050.8555621  , scale = 509.3631332,
      type = "Weibull",
      npy = 365.25, rate = 0.01)


wei3.fit0<-function(shape,scale,thres) { 
  -sum(dweibull3(listko[3][[1]][[4]],shape,scale,thres,log=T)) }
mean(thres(listko[3][[1]][[4]]))
mle.results.wei0<-mle2(wei3.fit0,start=list(shape=mm[5],scale=mm[2],thres=mm[3]))
mle.results.wei0@coef
quawei(f=0.99,para=c(mle.results.wei0@coef))
mm<-moments(listko[3][[1]][[4]])
quawei(f=0.99,pelwei(samlmu(listko[3][[1]][[4]])))
quantile(listko[3][[1]][[4]],0.99)
fitdistr(listko[3][[1]][[4]], densfun="weibull", lower = 0)
quawei(f=0.99,para=c(2.224648e+00 ,2.321599e+03,))

gg<-as.numeric(listko[5][[1]][[4]])
evplot(gg)
plot(devd(gg),type="l")
abline(h=seq(min(gg),max(gg),(min(gg)/max(gg))*8000),v=seq(0,10,0.1))
quantile(gg,probs = 0.99)
?evplot()
evdistp(gg)
jj<-fevd(gg)
jj<-fevd(gg,method="Lmoments")
jj<-fevd(gg,method="MLE")

uvdata <- fextreme(gg,start=list(shape=1,scale=1), distn="weibull")
uvdata
quawei(0.99,para=c(1.403,  22.696,1))
??weibull3()

### Weibull MLE
wei.b <- function(shape,scale,thres) {
  -sum(dweibull3(gg,shape,scale,thres,log=TRUE))
}
m1 <- mle2(wei.b,start=list(shape=1,scale=1),data=list(thres=1))
m3 <- mle2(wei.b,start=list(scale=1,thres=1),data=list(shape=m1@coef[1]))

# Weibul MLE über dweibull3
ml.wei<-pweibull3(0.99,shape=m1@coef[1],scale=)
?dweibull3()
# Weibull MLE über quawei
ml.wei<-quawei(0.99,para=c(m2@coef,thres=1))
m1
m3
pweibull3(0.99,shape=m1,scale=,thres=)


### Gumbel MLE
gum.b <- function(location,scale) {
  -sum(dgumbel(gg,location,scale,log=TRUE))
}
g1 <- mle2(gum.b,start=list(scale=1),fixed=list(location=mean(gg)))
g2 <- mle2(gum.b,start=list(location=mean(gg)),fixed=list(scale=g1@coef[[1]]))
g3 <- mle2(gum.b,start=list(location=g2@coef[[1]]),fixed=list(scale=g1@coef[[1]]))
g4 <- mle2(gum.b,start=list(scale=g1@coef[[1]]),fixed=list(location=g3@coef[[1]]))
g5 <- mle2(gum.b,start=list(location=g3@coef[[1]]),fixed=list(scale=g4@coef[[1]]))
g1
g2
g3
g4
g5
g3@coef
ml.gum<-qgumbel(0.99, scale=g4@coef[[1]], location=ga3@coef[1])
ml.gum
### LN3 MLE
ln3.b<-function(shape,scale,thres) { 
  -sum(dlnorm3(gg,shape,scale,thres,log=T)) }
ln1 <- mle2(ln3.b,start=list(shape=1,scale=1),data=list(thres=signif(min(gg)-1,1)))
ln2 <- mle2(ln3.b,start=list(shape=ln1@coef[1],thres=signif(min(gg)-1,1)),data=list(scale=ln1@coef[2]))
ml.ln3<-qualn3(0.99,para=c(ln2@coef[2],ln1@coef[2],ln2@coef[1]))
ln1
ln2
ml.ln3

debug(ev.mle)

### Pe3
pe3.b<-function(shape,location,scale) { 
  -sum(dpearsonIII(gg,shape,location,scale,log=T)) }
ga1 <- mle2(pe3.b,start=list(location=1,scale=1),data=list(shape=(skewness(gg))))
ga2 <- mle2(pe3.b,start=list(location=ga1@coef[1],shape=(1)),data=list(scale=(ga1@coef[2])))
ga3 <- mle2(pe3.b,start=list(location=ga1@coef[1],shape=ga2@coef[1]),data=list(scale=(ga1@coef[2])))
ga3 <- mle2(pe3.b,start=list(location=ga1@coef[1],scale=(ga1@coef[2])),data=list(shape=ga2@coef[1]))
ga4 <- mle2(pe3.b,start=list(location=ga3@coef[1],scale=(ga3@coef[2])),data=list(shape=ga2@coef[1]))
ga1
ga2
ga3
ga4
ml.pe31<-quape3(0.99,para=c(location=ga2@coef[2],scale=ga1@coef[2],shape=ga2@coef[1]))
ml.pe32<-qpearsonIII(0.99, shape=ga2@coef[1], location=ga3@coef[1], scale=ga3@coef[2])
ml.pe31
ml.pe32
?ppearsonIII()

### Vergleich MLE
y0.wei<-quawei(f=0.99, para=pelwei(samlmu((gg))))
y0.gum<-quagum(f=0.99, para=pelgum(samlmu((gg))))
y0.ln3<-qualn3(f=0.99, para=c(kk))
y0.pe3<-quape3(f=0.99, para=pelpe3(samlmu((gg))))
y0.wei
ml.wei
y0.gum
ml.gum
y0.ln3
ml.ln3
y0.pe3
ml.pe3
evplot(gg,type="l")
evdistq(quawei, para=c(m2@coef,thres=1),type="l",col=1)
evdistq(quagum, para=c(g1@coef,g2@coef),type="l",col="red")
evdistq(quagam, para=c(g1@coef,g2@coef),type="l",col="green")
evdistq(qualn3, para=c(ln2@coef[2],ln1@coef[2],ln2@coef[1]),type="l",col="blue")



plot(c(1:50),qviel.90[1][[1]],type="l")
abline(v=seq(1,50,1),h=seq(5000,5200,10))
5060-5190

63*365.25
64*365.25
65*365.25
66*365.25
summary(listko[[3]][[4]][23376:23741.25])
summary(listko[[3]][[4]][1:23010])
summary(listko[[3]][[4]])
which(max(listko[[3]][[4]])==listko[[3]][[4]])


y1965<-(listko[[3]][23376:23741.25,1:4])
which(max(y1964[4])==y1964[4])
summary(y1965[200:260,1:4])
maxmonth<-(y1965[200:240,4])
apply(y1965[4],1,function(x) x)

for (i in nrow(y1965[4])){
  ma<-y1965[4]
  ma2[(1:i),1]<-mean(ma[(1:i),])
}
nSkip<-40


max_month_80y<-function(df,quantile,year,...){
  dfff<-df
  nSubsets<-(length(dfff[[4]]))
  outList<-vector("list",length=nSubsets)
  meanOL<-vector("numeric",length=nSubsets)
  totRow<-nrow(dfff[[4]])
  
  for (i in seq_len(nSubsets)){
    rowsToGrab<-seq(i,(39+i),1)
    outList[[i]]<-dfff[4][rowsToGrab,]
    meanOL[i]<-mean(outList[[i]],na.rm=T)
    ah<-which(max(meanOL)==(meanOL))
  }
  outValue<-dfff[[4]][ah:(ah+39)]
  list_added_maxmonth<-list(1,2,3,(c((outValue),dfff[[4]])))
  new_hq_addMM<-par.80y(new_hq,quantile = quantile,year=year)
  
  return(list(ah,meanOL,list_added_maxmonth,new_hq_addMM))
  
}
max_month<-max_month_80y(listko[[3]])
dfff<-listko[[3]]
maxMonthList[[1]][[4]]


### Vorsicht, df (maxMonthList) ist um eines Versetzt. maxMonthList[[1]]=la_80[[2]] (plotdf)
plot_maxMonth_perc_wei<-function(df,plotdf){
  
nSubsets<-(length(df[[4]][[1]]))
outList<-matrix(NA,nSubsets,3)
#plot(c(1:50),plotdf[[1]],type="l",ylim=c(min(plotdf[[1]])*0.96,max(plotdf[[1]])*1.08))
#lines((df[[4]][[1]]),col="red")

for (i in seq_len(nSubsets)){
  outList[i,1]<-df[[4]][[1]][i]-plotdf[[1]][[1]][i]
  outList[i,2]<-(outList[i,1]/plotdf[[1]][[1]][i]*100)
  
}
return(list(outList))
}
df1<-plot_maxMonth_perc_wei(maxMonthList[[1]],plotdf=la_80_49[[1]])
df2<-plot_maxMonth_perc_wei(maxMonthList[[2]],plotdf=la_80_49[[2]])
df3<-plot_maxMonth_perc_wei(maxMonthList[[3]],plotdf=la_80_49[[3]])
df4<-plot_maxMonth_perc_wei(maxMonthList[[4]],plotdf=la_80_49[[4]])
df5<-plot_maxMonth_perc_wei(maxMonthList[[5]],plotdf=la_80_49[[5]])
df6<-plot_maxMonth_perc_wei(maxMonthList[[6]],plotdf=la_80_49[[6]])
df7<-plot_maxMonth_perc_wei(maxMonthList[[7]],plotdf=la_80_49[[7]])
df8<-plot_maxMonth_perc_wei(maxMonthList[[8]],plotdf=la_80_49[[8]])
df9<-plot_maxMonth_perc_wei(maxMonthList[[9]],plotdf=la_80_49[[9]])
df10<-plot_maxMonth_perc_wei(maxMonthList[[10]],plotdf=la_80_49[[10]])
df11<-plot_maxMonth_perc_wei(maxMonthList[[11]],plotdf=la_80_49[[11]])
df12<-plot_maxMonth_perc_wei(maxMonthList[[12]],plotdf=la_80_49[[12]])
df13<-plot_maxMonth_perc_wei(maxMonthList[[13]],plotdf=la_80_49[[13]])
df14<-plot_maxMonth_perc_wei(maxMonthList[[14]],plotdf=la_80_49[[14]])
df15<-plot_maxMonth_perc_wei(maxMonthList[[15]],plotdf=la_80_49[[15]])
df16<-plot_maxMonth_perc_wei(maxMonthList[[16]],plotdf=la_80_49[[16]])
df17<-plot_maxMonth_perc_wei(maxMonthList[[17]],plotdf=la_80_49[[17]])
df18<-plot_maxMonth_perc_wei(maxMonthList[[18]],plotdf=la_80_49[[18]])
df19<-plot_maxMonth_perc_wei(maxMonthList[[19]],plotdf=la_80_49[[19]])
df20<-plot_maxMonth_perc_wei(maxMonthList[[20]],plotdf=la_80_49[[20]])
df21<-plot_maxMonth_perc_wei(maxMonthList[[21]],plotdf=la_80_49[[21]])
df22<-plot_maxMonth_perc_wei(maxMonthList[[22]],plotdf=la_80_49[[22]])
df23<-plot_maxMonth_perc_wei(maxMonthList[[23]],plotdf=la_80_49[[23]])
df24<-plot_maxMonth_perc_wei(maxMonthList[[24]],plotdf=la_80_49[[24]])
df25<-plot_maxMonth_perc_wei(maxMonthList[[25]],plotdf=la_80_49[[25]])
df26<-plot_maxMonth_perc_wei(maxMonthList[[26]],plotdf=la_80_49[[26]])
df27<-plot_maxMonth_perc_wei(maxMonthList[[27]],plotdf=la_80_49[[27]])
df28<-plot_maxMonth_perc_wei(maxMonthList[[28]],plotdf=la_80_49[[28]])
df29<-plot_maxMonth_perc_wei(maxMonthList[[29]],plotdf=la_80_49[[29]])
df30<-plot_maxMonth_perc_wei(maxMonthList[[30]],plotdf=la_80_49[[30]])
df31<-plot_maxMonth_perc_wei(maxMonthList[[31]],plotdf=la_80_49[[31]])
df32<-plot_maxMonth_perc_wei(maxMonthList[[32]],plotdf=la_80_49[[32]])
df33<-plot_maxMonth_perc_wei(maxMonthList[[33]],plotdf=la_80_49[[33]])
df34<-plot_maxMonth_perc_wei(maxMonthList[[34]],plotdf=la_80_49[[34]])
df35<-plot_maxMonth_perc_wei(maxMonthList[[35]],plotdf=la_80_49[[35]])
df36<-plot_maxMonth_perc_wei(maxMonthList[[36]],plotdf=la_80_49[[36]])
df37<-plot_maxMonth_perc_wei(maxMonthList[[37]],plotdf=la_80_49[[37]])
df38<-plot_maxMonth_perc_wei(maxMonthList[[38]],plotdf=la_80_49[[38]])
df39<-plot_maxMonth_perc_wei(maxMonthList[[39]],plotdf=la_80_49[[39]])

proc<-list(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12,df13,df14,df15,df16,df17,df18,df19,df20,df21,df22,df23,df24,df25,df26,df27,df28,df29,df30,df31,df32,df33,df34,df35,df36,df37,df38,df39)
proc[[2]][,2][[2]]
proc_1<-(proc[[1]][,2][[1]]+proc[[1]][,2][[2]]+proc[[1]][,2][[3]]+proc[[1]][,2][[4]]+proc[[1]][,2][[5]]+proc[[1]][,2][[6]]+proc[[1]][,2][[7]]+proc[[1]][,2][[8]]+proc[[1]][,2][[9]]+proc[[1]][,2][[10]]+proc[[1]][,2][[11]]+proc[[1]][,2][[12]]+proc[[1]][,2][[13]]+proc[[1]][,2][[14]]+proc[[1]][,2][[15]]+proc[[1]][,2][[16]]+proc[[1]][,2][[17]]+proc[[1]][,2][[18]]+proc[[1]][,2][[19]]+proc[[1]][,2][[20]]+proc[[1]][,2][[21]]+proc[[1]][,2][[22]]+proc[[1]][,2][[23]]+proc[[1]][,2][[24]]+proc[[1]][,2][[25]]+proc[[1]][,2][[26]]+proc[[1]][,2][[27]]+proc[[1]][,2][[28]]+proc[[1]][,2][[29]]+proc[[1]][,2][[30]]+proc[[1]][,2][[31]]+proc[[1]][,2][[32]]+proc[[1]][,2][[33]]+proc[[1]][,2][[34]]+proc[[1]][,2][[35]]+proc[[1]][,2][[36]]+proc[[1]][,2][[37]]+proc[[1]][,2][[38]]+proc[[1]][,2][[39]])/39
proc_2<-(proc[[2]][,2][[1]]+proc[[2]][,2][[2]]+proc[[2]][,2][[3]]+proc[[2]][,2][[4]]+proc[[2]][,2][[5]]+proc[[2]][,2][[6]]+proc[[2]][,2][[7]]+proc[[2]][,2][[8]]+proc[[2]][,2][[9]]+proc[[2]][,2][[10]]+proc[[2]][,2][[11]]+proc[[2]][,2][[12]]+proc[[2]][,2][[13]]+proc[[2]][,2][[14]]+proc[[2]][,2][[15]]+proc[[2]][,2][[16]]+proc[[2]][,2][[17]]+proc[[2]][,2][[18]]+proc[[2]][,2][[19]]+proc[[2]][,2][[20]]+proc[[2]][,2][[21]]+proc[[2]][,2][[22]]+proc[[2]][,2][[23]]+proc[[2]][,2][[24]]+proc[[2]][,2][[25]]+proc[[2]][,2][[26]]+proc[[2]][,2][[27]]+proc[[2]][,2][[28]]+proc[[2]][,2][[29]]+proc[[2]][,2][[30]]+proc[[2]][,2][[31]]+proc[[2]][,2][[32]]+proc[[2]][,2][[33]]+proc[[2]][,2][[34]]+proc[[2]][,2][[35]]+proc[[2]][,2][[36]]+proc[[2]][,2][[37]]+proc[[2]][,2][[38]]+proc[[2]][,2][[39]])/39
proc_15<-(proc[[15]][,2][[1]]+proc[[15]][,2][[2]]+proc[[15]][,2][[3]]+proc[[15]][,2][[4]]+proc[[15]][,2][[5]]+proc[[15]][,2][[6]]+proc[[15]][,2][[7]]+proc[[15]][,2][[8]]+proc[[15]][,2][[9]]+proc[[15]][,2][[10]]+proc[[15]][,2][[11]]+proc[[15]][,2][[12]]+proc[[15]][,2][[13]]+proc[[15]][,2][[14]]+proc[[15]][,2][[15]]+proc[[15]][,2][[16]]+proc[[15]][,2][[17]]+proc[[15]][,2][[18]]+proc[[15]][,2][[19]]+proc[[15]][,2][[20]]+proc[[15]][,2][[21]]+proc[[15]][,2][[22]]+proc[[15]][,2][[23]]+proc[[15]][,2][[24]]+proc[[15]][,2][[25]]+proc[[15]][,2][[26]]+proc[[15]][,2][[27]]+proc[[15]][,2][[28]]+proc[[15]][,2][[29]]+proc[[15]][,2][[30]]+proc[[15]][,2][[31]]+proc[[15]][,2][[32]]+proc[[15]][,2][[33]]+proc[[15]][,2][[34]]+proc[[15]][,2][[35]]+proc[[15]][,2][[36]]+proc[[15]][,2][[37]]+proc[[15]][,2][[38]]+proc[[15]][,2][[39]])/39
proc_35<-(proc[[35]][,2][[1]]+proc[[35]][,2][[2]]+proc[[35]][,2][[3]]+proc[[35]][,2][[4]]+proc[[35]][,2][[5]]+proc[[35]][,2][[6]]+proc[[35]][,2][[7]]+proc[[35]][,2][[8]]+proc[[35]][,2][[9]]+proc[[35]][,2][[10]]+proc[[35]][,2][[11]]+proc[[35]][,2][[12]]+proc[[35]][,2][[13]]+proc[[35]][,2][[14]]+proc[[35]][,2][[15]]+proc[[35]][,2][[16]]+proc[[35]][,2][[17]]+proc[[35]][,2][[18]]+proc[[35]][,2][[19]]+proc[[35]][,2][[20]]+proc[[35]][,2][[21]]+proc[[35]][,2][[22]]+proc[[35]][,2][[23]]+proc[[35]][,2][[24]]+proc[[35]][,2][[25]]+proc[[35]][,2][[26]]+proc[[35]][,2][[27]]+proc[[35]][,2][[28]]+proc[[35]][,2][[29]]+proc[[35]][,2][[30]]+proc[[35]][,2][[31]]+proc[[35]][,2][[32]]+proc[[35]][,2][[33]]+proc[[35]][,2][[34]]+proc[[35]][,2][[35]]+proc[[35]][,2][[36]]+proc[[35]][,2][[37]]+proc[[35]][,2][[38]]+proc[[35]][,2][[39]])/39

mean(listko[[3]][[4]])
la_80_49<-la_80
la_80_49[1]<-NULL
lapply(maxMonthList[1:2],plotdf=la_80_49[1:2],FUN=plot_maxMonth_perc_wei)

la_80_49[[1]]
maxMonthList[[1]][[4]]

plot_maxMonth_perc_wei<-function(df,plotdf){
  
  nSubsets<-(length(df[[4]][[1]]))
  outList<-matrix(NA,nSubsets,3)
  #plot(c(1:50),plotdf[[1]],type="l",ylim=c(min(plotdf[[1]])*0.96,max(plotdf[[1]])*1.08))
  #lines((df[[4]][[1]]),col="red")
  
  for (i in seq_len(nSubsets)){
    outList[i,1]<-as.numeric(df[[4]][[1]][i])-as.numeric(plotdf[[1]][i])
    outList[i,2]<-(outList[i,1]/plotdf[[1]][i]*100)
  }
  return((outList))
}
plot_maxMonth_perc_wei(df=maxMonthList[[1]],la_80[[1]])
lapply(maxMonthList[1:49],plotdf=la_80,plot_maxMonth_perc_wei)


maxMonthList[[4]][[4]][[1]]

length(listko[[9]][[4]])/365

max_block<-function(df){
nSubsets<-(length(df[[4]])/365.25)
outList<-vector("list",length=nSubsets)
maxOL<-vector("numeric",length=nSubsets)
totRow<-nrow(df[[4]])

for (i in seq_len(nSubsets)){
  rowsToGrab<-seq(i*365.25-365.25,365.25*i,1)
  outList[[i]]<-df[[4]][rowsToGrab]
  maxOL[i]<-max(outList[[i]],na.rm=T)
}
return(list(1,2,3,maxOL))  
}


max_block_out<-lapply(listko[1:40],max_block)
y0.wei_try<-quawei(f=0.99, para=pelwei(samlmu(max_block_out[[3]][[4]][1:30])))

### max_block 80yrs cummulative;;; listko einlesen und raus

cummul_80_f<-function(df,quantile,...){
  
  nSubsets<-(length(df[[4]])/365.25)
  outList<-vector("list",length=nSubsets)
  maxOL<-vector("numeric",length=nSubsets)
  totRow<-nrow(df[[4]])
  
  for (i in seq_len(nSubsets)){
    rowsToGrab<-seq(i*365.25-365.25,365.25*i,1)
    outList[[i]]<-df[[4]][rowsToGrab]
    maxOL[i]<-max(outList[[i]],na.rm=T)
  } 

cummul_80<-matrix(NA,50,4)
colnames(cummul_80)<-c("Wei","Gum","PE3","LN3")

for (i in c(1:50)){
  
  seqq<-seq(1,29+i,1)
  y0.wei<-quawei(f=quantile, para=pelwei(samlmu(maxOL[seqq])))
  y0.gum<-quagum(f=quantile, para=pelgum(samlmu(maxOL[seqq])))
  y0.pe3<-quape3(f=quantile, para=pelpe3(samlmu(maxOL[seqq])))
  y0.ln3<-qualn3(f=quantile, para=pelln3(samlmu(maxOL[seqq])))
  cummul_80[i,1]<-y0.wei
  cummul_80[i,2]<-y0.gum
  cummul_80[i,3]<-y0.pe3
  cummul_80[i,4]<-y0.ln3
  
}
 return(cummul_80)
}
aha<-cummul_80_f(listko[[3]],quantile = 0.99)
aha2<-lapply(listko[1:8],quantile=0.99,cummul_80_f)
d<-list()
dd<-list()
aha3<-(lapply(seq_along(listko[1:40]),function(i){
  
for (i in seq_along(listko)){
  d<-list()
  d<-(as.numeric(listko[[i]][[4]]))
  for(j in c(1:50)){
    dd<-list()    
    dd[j]<-(max(d[1:(365.25*30+365.25*j-365.25)]))
    return(list(dd))
  }
}

aha3[[1]]
as.numeric(listko[[1]][[4]][1:8])
plot(aha2[[3]][,2],type="l",ylim=c(8000,10100))
lines(aha2[[3]][,3])
lines(aha2[[3]][,2])
ahah<-listko[[3]][[4]]
fg<-cbind(ahah,10500)
fgg<-list(list(1,2,3,rbind(matrix(10000,1,2),fg)))
fggg<-lapply(fgg,quantile=0.99,cummul_80_f)
plot(fggg[[1]][,2],type="l",ylim=c(8000,11000))
lines(fggg[[1]][,3])
lines(aha2[[3]][,3])
lines(aha2[[3]][,2])
(fggg[[1]][,3]-aha2[[3]][,3])/(fggg[[1]][,3])*100


fggg[[1]][,2]
aha2[[3]][,3]
(fggg[[1]][,2]-fggg[[1]][,3])/fggg[[1]][,2]*100
(aha2[[3]][,2]-aha2[[3]][,3])/aha2[[3]][,2]*100


plot(cummul_80[,2],type="l",ylim=c(8500,11000))
lines(cummul_80[,1])
lines(cummul_80[,3])
evplot(max_block_out[[3]][[4]])
### param

shift_30<-matrix(NA,30,5)
colnames(shift_30)<-c("raw","Wei","Gum","PE3","LN3")

for (i in c(1:nrow(shift_30))){
  seqq<-seq(i*365.25-365.25,30*365.25+i*365.25-365.25,1)
  #y0.wei<-quawei(f=0.99, para=pelwei(samlmu(listko[[3]][[4]][seqq])))
  y0.gum<-quagum(f=0.99, para=pelgum(samlmu(listko[[3]][[4]][seqq])))
  y0.pe3<-quape3(f=0.99, para=pelpe3(samlmu(listko[[3]][[4]][seqq])))
  #y0.ln3<-qualn3(f=0.99, para=pelln3(samlmu(listko[[3]][[4]][seqq])))
  shift_30[i,2]<-y0.wei
  shift_30[i,3]<-y0.gum
  shift_30[i,4]<-y0.pe3
  shift_30[i,5]<-y0.ln3
}
shift_30[,2]


nSubsets<-(length(listko[[12]][[4]])/365.25)
outList<-vector("list",length=nSubsets)
maxOL<-vector("numeric",length=nSubsets)
totRow<-nrow(listko[[12]][[4]])

for (i in seq_len(nSubsets)){
  rowsToGrab<-seq(i*365.25-365.25,365.25*i,1)
  outList[[i]]<-listko[[12]][[4]][rowsToGrab]
  maxOL[i]<-max(outList[[i]],na.rm=T)
} 

cummul_80<-matrix(NA,50,4)
colnames(cummul_80)<-c("Wei","Gum","PE3","LN3")

plot()

plot(maxOL[1:33],type="l")
plot(maxOL[1:30],type="l")
plot(maxOL[1:20],type="l")
plot(maxOL[1:80],type="l")

cdfwei
aha2[[12]]
plot(qweibull(p=seq(0.01,0.99,0.001),pelwei(samlmu(maxOL[1:80]))))
plot((pweibull(q=seq(0.001,1.999,0.001),pelwei(samlmu(maxOL[1:80]))))[,3])
(quawei(f=0.99,pelwei(samlmu(maxOL[1:40]))))
lines(cdfgum(x=c(1:1500),pelgum(samlmu(maxOL[1:80]))))
lines(cdfpe3(x=c(1:1500),pelpe3(samlmu(maxOL[1:80]))))
evplot(maxOL[1:80],type="l")

lad<-list()
for (i in c(1:50)){
lad[i]<-list(samlmu(maxOL[1:29+i]))  
}

listko[1]

mat3<-matrix(NA,50,160)
for (i in seq_along(aha3)){
  for (j in c(7:10)){
    for (k in c(1:50)){
      mat3[i,k]<-aha3[i][j][k]
    }
  }
}


mat4<-matrix(NA,40,50)
for (i in seq_along(aha3)){
  mat4[,i]<-aha[i]$wei_p
}
