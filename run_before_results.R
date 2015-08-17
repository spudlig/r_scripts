install.packages("evaluate")
install.packages("NbClust")
install.packages("apcluster")
install.packages("Rmisc")
install.packages("latticeExtra")
install.packages("gmm")
install.packages("bbmle")
install.packages("fBasics")
install.packages("dplyr")
install.packages("lmom")
install.packages("ismev")
install.packages("survival")
install.packages("MASS")
install.packages("PearsonDS")
install.packages("nsRFA")
install.packages("FAdist")
install.packages("lattice")
install.packages("ggvis")
install.packages("magrittr")
install.packages("vioplot")
install.packages("relaimpo")
install.packages("Hmisc")
install.packages("extRemes")
install.packages("evd")
install.packages("copula")
install.packages("fitdistrplus")
install.packages("animation")
install.packages("base")
install.packages("eqs2lavaan")
install.packages("moments")
install.packages("qualityTools")
install.packages("rgeos")
install.packages("ggExtra")
install.packages("hexbin")
install.packages("ggthemes")
install.packages("pdc")
install.packages("TSclust")
install.packages("fastcluster")
install.packages("corrplot")
install.packages("ggplot2")
library(TSclust)
library(NbClust)
library(corrplot)
library(fastcluster)
library(rgeos)
library(fitdistrplus)
library(logspline)
library(evaluate)
library(qualityTools)
library(moments)
library(eqs2lavaan)
library(animation)
library(fitdistrplus)
library(copula)
library(evd)
library(gmm)
library(relaimpo)
library(aplpack)
library(vioplot)
library(lattice)
library(latticeExtra)
library(grid)
library(dplyr)
library(ggvis)
library(magrittr)
library(bbmle)
library(FAdist)
library(nsRFA)
library(PearsonDS)
library(survival)
library(MASS)
library(ismev) 
library(fBasics)
library(lmom)
library(Hmisc)
library(extRemes)
library(Rmisc)
### Einlesen, und in eine formatierte Liste bringen
#setwd("./Studium/Watermanagement/Masterarbeit/data/y120/")
setwd("~/Studium/Watermanagement/Masterarbeit/data/ye60_all_3/")

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

### nur noch ohne -999 -> sind genau 9 (bei y120) 21.04.15, jetzt Versuch mit y80
kickoutnine<-function(df){
  if(min(df[[4]])=="-999"){
    df=NULL
  } 
  return(df)
}

##### AMS get max years.
AMS_get<-function(df,...){
  nSubsets<-NROW(df)/365.25
  outList<-vector("list",length=nSubsets)
  maxOL<-vector("numeric",length=nSubsets)
  totRow<-nrow(df[[4]])
  
  for (i in seq_len(nSubsets)){
    rowsToGrab<-seq(i*365.25-365.25,(365.25*i),1)
    outList[[i]]<-df[[4]][rowsToGrab]
    maxOL[i]<-max(outList[[i]],na.rm=T)
  } 
  return(maxOL)
}



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

### Punkte für die Plots (lmom zuerst)


## l-moments
# y10 Sequenzen 40 Jahres sequenzen mit je einem Jahr über 10 Jahre verschoben - 10 Schalttage 
###### Parameters EV (lmom)
param<-function(df,quantile,year){
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
  y6.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys6])))
  y6.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys6])))
  y6.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys6])))
  
  y7.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys7])))
  y7.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys7])))
  y7.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys7])))
  y7.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys7])))
  
  y8.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys8])))
  y8.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys8])))
  y8.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys8])))
  y8.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys8])))
  
  y9.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][ys9])))
  y9.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][ys9])))
  y9.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][ys9])))
  y9.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][ys9])))
  
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
  
  wei<-cbind(y0.wei,y1.wei,y2.wei,y3.wei,y4.wei,y5.wei,y6.wei,y7.wei,y8.wei,y9.wei,y10.wei,y11.wei,y12.wei,y13.wei,y14.wei,y15.wei,y16.wei,y17.wei,y18.wei,y19.wei,y20.wei,y21.wei,y22.wei,y23.wei,y24.wei,y25.wei,y26.wei,y27.wei,y28.wei,y29.wei,y30.wei)
  gum<-cbind(y0.gum,y1.gum,y2.gum,y3.gum,y4.gum,y5.gum,y6.gum,y7.gum,y8.gum,y9.gum,y10.gum,y11.gum,y12.gum,y13.gum,y14.gum,y15.gum,y16.gum,y17.gum,y18.gum,y19.gum,y20.gum,y21.gum,y22.gum,y23.gum,y24.gum,y25.gum,y26.gum,y27.gum,y28.gum,y29.gum,y30.gum)
  pe3<-cbind(y0.pe3,y1.pe3,y2.pe3,y3.pe3,y4.pe3,y5.pe3,y6.pe3,y7.pe3,y8.pe3,y9.pe3,y10.pe3,y11.pe3,y12.pe3,y13.pe3,y14.pe3,y15.pe3,y16.pe3,y17.pe3,y18.pe3,y19.pe3,y20.pe3,y21.pe3,y22.pe3,y23.pe3,y24.pe3,y25.pe3,y26.pe3,y27.pe3,y28.pe3,y29.pe3,y30.pe3)
  ln3<-cbind(y0.ln3,y1.ln3,y2.ln3,y3.ln3,y4.ln3,y5.ln3,y6.ln3,y7.ln3,y8.ln3,y9.ln3,y10.ln3,y11.ln3,y12.ln3,y13.ln3,y14.ln3,y15.ln3,y16.ln3,y17.ln3,y18.ln3,y19.ln3,y20.ln3,y21.ln3,y22.ln3,y23.ln3,y24.ln3,y25.ln3,y26.ln3,y27.ln3,y28.ln3,y29.ln3,y30.ln3)
  
  return(list(wei,gum,pe3,ln3))
  
}
par.80y<-function(dfm,quantile,year){
  df<-dfm
  ys0<-seq(1,year*365+11,1)
  ys1<-seq(1,(year*365+10+1*365),1)
  ys2<-seq(1,(year*365+10+2*365),1)
  ys3<-seq(1,(year*365+10+3*365),1)
  ys4<-seq(1,(year*365+10+4*365),1)
  ys5<-seq(1,(year*365+10+5*365),1)
  ys6<-seq(1,(year*365+10+6*365),1)
  ys7<-seq(1,(year*365+10+7*365),1)
  ys8<-seq(1,(year*365+10+8*365),1)
  ys9<-seq(1,(year*365+10+9*365),1)
  ys10<-seq(1,(year*365+10+10*365),1)
  ys11<-seq(1,(year*365+10+11*365),1)
  ys12<-seq(1,(year*365+10+12*365),1)
  ys13<-seq(1,(year*365+10+13*365),1)
  ys14<-seq(1,(year*365+10+14*365),1)
  ys15<-seq(1,(year*365+10+15*365),1)
  ys16<-seq(1,(year*365+10+16*365),1)
  ys17<-seq(1,(year*365+10+17*365),1)
  ys18<-seq(1,(year*365+10+18*365),1)
  ys19<-seq(1,(year*365+10+19*365),1)
  ys20<-seq(1,(year*365+10+20*365),1)
  ys21<-seq(1,(year*365+10+21*365),1)
  ys22<-seq(1,(year*365+10+22*365),1)
  ys23<-seq(1,(year*365+10+23*365),1)
  ys24<-seq(1,(year*365+10+24*365),1)
  ys25<-seq(1,(year*365+10+25*365),1)
  ys26<-seq(1,(year*365+10+26*365),1)
  ys27<-seq(1,(year*365+10+27*365),1)
  ys28<-seq(1,(year*365+10+28*365),1)
  ys29<-seq(1,(year*365+10+29*365),1)
  ys30<-seq(1,(year*365+10+30*365),1)
  ys31<-seq(1,(year*365+10+31*365),1)
  ys32<-seq(1,(year*365+10+32*365),1)
  ys33<-seq(1,(year*365+10+33*365),1)
  ys34<-seq(1,(year*365+10+34*365),1)
  ys35<-seq(1,(year*365+10+35*365),1)
  ys36<-seq(1,(year*365+10+36*365),1)
  ys37<-seq(1,(year*365+10+37*365),1)
  ys38<-seq(1,(year*365+10+38*365),1)
  ys39<-seq(1,(year*365+10+39*365),1)
  ys40<-seq(1,(year*365+10+40*365),1)
  ys41<-seq(1,(year*365+10+41*365),1)
  ys42<-seq(1,(year*365+10+42*365),1)
  ys43<-seq(1,(year*365+10+43*365),1)
  ys44<-seq(1,(year*365+10+44*365),1)
  ys45<-seq(1,(year*365+10+45*365),1)
  ys46<-seq(1,(year*365+10+46*365),1)
  ys47<-seq(1,(year*365+10+47*365),1)
  ys48<-seq(1,(year*365+10+48*365),1)
  ys49<-seq(1,(year*365+10+49*365),1)
  
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
  yy31<-df[[4]][ys31]
  yy32<-df[[4]][ys32]
  yy33<-df[[4]][ys33]
  yy34<-df[[4]][ys34]
  yy35<-df[[4]][ys35]
  yy36<-df[[4]][ys36]
  yy37<-df[[4]][ys37]
  yy38<-df[[4]][ys38]
  yy39<-df[[4]][ys39]
  yy40<-df[[4]][ys40]
  yy41<-df[[4]][ys41]
  yy42<-df[[4]][ys42]
  yy43<-df[[4]][ys43]
  yy44<-df[[4]][ys44]
  yy45<-df[[4]][ys45]
  yy46<-df[[4]][ys46]
  yy47<-df[[4]][ys47]
  yy48<-df[[4]][ys48]
  yy49<-df[[4]][ys49]
  
 
  
  y0.wei<-quawei(f=quantile, para=pelwei(samlmu(yy0)))
  y0.gum<-quagum(f=quantile, para=pelgum(samlmu(yy0)))
  y0.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy0)))
  y0.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy0)))
  
  y1.wei<-quawei(f=quantile, para=pelwei(samlmu(yy1)))
  y1.gum<-quagum(f=quantile, para=pelgum(samlmu(yy1)))
  y1.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy1)))
  y1.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy1)))
  
  y2.wei<-quawei(f=quantile, para=pelwei(samlmu(yy2)))
  y2.gum<-quagum(f=quantile, para=pelgum(samlmu(yy2)))
  y2.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy2)))
  y2.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy2)))
  
  y3.wei<-quawei(f=quantile, para=pelwei(samlmu(yy3)))
  y3.gum<-quagum(f=quantile, para=pelgum(samlmu(yy3)))
  y3.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy3)))
  y3.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy3)))
  
  y4.wei<-quawei(f=quantile, para=pelwei(samlmu(yy4)))
  y4.gum<-quagum(f=quantile, para=pelgum(samlmu(yy4)))
  y4.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy4)))
  y4.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy4)))
  
  y5.wei<-quawei(f=quantile, para=pelwei(samlmu(yy5)))
  y5.gum<-quagum(f=quantile, para=pelgum(samlmu(yy5)))
  y5.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy5)))
  y5.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy5)))
  
  y6.wei<-quawei(f=quantile, para=pelwei(samlmu(yy6)))
  y6.gum<-quagum(f=quantile, para=pelgum(samlmu(yy6)))
  y6.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy6)))
  y6.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy6)))
  
  y7.wei<-quawei(f=quantile, para=pelwei(samlmu(yy7)))
  y7.gum<-quagum(f=quantile, para=pelgum(samlmu(yy7)))
  y7.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy7)))
  y7.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy7)))
  
  y8.wei<-quawei(f=quantile, para=pelwei(samlmu(yy8)))
  y8.gum<-quagum(f=quantile, para=pelgum(samlmu(yy8)))
  y8.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy8)))
  y8.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy8)))
  
  y9.wei<-quawei(f=quantile, para=pelwei(samlmu(yy9)))
  y9.gum<-quagum(f=quantile, para=pelgum(samlmu(yy9)))
  y9.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy9)))
  y9.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy9)))
  
  y10.wei<-quawei(f=quantile, para=pelwei(samlmu(yy10)))
  y10.gum<-quagum(f=quantile, para=pelgum(samlmu(yy10)))
  y10.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy10)))
  y10.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy10)))
  
  y11.wei<-quawei(f=quantile, para=pelwei(samlmu(yy11)))
  y11.gum<-quagum(f=quantile, para=pelgum(samlmu(yy11)))
  y11.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy11)))
  y11.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy11)))
  
  y12.wei<-quawei(f=quantile, para=pelwei(samlmu(yy12)))
  y12.gum<-quagum(f=quantile, para=pelgum(samlmu(yy12)))
  y12.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy12)))
  y12.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy12)))
  
  y13.wei<-quawei(f=quantile, para=pelwei(samlmu(yy13)))
  y13.gum<-quagum(f=quantile, para=pelgum(samlmu(yy13)))
  y13.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy13)))
  y13.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy13)))
  
  y14.wei<-quawei(f=quantile, para=pelwei(samlmu(yy14)))
  y14.gum<-quagum(f=quantile, para=pelgum(samlmu(yy14)))
  y14.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy14)))
  y14.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy14)))
  
  y15.wei<-quawei(f=quantile, para=pelwei(samlmu(yy15)))
  y15.gum<-quagum(f=quantile, para=pelgum(samlmu(yy15)))
  y15.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy15)))
  y15.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy15)))
  
  y16.wei<-quawei(f=quantile, para=pelwei(samlmu(yy16)))
  y16.gum<-quagum(f=quantile, para=pelgum(samlmu(yy16)))
  y16.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy16)))
  y16.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy16)))
  
  y17.wei<-quawei(f=quantile, para=pelwei(samlmu(yy17)))
  y17.gum<-quagum(f=quantile, para=pelgum(samlmu(yy17)))
  y17.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy17)))
  y17.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy17)))
  
  y18.wei<-quawei(f=quantile, para=pelwei(samlmu(yy18)))
  y18.gum<-quagum(f=quantile, para=pelgum(samlmu(yy18)))
  y18.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy18)))
  y18.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy18)))
  
  y19.wei<-quawei(f=quantile, para=pelwei(samlmu(yy19)))
  y19.gum<-quagum(f=quantile, para=pelgum(samlmu(yy19)))
  y19.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy19)))
  y19.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy19)))
  
  y20.wei<-quawei(f=quantile, para=pelwei(samlmu(yy20)))
  y20.gum<-quagum(f=quantile, para=pelgum(samlmu(yy20)))
  y20.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy20)))
  y20.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy20)))
  
  y21.wei<-quawei(f=quantile, para=pelwei(samlmu(yy21)))
  y21.gum<-quagum(f=quantile, para=pelgum(samlmu(yy21)))
  y21.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy21)))
  y21.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy21)))
  
  y22.wei<-quawei(f=quantile, para=pelwei(samlmu(yy22)))
  y22.gum<-quagum(f=quantile, para=pelgum(samlmu(yy22)))
  y22.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy22)))
  y22.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy22)))
  
  y23.wei<-quawei(f=quantile, para=pelwei(samlmu(yy23)))
  y23.gum<-quagum(f=quantile, para=pelgum(samlmu(yy23)))
  y23.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy23)))
  y23.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy23)))
  
  y24.wei<-quawei(f=quantile, para=pelwei(samlmu(yy24)))
  y24.gum<-quagum(f=quantile, para=pelgum(samlmu(yy24)))
  y24.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy24)))
  y24.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy24)))
  
  y25.wei<-quawei(f=quantile, para=pelwei(samlmu(yy25)))
  y25.gum<-quagum(f=quantile, para=pelgum(samlmu(yy25)))
  y25.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy25)))
  y25.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy25)))
  
  y26.wei<-quawei(f=quantile, para=pelwei(samlmu(yy26)))
  y26.gum<-quagum(f=quantile, para=pelgum(samlmu(yy26)))
  y26.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy26)))
  y26.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy26)))
  
  y27.wei<-quawei(f=quantile, para=pelwei(samlmu(yy27)))
  y27.gum<-quagum(f=quantile, para=pelgum(samlmu(yy27)))
  y27.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy27)))
  y27.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy27)))
  
  y28.wei<-quawei(f=quantile, para=pelwei(samlmu(yy28)))
  y28.gum<-quagum(f=quantile, para=pelgum(samlmu(yy28)))
  y28.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy28)))
  y28.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy28)))
  
  y29.wei<-quawei(f=quantile, para=pelwei(samlmu(yy29)))
  y29.gum<-quagum(f=quantile, para=pelgum(samlmu(yy29)))
  y29.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy29)))
  y29.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy29)))
  
  y30.wei<-quawei(f=quantile, para=pelwei(samlmu(yy30)))
  y30.gum<-quagum(f=quantile, para=pelgum(samlmu(yy30)))
  y30.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy30)))
  y30.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy30)))
  
  y31.wei<-quawei(f=quantile, para=pelwei(samlmu(yy31)))
  y31.gum<-quagum(f=quantile, para=pelgum(samlmu(yy31)))
  y31.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy31)))
  y31.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy31)))
  
  y32.wei<-quawei(f=quantile, para=pelwei(samlmu(yy32)))
  y32.gum<-quagum(f=quantile, para=pelgum(samlmu(yy32)))
  y32.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy32)))
  y32.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy32)))
  
  y33.wei<-quawei(f=quantile, para=pelwei(samlmu(yy33)))
  y33.gum<-quagum(f=quantile, para=pelgum(samlmu(yy33)))
  y33.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy33)))
  y33.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy33)))
  
  y34.wei<-quawei(f=quantile, para=pelwei(samlmu(yy34)))
  y34.gum<-quagum(f=quantile, para=pelgum(samlmu(yy34)))
  y34.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy34)))
  y34.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy34)))
  
  y35.wei<-quawei(f=quantile, para=pelwei(samlmu(yy35)))
  y35.gum<-quagum(f=quantile, para=pelgum(samlmu(yy35)))
  y35.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy35)))
  y35.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy35)))
  
  y36.wei<-quawei(f=quantile, para=pelwei(samlmu(yy36)))
  y36.gum<-quagum(f=quantile, para=pelgum(samlmu(yy36)))
  y36.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy36)))
  y36.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy36)))
  
  y37.wei<-quawei(f=quantile, para=pelwei(samlmu(yy37)))
  y37.gum<-quagum(f=quantile, para=pelgum(samlmu(yy37)))
  y37.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy37)))
  y37.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy37)))
  
  y38.wei<-quawei(f=quantile, para=pelwei(samlmu(yy38)))
  y38.gum<-quagum(f=quantile, para=pelgum(samlmu(yy38)))
  y38.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy38)))
  y38.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy38)))
  
  y39.wei<-quawei(f=quantile, para=pelwei(samlmu(yy39)))
  y39.gum<-quagum(f=quantile, para=pelgum(samlmu(yy39)))
  y39.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy39)))
  y39.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy39)))
  
  y40.wei<-quawei(f=quantile, para=pelwei(samlmu(yy40)))
  y40.gum<-quagum(f=quantile, para=pelgum(samlmu(yy40)))
  y40.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy40)))
  y40.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy40)))
  
  y41.wei<-quawei(f=quantile, para=pelwei(samlmu(yy41)))
  y41.gum<-quagum(f=quantile, para=pelgum(samlmu(yy41)))
  y41.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy41)))
  y41.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy41)))
  
  y42.wei<-quawei(f=quantile, para=pelwei(samlmu(yy42)))
  y42.gum<-quagum(f=quantile, para=pelgum(samlmu(yy42)))
  y42.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy42)))
  y42.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy42)))
  
  y43.wei<-quawei(f=quantile, para=pelwei(samlmu(yy43)))
  y43.gum<-quagum(f=quantile, para=pelgum(samlmu(yy43)))
  y43.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy43)))
  y43.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy43)))
  
  y44.wei<-quawei(f=quantile, para=pelwei(samlmu(yy44)))
  y44.gum<-quagum(f=quantile, para=pelgum(samlmu(yy44)))
  y44.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy44)))
  y44.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy44)))
  
  y45.wei<-quawei(f=quantile, para=pelwei(samlmu(yy45)))
  y45.gum<-quagum(f=quantile, para=pelgum(samlmu(yy45)))
  y45.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy45)))
  y45.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy45)))
  
  y46.wei<-quawei(f=quantile, para=pelwei(samlmu(yy46)))
  y46.gum<-quagum(f=quantile, para=pelgum(samlmu(yy46)))
  y46.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy46)))
  y46.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy46)))
  
  y47.wei<-quawei(f=quantile, para=pelwei(samlmu(yy47)))
  y47.gum<-quagum(f=quantile, para=pelgum(samlmu(yy47)))
  y47.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy47)))
  y47.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy47)))
  
  y48.wei<-quawei(f=quantile, para=pelwei(samlmu(yy48)))
  y48.gum<-quagum(f=quantile, para=pelgum(samlmu(yy48)))
  y48.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy48)))
  y48.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy48)))
  
  y49.wei<-quawei(f=quantile, para=pelwei(samlmu(yy49)))
  y49.gum<-quagum(f=quantile, para=pelgum(samlmu(yy49)))
  y49.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy49)))
  y49.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy49)))
  
  
  
  
  wei<-cbind(y0.wei,y1.wei,y2.wei,y3.wei,y4.wei,y5.wei,y6.wei,y7.wei,y8.wei,y9.wei,y10.wei,y11.wei,y12.wei,y13.wei,y14.wei,y15.wei,y16.wei,y17.wei,y18.wei,y19.wei,y20.wei,y21.wei,y22.wei,y23.wei,y24.wei,y25.wei,y26.wei,y27.wei,y28.wei,y29.wei,y30.wei,y31.wei,y32.wei,y33.wei,y34.wei,y35.wei,y36.wei,y37.wei,y38.wei,y39.wei,y40.wei,y41.wei,y42.wei,y43.wei,y44.wei,y45.wei,y46.wei,y47.wei,y48.wei,y49.wei)
  gum<-cbind(y0.gum,y1.gum,y2.gum,y3.gum,y4.gum,y5.gum,y6.gum,y7.gum,y8.gum,y9.gum,y10.gum,y11.gum,y12.gum,y13.gum,y14.gum,y15.gum,y16.gum,y17.gum,y18.gum,y19.gum,y20.gum,y21.gum,y22.gum,y23.gum,y24.gum,y25.gum,y26.gum,y27.gum,y28.gum,y29.gum,y30.gum,y31.gum,y32.gum,y33.gum,y34.gum,y35.gum,y36.gum,y37.gum,y38.gum,y39.gum,y40.gum,y41.gum,y42.gum,y43.gum,y44.gum,y45.gum,y46.gum,y47.gum,y48.gum,y49.gum)
  pe3<-cbind(y0.pe3,y1.pe3,y2.pe3,y3.pe3,y4.pe3,y5.pe3,y6.pe3,y7.pe3,y8.pe3,y9.pe3,y10.pe3,y11.pe3,y12.pe3,y13.pe3,y14.pe3,y15.pe3,y16.pe3,y17.pe3,y18.pe3,y19.pe3,y20.pe3,y21.pe3,y22.pe3,y23.pe3,y24.pe3,y25.pe3,y26.pe3,y27.pe3,y28.pe3,y29.pe3,y30.pe3,y31.pe3,y32.pe3,y33.pe3,y34.pe3,y35.pe3,y36.pe3,y37.pe3,y38.pe3,y39.pe3,y40.pe3,y41.pe3,y42.pe3,y43.pe3,y44.pe3,y45.pe3,y46.pe3,y47.pe3,y48.pe3,y49.pe3)
  ln3<-cbind(y0.ln3,y1.ln3,y2.ln3,y3.ln3,y4.ln3,y5.ln3,y6.ln3,y7.ln3,y8.ln3,y9.ln3,y10.ln3,y11.ln3,y12.ln3,y13.ln3,y14.ln3,y15.ln3,y16.ln3,y17.ln3,y18.ln3,y19.ln3,y20.ln3,y21.ln3,y22.ln3,y23.ln3,y24.ln3,y25.ln3,y26.ln3,y27.ln3,y28.ln3,y29.ln3,y30.ln3,y31.ln3,y32.ln3,y33.ln3,y34.ln3,y35.ln3,y36.ln3,y37.ln3,y38.ln3,y39.ln3,y40.ln3,y41.ln3,y42.ln3,y43.ln3,y44.ln3,y45.ln3,y46.ln3,y47.ln3,y48.ln3,y49.ln3)
  
  return(list(wei,gum,pe3,ln3))
  
}

#### Extract 40 Days of the Time series with the highes mean discharge and add it at the beginning of
#### the TS, then calculate the summed years (30->80yrs) and a 0.99 quantile HQ
max_month_80y<-function(df){
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
  return(list(ah,meanOL))
  
}
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
  new_hq_addMM<-par.80y(list_added_maxmonth,quantile = quantile,year=year)
  
  return(list(ah,meanOL,list_added_maxmonth,new_hq_addMM))
  
}



##### Parameter EV (lmom) für die 0.90,0.99,0.999,0.9999

#### für ein 0.90/99/999 hochwasser am Anfang, dann ein 0.90 Hochwasser berechnet
inc.hqs.90.90<-function(dfs,year,quantile){
  dff<-dfs[[1]]
  df<-dfs[[1]]
  a90<-par.80y(dfm=dff,year = year,quantile=quantile)
  
  ys0<-seq(1,year*365+11,1)
  ys1<-seq(1,(year*365+10+1*365),1)
  ys2<-seq(1,(year*365+10+2*365),1)
  ys3<-seq(1,(year*365+10+3*365),1)
  ys4<-seq(1,(year*365+10+4*365),1)
  ys5<-seq(1,(year*365+10+5*365),1)
  ys6<-seq(1,(year*365+10+6*365),1)
  ys7<-seq(1,(year*365+10+7*365),1)
  ys8<-seq(1,(year*365+10+8*365),1)
  ys9<-seq(1,(year*365+10+9*365),1)
  ys10<-seq(1,(year*365+10+10*365),1)
  ys11<-seq(1,(year*365+10+11*365),1)
  ys12<-seq(1,(year*365+10+12*365),1)
  ys13<-seq(1,(year*365+10+13*365),1)
  ys14<-seq(1,(year*365+10+14*365),1)
  ys15<-seq(1,(year*365+10+15*365),1)
  ys16<-seq(1,(year*365+10+16*365),1)
  ys17<-seq(1,(year*365+10+17*365),1)
  ys18<-seq(1,(year*365+10+18*365),1)
  ys19<-seq(1,(year*365+10+19*365),1)
  ys20<-seq(1,(year*365+10+20*365),1)
  ys21<-seq(1,(year*365+10+21*365),1)
  ys22<-seq(1,(year*365+10+22*365),1)
  ys23<-seq(1,(year*365+10+23*365),1)
  ys24<-seq(1,(year*365+10+24*365),1)
  ys25<-seq(1,(year*365+10+25*365),1)
  ys26<-seq(1,(year*365+10+26*365),1)
  ys27<-seq(1,(year*365+10+27*365),1)
  ys28<-seq(1,(year*365+10+28*365),1)
  ys29<-seq(1,(year*365+10+29*365),1)
  ys30<-seq(1,(year*365+10+30*365),1)
  ys31<-seq(1,(year*365+10+31*365),1)
  ys32<-seq(1,(year*365+10+32*365),1)
  ys33<-seq(1,(year*365+10+33*365),1)
  ys34<-seq(1,(year*365+10+34*365),1)
  ys35<-seq(1,(year*365+10+35*365),1)
  ys36<-seq(1,(year*365+10+36*365),1)
  ys37<-seq(1,(year*365+10+37*365),1)
  ys38<-seq(1,(year*365+10+38*365),1)
  ys39<-seq(1,(year*365+10+39*365),1)
  ys40<-seq(1,(year*365+10+40*365),1)
  ys41<-seq(1,(year*365+10+41*365),1)
  ys42<-seq(1,(year*365+10+42*365),1)
  ys43<-seq(1,(year*365+10+43*365),1)
  ys44<-seq(1,(year*365+10+44*365),1)
  ys45<-seq(1,(year*365+10+45*365),1)
  ys46<-seq(1,(year*365+10+46*365),1)
  ys47<-seq(1,(year*365+10+47*365),1)
  ys48<-seq(1,(year*365+10+48*365),1)
  ys49<-seq(1,(year*365+10+49*365),1)
  
  yy0<-as.matrix(df[[4]][ys0])
  yy1<-as.matrix(df[[4]][ys1])
  yy2<-as.matrix(df[[4]][ys2])
  yy3<-as.matrix(df[[4]][ys3])
  yy4<-as.matrix(df[[4]][ys4])
  yy5<-as.matrix(df[[4]][ys5])
  yy6<-as.matrix(df[[4]][ys6])
  yy7<-as.matrix(df[[4]][ys7])
  yy8<-as.matrix(df[[4]][ys8])
  yy9<-as.matrix(df[[4]][ys9])
  yy10<-as.matrix(df[[4]][ys10])
  yy11<-as.matrix(df[[4]][ys11])
  yy12<-as.matrix(df[[4]][ys12])
  yy13<-as.matrix(df[[4]][ys13])
  yy14<-as.matrix(df[[4]][ys14])
  yy15<-as.matrix(df[[4]][ys15])
  yy16<-as.matrix(df[[4]][ys16])
  yy17<-as.matrix(df[[4]][ys17])
  yy18<-as.matrix(df[[4]][ys18])
  yy19<-as.matrix(df[[4]][ys19])
  yy20<-as.matrix(df[[4]][ys20])
  yy21<-as.matrix(df[[4]][ys21])
  yy22<-as.matrix(df[[4]][ys22])
  yy23<-as.matrix(df[[4]][ys23])
  yy24<-as.matrix(df[[4]][ys24])
  yy25<-as.matrix(df[[4]][ys25])
  yy26<-as.matrix(df[[4]][ys26])
  yy27<-as.matrix(df[[4]][ys27])
  yy28<-as.matrix(df[[4]][ys28])
  yy29<-as.matrix(df[[4]][ys29])
  yy30<-as.matrix(df[[4]][ys30])
  yy31<-as.matrix(df[[4]][ys31])
  yy32<-as.matrix(df[[4]][ys32])
  yy33<-as.matrix(df[[4]][ys33])
  yy34<-as.matrix(df[[4]][ys34])
  yy35<-as.matrix(df[[4]][ys35])
  yy36<-as.matrix(df[[4]][ys36])
  yy37<-as.matrix(df[[4]][ys37])
  yy38<-as.matrix(df[[4]][ys38])
  yy39<-as.matrix(df[[4]][ys39])
  yy40<-as.matrix(df[[4]][ys40])
  yy41<-as.matrix(df[[4]][ys41])
  yy42<-as.matrix(df[[4]][ys42])
  yy43<-as.matrix(df[[4]][ys43])
  yy44<-as.matrix(df[[4]][ys44])
  yy45<-as.matrix(df[[4]][ys45])
  yy46<-as.matrix(df[[4]][ys46])
  yy47<-as.matrix(df[[4]][ys47])
  yy48<-as.matrix(df[[4]][ys48])
  yy49<-as.matrix(df[[4]][ys49])
  
  df90.0<-rbind(a90[1][[1]][1],yy0)
  df90.1<-rbind(a90[1][[1]][2],yy1)
  df90.2<-rbind(a90[1][[1]][3],yy2)
  df90.3<-rbind(a90[1][[1]][4],yy3)
  df90.4<-rbind(a90[1][[1]][5],yy4)
  df90.5<-rbind(a90[1][[1]][6],yy5)
  df90.6<-rbind(a90[1][[1]][7],yy6)
  df90.7<-rbind(a90[1][[1]][8],yy7)
  df90.8<-rbind(a90[1][[1]][9],yy8)
  df90.9<-rbind(a90[1][[1]][10],yy9)
  df90.10<-rbind(a90[1][[1]][11],yy10)
  df90.11<-rbind(a90[1][[1]][12],yy11)
  df90.12<-rbind(a90[1][[1]][13],yy12)
  df90.13<-rbind(a90[1][[1]][14],yy13)
  df90.14<-rbind(a90[1][[1]][15],yy14)
  df90.15<-rbind(a90[1][[1]][16],yy15)
  df90.16<-rbind(a90[1][[1]][17],yy16)
  df90.17<-rbind(a90[1][[1]][18],yy17)
  df90.18<-rbind(a90[1][[1]][19],yy18)
  df90.19<-rbind(a90[1][[1]][20],yy19)
  df90.20<-rbind(a90[1][[1]][21],yy20)
  df90.21<-rbind(a90[1][[1]][22],yy21)
  df90.22<-rbind(a90[1][[1]][23],yy22)
  df90.23<-rbind(a90[1][[1]][24],yy23)
  df90.24<-rbind(a90[1][[1]][25],yy24)
  df90.25<-rbind(a90[1][[1]][26],yy25)
  df90.26<-rbind(a90[1][[1]][27],yy26)
  df90.27<-rbind(a90[1][[1]][28],yy27)
  df90.28<-rbind(a90[1][[1]][29],yy28)
  df90.29<-rbind(a90[1][[1]][30],yy29)
  df90.30<-rbind(a90[1][[1]][31],yy30)
  df90.31<-rbind(a90[1][[1]][32],yy31)
  df90.32<-rbind(a90[1][[1]][33],yy32)
  df90.33<-rbind(a90[1][[1]][34],yy33)
  df90.34<-rbind(a90[1][[1]][35],yy34)
  df90.35<-rbind(a90[1][[1]][36],yy35)
  df90.36<-rbind(a90[1][[1]][37],yy36)
  df90.37<-rbind(a90[1][[1]][38],yy37)
  df90.38<-rbind(a90[1][[1]][39],yy38)
  df90.39<-rbind(a90[1][[1]][40],yy39)
  df90.40<-rbind(a90[1][[1]][41],yy40)
  df90.41<-rbind(a90[1][[1]][42],yy41)
  df90.42<-rbind(a90[1][[1]][43],yy42)
  df90.43<-rbind(a90[1][[1]][44],yy43)
  df90.44<-rbind(a90[1][[1]][45],yy44)
  df90.45<-rbind(a90[1][[1]][46],yy45)
  df90.46<-rbind(a90[1][[1]][47],yy46)
  df90.47<-rbind(a90[1][[1]][48],yy47)
  df90.48<-rbind(a90[1][[1]][49],yy48)
  df90.49<-rbind(a90[1][[1]][50],yy49)
  
  y0.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.0)))
  y0.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.0)))
  y0.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.0)))
  y0.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.0)))
  
  y1.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.1)))
  y1.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.1)))
  y1.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.1)))
  y1.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.1)))
  
  y2.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.2)))
  y2.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.2)))
  y2.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.2)))
  y2.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.2)))
  
  y3.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.3)))
  y3.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.3)))
  y3.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.3)))
  y3.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.3)))
  
  y4.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.4)))
  y4.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.4)))
  y4.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.4)))
  y4.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.4)))
  
  y5.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.5)))
  y5.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.5)))
  y5.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.5)))
  y5.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.5)))
  
  y6.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.6)))
  y6.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.6)))
  y6.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.6)))
  y6.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.6)))
  
  y7.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.7)))
  y7.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.7)))
  y7.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.7)))
  y7.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.7)))
  
  y8.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.8)))
  y8.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.8)))
  y8.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.8)))
  y8.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.8)))
  
  y9.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.9)))
  y9.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.9)))
  y9.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.9)))
  y9.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.9)))
  
  y10.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.10)))
  y10.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.10)))
  y10.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.10)))
  y10.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.10)))
  
  y11.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.11)))
  y11.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.11)))
  y11.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.11)))
  y11.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.11)))
  
  y12.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.12)))
  y12.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.12)))
  y12.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.12)))
  y12.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.12)))
  
  y13.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.13)))
  y13.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.13)))
  y13.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.13)))
  y13.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.13)))
  
  y14.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.14)))
  y14.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.14)))
  y14.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.14)))
  y14.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.14)))
  
  y15.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.15)))
  y15.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.15)))
  y15.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.15)))
  y15.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.15)))
  
  y16.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.16)))
  y16.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.16)))
  y16.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.16)))
  y16.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.16)))
  
  y17.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.17)))
  y17.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.17)))
  y17.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.17)))
  y17.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.17)))
  
  y18.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.18)))
  y18.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.18)))
  y18.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.18)))
  y18.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.18)))
  
  y19.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.19)))
  y19.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.19)))
  y19.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.19)))
  y19.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.19)))
  y20.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.20)))
  y20.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.20)))
  y20.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.20)))
  y20.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.20)))
  
  y21.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.21)))
  y21.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.21)))
  y21.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.21)))
  y21.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.21)))
  
  y22.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.22)))
  y22.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.22)))
  y22.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.22)))
  y22.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.22)))
  
  y23.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.23)))
  y23.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.23)))
  y23.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.23)))
  y23.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.23)))
  
  y24.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.24)))
  y24.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.24)))
  y24.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.24)))
  y24.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.24)))
  
  y25.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.25)))
  y25.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.25)))
  y25.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.25)))
  y25.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.25)))
  
  y26.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.26)))
  y26.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.26)))
  y26.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.26)))
  y26.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.26)))
  
  y27.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.27)))
  y27.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.27)))
  y27.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.27)))
  y27.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.27)))
  
  y28.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.28)))
  y28.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.28)))
  y28.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.28)))
  y28.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.28)))
  
  y29.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.29)))
  y29.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.29)))
  y29.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.29)))
  y29.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.29)))
  y30.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.30)))
  y30.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.30)))
  y30.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.30)))
  y30.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.30)))
  
  y31.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.31)))
  y31.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.31)))
  y31.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.31)))
  y31.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.31)))
  
  y32.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.32)))
  y32.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.32)))
  y32.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.32)))
  y32.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.32)))
  
  y33.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.33)))
  y33.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.33)))
  y33.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.33)))
  y33.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.33)))
  
  y34.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.34)))
  y34.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.34)))
  y34.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.34)))
  y34.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.34)))
  
  y35.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.35)))
  y35.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.35)))
  y35.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.35)))
  y35.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.35)))
  
  y36.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.36)))
  y36.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.36)))
  y36.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.36)))
  y36.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.36)))
  
  y37.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.37)))
  y37.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.37)))
  y37.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.37)))
  y37.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.37)))
  
  y38.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.38)))
  y38.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.38)))
  y38.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.38)))
  y38.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.38)))
  
  y39.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.39)))
  y39.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.39)))
  y39.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.39)))
  y39.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.39)))
  y40.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.40)))
  y40.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.40)))
  y40.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.40)))
  y40.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.40)))
  
  y41.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.41)))
  y41.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.41)))
  y41.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.41)))
  y41.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.41)))
  
  y42.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.42)))
  y42.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.42)))
  y42.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.42)))
  y42.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.42)))
  
  y43.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.43)))
  y43.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.43)))
  y43.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.43)))
  y43.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.43)))
  
  y44.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.44)))
  y44.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.44)))
  y44.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.44)))
  y44.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.44)))
  
  y45.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.45)))
  y45.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.45)))
  y45.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.45)))
  y45.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.45)))
  
  y46.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.46)))
  y46.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.46)))
  y46.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.46)))
  y46.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.46)))
  
  y47.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.47)))
  y47.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.47)))
  y47.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.47)))
  y47.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.47)))
  
  y48.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.48)))
  y48.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.48)))
  y48.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.48)))
  y48.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.48)))
  
  y49.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.49)))
  y49.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.49)))
  y49.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.49)))
  y49.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.49)))
  
  wei<-cbind(y0.wei,y1.wei,y2.wei,y3.wei,y4.wei,y5.wei,y6.wei,y7.wei,y8.wei,y9.wei,y10.wei,y11.wei,y12.wei,y13.wei,y14.wei,y15.wei,y16.wei,y17.wei,y18.wei,y19.wei,y20.wei,y21.wei,y22.wei,y23.wei,y24.wei,y25.wei,y26.wei,y27.wei,y28.wei,y29.wei,y30.wei,y31.wei,y32.wei,y33.wei,y34.wei,y35.wei,y36.wei,y37.wei,y38.wei,y39.wei,y40.wei,y41.wei,y42.wei,y43.wei,y44.wei,y45.wei,y46.wei,y47.wei,y48.wei,y49.wei)
  gum<-cbind(y0.gum,y1.gum,y2.gum,y3.gum,y4.gum,y5.gum,y6.gum,y7.gum,y8.gum,y9.gum,y10.gum,y11.gum,y12.gum,y13.gum,y14.gum,y15.gum,y16.gum,y17.gum,y18.gum,y19.gum,y20.gum,y21.gum,y22.gum,y23.gum,y24.gum,y25.gum,y26.gum,y27.gum,y28.gum,y29.gum,y30.gum,y31.gum,y32.gum,y33.gum,y34.gum,y35.gum,y36.gum,y37.gum,y38.gum,y39.gum,y40.gum,y41.gum,y42.gum,y43.gum,y44.gum,y45.gum,y46.gum,y47.gum,y48.gum,y49.gum)
  pe3<-cbind(y0.pe3,y1.pe3,y2.pe3,y3.pe3,y4.pe3,y5.pe3,y6.pe3,y7.pe3,y8.pe3,y9.pe3,y10.pe3,y11.pe3,y12.pe3,y13.pe3,y14.pe3,y15.pe3,y16.pe3,y17.pe3,y18.pe3,y19.pe3,y20.pe3,y21.pe3,y22.pe3,y23.pe3,y24.pe3,y25.pe3,y26.pe3,y27.pe3,y28.pe3,y29.pe3,y30.pe3,y31.pe3,y32.pe3,y33.pe3,y34.pe3,y35.pe3,y36.pe3,y37.pe3,y38.pe3,y39.pe3,y40.pe3,y41.pe3,y42.pe3,y43.pe3,y44.pe3,y45.pe3,y46.pe3,y47.pe3,y48.pe3,y49.pe3)
  ln3<-cbind(y0.ln3,y1.ln3,y2.ln3,y3.ln3,y4.ln3,y5.ln3,y6.ln3,y7.ln3,y8.ln3,y9.ln3,y10.ln3,y11.ln3,y12.ln3,y13.ln3,y14.ln3,y15.ln3,y16.ln3,y17.ln3,y18.ln3,y19.ln3,y20.ln3,y21.ln3,y22.ln3,y23.ln3,y24.ln3,y25.ln3,y26.ln3,y27.ln3,y28.ln3,y29.ln3,y30.ln3,y31.ln3,y32.ln3,y33.ln3,y34.ln3,y35.ln3,y36.ln3,y37.ln3,y38.ln3,y39.ln3,y40.ln3,y41.ln3,y42.ln3,y43.ln3,y44.ln3,y45.ln3,y46.ln3,y47.ln3,y48.ln3,y49.ln3)
  
  return(list(wei,gum,pe3,ln3))

}
inc.hqs.99.90<-function(dfs,year,quantile){
  dff<-dfs[[1]]
  df<-dfs[[1]]
  a90<-par.80y(dfm=dff,year = year,quantile=quantile)
  quantile=0.90
  ys0<-seq(1,year*365+11,1)
  ys1<-seq(1,(year*365+10+1*365),1)
  ys2<-seq(1,(year*365+10+2*365),1)
  ys3<-seq(1,(year*365+10+3*365),1)
  ys4<-seq(1,(year*365+10+4*365),1)
  ys5<-seq(1,(year*365+10+5*365),1)
  ys6<-seq(1,(year*365+10+6*365),1)
  ys7<-seq(1,(year*365+10+7*365),1)
  ys8<-seq(1,(year*365+10+8*365),1)
  ys9<-seq(1,(year*365+10+9*365),1)
  ys10<-seq(1,(year*365+10+10*365),1)
  ys11<-seq(1,(year*365+10+11*365),1)
  ys12<-seq(1,(year*365+10+12*365),1)
  ys13<-seq(1,(year*365+10+13*365),1)
  ys14<-seq(1,(year*365+10+14*365),1)
  ys15<-seq(1,(year*365+10+15*365),1)
  ys16<-seq(1,(year*365+10+16*365),1)
  ys17<-seq(1,(year*365+10+17*365),1)
  ys18<-seq(1,(year*365+10+18*365),1)
  ys19<-seq(1,(year*365+10+19*365),1)
  ys20<-seq(1,(year*365+10+20*365),1)
  ys21<-seq(1,(year*365+10+21*365),1)
  ys22<-seq(1,(year*365+10+22*365),1)
  ys23<-seq(1,(year*365+10+23*365),1)
  ys24<-seq(1,(year*365+10+24*365),1)
  ys25<-seq(1,(year*365+10+25*365),1)
  ys26<-seq(1,(year*365+10+26*365),1)
  ys27<-seq(1,(year*365+10+27*365),1)
  ys28<-seq(1,(year*365+10+28*365),1)
  ys29<-seq(1,(year*365+10+29*365),1)
  ys30<-seq(1,(year*365+10+30*365),1)
  ys31<-seq(1,(year*365+10+31*365),1)
  ys32<-seq(1,(year*365+10+32*365),1)
  ys33<-seq(1,(year*365+10+33*365),1)
  ys34<-seq(1,(year*365+10+34*365),1)
  ys35<-seq(1,(year*365+10+35*365),1)
  ys36<-seq(1,(year*365+10+36*365),1)
  ys37<-seq(1,(year*365+10+37*365),1)
  ys38<-seq(1,(year*365+10+38*365),1)
  ys39<-seq(1,(year*365+10+39*365),1)
  ys40<-seq(1,(year*365+10+40*365),1)
  ys41<-seq(1,(year*365+10+41*365),1)
  ys42<-seq(1,(year*365+10+42*365),1)
  ys43<-seq(1,(year*365+10+43*365),1)
  ys44<-seq(1,(year*365+10+44*365),1)
  ys45<-seq(1,(year*365+10+45*365),1)
  ys46<-seq(1,(year*365+10+46*365),1)
  ys47<-seq(1,(year*365+10+47*365),1)
  ys48<-seq(1,(year*365+10+48*365),1)
  ys49<-seq(1,(year*365+10+49*365),1)
  
  yy0<-as.matrix(df[[4]][ys0])
  yy1<-as.matrix(df[[4]][ys1])
  yy2<-as.matrix(df[[4]][ys2])
  yy3<-as.matrix(df[[4]][ys3])
  yy4<-as.matrix(df[[4]][ys4])
  yy5<-as.matrix(df[[4]][ys5])
  yy6<-as.matrix(df[[4]][ys6])
  yy7<-as.matrix(df[[4]][ys7])
  yy8<-as.matrix(df[[4]][ys8])
  yy9<-as.matrix(df[[4]][ys9])
  yy10<-as.matrix(df[[4]][ys10])
  yy11<-as.matrix(df[[4]][ys11])
  yy12<-as.matrix(df[[4]][ys12])
  yy13<-as.matrix(df[[4]][ys13])
  yy14<-as.matrix(df[[4]][ys14])
  yy15<-as.matrix(df[[4]][ys15])
  yy16<-as.matrix(df[[4]][ys16])
  yy17<-as.matrix(df[[4]][ys17])
  yy18<-as.matrix(df[[4]][ys18])
  yy19<-as.matrix(df[[4]][ys19])
  yy20<-as.matrix(df[[4]][ys20])
  yy21<-as.matrix(df[[4]][ys21])
  yy22<-as.matrix(df[[4]][ys22])
  yy23<-as.matrix(df[[4]][ys23])
  yy24<-as.matrix(df[[4]][ys24])
  yy25<-as.matrix(df[[4]][ys25])
  yy26<-as.matrix(df[[4]][ys26])
  yy27<-as.matrix(df[[4]][ys27])
  yy28<-as.matrix(df[[4]][ys28])
  yy29<-as.matrix(df[[4]][ys29])
  yy30<-as.matrix(df[[4]][ys30])
  yy31<-as.matrix(df[[4]][ys31])
  yy32<-as.matrix(df[[4]][ys32])
  yy33<-as.matrix(df[[4]][ys33])
  yy34<-as.matrix(df[[4]][ys34])
  yy35<-as.matrix(df[[4]][ys35])
  yy36<-as.matrix(df[[4]][ys36])
  yy37<-as.matrix(df[[4]][ys37])
  yy38<-as.matrix(df[[4]][ys38])
  yy39<-as.matrix(df[[4]][ys39])
  yy40<-as.matrix(df[[4]][ys40])
  yy41<-as.matrix(df[[4]][ys41])
  yy42<-as.matrix(df[[4]][ys42])
  yy43<-as.matrix(df[[4]][ys43])
  yy44<-as.matrix(df[[4]][ys44])
  yy45<-as.matrix(df[[4]][ys45])
  yy46<-as.matrix(df[[4]][ys46])
  yy47<-as.matrix(df[[4]][ys47])
  yy48<-as.matrix(df[[4]][ys48])
  yy49<-as.matrix(df[[4]][ys49])
  
  df90.0<-rbind(a90[1][[1]][1],yy0)
  df90.1<-rbind(a90[1][[1]][2],yy1)
  df90.2<-rbind(a90[1][[1]][3],yy2)
  df90.3<-rbind(a90[1][[1]][4],yy3)
  df90.4<-rbind(a90[1][[1]][5],yy4)
  df90.5<-rbind(a90[1][[1]][6],yy5)
  df90.6<-rbind(a90[1][[1]][7],yy6)
  df90.7<-rbind(a90[1][[1]][8],yy7)
  df90.8<-rbind(a90[1][[1]][9],yy8)
  df90.9<-rbind(a90[1][[1]][10],yy9)
  df90.10<-rbind(a90[1][[1]][11],yy10)
  df90.11<-rbind(a90[1][[1]][12],yy11)
  df90.12<-rbind(a90[1][[1]][13],yy12)
  df90.13<-rbind(a90[1][[1]][14],yy13)
  df90.14<-rbind(a90[1][[1]][15],yy14)
  df90.15<-rbind(a90[1][[1]][16],yy15)
  df90.16<-rbind(a90[1][[1]][17],yy16)
  df90.17<-rbind(a90[1][[1]][18],yy17)
  df90.18<-rbind(a90[1][[1]][19],yy18)
  df90.19<-rbind(a90[1][[1]][20],yy19)
  df90.20<-rbind(a90[1][[1]][21],yy20)
  df90.21<-rbind(a90[1][[1]][22],yy21)
  df90.22<-rbind(a90[1][[1]][23],yy22)
  df90.23<-rbind(a90[1][[1]][24],yy23)
  df90.24<-rbind(a90[1][[1]][25],yy24)
  df90.25<-rbind(a90[1][[1]][26],yy25)
  df90.26<-rbind(a90[1][[1]][27],yy26)
  df90.27<-rbind(a90[1][[1]][28],yy27)
  df90.28<-rbind(a90[1][[1]][29],yy28)
  df90.29<-rbind(a90[1][[1]][30],yy29)
  df90.30<-rbind(a90[1][[1]][31],yy30)
  df90.31<-rbind(a90[1][[1]][32],yy31)
  df90.32<-rbind(a90[1][[1]][33],yy32)
  df90.33<-rbind(a90[1][[1]][34],yy33)
  df90.34<-rbind(a90[1][[1]][35],yy34)
  df90.35<-rbind(a90[1][[1]][36],yy35)
  df90.36<-rbind(a90[1][[1]][37],yy36)
  df90.37<-rbind(a90[1][[1]][38],yy37)
  df90.38<-rbind(a90[1][[1]][39],yy38)
  df90.39<-rbind(a90[1][[1]][40],yy39)
  df90.40<-rbind(a90[1][[1]][41],yy40)
  df90.41<-rbind(a90[1][[1]][42],yy41)
  df90.42<-rbind(a90[1][[1]][43],yy42)
  df90.43<-rbind(a90[1][[1]][44],yy43)
  df90.44<-rbind(a90[1][[1]][45],yy44)
  df90.45<-rbind(a90[1][[1]][46],yy45)
  df90.46<-rbind(a90[1][[1]][47],yy46)
  df90.47<-rbind(a90[1][[1]][48],yy47)
  df90.48<-rbind(a90[1][[1]][49],yy48)
  df90.49<-rbind(a90[1][[1]][50],yy49)
  
  y0.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.0)))
  y0.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.0)))
  y0.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.0)))
  y0.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.0)))
  
  y1.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.1)))
  y1.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.1)))
  y1.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.1)))
  y1.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.1)))
  
  y2.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.2)))
  y2.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.2)))
  y2.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.2)))
  y2.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.2)))
  
  y3.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.3)))
  y3.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.3)))
  y3.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.3)))
  y3.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.3)))
  
  y4.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.4)))
  y4.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.4)))
  y4.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.4)))
  y4.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.4)))
  
  y5.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.5)))
  y5.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.5)))
  y5.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.5)))
  y5.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.5)))
  
  y6.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.6)))
  y6.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.6)))
  y6.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.6)))
  y6.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.6)))
  
  y7.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.7)))
  y7.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.7)))
  y7.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.7)))
  y7.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.7)))
  
  y8.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.8)))
  y8.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.8)))
  y8.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.8)))
  y8.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.8)))
  
  y9.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.9)))
  y9.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.9)))
  y9.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.9)))
  y9.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.9)))
  
  y10.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.10)))
  y10.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.10)))
  y10.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.10)))
  y10.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.10)))
  
  y11.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.11)))
  y11.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.11)))
  y11.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.11)))
  y11.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.11)))
  
  y12.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.12)))
  y12.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.12)))
  y12.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.12)))
  y12.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.12)))
  
  y13.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.13)))
  y13.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.13)))
  y13.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.13)))
  y13.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.13)))
  
  y14.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.14)))
  y14.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.14)))
  y14.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.14)))
  y14.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.14)))
  
  y15.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.15)))
  y15.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.15)))
  y15.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.15)))
  y15.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.15)))
  
  y16.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.16)))
  y16.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.16)))
  y16.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.16)))
  y16.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.16)))
  
  y17.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.17)))
  y17.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.17)))
  y17.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.17)))
  y17.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.17)))
  
  y18.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.18)))
  y18.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.18)))
  y18.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.18)))
  y18.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.18)))
  
  y19.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.19)))
  y19.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.19)))
  y19.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.19)))
  y19.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.19)))
  y20.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.20)))
  y20.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.20)))
  y20.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.20)))
  y20.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.20)))
  
  y21.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.21)))
  y21.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.21)))
  y21.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.21)))
  y21.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.21)))
  
  y22.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.22)))
  y22.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.22)))
  y22.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.22)))
  y22.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.22)))
  
  y23.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.23)))
  y23.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.23)))
  y23.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.23)))
  y23.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.23)))
  
  y24.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.24)))
  y24.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.24)))
  y24.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.24)))
  y24.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.24)))
  
  y25.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.25)))
  y25.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.25)))
  y25.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.25)))
  y25.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.25)))
  
  y26.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.26)))
  y26.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.26)))
  y26.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.26)))
  y26.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.26)))
  
  y27.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.27)))
  y27.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.27)))
  y27.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.27)))
  y27.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.27)))
  
  y28.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.28)))
  y28.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.28)))
  y28.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.28)))
  y28.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.28)))
  
  y29.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.29)))
  y29.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.29)))
  y29.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.29)))
  y29.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.29)))
  y30.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.30)))
  y30.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.30)))
  y30.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.30)))
  y30.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.30)))
  
  y31.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.31)))
  y31.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.31)))
  y31.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.31)))
  y31.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.31)))
  
  y32.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.32)))
  y32.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.32)))
  y32.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.32)))
  y32.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.32)))
  
  y33.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.33)))
  y33.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.33)))
  y33.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.33)))
  y33.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.33)))
  
  y34.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.34)))
  y34.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.34)))
  y34.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.34)))
  y34.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.34)))
  
  y35.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.35)))
  y35.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.35)))
  y35.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.35)))
  y35.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.35)))
  
  y36.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.36)))
  y36.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.36)))
  y36.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.36)))
  y36.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.36)))
  
  y37.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.37)))
  y37.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.37)))
  y37.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.37)))
  y37.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.37)))
  
  y38.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.38)))
  y38.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.38)))
  y38.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.38)))
  y38.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.38)))
  
  y39.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.39)))
  y39.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.39)))
  y39.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.39)))
  y39.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.39)))
  y40.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.40)))
  y40.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.40)))
  y40.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.40)))
  y40.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.40)))
  
  y41.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.41)))
  y41.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.41)))
  y41.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.41)))
  y41.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.41)))
  
  y42.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.42)))
  y42.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.42)))
  y42.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.42)))
  y42.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.42)))
  
  y43.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.43)))
  y43.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.43)))
  y43.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.43)))
  y43.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.43)))
  
  y44.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.44)))
  y44.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.44)))
  y44.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.44)))
  y44.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.44)))
  
  y45.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.45)))
  y45.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.45)))
  y45.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.45)))
  y45.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.45)))
  
  y46.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.46)))
  y46.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.46)))
  y46.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.46)))
  y46.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.46)))
  
  y47.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.47)))
  y47.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.47)))
  y47.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.47)))
  y47.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.47)))
  
  y48.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.48)))
  y48.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.48)))
  y48.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.48)))
  y48.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.48)))
  
  y49.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.49)))
  y49.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.49)))
  y49.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.49)))
  y49.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.49)))
  
  wei<-cbind(y0.wei,y1.wei,y2.wei,y3.wei,y4.wei,y5.wei,y6.wei,y7.wei,y8.wei,y9.wei,y10.wei,y11.wei,y12.wei,y13.wei,y14.wei,y15.wei,y16.wei,y17.wei,y18.wei,y19.wei,y20.wei,y21.wei,y22.wei,y23.wei,y24.wei,y25.wei,y26.wei,y27.wei,y28.wei,y29.wei,y30.wei,y31.wei,y32.wei,y33.wei,y34.wei,y35.wei,y36.wei,y37.wei,y38.wei,y39.wei,y40.wei,y41.wei,y42.wei,y43.wei,y44.wei,y45.wei,y46.wei,y47.wei,y48.wei,y49.wei)
  gum<-cbind(y0.gum,y1.gum,y2.gum,y3.gum,y4.gum,y5.gum,y6.gum,y7.gum,y8.gum,y9.gum,y10.gum,y11.gum,y12.gum,y13.gum,y14.gum,y15.gum,y16.gum,y17.gum,y18.gum,y19.gum,y20.gum,y21.gum,y22.gum,y23.gum,y24.gum,y25.gum,y26.gum,y27.gum,y28.gum,y29.gum,y30.gum,y31.gum,y32.gum,y33.gum,y34.gum,y35.gum,y36.gum,y37.gum,y38.gum,y39.gum,y40.gum,y41.gum,y42.gum,y43.gum,y44.gum,y45.gum,y46.gum,y47.gum,y48.gum,y49.gum)
  pe3<-cbind(y0.pe3,y1.pe3,y2.pe3,y3.pe3,y4.pe3,y5.pe3,y6.pe3,y7.pe3,y8.pe3,y9.pe3,y10.pe3,y11.pe3,y12.pe3,y13.pe3,y14.pe3,y15.pe3,y16.pe3,y17.pe3,y18.pe3,y19.pe3,y20.pe3,y21.pe3,y22.pe3,y23.pe3,y24.pe3,y25.pe3,y26.pe3,y27.pe3,y28.pe3,y29.pe3,y30.pe3,y31.pe3,y32.pe3,y33.pe3,y34.pe3,y35.pe3,y36.pe3,y37.pe3,y38.pe3,y39.pe3,y40.pe3,y41.pe3,y42.pe3,y43.pe3,y44.pe3,y45.pe3,y46.pe3,y47.pe3,y48.pe3,y49.pe3)
  ln3<-cbind(y0.ln3,y1.ln3,y2.ln3,y3.ln3,y4.ln3,y5.ln3,y6.ln3,y7.ln3,y8.ln3,y9.ln3,y10.ln3,y11.ln3,y12.ln3,y13.ln3,y14.ln3,y15.ln3,y16.ln3,y17.ln3,y18.ln3,y19.ln3,y20.ln3,y21.ln3,y22.ln3,y23.ln3,y24.ln3,y25.ln3,y26.ln3,y27.ln3,y28.ln3,y29.ln3,y30.ln3,y31.ln3,y32.ln3,y33.ln3,y34.ln3,y35.ln3,y36.ln3,y37.ln3,y38.ln3,y39.ln3,y40.ln3,y41.ln3,y42.ln3,y43.ln3,y44.ln3,y45.ln3,y46.ln3,y47.ln3,y48.ln3,y49.ln3)
  
  return(list(wei,gum,pe3,ln3))
  
}
inc.hqs.999.90<-function(dfs,year,quantile){
  dff<-dfs[[1]]
  df<-dfs[[1]]
  a90<-par.80y(dfm=dff,year = year,quantile=quantile)
  quantile=0.90
  ys0<-seq(1,year*365+11,1)
  ys1<-seq(1,(year*365+10+1*365),1)
  ys2<-seq(1,(year*365+10+2*365),1)
  ys3<-seq(1,(year*365+10+3*365),1)
  ys4<-seq(1,(year*365+10+4*365),1)
  ys5<-seq(1,(year*365+10+5*365),1)
  ys6<-seq(1,(year*365+10+6*365),1)
  ys7<-seq(1,(year*365+10+7*365),1)
  ys8<-seq(1,(year*365+10+8*365),1)
  ys9<-seq(1,(year*365+10+9*365),1)
  ys10<-seq(1,(year*365+10+10*365),1)
  ys11<-seq(1,(year*365+10+11*365),1)
  ys12<-seq(1,(year*365+10+12*365),1)
  ys13<-seq(1,(year*365+10+13*365),1)
  ys14<-seq(1,(year*365+10+14*365),1)
  ys15<-seq(1,(year*365+10+15*365),1)
  ys16<-seq(1,(year*365+10+16*365),1)
  ys17<-seq(1,(year*365+10+17*365),1)
  ys18<-seq(1,(year*365+10+18*365),1)
  ys19<-seq(1,(year*365+10+19*365),1)
  ys20<-seq(1,(year*365+10+20*365),1)
  ys21<-seq(1,(year*365+10+21*365),1)
  ys22<-seq(1,(year*365+10+22*365),1)
  ys23<-seq(1,(year*365+10+23*365),1)
  ys24<-seq(1,(year*365+10+24*365),1)
  ys25<-seq(1,(year*365+10+25*365),1)
  ys26<-seq(1,(year*365+10+26*365),1)
  ys27<-seq(1,(year*365+10+27*365),1)
  ys28<-seq(1,(year*365+10+28*365),1)
  ys29<-seq(1,(year*365+10+29*365),1)
  ys30<-seq(1,(year*365+10+30*365),1)
  ys31<-seq(1,(year*365+10+31*365),1)
  ys32<-seq(1,(year*365+10+32*365),1)
  ys33<-seq(1,(year*365+10+33*365),1)
  ys34<-seq(1,(year*365+10+34*365),1)
  ys35<-seq(1,(year*365+10+35*365),1)
  ys36<-seq(1,(year*365+10+36*365),1)
  ys37<-seq(1,(year*365+10+37*365),1)
  ys38<-seq(1,(year*365+10+38*365),1)
  ys39<-seq(1,(year*365+10+39*365),1)
  ys40<-seq(1,(year*365+10+40*365),1)
  ys41<-seq(1,(year*365+10+41*365),1)
  ys42<-seq(1,(year*365+10+42*365),1)
  ys43<-seq(1,(year*365+10+43*365),1)
  ys44<-seq(1,(year*365+10+44*365),1)
  ys45<-seq(1,(year*365+10+45*365),1)
  ys46<-seq(1,(year*365+10+46*365),1)
  ys47<-seq(1,(year*365+10+47*365),1)
  ys48<-seq(1,(year*365+10+48*365),1)
  ys49<-seq(1,(year*365+10+49*365),1)
  
  yy0<-as.matrix(df[[4]][ys0])
  yy1<-as.matrix(df[[4]][ys1])
  yy2<-as.matrix(df[[4]][ys2])
  yy3<-as.matrix(df[[4]][ys3])
  yy4<-as.matrix(df[[4]][ys4])
  yy5<-as.matrix(df[[4]][ys5])
  yy6<-as.matrix(df[[4]][ys6])
  yy7<-as.matrix(df[[4]][ys7])
  yy8<-as.matrix(df[[4]][ys8])
  yy9<-as.matrix(df[[4]][ys9])
  yy10<-as.matrix(df[[4]][ys10])
  yy11<-as.matrix(df[[4]][ys11])
  yy12<-as.matrix(df[[4]][ys12])
  yy13<-as.matrix(df[[4]][ys13])
  yy14<-as.matrix(df[[4]][ys14])
  yy15<-as.matrix(df[[4]][ys15])
  yy16<-as.matrix(df[[4]][ys16])
  yy17<-as.matrix(df[[4]][ys17])
  yy18<-as.matrix(df[[4]][ys18])
  yy19<-as.matrix(df[[4]][ys19])
  yy20<-as.matrix(df[[4]][ys20])
  yy21<-as.matrix(df[[4]][ys21])
  yy22<-as.matrix(df[[4]][ys22])
  yy23<-as.matrix(df[[4]][ys23])
  yy24<-as.matrix(df[[4]][ys24])
  yy25<-as.matrix(df[[4]][ys25])
  yy26<-as.matrix(df[[4]][ys26])
  yy27<-as.matrix(df[[4]][ys27])
  yy28<-as.matrix(df[[4]][ys28])
  yy29<-as.matrix(df[[4]][ys29])
  yy30<-as.matrix(df[[4]][ys30])
  yy31<-as.matrix(df[[4]][ys31])
  yy32<-as.matrix(df[[4]][ys32])
  yy33<-as.matrix(df[[4]][ys33])
  yy34<-as.matrix(df[[4]][ys34])
  yy35<-as.matrix(df[[4]][ys35])
  yy36<-as.matrix(df[[4]][ys36])
  yy37<-as.matrix(df[[4]][ys37])
  yy38<-as.matrix(df[[4]][ys38])
  yy39<-as.matrix(df[[4]][ys39])
  yy40<-as.matrix(df[[4]][ys40])
  yy41<-as.matrix(df[[4]][ys41])
  yy42<-as.matrix(df[[4]][ys42])
  yy43<-as.matrix(df[[4]][ys43])
  yy44<-as.matrix(df[[4]][ys44])
  yy45<-as.matrix(df[[4]][ys45])
  yy46<-as.matrix(df[[4]][ys46])
  yy47<-as.matrix(df[[4]][ys47])
  yy48<-as.matrix(df[[4]][ys48])
  yy49<-as.matrix(df[[4]][ys49])
  
  df90.0<-rbind(a90[1][[1]][1],yy0)
  df90.1<-rbind(a90[1][[1]][2],yy1)
  df90.2<-rbind(a90[1][[1]][3],yy2)
  df90.3<-rbind(a90[1][[1]][4],yy3)
  df90.4<-rbind(a90[1][[1]][5],yy4)
  df90.5<-rbind(a90[1][[1]][6],yy5)
  df90.6<-rbind(a90[1][[1]][7],yy6)
  df90.7<-rbind(a90[1][[1]][8],yy7)
  df90.8<-rbind(a90[1][[1]][9],yy8)
  df90.9<-rbind(a90[1][[1]][10],yy9)
  df90.10<-rbind(a90[1][[1]][11],yy10)
  df90.11<-rbind(a90[1][[1]][12],yy11)
  df90.12<-rbind(a90[1][[1]][13],yy12)
  df90.13<-rbind(a90[1][[1]][14],yy13)
  df90.14<-rbind(a90[1][[1]][15],yy14)
  df90.15<-rbind(a90[1][[1]][16],yy15)
  df90.16<-rbind(a90[1][[1]][17],yy16)
  df90.17<-rbind(a90[1][[1]][18],yy17)
  df90.18<-rbind(a90[1][[1]][19],yy18)
  df90.19<-rbind(a90[1][[1]][20],yy19)
  df90.20<-rbind(a90[1][[1]][21],yy20)
  df90.21<-rbind(a90[1][[1]][22],yy21)
  df90.22<-rbind(a90[1][[1]][23],yy22)
  df90.23<-rbind(a90[1][[1]][24],yy23)
  df90.24<-rbind(a90[1][[1]][25],yy24)
  df90.25<-rbind(a90[1][[1]][26],yy25)
  df90.26<-rbind(a90[1][[1]][27],yy26)
  df90.27<-rbind(a90[1][[1]][28],yy27)
  df90.28<-rbind(a90[1][[1]][29],yy28)
  df90.29<-rbind(a90[1][[1]][30],yy29)
  df90.30<-rbind(a90[1][[1]][31],yy30)
  df90.31<-rbind(a90[1][[1]][32],yy31)
  df90.32<-rbind(a90[1][[1]][33],yy32)
  df90.33<-rbind(a90[1][[1]][34],yy33)
  df90.34<-rbind(a90[1][[1]][35],yy34)
  df90.35<-rbind(a90[1][[1]][36],yy35)
  df90.36<-rbind(a90[1][[1]][37],yy36)
  df90.37<-rbind(a90[1][[1]][38],yy37)
  df90.38<-rbind(a90[1][[1]][39],yy38)
  df90.39<-rbind(a90[1][[1]][40],yy39)
  df90.40<-rbind(a90[1][[1]][41],yy40)
  df90.41<-rbind(a90[1][[1]][42],yy41)
  df90.42<-rbind(a90[1][[1]][43],yy42)
  df90.43<-rbind(a90[1][[1]][44],yy43)
  df90.44<-rbind(a90[1][[1]][45],yy44)
  df90.45<-rbind(a90[1][[1]][46],yy45)
  df90.46<-rbind(a90[1][[1]][47],yy46)
  df90.47<-rbind(a90[1][[1]][48],yy47)
  df90.48<-rbind(a90[1][[1]][49],yy48)
  df90.49<-rbind(a90[1][[1]][50],yy49)
  
  y0.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.0)))
  y0.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.0)))
  y0.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.0)))
  y0.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.0)))
  
  y1.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.1)))
  y1.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.1)))
  y1.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.1)))
  y1.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.1)))
  
  y2.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.2)))
  y2.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.2)))
  y2.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.2)))
  y2.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.2)))
  
  y3.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.3)))
  y3.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.3)))
  y3.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.3)))
  y3.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.3)))
  
  y4.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.4)))
  y4.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.4)))
  y4.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.4)))
  y4.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.4)))
  
  y5.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.5)))
  y5.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.5)))
  y5.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.5)))
  y5.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.5)))
  
  y6.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.6)))
  y6.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.6)))
  y6.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.6)))
  y6.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.6)))
  
  y7.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.7)))
  y7.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.7)))
  y7.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.7)))
  y7.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.7)))
  
  y8.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.8)))
  y8.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.8)))
  y8.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.8)))
  y8.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.8)))
  
  y9.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.9)))
  y9.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.9)))
  y9.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.9)))
  y9.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.9)))
  
  y10.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.10)))
  y10.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.10)))
  y10.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.10)))
  y10.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.10)))
  
  y11.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.11)))
  y11.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.11)))
  y11.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.11)))
  y11.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.11)))
  
  y12.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.12)))
  y12.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.12)))
  y12.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.12)))
  y12.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.12)))
  
  y13.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.13)))
  y13.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.13)))
  y13.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.13)))
  y13.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.13)))
  
  y14.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.14)))
  y14.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.14)))
  y14.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.14)))
  y14.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.14)))
  
  y15.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.15)))
  y15.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.15)))
  y15.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.15)))
  y15.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.15)))
  
  y16.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.16)))
  y16.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.16)))
  y16.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.16)))
  y16.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.16)))
  
  y17.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.17)))
  y17.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.17)))
  y17.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.17)))
  y17.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.17)))
  
  y18.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.18)))
  y18.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.18)))
  y18.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.18)))
  y18.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.18)))
  
  y19.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.19)))
  y19.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.19)))
  y19.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.19)))
  y19.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.19)))
  y20.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.20)))
  y20.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.20)))
  y20.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.20)))
  y20.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.20)))
  
  y21.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.21)))
  y21.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.21)))
  y21.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.21)))
  y21.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.21)))
  
  y22.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.22)))
  y22.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.22)))
  y22.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.22)))
  y22.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.22)))
  
  y23.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.23)))
  y23.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.23)))
  y23.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.23)))
  y23.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.23)))
  
  y24.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.24)))
  y24.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.24)))
  y24.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.24)))
  y24.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.24)))
  
  y25.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.25)))
  y25.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.25)))
  y25.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.25)))
  y25.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.25)))
  
  y26.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.26)))
  y26.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.26)))
  y26.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.26)))
  y26.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.26)))
  
  y27.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.27)))
  y27.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.27)))
  y27.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.27)))
  y27.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.27)))
  
  y28.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.28)))
  y28.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.28)))
  y28.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.28)))
  y28.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.28)))
  
  y29.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.29)))
  y29.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.29)))
  y29.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.29)))
  y29.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.29)))
  y30.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.30)))
  y30.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.30)))
  y30.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.30)))
  y30.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.30)))
  
  y31.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.31)))
  y31.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.31)))
  y31.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.31)))
  y31.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.31)))
  
  y32.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.32)))
  y32.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.32)))
  y32.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.32)))
  y32.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.32)))
  
  y33.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.33)))
  y33.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.33)))
  y33.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.33)))
  y33.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.33)))
  
  y34.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.34)))
  y34.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.34)))
  y34.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.34)))
  y34.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.34)))
  
  y35.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.35)))
  y35.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.35)))
  y35.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.35)))
  y35.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.35)))
  
  y36.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.36)))
  y36.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.36)))
  y36.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.36)))
  y36.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.36)))
  
  y37.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.37)))
  y37.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.37)))
  y37.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.37)))
  y37.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.37)))
  
  y38.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.38)))
  y38.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.38)))
  y38.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.38)))
  y38.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.38)))
  
  y39.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.39)))
  y39.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.39)))
  y39.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.39)))
  y39.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.39)))
  y40.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.40)))
  y40.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.40)))
  y40.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.40)))
  y40.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.40)))
  
  y41.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.41)))
  y41.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.41)))
  y41.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.41)))
  y41.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.41)))
  
  y42.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.42)))
  y42.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.42)))
  y42.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.42)))
  y42.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.42)))
  
  y43.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.43)))
  y43.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.43)))
  y43.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.43)))
  y43.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.43)))
  
  y44.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.44)))
  y44.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.44)))
  y44.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.44)))
  y44.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.44)))
  
  y45.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.45)))
  y45.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.45)))
  y45.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.45)))
  y45.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.45)))
  
  y46.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.46)))
  y46.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.46)))
  y46.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.46)))
  y46.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.46)))
  
  y47.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.47)))
  y47.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.47)))
  y47.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.47)))
  y47.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.47)))
  
  y48.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.48)))
  y48.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.48)))
  y48.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.48)))
  y48.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.48)))
  
  y49.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.49)))
  y49.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.49)))
  y49.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.49)))
  y49.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.49)))
  
  wei<-cbind(y0.wei,y1.wei,y2.wei,y3.wei,y4.wei,y5.wei,y6.wei,y7.wei,y8.wei,y9.wei,y10.wei,y11.wei,y12.wei,y13.wei,y14.wei,y15.wei,y16.wei,y17.wei,y18.wei,y19.wei,y20.wei,y21.wei,y22.wei,y23.wei,y24.wei,y25.wei,y26.wei,y27.wei,y28.wei,y29.wei,y30.wei,y31.wei,y32.wei,y33.wei,y34.wei,y35.wei,y36.wei,y37.wei,y38.wei,y39.wei,y40.wei,y41.wei,y42.wei,y43.wei,y44.wei,y45.wei,y46.wei,y47.wei,y48.wei,y49.wei)
  gum<-cbind(y0.gum,y1.gum,y2.gum,y3.gum,y4.gum,y5.gum,y6.gum,y7.gum,y8.gum,y9.gum,y10.gum,y11.gum,y12.gum,y13.gum,y14.gum,y15.gum,y16.gum,y17.gum,y18.gum,y19.gum,y20.gum,y21.gum,y22.gum,y23.gum,y24.gum,y25.gum,y26.gum,y27.gum,y28.gum,y29.gum,y30.gum,y31.gum,y32.gum,y33.gum,y34.gum,y35.gum,y36.gum,y37.gum,y38.gum,y39.gum,y40.gum,y41.gum,y42.gum,y43.gum,y44.gum,y45.gum,y46.gum,y47.gum,y48.gum,y49.gum)
  pe3<-cbind(y0.pe3,y1.pe3,y2.pe3,y3.pe3,y4.pe3,y5.pe3,y6.pe3,y7.pe3,y8.pe3,y9.pe3,y10.pe3,y11.pe3,y12.pe3,y13.pe3,y14.pe3,y15.pe3,y16.pe3,y17.pe3,y18.pe3,y19.pe3,y20.pe3,y21.pe3,y22.pe3,y23.pe3,y24.pe3,y25.pe3,y26.pe3,y27.pe3,y28.pe3,y29.pe3,y30.pe3,y31.pe3,y32.pe3,y33.pe3,y34.pe3,y35.pe3,y36.pe3,y37.pe3,y38.pe3,y39.pe3,y40.pe3,y41.pe3,y42.pe3,y43.pe3,y44.pe3,y45.pe3,y46.pe3,y47.pe3,y48.pe3,y49.pe3)
  ln3<-cbind(y0.ln3,y1.ln3,y2.ln3,y3.ln3,y4.ln3,y5.ln3,y6.ln3,y7.ln3,y8.ln3,y9.ln3,y10.ln3,y11.ln3,y12.ln3,y13.ln3,y14.ln3,y15.ln3,y16.ln3,y17.ln3,y18.ln3,y19.ln3,y20.ln3,y21.ln3,y22.ln3,y23.ln3,y24.ln3,y25.ln3,y26.ln3,y27.ln3,y28.ln3,y29.ln3,y30.ln3,y31.ln3,y32.ln3,y33.ln3,y34.ln3,y35.ln3,y36.ln3,y37.ln3,y38.ln3,y39.ln3,y40.ln3,y41.ln3,y42.ln3,y43.ln3,y44.ln3,y45.ln3,y46.ln3,y47.ln3,y48.ln3,y49.ln3)
  
  return(list(wei,gum,pe3,ln3))
  
}

#### für ein 0.90/99/999 hochwasser am Anfang, dann ein 0.99 Hochwasser berechnet
inc.hqs.90.99<-function(dfs,year,quantile){
  dff<-dfs[[1]]
  df<-dfs[[1]]
  a90<-par.80y(dfm=dff,year = year,quantile=quantile)
  quantile=0.99
  ys0<-seq(1,year*365+11,1)
  ys1<-seq(1,(year*365+10+1*365),1)
  ys2<-seq(1,(year*365+10+2*365),1)
  ys3<-seq(1,(year*365+10+3*365),1)
  ys4<-seq(1,(year*365+10+4*365),1)
  ys5<-seq(1,(year*365+10+5*365),1)
  ys6<-seq(1,(year*365+10+6*365),1)
  ys7<-seq(1,(year*365+10+7*365),1)
  ys8<-seq(1,(year*365+10+8*365),1)
  ys9<-seq(1,(year*365+10+9*365),1)
  ys10<-seq(1,(year*365+10+10*365),1)
  ys11<-seq(1,(year*365+10+11*365),1)
  ys12<-seq(1,(year*365+10+12*365),1)
  ys13<-seq(1,(year*365+10+13*365),1)
  ys14<-seq(1,(year*365+10+14*365),1)
  ys15<-seq(1,(year*365+10+15*365),1)
  ys16<-seq(1,(year*365+10+16*365),1)
  ys17<-seq(1,(year*365+10+17*365),1)
  ys18<-seq(1,(year*365+10+18*365),1)
  ys19<-seq(1,(year*365+10+19*365),1)
  ys20<-seq(1,(year*365+10+20*365),1)
  ys21<-seq(1,(year*365+10+21*365),1)
  ys22<-seq(1,(year*365+10+22*365),1)
  ys23<-seq(1,(year*365+10+23*365),1)
  ys24<-seq(1,(year*365+10+24*365),1)
  ys25<-seq(1,(year*365+10+25*365),1)
  ys26<-seq(1,(year*365+10+26*365),1)
  ys27<-seq(1,(year*365+10+27*365),1)
  ys28<-seq(1,(year*365+10+28*365),1)
  ys29<-seq(1,(year*365+10+29*365),1)
  ys30<-seq(1,(year*365+10+30*365),1)
  ys31<-seq(1,(year*365+10+31*365),1)
  ys32<-seq(1,(year*365+10+32*365),1)
  ys33<-seq(1,(year*365+10+33*365),1)
  ys34<-seq(1,(year*365+10+34*365),1)
  ys35<-seq(1,(year*365+10+35*365),1)
  ys36<-seq(1,(year*365+10+36*365),1)
  ys37<-seq(1,(year*365+10+37*365),1)
  ys38<-seq(1,(year*365+10+38*365),1)
  ys39<-seq(1,(year*365+10+39*365),1)
  ys40<-seq(1,(year*365+10+40*365),1)
  ys41<-seq(1,(year*365+10+41*365),1)
  ys42<-seq(1,(year*365+10+42*365),1)
  ys43<-seq(1,(year*365+10+43*365),1)
  ys44<-seq(1,(year*365+10+44*365),1)
  ys45<-seq(1,(year*365+10+45*365),1)
  ys46<-seq(1,(year*365+10+46*365),1)
  ys47<-seq(1,(year*365+10+47*365),1)
  ys48<-seq(1,(year*365+10+48*365),1)
  ys49<-seq(1,(year*365+10+49*365),1)
  
  yy0<-as.matrix(df[[4]][ys0])
  yy1<-as.matrix(df[[4]][ys1])
  yy2<-as.matrix(df[[4]][ys2])
  yy3<-as.matrix(df[[4]][ys3])
  yy4<-as.matrix(df[[4]][ys4])
  yy5<-as.matrix(df[[4]][ys5])
  yy6<-as.matrix(df[[4]][ys6])
  yy7<-as.matrix(df[[4]][ys7])
  yy8<-as.matrix(df[[4]][ys8])
  yy9<-as.matrix(df[[4]][ys9])
  yy10<-as.matrix(df[[4]][ys10])
  yy11<-as.matrix(df[[4]][ys11])
  yy12<-as.matrix(df[[4]][ys12])
  yy13<-as.matrix(df[[4]][ys13])
  yy14<-as.matrix(df[[4]][ys14])
  yy15<-as.matrix(df[[4]][ys15])
  yy16<-as.matrix(df[[4]][ys16])
  yy17<-as.matrix(df[[4]][ys17])
  yy18<-as.matrix(df[[4]][ys18])
  yy19<-as.matrix(df[[4]][ys19])
  yy20<-as.matrix(df[[4]][ys20])
  yy21<-as.matrix(df[[4]][ys21])
  yy22<-as.matrix(df[[4]][ys22])
  yy23<-as.matrix(df[[4]][ys23])
  yy24<-as.matrix(df[[4]][ys24])
  yy25<-as.matrix(df[[4]][ys25])
  yy26<-as.matrix(df[[4]][ys26])
  yy27<-as.matrix(df[[4]][ys27])
  yy28<-as.matrix(df[[4]][ys28])
  yy29<-as.matrix(df[[4]][ys29])
  yy30<-as.matrix(df[[4]][ys30])
  yy31<-as.matrix(df[[4]][ys31])
  yy32<-as.matrix(df[[4]][ys32])
  yy33<-as.matrix(df[[4]][ys33])
  yy34<-as.matrix(df[[4]][ys34])
  yy35<-as.matrix(df[[4]][ys35])
  yy36<-as.matrix(df[[4]][ys36])
  yy37<-as.matrix(df[[4]][ys37])
  yy38<-as.matrix(df[[4]][ys38])
  yy39<-as.matrix(df[[4]][ys39])
  yy40<-as.matrix(df[[4]][ys40])
  yy41<-as.matrix(df[[4]][ys41])
  yy42<-as.matrix(df[[4]][ys42])
  yy43<-as.matrix(df[[4]][ys43])
  yy44<-as.matrix(df[[4]][ys44])
  yy45<-as.matrix(df[[4]][ys45])
  yy46<-as.matrix(df[[4]][ys46])
  yy47<-as.matrix(df[[4]][ys47])
  yy48<-as.matrix(df[[4]][ys48])
  yy49<-as.matrix(df[[4]][ys49])
  
  df90.0<-rbind(a90[1][[1]][1],yy0)
  df90.1<-rbind(a90[1][[1]][2],yy1)
  df90.2<-rbind(a90[1][[1]][3],yy2)
  df90.3<-rbind(a90[1][[1]][4],yy3)
  df90.4<-rbind(a90[1][[1]][5],yy4)
  df90.5<-rbind(a90[1][[1]][6],yy5)
  df90.6<-rbind(a90[1][[1]][7],yy6)
  df90.7<-rbind(a90[1][[1]][8],yy7)
  df90.8<-rbind(a90[1][[1]][9],yy8)
  df90.9<-rbind(a90[1][[1]][10],yy9)
  df90.10<-rbind(a90[1][[1]][11],yy10)
  df90.11<-rbind(a90[1][[1]][12],yy11)
  df90.12<-rbind(a90[1][[1]][13],yy12)
  df90.13<-rbind(a90[1][[1]][14],yy13)
  df90.14<-rbind(a90[1][[1]][15],yy14)
  df90.15<-rbind(a90[1][[1]][16],yy15)
  df90.16<-rbind(a90[1][[1]][17],yy16)
  df90.17<-rbind(a90[1][[1]][18],yy17)
  df90.18<-rbind(a90[1][[1]][19],yy18)
  df90.19<-rbind(a90[1][[1]][20],yy19)
  df90.20<-rbind(a90[1][[1]][21],yy20)
  df90.21<-rbind(a90[1][[1]][22],yy21)
  df90.22<-rbind(a90[1][[1]][23],yy22)
  df90.23<-rbind(a90[1][[1]][24],yy23)
  df90.24<-rbind(a90[1][[1]][25],yy24)
  df90.25<-rbind(a90[1][[1]][26],yy25)
  df90.26<-rbind(a90[1][[1]][27],yy26)
  df90.27<-rbind(a90[1][[1]][28],yy27)
  df90.28<-rbind(a90[1][[1]][29],yy28)
  df90.29<-rbind(a90[1][[1]][30],yy29)
  df90.30<-rbind(a90[1][[1]][31],yy30)
  df90.31<-rbind(a90[1][[1]][32],yy31)
  df90.32<-rbind(a90[1][[1]][33],yy32)
  df90.33<-rbind(a90[1][[1]][34],yy33)
  df90.34<-rbind(a90[1][[1]][35],yy34)
  df90.35<-rbind(a90[1][[1]][36],yy35)
  df90.36<-rbind(a90[1][[1]][37],yy36)
  df90.37<-rbind(a90[1][[1]][38],yy37)
  df90.38<-rbind(a90[1][[1]][39],yy38)
  df90.39<-rbind(a90[1][[1]][40],yy39)
  df90.40<-rbind(a90[1][[1]][41],yy40)
  df90.41<-rbind(a90[1][[1]][42],yy41)
  df90.42<-rbind(a90[1][[1]][43],yy42)
  df90.43<-rbind(a90[1][[1]][44],yy43)
  df90.44<-rbind(a90[1][[1]][45],yy44)
  df90.45<-rbind(a90[1][[1]][46],yy45)
  df90.46<-rbind(a90[1][[1]][47],yy46)
  df90.47<-rbind(a90[1][[1]][48],yy47)
  df90.48<-rbind(a90[1][[1]][49],yy48)
  df90.49<-rbind(a90[1][[1]][50],yy49)
  
  y0.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.0)))
  y0.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.0)))
  y0.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.0)))
  y0.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.0)))
  
  y1.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.1)))
  y1.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.1)))
  y1.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.1)))
  y1.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.1)))
  
  y2.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.2)))
  y2.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.2)))
  y2.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.2)))
  y2.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.2)))
  
  y3.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.3)))
  y3.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.3)))
  y3.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.3)))
  y3.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.3)))
  
  y4.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.4)))
  y4.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.4)))
  y4.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.4)))
  y4.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.4)))
  
  y5.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.5)))
  y5.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.5)))
  y5.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.5)))
  y5.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.5)))
  
  y6.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.6)))
  y6.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.6)))
  y6.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.6)))
  y6.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.6)))
  
  y7.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.7)))
  y7.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.7)))
  y7.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.7)))
  y7.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.7)))
  
  y8.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.8)))
  y8.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.8)))
  y8.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.8)))
  y8.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.8)))
  
  y9.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.9)))
  y9.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.9)))
  y9.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.9)))
  y9.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.9)))
  
  y10.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.10)))
  y10.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.10)))
  y10.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.10)))
  y10.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.10)))
  
  y11.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.11)))
  y11.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.11)))
  y11.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.11)))
  y11.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.11)))
  
  y12.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.12)))
  y12.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.12)))
  y12.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.12)))
  y12.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.12)))
  
  y13.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.13)))
  y13.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.13)))
  y13.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.13)))
  y13.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.13)))
  
  y14.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.14)))
  y14.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.14)))
  y14.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.14)))
  y14.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.14)))
  
  y15.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.15)))
  y15.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.15)))
  y15.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.15)))
  y15.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.15)))
  
  y16.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.16)))
  y16.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.16)))
  y16.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.16)))
  y16.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.16)))
  
  y17.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.17)))
  y17.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.17)))
  y17.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.17)))
  y17.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.17)))
  
  y18.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.18)))
  y18.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.18)))
  y18.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.18)))
  y18.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.18)))
  
  y19.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.19)))
  y19.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.19)))
  y19.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.19)))
  y19.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.19)))
  y20.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.20)))
  y20.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.20)))
  y20.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.20)))
  y20.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.20)))
  
  y21.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.21)))
  y21.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.21)))
  y21.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.21)))
  y21.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.21)))
  
  y22.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.22)))
  y22.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.22)))
  y22.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.22)))
  y22.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.22)))
  
  y23.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.23)))
  y23.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.23)))
  y23.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.23)))
  y23.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.23)))
  
  y24.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.24)))
  y24.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.24)))
  y24.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.24)))
  y24.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.24)))
  
  y25.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.25)))
  y25.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.25)))
  y25.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.25)))
  y25.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.25)))
  
  y26.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.26)))
  y26.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.26)))
  y26.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.26)))
  y26.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.26)))
  
  y27.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.27)))
  y27.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.27)))
  y27.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.27)))
  y27.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.27)))
  
  y28.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.28)))
  y28.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.28)))
  y28.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.28)))
  y28.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.28)))
  
  y29.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.29)))
  y29.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.29)))
  y29.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.29)))
  y29.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.29)))
  y30.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.30)))
  y30.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.30)))
  y30.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.30)))
  y30.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.30)))
  
  y31.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.31)))
  y31.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.31)))
  y31.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.31)))
  y31.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.31)))
  
  y32.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.32)))
  y32.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.32)))
  y32.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.32)))
  y32.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.32)))
  
  y33.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.33)))
  y33.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.33)))
  y33.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.33)))
  y33.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.33)))
  
  y34.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.34)))
  y34.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.34)))
  y34.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.34)))
  y34.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.34)))
  
  y35.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.35)))
  y35.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.35)))
  y35.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.35)))
  y35.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.35)))
  
  y36.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.36)))
  y36.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.36)))
  y36.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.36)))
  y36.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.36)))
  
  y37.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.37)))
  y37.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.37)))
  y37.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.37)))
  y37.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.37)))
  
  y38.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.38)))
  y38.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.38)))
  y38.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.38)))
  y38.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.38)))
  
  y39.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.39)))
  y39.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.39)))
  y39.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.39)))
  y39.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.39)))
  y40.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.40)))
  y40.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.40)))
  y40.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.40)))
  y40.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.40)))
  
  y41.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.41)))
  y41.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.41)))
  y41.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.41)))
  y41.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.41)))
  
  y42.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.42)))
  y42.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.42)))
  y42.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.42)))
  y42.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.42)))
  
  y43.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.43)))
  y43.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.43)))
  y43.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.43)))
  y43.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.43)))
  
  y44.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.44)))
  y44.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.44)))
  y44.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.44)))
  y44.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.44)))
  
  y45.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.45)))
  y45.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.45)))
  y45.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.45)))
  y45.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.45)))
  
  y46.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.46)))
  y46.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.46)))
  y46.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.46)))
  y46.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.46)))
  
  y47.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.47)))
  y47.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.47)))
  y47.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.47)))
  y47.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.47)))
  
  y48.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.48)))
  y48.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.48)))
  y48.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.48)))
  y48.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.48)))
  
  y49.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.49)))
  y49.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.49)))
  y49.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.49)))
  y49.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.49)))
  
  wei<-cbind(y0.wei,y1.wei,y2.wei,y3.wei,y4.wei,y5.wei,y6.wei,y7.wei,y8.wei,y9.wei,y10.wei,y11.wei,y12.wei,y13.wei,y14.wei,y15.wei,y16.wei,y17.wei,y18.wei,y19.wei,y20.wei,y21.wei,y22.wei,y23.wei,y24.wei,y25.wei,y26.wei,y27.wei,y28.wei,y29.wei,y30.wei,y31.wei,y32.wei,y33.wei,y34.wei,y35.wei,y36.wei,y37.wei,y38.wei,y39.wei,y40.wei,y41.wei,y42.wei,y43.wei,y44.wei,y45.wei,y46.wei,y47.wei,y48.wei,y49.wei)
  gum<-cbind(y0.gum,y1.gum,y2.gum,y3.gum,y4.gum,y5.gum,y6.gum,y7.gum,y8.gum,y9.gum,y10.gum,y11.gum,y12.gum,y13.gum,y14.gum,y15.gum,y16.gum,y17.gum,y18.gum,y19.gum,y20.gum,y21.gum,y22.gum,y23.gum,y24.gum,y25.gum,y26.gum,y27.gum,y28.gum,y29.gum,y30.gum,y31.gum,y32.gum,y33.gum,y34.gum,y35.gum,y36.gum,y37.gum,y38.gum,y39.gum,y40.gum,y41.gum,y42.gum,y43.gum,y44.gum,y45.gum,y46.gum,y47.gum,y48.gum,y49.gum)
  pe3<-cbind(y0.pe3,y1.pe3,y2.pe3,y3.pe3,y4.pe3,y5.pe3,y6.pe3,y7.pe3,y8.pe3,y9.pe3,y10.pe3,y11.pe3,y12.pe3,y13.pe3,y14.pe3,y15.pe3,y16.pe3,y17.pe3,y18.pe3,y19.pe3,y20.pe3,y21.pe3,y22.pe3,y23.pe3,y24.pe3,y25.pe3,y26.pe3,y27.pe3,y28.pe3,y29.pe3,y30.pe3,y31.pe3,y32.pe3,y33.pe3,y34.pe3,y35.pe3,y36.pe3,y37.pe3,y38.pe3,y39.pe3,y40.pe3,y41.pe3,y42.pe3,y43.pe3,y44.pe3,y45.pe3,y46.pe3,y47.pe3,y48.pe3,y49.pe3)
  ln3<-cbind(y0.ln3,y1.ln3,y2.ln3,y3.ln3,y4.ln3,y5.ln3,y6.ln3,y7.ln3,y8.ln3,y9.ln3,y10.ln3,y11.ln3,y12.ln3,y13.ln3,y14.ln3,y15.ln3,y16.ln3,y17.ln3,y18.ln3,y19.ln3,y20.ln3,y21.ln3,y22.ln3,y23.ln3,y24.ln3,y25.ln3,y26.ln3,y27.ln3,y28.ln3,y29.ln3,y30.ln3,y31.ln3,y32.ln3,y33.ln3,y34.ln3,y35.ln3,y36.ln3,y37.ln3,y38.ln3,y39.ln3,y40.ln3,y41.ln3,y42.ln3,y43.ln3,y44.ln3,y45.ln3,y46.ln3,y47.ln3,y48.ln3,y49.ln3)
  
  return(list(wei,gum,pe3,ln3))
  
 }
inc.hqs.99.99<-function(dfs,year,quantile){
  dff<-dfs[[1]]
  df<-dfs[[1]]
  a90<-par.80y(dfm=dff,year = year,quantile=quantile)
  quantile=0.99
  ys0<-seq(1,year*365+11,1)
  ys1<-seq(1,(year*365+10+1*365),1)
  ys2<-seq(1,(year*365+10+2*365),1)
  ys3<-seq(1,(year*365+10+3*365),1)
  ys4<-seq(1,(year*365+10+4*365),1)
  ys5<-seq(1,(year*365+10+5*365),1)
  ys6<-seq(1,(year*365+10+6*365),1)
  ys7<-seq(1,(year*365+10+7*365),1)
  ys8<-seq(1,(year*365+10+8*365),1)
  ys9<-seq(1,(year*365+10+9*365),1)
  ys10<-seq(1,(year*365+10+10*365),1)
  ys11<-seq(1,(year*365+10+11*365),1)
  ys12<-seq(1,(year*365+10+12*365),1)
  ys13<-seq(1,(year*365+10+13*365),1)
  ys14<-seq(1,(year*365+10+14*365),1)
  ys15<-seq(1,(year*365+10+15*365),1)
  ys16<-seq(1,(year*365+10+16*365),1)
  ys17<-seq(1,(year*365+10+17*365),1)
  ys18<-seq(1,(year*365+10+18*365),1)
  ys19<-seq(1,(year*365+10+19*365),1)
  ys20<-seq(1,(year*365+10+20*365),1)
  ys21<-seq(1,(year*365+10+21*365),1)
  ys22<-seq(1,(year*365+10+22*365),1)
  ys23<-seq(1,(year*365+10+23*365),1)
  ys24<-seq(1,(year*365+10+24*365),1)
  ys25<-seq(1,(year*365+10+25*365),1)
  ys26<-seq(1,(year*365+10+26*365),1)
  ys27<-seq(1,(year*365+10+27*365),1)
  ys28<-seq(1,(year*365+10+28*365),1)
  ys29<-seq(1,(year*365+10+29*365),1)
  ys30<-seq(1,(year*365+10+30*365),1)
  ys31<-seq(1,(year*365+10+31*365),1)
  ys32<-seq(1,(year*365+10+32*365),1)
  ys33<-seq(1,(year*365+10+33*365),1)
  ys34<-seq(1,(year*365+10+34*365),1)
  ys35<-seq(1,(year*365+10+35*365),1)
  ys36<-seq(1,(year*365+10+36*365),1)
  ys37<-seq(1,(year*365+10+37*365),1)
  ys38<-seq(1,(year*365+10+38*365),1)
  ys39<-seq(1,(year*365+10+39*365),1)
  ys40<-seq(1,(year*365+10+40*365),1)
  ys41<-seq(1,(year*365+10+41*365),1)
  ys42<-seq(1,(year*365+10+42*365),1)
  ys43<-seq(1,(year*365+10+43*365),1)
  ys44<-seq(1,(year*365+10+44*365),1)
  ys45<-seq(1,(year*365+10+45*365),1)
  ys46<-seq(1,(year*365+10+46*365),1)
  ys47<-seq(1,(year*365+10+47*365),1)
  ys48<-seq(1,(year*365+10+48*365),1)
  ys49<-seq(1,(year*365+10+49*365),1)
  
  yy0<-as.matrix(df[[4]][ys0])
  yy1<-as.matrix(df[[4]][ys1])
  yy2<-as.matrix(df[[4]][ys2])
  yy3<-as.matrix(df[[4]][ys3])
  yy4<-as.matrix(df[[4]][ys4])
  yy5<-as.matrix(df[[4]][ys5])
  yy6<-as.matrix(df[[4]][ys6])
  yy7<-as.matrix(df[[4]][ys7])
  yy8<-as.matrix(df[[4]][ys8])
  yy9<-as.matrix(df[[4]][ys9])
  yy10<-as.matrix(df[[4]][ys10])
  yy11<-as.matrix(df[[4]][ys11])
  yy12<-as.matrix(df[[4]][ys12])
  yy13<-as.matrix(df[[4]][ys13])
  yy14<-as.matrix(df[[4]][ys14])
  yy15<-as.matrix(df[[4]][ys15])
  yy16<-as.matrix(df[[4]][ys16])
  yy17<-as.matrix(df[[4]][ys17])
  yy18<-as.matrix(df[[4]][ys18])
  yy19<-as.matrix(df[[4]][ys19])
  yy20<-as.matrix(df[[4]][ys20])
  yy21<-as.matrix(df[[4]][ys21])
  yy22<-as.matrix(df[[4]][ys22])
  yy23<-as.matrix(df[[4]][ys23])
  yy24<-as.matrix(df[[4]][ys24])
  yy25<-as.matrix(df[[4]][ys25])
  yy26<-as.matrix(df[[4]][ys26])
  yy27<-as.matrix(df[[4]][ys27])
  yy28<-as.matrix(df[[4]][ys28])
  yy29<-as.matrix(df[[4]][ys29])
  yy30<-as.matrix(df[[4]][ys30])
  yy31<-as.matrix(df[[4]][ys31])
  yy32<-as.matrix(df[[4]][ys32])
  yy33<-as.matrix(df[[4]][ys33])
  yy34<-as.matrix(df[[4]][ys34])
  yy35<-as.matrix(df[[4]][ys35])
  yy36<-as.matrix(df[[4]][ys36])
  yy37<-as.matrix(df[[4]][ys37])
  yy38<-as.matrix(df[[4]][ys38])
  yy39<-as.matrix(df[[4]][ys39])
  yy40<-as.matrix(df[[4]][ys40])
  yy41<-as.matrix(df[[4]][ys41])
  yy42<-as.matrix(df[[4]][ys42])
  yy43<-as.matrix(df[[4]][ys43])
  yy44<-as.matrix(df[[4]][ys44])
  yy45<-as.matrix(df[[4]][ys45])
  yy46<-as.matrix(df[[4]][ys46])
  yy47<-as.matrix(df[[4]][ys47])
  yy48<-as.matrix(df[[4]][ys48])
  yy49<-as.matrix(df[[4]][ys49])
  
  df90.0<-rbind(a90[1][[1]][1],yy0)
  df90.1<-rbind(a90[1][[1]][2],yy1)
  df90.2<-rbind(a90[1][[1]][3],yy2)
  df90.3<-rbind(a90[1][[1]][4],yy3)
  df90.4<-rbind(a90[1][[1]][5],yy4)
  df90.5<-rbind(a90[1][[1]][6],yy5)
  df90.6<-rbind(a90[1][[1]][7],yy6)
  df90.7<-rbind(a90[1][[1]][8],yy7)
  df90.8<-rbind(a90[1][[1]][9],yy8)
  df90.9<-rbind(a90[1][[1]][10],yy9)
  df90.10<-rbind(a90[1][[1]][11],yy10)
  df90.11<-rbind(a90[1][[1]][12],yy11)
  df90.12<-rbind(a90[1][[1]][13],yy12)
  df90.13<-rbind(a90[1][[1]][14],yy13)
  df90.14<-rbind(a90[1][[1]][15],yy14)
  df90.15<-rbind(a90[1][[1]][16],yy15)
  df90.16<-rbind(a90[1][[1]][17],yy16)
  df90.17<-rbind(a90[1][[1]][18],yy17)
  df90.18<-rbind(a90[1][[1]][19],yy18)
  df90.19<-rbind(a90[1][[1]][20],yy19)
  df90.20<-rbind(a90[1][[1]][21],yy20)
  df90.21<-rbind(a90[1][[1]][22],yy21)
  df90.22<-rbind(a90[1][[1]][23],yy22)
  df90.23<-rbind(a90[1][[1]][24],yy23)
  df90.24<-rbind(a90[1][[1]][25],yy24)
  df90.25<-rbind(a90[1][[1]][26],yy25)
  df90.26<-rbind(a90[1][[1]][27],yy26)
  df90.27<-rbind(a90[1][[1]][28],yy27)
  df90.28<-rbind(a90[1][[1]][29],yy28)
  df90.29<-rbind(a90[1][[1]][30],yy29)
  df90.30<-rbind(a90[1][[1]][31],yy30)
  df90.31<-rbind(a90[1][[1]][32],yy31)
  df90.32<-rbind(a90[1][[1]][33],yy32)
  df90.33<-rbind(a90[1][[1]][34],yy33)
  df90.34<-rbind(a90[1][[1]][35],yy34)
  df90.35<-rbind(a90[1][[1]][36],yy35)
  df90.36<-rbind(a90[1][[1]][37],yy36)
  df90.37<-rbind(a90[1][[1]][38],yy37)
  df90.38<-rbind(a90[1][[1]][39],yy38)
  df90.39<-rbind(a90[1][[1]][40],yy39)
  df90.40<-rbind(a90[1][[1]][41],yy40)
  df90.41<-rbind(a90[1][[1]][42],yy41)
  df90.42<-rbind(a90[1][[1]][43],yy42)
  df90.43<-rbind(a90[1][[1]][44],yy43)
  df90.44<-rbind(a90[1][[1]][45],yy44)
  df90.45<-rbind(a90[1][[1]][46],yy45)
  df90.46<-rbind(a90[1][[1]][47],yy46)
  df90.47<-rbind(a90[1][[1]][48],yy47)
  df90.48<-rbind(a90[1][[1]][49],yy48)
  df90.49<-rbind(a90[1][[1]][50],yy49)
  
  y0.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.0)))
  y0.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.0)))
  y0.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.0)))
  y0.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.0)))
  
  y1.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.1)))
  y1.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.1)))
  y1.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.1)))
  y1.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.1)))
  
  y2.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.2)))
  y2.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.2)))
  y2.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.2)))
  y2.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.2)))
  
  y3.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.3)))
  y3.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.3)))
  y3.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.3)))
  y3.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.3)))
  
  y4.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.4)))
  y4.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.4)))
  y4.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.4)))
  y4.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.4)))
  
  y5.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.5)))
  y5.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.5)))
  y5.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.5)))
  y5.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.5)))
  
  y6.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.6)))
  y6.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.6)))
  y6.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.6)))
  y6.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.6)))
  
  y7.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.7)))
  y7.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.7)))
  y7.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.7)))
  y7.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.7)))
  
  y8.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.8)))
  y8.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.8)))
  y8.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.8)))
  y8.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.8)))
  
  y9.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.9)))
  y9.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.9)))
  y9.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.9)))
  y9.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.9)))
  
  y10.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.10)))
  y10.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.10)))
  y10.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.10)))
  y10.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.10)))
  
  y11.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.11)))
  y11.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.11)))
  y11.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.11)))
  y11.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.11)))
  
  y12.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.12)))
  y12.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.12)))
  y12.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.12)))
  y12.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.12)))
  
  y13.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.13)))
  y13.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.13)))
  y13.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.13)))
  y13.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.13)))
  
  y14.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.14)))
  y14.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.14)))
  y14.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.14)))
  y14.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.14)))
  
  y15.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.15)))
  y15.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.15)))
  y15.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.15)))
  y15.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.15)))
  
  y16.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.16)))
  y16.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.16)))
  y16.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.16)))
  y16.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.16)))
  
  y17.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.17)))
  y17.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.17)))
  y17.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.17)))
  y17.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.17)))
  
  y18.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.18)))
  y18.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.18)))
  y18.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.18)))
  y18.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.18)))
  
  y19.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.19)))
  y19.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.19)))
  y19.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.19)))
  y19.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.19)))
  y20.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.20)))
  y20.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.20)))
  y20.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.20)))
  y20.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.20)))
  
  y21.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.21)))
  y21.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.21)))
  y21.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.21)))
  y21.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.21)))
  
  y22.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.22)))
  y22.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.22)))
  y22.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.22)))
  y22.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.22)))
  
  y23.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.23)))
  y23.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.23)))
  y23.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.23)))
  y23.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.23)))
  
  y24.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.24)))
  y24.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.24)))
  y24.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.24)))
  y24.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.24)))
  
  y25.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.25)))
  y25.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.25)))
  y25.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.25)))
  y25.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.25)))
  
  y26.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.26)))
  y26.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.26)))
  y26.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.26)))
  y26.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.26)))
  
  y27.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.27)))
  y27.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.27)))
  y27.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.27)))
  y27.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.27)))
  
  y28.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.28)))
  y28.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.28)))
  y28.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.28)))
  y28.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.28)))
  
  y29.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.29)))
  y29.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.29)))
  y29.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.29)))
  y29.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.29)))
  y30.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.30)))
  y30.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.30)))
  y30.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.30)))
  y30.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.30)))
  
  y31.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.31)))
  y31.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.31)))
  y31.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.31)))
  y31.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.31)))
  
  y32.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.32)))
  y32.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.32)))
  y32.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.32)))
  y32.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.32)))
  
  y33.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.33)))
  y33.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.33)))
  y33.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.33)))
  y33.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.33)))
  
  y34.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.34)))
  y34.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.34)))
  y34.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.34)))
  y34.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.34)))
  
  y35.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.35)))
  y35.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.35)))
  y35.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.35)))
  y35.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.35)))
  
  y36.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.36)))
  y36.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.36)))
  y36.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.36)))
  y36.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.36)))
  
  y37.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.37)))
  y37.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.37)))
  y37.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.37)))
  y37.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.37)))
  
  y38.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.38)))
  y38.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.38)))
  y38.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.38)))
  y38.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.38)))
  
  y39.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.39)))
  y39.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.39)))
  y39.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.39)))
  y39.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.39)))
  y40.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.40)))
  y40.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.40)))
  y40.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.40)))
  y40.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.40)))
  
  y41.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.41)))
  y41.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.41)))
  y41.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.41)))
  y41.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.41)))
  
  y42.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.42)))
  y42.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.42)))
  y42.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.42)))
  y42.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.42)))
  
  y43.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.43)))
  y43.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.43)))
  y43.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.43)))
  y43.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.43)))
  
  y44.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.44)))
  y44.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.44)))
  y44.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.44)))
  y44.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.44)))
  
  y45.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.45)))
  y45.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.45)))
  y45.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.45)))
  y45.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.45)))
  
  y46.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.46)))
  y46.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.46)))
  y46.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.46)))
  y46.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.46)))
  
  y47.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.47)))
  y47.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.47)))
  y47.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.47)))
  y47.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.47)))
  
  y48.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.48)))
  y48.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.48)))
  y48.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.48)))
  y48.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.48)))
  
  y49.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.49)))
  y49.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.49)))
  y49.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.49)))
  y49.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.49)))
  
  wei<-cbind(y0.wei,y1.wei,y2.wei,y3.wei,y4.wei,y5.wei,y6.wei,y7.wei,y8.wei,y9.wei,y10.wei,y11.wei,y12.wei,y13.wei,y14.wei,y15.wei,y16.wei,y17.wei,y18.wei,y19.wei,y20.wei,y21.wei,y22.wei,y23.wei,y24.wei,y25.wei,y26.wei,y27.wei,y28.wei,y29.wei,y30.wei,y31.wei,y32.wei,y33.wei,y34.wei,y35.wei,y36.wei,y37.wei,y38.wei,y39.wei,y40.wei,y41.wei,y42.wei,y43.wei,y44.wei,y45.wei,y46.wei,y47.wei,y48.wei,y49.wei)
  gum<-cbind(y0.gum,y1.gum,y2.gum,y3.gum,y4.gum,y5.gum,y6.gum,y7.gum,y8.gum,y9.gum,y10.gum,y11.gum,y12.gum,y13.gum,y14.gum,y15.gum,y16.gum,y17.gum,y18.gum,y19.gum,y20.gum,y21.gum,y22.gum,y23.gum,y24.gum,y25.gum,y26.gum,y27.gum,y28.gum,y29.gum,y30.gum,y31.gum,y32.gum,y33.gum,y34.gum,y35.gum,y36.gum,y37.gum,y38.gum,y39.gum,y40.gum,y41.gum,y42.gum,y43.gum,y44.gum,y45.gum,y46.gum,y47.gum,y48.gum,y49.gum)
  pe3<-cbind(y0.pe3,y1.pe3,y2.pe3,y3.pe3,y4.pe3,y5.pe3,y6.pe3,y7.pe3,y8.pe3,y9.pe3,y10.pe3,y11.pe3,y12.pe3,y13.pe3,y14.pe3,y15.pe3,y16.pe3,y17.pe3,y18.pe3,y19.pe3,y20.pe3,y21.pe3,y22.pe3,y23.pe3,y24.pe3,y25.pe3,y26.pe3,y27.pe3,y28.pe3,y29.pe3,y30.pe3,y31.pe3,y32.pe3,y33.pe3,y34.pe3,y35.pe3,y36.pe3,y37.pe3,y38.pe3,y39.pe3,y40.pe3,y41.pe3,y42.pe3,y43.pe3,y44.pe3,y45.pe3,y46.pe3,y47.pe3,y48.pe3,y49.pe3)
  ln3<-cbind(y0.ln3,y1.ln3,y2.ln3,y3.ln3,y4.ln3,y5.ln3,y6.ln3,y7.ln3,y8.ln3,y9.ln3,y10.ln3,y11.ln3,y12.ln3,y13.ln3,y14.ln3,y15.ln3,y16.ln3,y17.ln3,y18.ln3,y19.ln3,y20.ln3,y21.ln3,y22.ln3,y23.ln3,y24.ln3,y25.ln3,y26.ln3,y27.ln3,y28.ln3,y29.ln3,y30.ln3,y31.ln3,y32.ln3,y33.ln3,y34.ln3,y35.ln3,y36.ln3,y37.ln3,y38.ln3,y39.ln3,y40.ln3,y41.ln3,y42.ln3,y43.ln3,y44.ln3,y45.ln3,y46.ln3,y47.ln3,y48.ln3,y49.ln3)
  
  return(list(wei,gum,pe3,ln3))
  
 }
inc.hqs.999.99<-function(dfs,year,quantile){
  dff<-dfs[[1]]
  df<-dfs[[1]]
  a90<-par.80y(dfm=dff,year = year,quantile=quantile)
  quantile=0.99
  ys0<-seq(1,year*365+11,1)
  ys1<-seq(1,(year*365+10+1*365),1)
  ys2<-seq(1,(year*365+10+2*365),1)
  ys3<-seq(1,(year*365+10+3*365),1)
  ys4<-seq(1,(year*365+10+4*365),1)
  ys5<-seq(1,(year*365+10+5*365),1)
  ys6<-seq(1,(year*365+10+6*365),1)
  ys7<-seq(1,(year*365+10+7*365),1)
  ys8<-seq(1,(year*365+10+8*365),1)
  ys9<-seq(1,(year*365+10+9*365),1)
  ys10<-seq(1,(year*365+10+10*365),1)
  ys11<-seq(1,(year*365+10+11*365),1)
  ys12<-seq(1,(year*365+10+12*365),1)
  ys13<-seq(1,(year*365+10+13*365),1)
  ys14<-seq(1,(year*365+10+14*365),1)
  ys15<-seq(1,(year*365+10+15*365),1)
  ys16<-seq(1,(year*365+10+16*365),1)
  ys17<-seq(1,(year*365+10+17*365),1)
  ys18<-seq(1,(year*365+10+18*365),1)
  ys19<-seq(1,(year*365+10+19*365),1)
  ys20<-seq(1,(year*365+10+20*365),1)
  ys21<-seq(1,(year*365+10+21*365),1)
  ys22<-seq(1,(year*365+10+22*365),1)
  ys23<-seq(1,(year*365+10+23*365),1)
  ys24<-seq(1,(year*365+10+24*365),1)
  ys25<-seq(1,(year*365+10+25*365),1)
  ys26<-seq(1,(year*365+10+26*365),1)
  ys27<-seq(1,(year*365+10+27*365),1)
  ys28<-seq(1,(year*365+10+28*365),1)
  ys29<-seq(1,(year*365+10+29*365),1)
  ys30<-seq(1,(year*365+10+30*365),1)
  ys31<-seq(1,(year*365+10+31*365),1)
  ys32<-seq(1,(year*365+10+32*365),1)
  ys33<-seq(1,(year*365+10+33*365),1)
  ys34<-seq(1,(year*365+10+34*365),1)
  ys35<-seq(1,(year*365+10+35*365),1)
  ys36<-seq(1,(year*365+10+36*365),1)
  ys37<-seq(1,(year*365+10+37*365),1)
  ys38<-seq(1,(year*365+10+38*365),1)
  ys39<-seq(1,(year*365+10+39*365),1)
  ys40<-seq(1,(year*365+10+40*365),1)
  ys41<-seq(1,(year*365+10+41*365),1)
  ys42<-seq(1,(year*365+10+42*365),1)
  ys43<-seq(1,(year*365+10+43*365),1)
  ys44<-seq(1,(year*365+10+44*365),1)
  ys45<-seq(1,(year*365+10+45*365),1)
  ys46<-seq(1,(year*365+10+46*365),1)
  ys47<-seq(1,(year*365+10+47*365),1)
  ys48<-seq(1,(year*365+10+48*365),1)
  ys49<-seq(1,(year*365+10+49*365),1)
  
  yy0<-as.matrix(df[[4]][ys0])
  yy1<-as.matrix(df[[4]][ys1])
  yy2<-as.matrix(df[[4]][ys2])
  yy3<-as.matrix(df[[4]][ys3])
  yy4<-as.matrix(df[[4]][ys4])
  yy5<-as.matrix(df[[4]][ys5])
  yy6<-as.matrix(df[[4]][ys6])
  yy7<-as.matrix(df[[4]][ys7])
  yy8<-as.matrix(df[[4]][ys8])
  yy9<-as.matrix(df[[4]][ys9])
  yy10<-as.matrix(df[[4]][ys10])
  yy11<-as.matrix(df[[4]][ys11])
  yy12<-as.matrix(df[[4]][ys12])
  yy13<-as.matrix(df[[4]][ys13])
  yy14<-as.matrix(df[[4]][ys14])
  yy15<-as.matrix(df[[4]][ys15])
  yy16<-as.matrix(df[[4]][ys16])
  yy17<-as.matrix(df[[4]][ys17])
  yy18<-as.matrix(df[[4]][ys18])
  yy19<-as.matrix(df[[4]][ys19])
  yy20<-as.matrix(df[[4]][ys20])
  yy21<-as.matrix(df[[4]][ys21])
  yy22<-as.matrix(df[[4]][ys22])
  yy23<-as.matrix(df[[4]][ys23])
  yy24<-as.matrix(df[[4]][ys24])
  yy25<-as.matrix(df[[4]][ys25])
  yy26<-as.matrix(df[[4]][ys26])
  yy27<-as.matrix(df[[4]][ys27])
  yy28<-as.matrix(df[[4]][ys28])
  yy29<-as.matrix(df[[4]][ys29])
  yy30<-as.matrix(df[[4]][ys30])
  yy31<-as.matrix(df[[4]][ys31])
  yy32<-as.matrix(df[[4]][ys32])
  yy33<-as.matrix(df[[4]][ys33])
  yy34<-as.matrix(df[[4]][ys34])
  yy35<-as.matrix(df[[4]][ys35])
  yy36<-as.matrix(df[[4]][ys36])
  yy37<-as.matrix(df[[4]][ys37])
  yy38<-as.matrix(df[[4]][ys38])
  yy39<-as.matrix(df[[4]][ys39])
  yy40<-as.matrix(df[[4]][ys40])
  yy41<-as.matrix(df[[4]][ys41])
  yy42<-as.matrix(df[[4]][ys42])
  yy43<-as.matrix(df[[4]][ys43])
  yy44<-as.matrix(df[[4]][ys44])
  yy45<-as.matrix(df[[4]][ys45])
  yy46<-as.matrix(df[[4]][ys46])
  yy47<-as.matrix(df[[4]][ys47])
  yy48<-as.matrix(df[[4]][ys48])
  yy49<-as.matrix(df[[4]][ys49])
  
  df90.0<-rbind(a90[1][[1]][1],yy0)
  df90.1<-rbind(a90[1][[1]][2],yy1)
  df90.2<-rbind(a90[1][[1]][3],yy2)
  df90.3<-rbind(a90[1][[1]][4],yy3)
  df90.4<-rbind(a90[1][[1]][5],yy4)
  df90.5<-rbind(a90[1][[1]][6],yy5)
  df90.6<-rbind(a90[1][[1]][7],yy6)
  df90.7<-rbind(a90[1][[1]][8],yy7)
  df90.8<-rbind(a90[1][[1]][9],yy8)
  df90.9<-rbind(a90[1][[1]][10],yy9)
  df90.10<-rbind(a90[1][[1]][11],yy10)
  df90.11<-rbind(a90[1][[1]][12],yy11)
  df90.12<-rbind(a90[1][[1]][13],yy12)
  df90.13<-rbind(a90[1][[1]][14],yy13)
  df90.14<-rbind(a90[1][[1]][15],yy14)
  df90.15<-rbind(a90[1][[1]][16],yy15)
  df90.16<-rbind(a90[1][[1]][17],yy16)
  df90.17<-rbind(a90[1][[1]][18],yy17)
  df90.18<-rbind(a90[1][[1]][19],yy18)
  df90.19<-rbind(a90[1][[1]][20],yy19)
  df90.20<-rbind(a90[1][[1]][21],yy20)
  df90.21<-rbind(a90[1][[1]][22],yy21)
  df90.22<-rbind(a90[1][[1]][23],yy22)
  df90.23<-rbind(a90[1][[1]][24],yy23)
  df90.24<-rbind(a90[1][[1]][25],yy24)
  df90.25<-rbind(a90[1][[1]][26],yy25)
  df90.26<-rbind(a90[1][[1]][27],yy26)
  df90.27<-rbind(a90[1][[1]][28],yy27)
  df90.28<-rbind(a90[1][[1]][29],yy28)
  df90.29<-rbind(a90[1][[1]][30],yy29)
  df90.30<-rbind(a90[1][[1]][31],yy30)
  df90.31<-rbind(a90[1][[1]][32],yy31)
  df90.32<-rbind(a90[1][[1]][33],yy32)
  df90.33<-rbind(a90[1][[1]][34],yy33)
  df90.34<-rbind(a90[1][[1]][35],yy34)
  df90.35<-rbind(a90[1][[1]][36],yy35)
  df90.36<-rbind(a90[1][[1]][37],yy36)
  df90.37<-rbind(a90[1][[1]][38],yy37)
  df90.38<-rbind(a90[1][[1]][39],yy38)
  df90.39<-rbind(a90[1][[1]][40],yy39)
  df90.40<-rbind(a90[1][[1]][41],yy40)
  df90.41<-rbind(a90[1][[1]][42],yy41)
  df90.42<-rbind(a90[1][[1]][43],yy42)
  df90.43<-rbind(a90[1][[1]][44],yy43)
  df90.44<-rbind(a90[1][[1]][45],yy44)
  df90.45<-rbind(a90[1][[1]][46],yy45)
  df90.46<-rbind(a90[1][[1]][47],yy46)
  df90.47<-rbind(a90[1][[1]][48],yy47)
  df90.48<-rbind(a90[1][[1]][49],yy48)
  df90.49<-rbind(a90[1][[1]][50],yy49)
  
  y0.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.0)))
  y0.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.0)))
  y0.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.0)))
  y0.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.0)))
  
  y1.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.1)))
  y1.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.1)))
  y1.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.1)))
  y1.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.1)))
  
  y2.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.2)))
  y2.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.2)))
  y2.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.2)))
  y2.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.2)))
  
  y3.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.3)))
  y3.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.3)))
  y3.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.3)))
  y3.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.3)))
  
  y4.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.4)))
  y4.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.4)))
  y4.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.4)))
  y4.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.4)))
  
  y5.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.5)))
  y5.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.5)))
  y5.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.5)))
  y5.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.5)))
  
  y6.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.6)))
  y6.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.6)))
  y6.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.6)))
  y6.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.6)))
  
  y7.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.7)))
  y7.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.7)))
  y7.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.7)))
  y7.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.7)))
  
  y8.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.8)))
  y8.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.8)))
  y8.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.8)))
  y8.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.8)))
  
  y9.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.9)))
  y9.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.9)))
  y9.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.9)))
  y9.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.9)))
  
  y10.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.10)))
  y10.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.10)))
  y10.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.10)))
  y10.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.10)))
  
  y11.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.11)))
  y11.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.11)))
  y11.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.11)))
  y11.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.11)))
  
  y12.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.12)))
  y12.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.12)))
  y12.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.12)))
  y12.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.12)))
  
  y13.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.13)))
  y13.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.13)))
  y13.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.13)))
  y13.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.13)))
  
  y14.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.14)))
  y14.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.14)))
  y14.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.14)))
  y14.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.14)))
  
  y15.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.15)))
  y15.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.15)))
  y15.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.15)))
  y15.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.15)))
  
  y16.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.16)))
  y16.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.16)))
  y16.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.16)))
  y16.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.16)))
  
  y17.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.17)))
  y17.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.17)))
  y17.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.17)))
  y17.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.17)))
  
  y18.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.18)))
  y18.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.18)))
  y18.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.18)))
  y18.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.18)))
  
  y19.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.19)))
  y19.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.19)))
  y19.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.19)))
  y19.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.19)))
  y20.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.20)))
  y20.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.20)))
  y20.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.20)))
  y20.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.20)))
  
  y21.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.21)))
  y21.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.21)))
  y21.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.21)))
  y21.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.21)))
  
  y22.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.22)))
  y22.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.22)))
  y22.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.22)))
  y22.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.22)))
  
  y23.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.23)))
  y23.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.23)))
  y23.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.23)))
  y23.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.23)))
  
  y24.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.24)))
  y24.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.24)))
  y24.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.24)))
  y24.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.24)))
  
  y25.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.25)))
  y25.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.25)))
  y25.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.25)))
  y25.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.25)))
  
  y26.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.26)))
  y26.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.26)))
  y26.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.26)))
  y26.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.26)))
  
  y27.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.27)))
  y27.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.27)))
  y27.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.27)))
  y27.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.27)))
  
  y28.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.28)))
  y28.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.28)))
  y28.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.28)))
  y28.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.28)))
  
  y29.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.29)))
  y29.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.29)))
  y29.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.29)))
  y29.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.29)))
  y30.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.30)))
  y30.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.30)))
  y30.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.30)))
  y30.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.30)))
  
  y31.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.31)))
  y31.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.31)))
  y31.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.31)))
  y31.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.31)))
  
  y32.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.32)))
  y32.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.32)))
  y32.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.32)))
  y32.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.32)))
  
  y33.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.33)))
  y33.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.33)))
  y33.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.33)))
  y33.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.33)))
  
  y34.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.34)))
  y34.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.34)))
  y34.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.34)))
  y34.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.34)))
  
  y35.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.35)))
  y35.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.35)))
  y35.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.35)))
  y35.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.35)))
  
  y36.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.36)))
  y36.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.36)))
  y36.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.36)))
  y36.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.36)))
  
  y37.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.37)))
  y37.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.37)))
  y37.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.37)))
  y37.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.37)))
  
  y38.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.38)))
  y38.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.38)))
  y38.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.38)))
  y38.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.38)))
  
  y39.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.39)))
  y39.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.39)))
  y39.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.39)))
  y39.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.39)))
  y40.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.40)))
  y40.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.40)))
  y40.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.40)))
  y40.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.40)))
  
  y41.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.41)))
  y41.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.41)))
  y41.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.41)))
  y41.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.41)))
  
  y42.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.42)))
  y42.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.42)))
  y42.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.42)))
  y42.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.42)))
  
  y43.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.43)))
  y43.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.43)))
  y43.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.43)))
  y43.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.43)))
  
  y44.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.44)))
  y44.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.44)))
  y44.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.44)))
  y44.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.44)))
  
  y45.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.45)))
  y45.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.45)))
  y45.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.45)))
  y45.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.45)))
  
  y46.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.46)))
  y46.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.46)))
  y46.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.46)))
  y46.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.46)))
  
  y47.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.47)))
  y47.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.47)))
  y47.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.47)))
  y47.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.47)))
  
  y48.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.48)))
  y48.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.48)))
  y48.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.48)))
  y48.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.48)))
  
  y49.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.49)))
  y49.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.49)))
  y49.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.49)))
  y49.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.49)))
  
  wei<-cbind(y0.wei,y1.wei,y2.wei,y3.wei,y4.wei,y5.wei,y6.wei,y7.wei,y8.wei,y9.wei,y10.wei,y11.wei,y12.wei,y13.wei,y14.wei,y15.wei,y16.wei,y17.wei,y18.wei,y19.wei,y20.wei,y21.wei,y22.wei,y23.wei,y24.wei,y25.wei,y26.wei,y27.wei,y28.wei,y29.wei,y30.wei,y31.wei,y32.wei,y33.wei,y34.wei,y35.wei,y36.wei,y37.wei,y38.wei,y39.wei,y40.wei,y41.wei,y42.wei,y43.wei,y44.wei,y45.wei,y46.wei,y47.wei,y48.wei,y49.wei)
  gum<-cbind(y0.gum,y1.gum,y2.gum,y3.gum,y4.gum,y5.gum,y6.gum,y7.gum,y8.gum,y9.gum,y10.gum,y11.gum,y12.gum,y13.gum,y14.gum,y15.gum,y16.gum,y17.gum,y18.gum,y19.gum,y20.gum,y21.gum,y22.gum,y23.gum,y24.gum,y25.gum,y26.gum,y27.gum,y28.gum,y29.gum,y30.gum,y31.gum,y32.gum,y33.gum,y34.gum,y35.gum,y36.gum,y37.gum,y38.gum,y39.gum,y40.gum,y41.gum,y42.gum,y43.gum,y44.gum,y45.gum,y46.gum,y47.gum,y48.gum,y49.gum)
  pe3<-cbind(y0.pe3,y1.pe3,y2.pe3,y3.pe3,y4.pe3,y5.pe3,y6.pe3,y7.pe3,y8.pe3,y9.pe3,y10.pe3,y11.pe3,y12.pe3,y13.pe3,y14.pe3,y15.pe3,y16.pe3,y17.pe3,y18.pe3,y19.pe3,y20.pe3,y21.pe3,y22.pe3,y23.pe3,y24.pe3,y25.pe3,y26.pe3,y27.pe3,y28.pe3,y29.pe3,y30.pe3,y31.pe3,y32.pe3,y33.pe3,y34.pe3,y35.pe3,y36.pe3,y37.pe3,y38.pe3,y39.pe3,y40.pe3,y41.pe3,y42.pe3,y43.pe3,y44.pe3,y45.pe3,y46.pe3,y47.pe3,y48.pe3,y49.pe3)
  ln3<-cbind(y0.ln3,y1.ln3,y2.ln3,y3.ln3,y4.ln3,y5.ln3,y6.ln3,y7.ln3,y8.ln3,y9.ln3,y10.ln3,y11.ln3,y12.ln3,y13.ln3,y14.ln3,y15.ln3,y16.ln3,y17.ln3,y18.ln3,y19.ln3,y20.ln3,y21.ln3,y22.ln3,y23.ln3,y24.ln3,y25.ln3,y26.ln3,y27.ln3,y28.ln3,y29.ln3,y30.ln3,y31.ln3,y32.ln3,y33.ln3,y34.ln3,y35.ln3,y36.ln3,y37.ln3,y38.ln3,y39.ln3,y40.ln3,y41.ln3,y42.ln3,y43.ln3,y44.ln3,y45.ln3,y46.ln3,y47.ln3,y48.ln3,y49.ln3)
  
  return(list(wei,gum,pe3,ln3))
  
}
#### für ein 0.90/99/999 hochwasser am Anfang, dann ein 0.999 Hochwasser berechnet
inc.hqs.90.999<-function(dfs,year,quantile){
  dff<-dfs[[1]]
  df<-dfs[[1]]
  a90<-par.80y(dfm=dff,year = year,quantile=quantile)
  quantile=0.999
  ys0<-seq(1,year*365+11,1)
  ys1<-seq(1,(year*365+10+1*365),1)
  ys2<-seq(1,(year*365+10+2*365),1)
  ys3<-seq(1,(year*365+10+3*365),1)
  ys4<-seq(1,(year*365+10+4*365),1)
  ys5<-seq(1,(year*365+10+5*365),1)
  ys6<-seq(1,(year*365+10+6*365),1)
  ys7<-seq(1,(year*365+10+7*365),1)
  ys8<-seq(1,(year*365+10+8*365),1)
  ys9<-seq(1,(year*365+10+9*365),1)
  ys10<-seq(1,(year*365+10+10*365),1)
  ys11<-seq(1,(year*365+10+11*365),1)
  ys12<-seq(1,(year*365+10+12*365),1)
  ys13<-seq(1,(year*365+10+13*365),1)
  ys14<-seq(1,(year*365+10+14*365),1)
  ys15<-seq(1,(year*365+10+15*365),1)
  ys16<-seq(1,(year*365+10+16*365),1)
  ys17<-seq(1,(year*365+10+17*365),1)
  ys18<-seq(1,(year*365+10+18*365),1)
  ys19<-seq(1,(year*365+10+19*365),1)
  ys20<-seq(1,(year*365+10+20*365),1)
  ys21<-seq(1,(year*365+10+21*365),1)
  ys22<-seq(1,(year*365+10+22*365),1)
  ys23<-seq(1,(year*365+10+23*365),1)
  ys24<-seq(1,(year*365+10+24*365),1)
  ys25<-seq(1,(year*365+10+25*365),1)
  ys26<-seq(1,(year*365+10+26*365),1)
  ys27<-seq(1,(year*365+10+27*365),1)
  ys28<-seq(1,(year*365+10+28*365),1)
  ys29<-seq(1,(year*365+10+29*365),1)
  ys30<-seq(1,(year*365+10+30*365),1)
  ys31<-seq(1,(year*365+10+31*365),1)
  ys32<-seq(1,(year*365+10+32*365),1)
  ys33<-seq(1,(year*365+10+33*365),1)
  ys34<-seq(1,(year*365+10+34*365),1)
  ys35<-seq(1,(year*365+10+35*365),1)
  ys36<-seq(1,(year*365+10+36*365),1)
  ys37<-seq(1,(year*365+10+37*365),1)
  ys38<-seq(1,(year*365+10+38*365),1)
  ys39<-seq(1,(year*365+10+39*365),1)
  ys40<-seq(1,(year*365+10+40*365),1)
  ys41<-seq(1,(year*365+10+41*365),1)
  ys42<-seq(1,(year*365+10+42*365),1)
  ys43<-seq(1,(year*365+10+43*365),1)
  ys44<-seq(1,(year*365+10+44*365),1)
  ys45<-seq(1,(year*365+10+45*365),1)
  ys46<-seq(1,(year*365+10+46*365),1)
  ys47<-seq(1,(year*365+10+47*365),1)
  ys48<-seq(1,(year*365+10+48*365),1)
  ys49<-seq(1,(year*365+10+49*365),1)
  
  yy0<-as.matrix(df[[4]][ys0])
  yy1<-as.matrix(df[[4]][ys1])
  yy2<-as.matrix(df[[4]][ys2])
  yy3<-as.matrix(df[[4]][ys3])
  yy4<-as.matrix(df[[4]][ys4])
  yy5<-as.matrix(df[[4]][ys5])
  yy6<-as.matrix(df[[4]][ys6])
  yy7<-as.matrix(df[[4]][ys7])
  yy8<-as.matrix(df[[4]][ys8])
  yy9<-as.matrix(df[[4]][ys9])
  yy10<-as.matrix(df[[4]][ys10])
  yy11<-as.matrix(df[[4]][ys11])
  yy12<-as.matrix(df[[4]][ys12])
  yy13<-as.matrix(df[[4]][ys13])
  yy14<-as.matrix(df[[4]][ys14])
  yy15<-as.matrix(df[[4]][ys15])
  yy16<-as.matrix(df[[4]][ys16])
  yy17<-as.matrix(df[[4]][ys17])
  yy18<-as.matrix(df[[4]][ys18])
  yy19<-as.matrix(df[[4]][ys19])
  yy20<-as.matrix(df[[4]][ys20])
  yy21<-as.matrix(df[[4]][ys21])
  yy22<-as.matrix(df[[4]][ys22])
  yy23<-as.matrix(df[[4]][ys23])
  yy24<-as.matrix(df[[4]][ys24])
  yy25<-as.matrix(df[[4]][ys25])
  yy26<-as.matrix(df[[4]][ys26])
  yy27<-as.matrix(df[[4]][ys27])
  yy28<-as.matrix(df[[4]][ys28])
  yy29<-as.matrix(df[[4]][ys29])
  yy30<-as.matrix(df[[4]][ys30])
  yy31<-as.matrix(df[[4]][ys31])
  yy32<-as.matrix(df[[4]][ys32])
  yy33<-as.matrix(df[[4]][ys33])
  yy34<-as.matrix(df[[4]][ys34])
  yy35<-as.matrix(df[[4]][ys35])
  yy36<-as.matrix(df[[4]][ys36])
  yy37<-as.matrix(df[[4]][ys37])
  yy38<-as.matrix(df[[4]][ys38])
  yy39<-as.matrix(df[[4]][ys39])
  yy40<-as.matrix(df[[4]][ys40])
  yy41<-as.matrix(df[[4]][ys41])
  yy42<-as.matrix(df[[4]][ys42])
  yy43<-as.matrix(df[[4]][ys43])
  yy44<-as.matrix(df[[4]][ys44])
  yy45<-as.matrix(df[[4]][ys45])
  yy46<-as.matrix(df[[4]][ys46])
  yy47<-as.matrix(df[[4]][ys47])
  yy48<-as.matrix(df[[4]][ys48])
  yy49<-as.matrix(df[[4]][ys49])
  
  df90.0<-rbind(a90[1][[1]][1],yy0)
  df90.1<-rbind(a90[1][[1]][2],yy1)
  df90.2<-rbind(a90[1][[1]][3],yy2)
  df90.3<-rbind(a90[1][[1]][4],yy3)
  df90.4<-rbind(a90[1][[1]][5],yy4)
  df90.5<-rbind(a90[1][[1]][6],yy5)
  df90.6<-rbind(a90[1][[1]][7],yy6)
  df90.7<-rbind(a90[1][[1]][8],yy7)
  df90.8<-rbind(a90[1][[1]][9],yy8)
  df90.9<-rbind(a90[1][[1]][10],yy9)
  df90.10<-rbind(a90[1][[1]][11],yy10)
  df90.11<-rbind(a90[1][[1]][12],yy11)
  df90.12<-rbind(a90[1][[1]][13],yy12)
  df90.13<-rbind(a90[1][[1]][14],yy13)
  df90.14<-rbind(a90[1][[1]][15],yy14)
  df90.15<-rbind(a90[1][[1]][16],yy15)
  df90.16<-rbind(a90[1][[1]][17],yy16)
  df90.17<-rbind(a90[1][[1]][18],yy17)
  df90.18<-rbind(a90[1][[1]][19],yy18)
  df90.19<-rbind(a90[1][[1]][20],yy19)
  df90.20<-rbind(a90[1][[1]][21],yy20)
  df90.21<-rbind(a90[1][[1]][22],yy21)
  df90.22<-rbind(a90[1][[1]][23],yy22)
  df90.23<-rbind(a90[1][[1]][24],yy23)
  df90.24<-rbind(a90[1][[1]][25],yy24)
  df90.25<-rbind(a90[1][[1]][26],yy25)
  df90.26<-rbind(a90[1][[1]][27],yy26)
  df90.27<-rbind(a90[1][[1]][28],yy27)
  df90.28<-rbind(a90[1][[1]][29],yy28)
  df90.29<-rbind(a90[1][[1]][30],yy29)
  df90.30<-rbind(a90[1][[1]][31],yy30)
  df90.31<-rbind(a90[1][[1]][32],yy31)
  df90.32<-rbind(a90[1][[1]][33],yy32)
  df90.33<-rbind(a90[1][[1]][34],yy33)
  df90.34<-rbind(a90[1][[1]][35],yy34)
  df90.35<-rbind(a90[1][[1]][36],yy35)
  df90.36<-rbind(a90[1][[1]][37],yy36)
  df90.37<-rbind(a90[1][[1]][38],yy37)
  df90.38<-rbind(a90[1][[1]][39],yy38)
  df90.39<-rbind(a90[1][[1]][40],yy39)
  df90.40<-rbind(a90[1][[1]][41],yy40)
  df90.41<-rbind(a90[1][[1]][42],yy41)
  df90.42<-rbind(a90[1][[1]][43],yy42)
  df90.43<-rbind(a90[1][[1]][44],yy43)
  df90.44<-rbind(a90[1][[1]][45],yy44)
  df90.45<-rbind(a90[1][[1]][46],yy45)
  df90.46<-rbind(a90[1][[1]][47],yy46)
  df90.47<-rbind(a90[1][[1]][48],yy47)
  df90.48<-rbind(a90[1][[1]][49],yy48)
  df90.49<-rbind(a90[1][[1]][50],yy49)
  
  y0.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.0)))
  y0.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.0)))
  y0.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.0)))
  y0.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.0)))
  
  y1.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.1)))
  y1.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.1)))
  y1.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.1)))
  y1.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.1)))
  
  y2.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.2)))
  y2.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.2)))
  y2.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.2)))
  y2.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.2)))
  
  y3.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.3)))
  y3.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.3)))
  y3.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.3)))
  y3.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.3)))
  
  y4.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.4)))
  y4.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.4)))
  y4.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.4)))
  y4.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.4)))
  
  y5.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.5)))
  y5.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.5)))
  y5.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.5)))
  y5.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.5)))
  
  y6.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.6)))
  y6.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.6)))
  y6.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.6)))
  y6.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.6)))
  
  y7.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.7)))
  y7.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.7)))
  y7.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.7)))
  y7.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.7)))
  
  y8.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.8)))
  y8.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.8)))
  y8.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.8)))
  y8.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.8)))
  
  y9.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.9)))
  y9.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.9)))
  y9.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.9)))
  y9.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.9)))
  
  y10.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.10)))
  y10.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.10)))
  y10.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.10)))
  y10.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.10)))
  
  y11.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.11)))
  y11.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.11)))
  y11.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.11)))
  y11.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.11)))
  
  y12.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.12)))
  y12.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.12)))
  y12.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.12)))
  y12.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.12)))
  
  y13.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.13)))
  y13.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.13)))
  y13.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.13)))
  y13.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.13)))
  
  y14.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.14)))
  y14.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.14)))
  y14.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.14)))
  y14.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.14)))
  
  y15.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.15)))
  y15.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.15)))
  y15.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.15)))
  y15.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.15)))
  
  y16.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.16)))
  y16.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.16)))
  y16.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.16)))
  y16.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.16)))
  
  y17.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.17)))
  y17.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.17)))
  y17.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.17)))
  y17.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.17)))
  
  y18.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.18)))
  y18.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.18)))
  y18.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.18)))
  y18.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.18)))
  
  y19.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.19)))
  y19.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.19)))
  y19.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.19)))
  y19.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.19)))
  y20.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.20)))
  y20.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.20)))
  y20.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.20)))
  y20.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.20)))
  
  y21.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.21)))
  y21.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.21)))
  y21.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.21)))
  y21.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.21)))
  
  y22.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.22)))
  y22.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.22)))
  y22.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.22)))
  y22.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.22)))
  
  y23.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.23)))
  y23.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.23)))
  y23.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.23)))
  y23.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.23)))
  
  y24.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.24)))
  y24.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.24)))
  y24.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.24)))
  y24.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.24)))
  
  y25.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.25)))
  y25.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.25)))
  y25.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.25)))
  y25.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.25)))
  
  y26.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.26)))
  y26.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.26)))
  y26.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.26)))
  y26.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.26)))
  
  y27.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.27)))
  y27.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.27)))
  y27.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.27)))
  y27.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.27)))
  
  y28.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.28)))
  y28.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.28)))
  y28.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.28)))
  y28.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.28)))
  
  y29.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.29)))
  y29.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.29)))
  y29.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.29)))
  y29.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.29)))
  y30.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.30)))
  y30.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.30)))
  y30.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.30)))
  y30.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.30)))
  
  y31.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.31)))
  y31.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.31)))
  y31.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.31)))
  y31.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.31)))
  
  y32.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.32)))
  y32.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.32)))
  y32.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.32)))
  y32.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.32)))
  
  y33.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.33)))
  y33.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.33)))
  y33.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.33)))
  y33.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.33)))
  
  y34.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.34)))
  y34.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.34)))
  y34.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.34)))
  y34.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.34)))
  
  y35.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.35)))
  y35.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.35)))
  y35.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.35)))
  y35.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.35)))
  
  y36.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.36)))
  y36.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.36)))
  y36.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.36)))
  y36.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.36)))
  
  y37.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.37)))
  y37.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.37)))
  y37.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.37)))
  y37.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.37)))
  
  y38.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.38)))
  y38.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.38)))
  y38.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.38)))
  y38.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.38)))
  
  y39.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.39)))
  y39.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.39)))
  y39.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.39)))
  y39.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.39)))
  y40.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.40)))
  y40.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.40)))
  y40.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.40)))
  y40.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.40)))
  
  y41.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.41)))
  y41.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.41)))
  y41.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.41)))
  y41.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.41)))
  
  y42.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.42)))
  y42.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.42)))
  y42.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.42)))
  y42.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.42)))
  
  y43.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.43)))
  y43.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.43)))
  y43.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.43)))
  y43.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.43)))
  
  y44.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.44)))
  y44.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.44)))
  y44.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.44)))
  y44.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.44)))
  
  y45.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.45)))
  y45.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.45)))
  y45.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.45)))
  y45.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.45)))
  
  y46.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.46)))
  y46.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.46)))
  y46.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.46)))
  y46.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.46)))
  
  y47.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.47)))
  y47.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.47)))
  y47.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.47)))
  y47.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.47)))
  
  y48.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.48)))
  y48.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.48)))
  y48.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.48)))
  y48.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.48)))
  
  y49.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.49)))
  y49.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.49)))
  y49.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.49)))
  y49.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.49)))
  
  wei<-cbind(y0.wei,y1.wei,y2.wei,y3.wei,y4.wei,y5.wei,y6.wei,y7.wei,y8.wei,y9.wei,y10.wei,y11.wei,y12.wei,y13.wei,y14.wei,y15.wei,y16.wei,y17.wei,y18.wei,y19.wei,y20.wei,y21.wei,y22.wei,y23.wei,y24.wei,y25.wei,y26.wei,y27.wei,y28.wei,y29.wei,y30.wei,y31.wei,y32.wei,y33.wei,y34.wei,y35.wei,y36.wei,y37.wei,y38.wei,y39.wei,y40.wei,y41.wei,y42.wei,y43.wei,y44.wei,y45.wei,y46.wei,y47.wei,y48.wei,y49.wei)
  gum<-cbind(y0.gum,y1.gum,y2.gum,y3.gum,y4.gum,y5.gum,y6.gum,y7.gum,y8.gum,y9.gum,y10.gum,y11.gum,y12.gum,y13.gum,y14.gum,y15.gum,y16.gum,y17.gum,y18.gum,y19.gum,y20.gum,y21.gum,y22.gum,y23.gum,y24.gum,y25.gum,y26.gum,y27.gum,y28.gum,y29.gum,y30.gum,y31.gum,y32.gum,y33.gum,y34.gum,y35.gum,y36.gum,y37.gum,y38.gum,y39.gum,y40.gum,y41.gum,y42.gum,y43.gum,y44.gum,y45.gum,y46.gum,y47.gum,y48.gum,y49.gum)
  pe3<-cbind(y0.pe3,y1.pe3,y2.pe3,y3.pe3,y4.pe3,y5.pe3,y6.pe3,y7.pe3,y8.pe3,y9.pe3,y10.pe3,y11.pe3,y12.pe3,y13.pe3,y14.pe3,y15.pe3,y16.pe3,y17.pe3,y18.pe3,y19.pe3,y20.pe3,y21.pe3,y22.pe3,y23.pe3,y24.pe3,y25.pe3,y26.pe3,y27.pe3,y28.pe3,y29.pe3,y30.pe3,y31.pe3,y32.pe3,y33.pe3,y34.pe3,y35.pe3,y36.pe3,y37.pe3,y38.pe3,y39.pe3,y40.pe3,y41.pe3,y42.pe3,y43.pe3,y44.pe3,y45.pe3,y46.pe3,y47.pe3,y48.pe3,y49.pe3)
  ln3<-cbind(y0.ln3,y1.ln3,y2.ln3,y3.ln3,y4.ln3,y5.ln3,y6.ln3,y7.ln3,y8.ln3,y9.ln3,y10.ln3,y11.ln3,y12.ln3,y13.ln3,y14.ln3,y15.ln3,y16.ln3,y17.ln3,y18.ln3,y19.ln3,y20.ln3,y21.ln3,y22.ln3,y23.ln3,y24.ln3,y25.ln3,y26.ln3,y27.ln3,y28.ln3,y29.ln3,y30.ln3,y31.ln3,y32.ln3,y33.ln3,y34.ln3,y35.ln3,y36.ln3,y37.ln3,y38.ln3,y39.ln3,y40.ln3,y41.ln3,y42.ln3,y43.ln3,y44.ln3,y45.ln3,y46.ln3,y47.ln3,y48.ln3,y49.ln3)
  
  return(list(wei,gum,pe3,ln3))
  
}
inc.hqs.99.999<-function(dfs,year,quantile){
  dff<-dfs[[1]]
  df<-dfs[[1]]
  a90<-par.80y(dfm=dff,year = year,quantile=quantile)
  quantile=0.999
  ys0<-seq(1,year*365+11,1)
  ys1<-seq(1,(year*365+10+1*365),1)
  ys2<-seq(1,(year*365+10+2*365),1)
  ys3<-seq(1,(year*365+10+3*365),1)
  ys4<-seq(1,(year*365+10+4*365),1)
  ys5<-seq(1,(year*365+10+5*365),1)
  ys6<-seq(1,(year*365+10+6*365),1)
  ys7<-seq(1,(year*365+10+7*365),1)
  ys8<-seq(1,(year*365+10+8*365),1)
  ys9<-seq(1,(year*365+10+9*365),1)
  ys10<-seq(1,(year*365+10+10*365),1)
  ys11<-seq(1,(year*365+10+11*365),1)
  ys12<-seq(1,(year*365+10+12*365),1)
  ys13<-seq(1,(year*365+10+13*365),1)
  ys14<-seq(1,(year*365+10+14*365),1)
  ys15<-seq(1,(year*365+10+15*365),1)
  ys16<-seq(1,(year*365+10+16*365),1)
  ys17<-seq(1,(year*365+10+17*365),1)
  ys18<-seq(1,(year*365+10+18*365),1)
  ys19<-seq(1,(year*365+10+19*365),1)
  ys20<-seq(1,(year*365+10+20*365),1)
  ys21<-seq(1,(year*365+10+21*365),1)
  ys22<-seq(1,(year*365+10+22*365),1)
  ys23<-seq(1,(year*365+10+23*365),1)
  ys24<-seq(1,(year*365+10+24*365),1)
  ys25<-seq(1,(year*365+10+25*365),1)
  ys26<-seq(1,(year*365+10+26*365),1)
  ys27<-seq(1,(year*365+10+27*365),1)
  ys28<-seq(1,(year*365+10+28*365),1)
  ys29<-seq(1,(year*365+10+29*365),1)
  ys30<-seq(1,(year*365+10+30*365),1)
  ys31<-seq(1,(year*365+10+31*365),1)
  ys32<-seq(1,(year*365+10+32*365),1)
  ys33<-seq(1,(year*365+10+33*365),1)
  ys34<-seq(1,(year*365+10+34*365),1)
  ys35<-seq(1,(year*365+10+35*365),1)
  ys36<-seq(1,(year*365+10+36*365),1)
  ys37<-seq(1,(year*365+10+37*365),1)
  ys38<-seq(1,(year*365+10+38*365),1)
  ys39<-seq(1,(year*365+10+39*365),1)
  ys40<-seq(1,(year*365+10+40*365),1)
  ys41<-seq(1,(year*365+10+41*365),1)
  ys42<-seq(1,(year*365+10+42*365),1)
  ys43<-seq(1,(year*365+10+43*365),1)
  ys44<-seq(1,(year*365+10+44*365),1)
  ys45<-seq(1,(year*365+10+45*365),1)
  ys46<-seq(1,(year*365+10+46*365),1)
  ys47<-seq(1,(year*365+10+47*365),1)
  ys48<-seq(1,(year*365+10+48*365),1)
  ys49<-seq(1,(year*365+10+49*365),1)
  
  yy0<-as.matrix(df[[4]][ys0])
  yy1<-as.matrix(df[[4]][ys1])
  yy2<-as.matrix(df[[4]][ys2])
  yy3<-as.matrix(df[[4]][ys3])
  yy4<-as.matrix(df[[4]][ys4])
  yy5<-as.matrix(df[[4]][ys5])
  yy6<-as.matrix(df[[4]][ys6])
  yy7<-as.matrix(df[[4]][ys7])
  yy8<-as.matrix(df[[4]][ys8])
  yy9<-as.matrix(df[[4]][ys9])
  yy10<-as.matrix(df[[4]][ys10])
  yy11<-as.matrix(df[[4]][ys11])
  yy12<-as.matrix(df[[4]][ys12])
  yy13<-as.matrix(df[[4]][ys13])
  yy14<-as.matrix(df[[4]][ys14])
  yy15<-as.matrix(df[[4]][ys15])
  yy16<-as.matrix(df[[4]][ys16])
  yy17<-as.matrix(df[[4]][ys17])
  yy18<-as.matrix(df[[4]][ys18])
  yy19<-as.matrix(df[[4]][ys19])
  yy20<-as.matrix(df[[4]][ys20])
  yy21<-as.matrix(df[[4]][ys21])
  yy22<-as.matrix(df[[4]][ys22])
  yy23<-as.matrix(df[[4]][ys23])
  yy24<-as.matrix(df[[4]][ys24])
  yy25<-as.matrix(df[[4]][ys25])
  yy26<-as.matrix(df[[4]][ys26])
  yy27<-as.matrix(df[[4]][ys27])
  yy28<-as.matrix(df[[4]][ys28])
  yy29<-as.matrix(df[[4]][ys29])
  yy30<-as.matrix(df[[4]][ys30])
  yy31<-as.matrix(df[[4]][ys31])
  yy32<-as.matrix(df[[4]][ys32])
  yy33<-as.matrix(df[[4]][ys33])
  yy34<-as.matrix(df[[4]][ys34])
  yy35<-as.matrix(df[[4]][ys35])
  yy36<-as.matrix(df[[4]][ys36])
  yy37<-as.matrix(df[[4]][ys37])
  yy38<-as.matrix(df[[4]][ys38])
  yy39<-as.matrix(df[[4]][ys39])
  yy40<-as.matrix(df[[4]][ys40])
  yy41<-as.matrix(df[[4]][ys41])
  yy42<-as.matrix(df[[4]][ys42])
  yy43<-as.matrix(df[[4]][ys43])
  yy44<-as.matrix(df[[4]][ys44])
  yy45<-as.matrix(df[[4]][ys45])
  yy46<-as.matrix(df[[4]][ys46])
  yy47<-as.matrix(df[[4]][ys47])
  yy48<-as.matrix(df[[4]][ys48])
  yy49<-as.matrix(df[[4]][ys49])
  
  df90.0<-rbind(a90[1][[1]][1],yy0)
  df90.1<-rbind(a90[1][[1]][2],yy1)
  df90.2<-rbind(a90[1][[1]][3],yy2)
  df90.3<-rbind(a90[1][[1]][4],yy3)
  df90.4<-rbind(a90[1][[1]][5],yy4)
  df90.5<-rbind(a90[1][[1]][6],yy5)
  df90.6<-rbind(a90[1][[1]][7],yy6)
  df90.7<-rbind(a90[1][[1]][8],yy7)
  df90.8<-rbind(a90[1][[1]][9],yy8)
  df90.9<-rbind(a90[1][[1]][10],yy9)
  df90.10<-rbind(a90[1][[1]][11],yy10)
  df90.11<-rbind(a90[1][[1]][12],yy11)
  df90.12<-rbind(a90[1][[1]][13],yy12)
  df90.13<-rbind(a90[1][[1]][14],yy13)
  df90.14<-rbind(a90[1][[1]][15],yy14)
  df90.15<-rbind(a90[1][[1]][16],yy15)
  df90.16<-rbind(a90[1][[1]][17],yy16)
  df90.17<-rbind(a90[1][[1]][18],yy17)
  df90.18<-rbind(a90[1][[1]][19],yy18)
  df90.19<-rbind(a90[1][[1]][20],yy19)
  df90.20<-rbind(a90[1][[1]][21],yy20)
  df90.21<-rbind(a90[1][[1]][22],yy21)
  df90.22<-rbind(a90[1][[1]][23],yy22)
  df90.23<-rbind(a90[1][[1]][24],yy23)
  df90.24<-rbind(a90[1][[1]][25],yy24)
  df90.25<-rbind(a90[1][[1]][26],yy25)
  df90.26<-rbind(a90[1][[1]][27],yy26)
  df90.27<-rbind(a90[1][[1]][28],yy27)
  df90.28<-rbind(a90[1][[1]][29],yy28)
  df90.29<-rbind(a90[1][[1]][30],yy29)
  df90.30<-rbind(a90[1][[1]][31],yy30)
  df90.31<-rbind(a90[1][[1]][32],yy31)
  df90.32<-rbind(a90[1][[1]][33],yy32)
  df90.33<-rbind(a90[1][[1]][34],yy33)
  df90.34<-rbind(a90[1][[1]][35],yy34)
  df90.35<-rbind(a90[1][[1]][36],yy35)
  df90.36<-rbind(a90[1][[1]][37],yy36)
  df90.37<-rbind(a90[1][[1]][38],yy37)
  df90.38<-rbind(a90[1][[1]][39],yy38)
  df90.39<-rbind(a90[1][[1]][40],yy39)
  df90.40<-rbind(a90[1][[1]][41],yy40)
  df90.41<-rbind(a90[1][[1]][42],yy41)
  df90.42<-rbind(a90[1][[1]][43],yy42)
  df90.43<-rbind(a90[1][[1]][44],yy43)
  df90.44<-rbind(a90[1][[1]][45],yy44)
  df90.45<-rbind(a90[1][[1]][46],yy45)
  df90.46<-rbind(a90[1][[1]][47],yy46)
  df90.47<-rbind(a90[1][[1]][48],yy47)
  df90.48<-rbind(a90[1][[1]][49],yy48)
  df90.49<-rbind(a90[1][[1]][50],yy49)
  
  y0.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.0)))
  y0.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.0)))
  y0.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.0)))
  y0.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.0)))
  
  y1.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.1)))
  y1.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.1)))
  y1.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.1)))
  y1.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.1)))
  
  y2.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.2)))
  y2.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.2)))
  y2.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.2)))
  y2.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.2)))
  
  y3.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.3)))
  y3.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.3)))
  y3.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.3)))
  y3.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.3)))
  
  y4.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.4)))
  y4.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.4)))
  y4.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.4)))
  y4.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.4)))
  
  y5.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.5)))
  y5.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.5)))
  y5.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.5)))
  y5.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.5)))
  
  y6.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.6)))
  y6.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.6)))
  y6.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.6)))
  y6.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.6)))
  
  y7.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.7)))
  y7.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.7)))
  y7.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.7)))
  y7.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.7)))
  
  y8.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.8)))
  y8.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.8)))
  y8.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.8)))
  y8.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.8)))
  
  y9.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.9)))
  y9.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.9)))
  y9.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.9)))
  y9.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.9)))
  
  y10.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.10)))
  y10.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.10)))
  y10.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.10)))
  y10.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.10)))
  
  y11.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.11)))
  y11.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.11)))
  y11.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.11)))
  y11.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.11)))
  
  y12.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.12)))
  y12.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.12)))
  y12.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.12)))
  y12.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.12)))
  
  y13.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.13)))
  y13.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.13)))
  y13.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.13)))
  y13.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.13)))
  
  y14.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.14)))
  y14.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.14)))
  y14.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.14)))
  y14.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.14)))
  
  y15.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.15)))
  y15.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.15)))
  y15.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.15)))
  y15.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.15)))
  
  y16.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.16)))
  y16.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.16)))
  y16.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.16)))
  y16.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.16)))
  
  y17.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.17)))
  y17.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.17)))
  y17.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.17)))
  y17.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.17)))
  
  y18.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.18)))
  y18.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.18)))
  y18.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.18)))
  y18.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.18)))
  
  y19.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.19)))
  y19.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.19)))
  y19.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.19)))
  y19.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.19)))
  y20.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.20)))
  y20.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.20)))
  y20.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.20)))
  y20.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.20)))
  
  y21.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.21)))
  y21.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.21)))
  y21.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.21)))
  y21.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.21)))
  
  y22.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.22)))
  y22.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.22)))
  y22.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.22)))
  y22.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.22)))
  
  y23.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.23)))
  y23.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.23)))
  y23.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.23)))
  y23.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.23)))
  
  y24.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.24)))
  y24.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.24)))
  y24.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.24)))
  y24.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.24)))
  
  y25.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.25)))
  y25.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.25)))
  y25.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.25)))
  y25.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.25)))
  
  y26.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.26)))
  y26.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.26)))
  y26.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.26)))
  y26.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.26)))
  
  y27.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.27)))
  y27.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.27)))
  y27.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.27)))
  y27.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.27)))
  
  y28.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.28)))
  y28.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.28)))
  y28.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.28)))
  y28.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.28)))
  
  y29.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.29)))
  y29.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.29)))
  y29.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.29)))
  y29.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.29)))
  y30.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.30)))
  y30.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.30)))
  y30.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.30)))
  y30.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.30)))
  
  y31.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.31)))
  y31.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.31)))
  y31.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.31)))
  y31.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.31)))
  
  y32.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.32)))
  y32.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.32)))
  y32.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.32)))
  y32.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.32)))
  
  y33.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.33)))
  y33.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.33)))
  y33.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.33)))
  y33.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.33)))
  
  y34.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.34)))
  y34.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.34)))
  y34.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.34)))
  y34.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.34)))
  
  y35.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.35)))
  y35.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.35)))
  y35.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.35)))
  y35.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.35)))
  
  y36.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.36)))
  y36.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.36)))
  y36.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.36)))
  y36.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.36)))
  
  y37.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.37)))
  y37.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.37)))
  y37.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.37)))
  y37.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.37)))
  
  y38.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.38)))
  y38.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.38)))
  y38.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.38)))
  y38.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.38)))
  
  y39.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.39)))
  y39.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.39)))
  y39.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.39)))
  y39.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.39)))
  y40.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.40)))
  y40.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.40)))
  y40.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.40)))
  y40.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.40)))
  
  y41.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.41)))
  y41.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.41)))
  y41.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.41)))
  y41.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.41)))
  
  y42.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.42)))
  y42.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.42)))
  y42.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.42)))
  y42.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.42)))
  
  y43.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.43)))
  y43.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.43)))
  y43.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.43)))
  y43.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.43)))
  
  y44.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.44)))
  y44.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.44)))
  y44.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.44)))
  y44.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.44)))
  
  y45.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.45)))
  y45.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.45)))
  y45.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.45)))
  y45.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.45)))
  
  y46.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.46)))
  y46.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.46)))
  y46.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.46)))
  y46.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.46)))
  
  y47.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.47)))
  y47.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.47)))
  y47.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.47)))
  y47.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.47)))
  
  y48.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.48)))
  y48.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.48)))
  y48.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.48)))
  y48.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.48)))
  
  y49.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.49)))
  y49.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.49)))
  y49.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.49)))
  y49.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.49)))
  
  wei<-cbind(y0.wei,y1.wei,y2.wei,y3.wei,y4.wei,y5.wei,y6.wei,y7.wei,y8.wei,y9.wei,y10.wei,y11.wei,y12.wei,y13.wei,y14.wei,y15.wei,y16.wei,y17.wei,y18.wei,y19.wei,y20.wei,y21.wei,y22.wei,y23.wei,y24.wei,y25.wei,y26.wei,y27.wei,y28.wei,y29.wei,y30.wei,y31.wei,y32.wei,y33.wei,y34.wei,y35.wei,y36.wei,y37.wei,y38.wei,y39.wei,y40.wei,y41.wei,y42.wei,y43.wei,y44.wei,y45.wei,y46.wei,y47.wei,y48.wei,y49.wei)
  gum<-cbind(y0.gum,y1.gum,y2.gum,y3.gum,y4.gum,y5.gum,y6.gum,y7.gum,y8.gum,y9.gum,y10.gum,y11.gum,y12.gum,y13.gum,y14.gum,y15.gum,y16.gum,y17.gum,y18.gum,y19.gum,y20.gum,y21.gum,y22.gum,y23.gum,y24.gum,y25.gum,y26.gum,y27.gum,y28.gum,y29.gum,y30.gum,y31.gum,y32.gum,y33.gum,y34.gum,y35.gum,y36.gum,y37.gum,y38.gum,y39.gum,y40.gum,y41.gum,y42.gum,y43.gum,y44.gum,y45.gum,y46.gum,y47.gum,y48.gum,y49.gum)
  pe3<-cbind(y0.pe3,y1.pe3,y2.pe3,y3.pe3,y4.pe3,y5.pe3,y6.pe3,y7.pe3,y8.pe3,y9.pe3,y10.pe3,y11.pe3,y12.pe3,y13.pe3,y14.pe3,y15.pe3,y16.pe3,y17.pe3,y18.pe3,y19.pe3,y20.pe3,y21.pe3,y22.pe3,y23.pe3,y24.pe3,y25.pe3,y26.pe3,y27.pe3,y28.pe3,y29.pe3,y30.pe3,y31.pe3,y32.pe3,y33.pe3,y34.pe3,y35.pe3,y36.pe3,y37.pe3,y38.pe3,y39.pe3,y40.pe3,y41.pe3,y42.pe3,y43.pe3,y44.pe3,y45.pe3,y46.pe3,y47.pe3,y48.pe3,y49.pe3)
  ln3<-cbind(y0.ln3,y1.ln3,y2.ln3,y3.ln3,y4.ln3,y5.ln3,y6.ln3,y7.ln3,y8.ln3,y9.ln3,y10.ln3,y11.ln3,y12.ln3,y13.ln3,y14.ln3,y15.ln3,y16.ln3,y17.ln3,y18.ln3,y19.ln3,y20.ln3,y21.ln3,y22.ln3,y23.ln3,y24.ln3,y25.ln3,y26.ln3,y27.ln3,y28.ln3,y29.ln3,y30.ln3,y31.ln3,y32.ln3,y33.ln3,y34.ln3,y35.ln3,y36.ln3,y37.ln3,y38.ln3,y39.ln3,y40.ln3,y41.ln3,y42.ln3,y43.ln3,y44.ln3,y45.ln3,y46.ln3,y47.ln3,y48.ln3,y49.ln3)
  
  return(list(wei,gum,pe3,ln3))
  
}
inc.hqs.999.999<-function(dfs,year,quantile){
  dff<-dfs[[1]]
  df<-dfs[[1]]
  a90<-par.80y(dfm=dff,year = year,quantile=quantile)
  quantile=0.999
  ys0<-seq(1,year*365+11,1)
  ys1<-seq(1,(year*365+10+1*365),1)
  ys2<-seq(1,(year*365+10+2*365),1)
  ys3<-seq(1,(year*365+10+3*365),1)
  ys4<-seq(1,(year*365+10+4*365),1)
  ys5<-seq(1,(year*365+10+5*365),1)
  ys6<-seq(1,(year*365+10+6*365),1)
  ys7<-seq(1,(year*365+10+7*365),1)
  ys8<-seq(1,(year*365+10+8*365),1)
  ys9<-seq(1,(year*365+10+9*365),1)
  ys10<-seq(1,(year*365+10+10*365),1)
  ys11<-seq(1,(year*365+10+11*365),1)
  ys12<-seq(1,(year*365+10+12*365),1)
  ys13<-seq(1,(year*365+10+13*365),1)
  ys14<-seq(1,(year*365+10+14*365),1)
  ys15<-seq(1,(year*365+10+15*365),1)
  ys16<-seq(1,(year*365+10+16*365),1)
  ys17<-seq(1,(year*365+10+17*365),1)
  ys18<-seq(1,(year*365+10+18*365),1)
  ys19<-seq(1,(year*365+10+19*365),1)
  ys20<-seq(1,(year*365+10+20*365),1)
  ys21<-seq(1,(year*365+10+21*365),1)
  ys22<-seq(1,(year*365+10+22*365),1)
  ys23<-seq(1,(year*365+10+23*365),1)
  ys24<-seq(1,(year*365+10+24*365),1)
  ys25<-seq(1,(year*365+10+25*365),1)
  ys26<-seq(1,(year*365+10+26*365),1)
  ys27<-seq(1,(year*365+10+27*365),1)
  ys28<-seq(1,(year*365+10+28*365),1)
  ys29<-seq(1,(year*365+10+29*365),1)
  ys30<-seq(1,(year*365+10+30*365),1)
  ys31<-seq(1,(year*365+10+31*365),1)
  ys32<-seq(1,(year*365+10+32*365),1)
  ys33<-seq(1,(year*365+10+33*365),1)
  ys34<-seq(1,(year*365+10+34*365),1)
  ys35<-seq(1,(year*365+10+35*365),1)
  ys36<-seq(1,(year*365+10+36*365),1)
  ys37<-seq(1,(year*365+10+37*365),1)
  ys38<-seq(1,(year*365+10+38*365),1)
  ys39<-seq(1,(year*365+10+39*365),1)
  ys40<-seq(1,(year*365+10+40*365),1)
  ys41<-seq(1,(year*365+10+41*365),1)
  ys42<-seq(1,(year*365+10+42*365),1)
  ys43<-seq(1,(year*365+10+43*365),1)
  ys44<-seq(1,(year*365+10+44*365),1)
  ys45<-seq(1,(year*365+10+45*365),1)
  ys46<-seq(1,(year*365+10+46*365),1)
  ys47<-seq(1,(year*365+10+47*365),1)
  ys48<-seq(1,(year*365+10+48*365),1)
  ys49<-seq(1,(year*365+10+49*365),1)
  
  yy0<-as.matrix(df[[4]][ys0])
  yy1<-as.matrix(df[[4]][ys1])
  yy2<-as.matrix(df[[4]][ys2])
  yy3<-as.matrix(df[[4]][ys3])
  yy4<-as.matrix(df[[4]][ys4])
  yy5<-as.matrix(df[[4]][ys5])
  yy6<-as.matrix(df[[4]][ys6])
  yy7<-as.matrix(df[[4]][ys7])
  yy8<-as.matrix(df[[4]][ys8])
  yy9<-as.matrix(df[[4]][ys9])
  yy10<-as.matrix(df[[4]][ys10])
  yy11<-as.matrix(df[[4]][ys11])
  yy12<-as.matrix(df[[4]][ys12])
  yy13<-as.matrix(df[[4]][ys13])
  yy14<-as.matrix(df[[4]][ys14])
  yy15<-as.matrix(df[[4]][ys15])
  yy16<-as.matrix(df[[4]][ys16])
  yy17<-as.matrix(df[[4]][ys17])
  yy18<-as.matrix(df[[4]][ys18])
  yy19<-as.matrix(df[[4]][ys19])
  yy20<-as.matrix(df[[4]][ys20])
  yy21<-as.matrix(df[[4]][ys21])
  yy22<-as.matrix(df[[4]][ys22])
  yy23<-as.matrix(df[[4]][ys23])
  yy24<-as.matrix(df[[4]][ys24])
  yy25<-as.matrix(df[[4]][ys25])
  yy26<-as.matrix(df[[4]][ys26])
  yy27<-as.matrix(df[[4]][ys27])
  yy28<-as.matrix(df[[4]][ys28])
  yy29<-as.matrix(df[[4]][ys29])
  yy30<-as.matrix(df[[4]][ys30])
  yy31<-as.matrix(df[[4]][ys31])
  yy32<-as.matrix(df[[4]][ys32])
  yy33<-as.matrix(df[[4]][ys33])
  yy34<-as.matrix(df[[4]][ys34])
  yy35<-as.matrix(df[[4]][ys35])
  yy36<-as.matrix(df[[4]][ys36])
  yy37<-as.matrix(df[[4]][ys37])
  yy38<-as.matrix(df[[4]][ys38])
  yy39<-as.matrix(df[[4]][ys39])
  yy40<-as.matrix(df[[4]][ys40])
  yy41<-as.matrix(df[[4]][ys41])
  yy42<-as.matrix(df[[4]][ys42])
  yy43<-as.matrix(df[[4]][ys43])
  yy44<-as.matrix(df[[4]][ys44])
  yy45<-as.matrix(df[[4]][ys45])
  yy46<-as.matrix(df[[4]][ys46])
  yy47<-as.matrix(df[[4]][ys47])
  yy48<-as.matrix(df[[4]][ys48])
  yy49<-as.matrix(df[[4]][ys49])
  
  df90.0<-rbind(a90[1][[1]][1],yy0)
  df90.1<-rbind(a90[1][[1]][2],yy1)
  df90.2<-rbind(a90[1][[1]][3],yy2)
  df90.3<-rbind(a90[1][[1]][4],yy3)
  df90.4<-rbind(a90[1][[1]][5],yy4)
  df90.5<-rbind(a90[1][[1]][6],yy5)
  df90.6<-rbind(a90[1][[1]][7],yy6)
  df90.7<-rbind(a90[1][[1]][8],yy7)
  df90.8<-rbind(a90[1][[1]][9],yy8)
  df90.9<-rbind(a90[1][[1]][10],yy9)
  df90.10<-rbind(a90[1][[1]][11],yy10)
  df90.11<-rbind(a90[1][[1]][12],yy11)
  df90.12<-rbind(a90[1][[1]][13],yy12)
  df90.13<-rbind(a90[1][[1]][14],yy13)
  df90.14<-rbind(a90[1][[1]][15],yy14)
  df90.15<-rbind(a90[1][[1]][16],yy15)
  df90.16<-rbind(a90[1][[1]][17],yy16)
  df90.17<-rbind(a90[1][[1]][18],yy17)
  df90.18<-rbind(a90[1][[1]][19],yy18)
  df90.19<-rbind(a90[1][[1]][20],yy19)
  df90.20<-rbind(a90[1][[1]][21],yy20)
  df90.21<-rbind(a90[1][[1]][22],yy21)
  df90.22<-rbind(a90[1][[1]][23],yy22)
  df90.23<-rbind(a90[1][[1]][24],yy23)
  df90.24<-rbind(a90[1][[1]][25],yy24)
  df90.25<-rbind(a90[1][[1]][26],yy25)
  df90.26<-rbind(a90[1][[1]][27],yy26)
  df90.27<-rbind(a90[1][[1]][28],yy27)
  df90.28<-rbind(a90[1][[1]][29],yy28)
  df90.29<-rbind(a90[1][[1]][30],yy29)
  df90.30<-rbind(a90[1][[1]][31],yy30)
  df90.31<-rbind(a90[1][[1]][32],yy31)
  df90.32<-rbind(a90[1][[1]][33],yy32)
  df90.33<-rbind(a90[1][[1]][34],yy33)
  df90.34<-rbind(a90[1][[1]][35],yy34)
  df90.35<-rbind(a90[1][[1]][36],yy35)
  df90.36<-rbind(a90[1][[1]][37],yy36)
  df90.37<-rbind(a90[1][[1]][38],yy37)
  df90.38<-rbind(a90[1][[1]][39],yy38)
  df90.39<-rbind(a90[1][[1]][40],yy39)
  df90.40<-rbind(a90[1][[1]][41],yy40)
  df90.41<-rbind(a90[1][[1]][42],yy41)
  df90.42<-rbind(a90[1][[1]][43],yy42)
  df90.43<-rbind(a90[1][[1]][44],yy43)
  df90.44<-rbind(a90[1][[1]][45],yy44)
  df90.45<-rbind(a90[1][[1]][46],yy45)
  df90.46<-rbind(a90[1][[1]][47],yy46)
  df90.47<-rbind(a90[1][[1]][48],yy47)
  df90.48<-rbind(a90[1][[1]][49],yy48)
  df90.49<-rbind(a90[1][[1]][50],yy49)
  
  y0.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.0)))
  y0.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.0)))
  y0.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.0)))
  y0.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.0)))
  
  y1.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.1)))
  y1.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.1)))
  y1.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.1)))
  y1.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.1)))
  
  y2.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.2)))
  y2.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.2)))
  y2.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.2)))
  y2.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.2)))
  
  y3.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.3)))
  y3.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.3)))
  y3.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.3)))
  y3.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.3)))
  
  y4.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.4)))
  y4.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.4)))
  y4.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.4)))
  y4.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.4)))
  
  y5.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.5)))
  y5.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.5)))
  y5.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.5)))
  y5.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.5)))
  
  y6.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.6)))
  y6.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.6)))
  y6.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.6)))
  y6.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.6)))
  
  y7.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.7)))
  y7.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.7)))
  y7.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.7)))
  y7.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.7)))
  
  y8.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.8)))
  y8.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.8)))
  y8.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.8)))
  y8.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.8)))
  
  y9.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.9)))
  y9.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.9)))
  y9.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.9)))
  y9.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.9)))
  
  y10.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.10)))
  y10.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.10)))
  y10.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.10)))
  y10.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.10)))
  
  y11.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.11)))
  y11.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.11)))
  y11.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.11)))
  y11.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.11)))
  
  y12.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.12)))
  y12.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.12)))
  y12.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.12)))
  y12.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.12)))
  
  y13.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.13)))
  y13.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.13)))
  y13.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.13)))
  y13.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.13)))
  
  y14.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.14)))
  y14.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.14)))
  y14.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.14)))
  y14.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.14)))
  
  y15.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.15)))
  y15.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.15)))
  y15.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.15)))
  y15.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.15)))
  
  y16.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.16)))
  y16.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.16)))
  y16.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.16)))
  y16.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.16)))
  
  y17.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.17)))
  y17.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.17)))
  y17.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.17)))
  y17.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.17)))
  
  y18.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.18)))
  y18.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.18)))
  y18.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.18)))
  y18.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.18)))
  
  y19.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.19)))
  y19.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.19)))
  y19.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.19)))
  y19.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.19)))
  y20.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.20)))
  y20.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.20)))
  y20.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.20)))
  y20.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.20)))
  
  y21.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.21)))
  y21.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.21)))
  y21.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.21)))
  y21.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.21)))
  
  y22.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.22)))
  y22.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.22)))
  y22.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.22)))
  y22.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.22)))
  
  y23.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.23)))
  y23.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.23)))
  y23.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.23)))
  y23.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.23)))
  
  y24.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.24)))
  y24.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.24)))
  y24.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.24)))
  y24.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.24)))
  
  y25.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.25)))
  y25.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.25)))
  y25.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.25)))
  y25.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.25)))
  
  y26.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.26)))
  y26.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.26)))
  y26.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.26)))
  y26.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.26)))
  
  y27.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.27)))
  y27.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.27)))
  y27.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.27)))
  y27.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.27)))
  
  y28.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.28)))
  y28.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.28)))
  y28.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.28)))
  y28.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.28)))
  
  y29.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.29)))
  y29.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.29)))
  y29.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.29)))
  y29.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.29)))
  y30.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.30)))
  y30.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.30)))
  y30.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.30)))
  y30.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.30)))
  
  y31.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.31)))
  y31.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.31)))
  y31.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.31)))
  y31.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.31)))
  
  y32.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.32)))
  y32.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.32)))
  y32.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.32)))
  y32.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.32)))
  
  y33.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.33)))
  y33.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.33)))
  y33.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.33)))
  y33.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.33)))
  
  y34.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.34)))
  y34.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.34)))
  y34.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.34)))
  y34.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.34)))
  
  y35.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.35)))
  y35.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.35)))
  y35.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.35)))
  y35.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.35)))
  
  y36.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.36)))
  y36.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.36)))
  y36.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.36)))
  y36.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.36)))
  
  y37.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.37)))
  y37.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.37)))
  y37.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.37)))
  y37.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.37)))
  
  y38.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.38)))
  y38.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.38)))
  y38.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.38)))
  y38.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.38)))
  
  y39.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.39)))
  y39.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.39)))
  y39.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.39)))
  y39.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.39)))
  y40.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.40)))
  y40.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.40)))
  y40.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.40)))
  y40.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.40)))
  
  y41.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.41)))
  y41.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.41)))
  y41.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.41)))
  y41.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.41)))
  
  y42.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.42)))
  y42.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.42)))
  y42.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.42)))
  y42.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.42)))
  
  y43.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.43)))
  y43.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.43)))
  y43.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.43)))
  y43.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.43)))
  
  y44.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.44)))
  y44.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.44)))
  y44.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.44)))
  y44.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.44)))
  
  y45.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.45)))
  y45.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.45)))
  y45.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.45)))
  y45.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.45)))
  
  y46.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.46)))
  y46.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.46)))
  y46.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.46)))
  y46.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.46)))
  
  y47.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.47)))
  y47.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.47)))
  y47.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.47)))
  y47.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.47)))
  
  y48.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.48)))
  y48.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.48)))
  y48.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.48)))
  y48.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.48)))
  
  y49.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.49)))
  y49.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.49)))
  y49.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.49)))
  y49.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.49)))
  
  wei<-cbind(y0.wei,y1.wei,y2.wei,y3.wei,y4.wei,y5.wei,y6.wei,y7.wei,y8.wei,y9.wei,y10.wei,y11.wei,y12.wei,y13.wei,y14.wei,y15.wei,y16.wei,y17.wei,y18.wei,y19.wei,y20.wei,y21.wei,y22.wei,y23.wei,y24.wei,y25.wei,y26.wei,y27.wei,y28.wei,y29.wei,y30.wei,y31.wei,y32.wei,y33.wei,y34.wei,y35.wei,y36.wei,y37.wei,y38.wei,y39.wei,y40.wei,y41.wei,y42.wei,y43.wei,y44.wei,y45.wei,y46.wei,y47.wei,y48.wei,y49.wei)
  gum<-cbind(y0.gum,y1.gum,y2.gum,y3.gum,y4.gum,y5.gum,y6.gum,y7.gum,y8.gum,y9.gum,y10.gum,y11.gum,y12.gum,y13.gum,y14.gum,y15.gum,y16.gum,y17.gum,y18.gum,y19.gum,y20.gum,y21.gum,y22.gum,y23.gum,y24.gum,y25.gum,y26.gum,y27.gum,y28.gum,y29.gum,y30.gum,y31.gum,y32.gum,y33.gum,y34.gum,y35.gum,y36.gum,y37.gum,y38.gum,y39.gum,y40.gum,y41.gum,y42.gum,y43.gum,y44.gum,y45.gum,y46.gum,y47.gum,y48.gum,y49.gum)
  pe3<-cbind(y0.pe3,y1.pe3,y2.pe3,y3.pe3,y4.pe3,y5.pe3,y6.pe3,y7.pe3,y8.pe3,y9.pe3,y10.pe3,y11.pe3,y12.pe3,y13.pe3,y14.pe3,y15.pe3,y16.pe3,y17.pe3,y18.pe3,y19.pe3,y20.pe3,y21.pe3,y22.pe3,y23.pe3,y24.pe3,y25.pe3,y26.pe3,y27.pe3,y28.pe3,y29.pe3,y30.pe3,y31.pe3,y32.pe3,y33.pe3,y34.pe3,y35.pe3,y36.pe3,y37.pe3,y38.pe3,y39.pe3,y40.pe3,y41.pe3,y42.pe3,y43.pe3,y44.pe3,y45.pe3,y46.pe3,y47.pe3,y48.pe3,y49.pe3)
  ln3<-cbind(y0.ln3,y1.ln3,y2.ln3,y3.ln3,y4.ln3,y5.ln3,y6.ln3,y7.ln3,y8.ln3,y9.ln3,y10.ln3,y11.ln3,y12.ln3,y13.ln3,y14.ln3,y15.ln3,y16.ln3,y17.ln3,y18.ln3,y19.ln3,y20.ln3,y21.ln3,y22.ln3,y23.ln3,y24.ln3,y25.ln3,y26.ln3,y27.ln3,y28.ln3,y29.ln3,y30.ln3,y31.ln3,y32.ln3,y33.ln3,y34.ln3,y35.ln3,y36.ln3,y37.ln3,y38.ln3,y39.ln3,y40.ln3,y41.ln3,y42.ln3,y43.ln3,y44.ln3,y45.ln3,y46.ln3,y47.ln3,y48.ln3,y49.ln3)
  
  return(list(wei,gum,pe3,ln3))
  
}

inc.hqs.9999.99<-function(dfs,year,quantile){
  dff<-dfs[[1]]
  df<-dfs[[1]]
  a90<-par.80y(dfm=dff,year = year,quantile=quantile) # Wird eingefügt als Fiktivwert
  quantile=0.99 # für die Hochwasserberechnung
  ys0<-seq(1,year*365+11,1)
  ys1<-seq(1,(year*365+10+1*365),1)
  ys2<-seq(1,(year*365+10+2*365),1)
  ys3<-seq(1,(year*365+10+3*365),1)
  ys4<-seq(1,(year*365+10+4*365),1)
  ys5<-seq(1,(year*365+10+5*365),1)
  ys6<-seq(1,(year*365+10+6*365),1)
  ys7<-seq(1,(year*365+10+7*365),1)
  ys8<-seq(1,(year*365+10+8*365),1)
  ys9<-seq(1,(year*365+10+9*365),1)
  ys10<-seq(1,(year*365+10+10*365),1)
  ys11<-seq(1,(year*365+10+11*365),1)
  ys12<-seq(1,(year*365+10+12*365),1)
  ys13<-seq(1,(year*365+10+13*365),1)
  ys14<-seq(1,(year*365+10+14*365),1)
  ys15<-seq(1,(year*365+10+15*365),1)
  ys16<-seq(1,(year*365+10+16*365),1)
  ys17<-seq(1,(year*365+10+17*365),1)
  ys18<-seq(1,(year*365+10+18*365),1)
  ys19<-seq(1,(year*365+10+19*365),1)
  ys20<-seq(1,(year*365+10+20*365),1)
  ys21<-seq(1,(year*365+10+21*365),1)
  ys22<-seq(1,(year*365+10+22*365),1)
  ys23<-seq(1,(year*365+10+23*365),1)
  ys24<-seq(1,(year*365+10+24*365),1)
  ys25<-seq(1,(year*365+10+25*365),1)
  ys26<-seq(1,(year*365+10+26*365),1)
  ys27<-seq(1,(year*365+10+27*365),1)
  ys28<-seq(1,(year*365+10+28*365),1)
  ys29<-seq(1,(year*365+10+29*365),1)
  ys30<-seq(1,(year*365+10+30*365),1)
  ys31<-seq(1,(year*365+10+31*365),1)
  ys32<-seq(1,(year*365+10+32*365),1)
  ys33<-seq(1,(year*365+10+33*365),1)
  ys34<-seq(1,(year*365+10+34*365),1)
  ys35<-seq(1,(year*365+10+35*365),1)
  ys36<-seq(1,(year*365+10+36*365),1)
  ys37<-seq(1,(year*365+10+37*365),1)
  ys38<-seq(1,(year*365+10+38*365),1)
  ys39<-seq(1,(year*365+10+39*365),1)
  ys40<-seq(1,(year*365+10+40*365),1)
  ys41<-seq(1,(year*365+10+41*365),1)
  ys42<-seq(1,(year*365+10+42*365),1)
  ys43<-seq(1,(year*365+10+43*365),1)
  ys44<-seq(1,(year*365+10+44*365),1)
  ys45<-seq(1,(year*365+10+45*365),1)
  ys46<-seq(1,(year*365+10+46*365),1)
  ys47<-seq(1,(year*365+10+47*365),1)
  ys48<-seq(1,(year*365+10+48*365),1)
  ys49<-seq(1,(year*365+10+49*365),1)
  
  yy0<-as.matrix(df[[4]][ys0])
  yy1<-as.matrix(df[[4]][ys1])
  yy2<-as.matrix(df[[4]][ys2])
  yy3<-as.matrix(df[[4]][ys3])
  yy4<-as.matrix(df[[4]][ys4])
  yy5<-as.matrix(df[[4]][ys5])
  yy6<-as.matrix(df[[4]][ys6])
  yy7<-as.matrix(df[[4]][ys7])
  yy8<-as.matrix(df[[4]][ys8])
  yy9<-as.matrix(df[[4]][ys9])
  yy10<-as.matrix(df[[4]][ys10])
  yy11<-as.matrix(df[[4]][ys11])
  yy12<-as.matrix(df[[4]][ys12])
  yy13<-as.matrix(df[[4]][ys13])
  yy14<-as.matrix(df[[4]][ys14])
  yy15<-as.matrix(df[[4]][ys15])
  yy16<-as.matrix(df[[4]][ys16])
  yy17<-as.matrix(df[[4]][ys17])
  yy18<-as.matrix(df[[4]][ys18])
  yy19<-as.matrix(df[[4]][ys19])
  yy20<-as.matrix(df[[4]][ys20])
  yy21<-as.matrix(df[[4]][ys21])
  yy22<-as.matrix(df[[4]][ys22])
  yy23<-as.matrix(df[[4]][ys23])
  yy24<-as.matrix(df[[4]][ys24])
  yy25<-as.matrix(df[[4]][ys25])
  yy26<-as.matrix(df[[4]][ys26])
  yy27<-as.matrix(df[[4]][ys27])
  yy28<-as.matrix(df[[4]][ys28])
  yy29<-as.matrix(df[[4]][ys29])
  yy30<-as.matrix(df[[4]][ys30])
  yy31<-as.matrix(df[[4]][ys31])
  yy32<-as.matrix(df[[4]][ys32])
  yy33<-as.matrix(df[[4]][ys33])
  yy34<-as.matrix(df[[4]][ys34])
  yy35<-as.matrix(df[[4]][ys35])
  yy36<-as.matrix(df[[4]][ys36])
  yy37<-as.matrix(df[[4]][ys37])
  yy38<-as.matrix(df[[4]][ys38])
  yy39<-as.matrix(df[[4]][ys39])
  yy40<-as.matrix(df[[4]][ys40])
  yy41<-as.matrix(df[[4]][ys41])
  yy42<-as.matrix(df[[4]][ys42])
  yy43<-as.matrix(df[[4]][ys43])
  yy44<-as.matrix(df[[4]][ys44])
  yy45<-as.matrix(df[[4]][ys45])
  yy46<-as.matrix(df[[4]][ys46])
  yy47<-as.matrix(df[[4]][ys47])
  yy48<-as.matrix(df[[4]][ys48])
  yy49<-as.matrix(df[[4]][ys49])
  
  df90.0<-rbind(a90[1][[1]][1],yy0)
  df90.1<-rbind(a90[1][[1]][2],yy1)
  df90.2<-rbind(a90[1][[1]][3],yy2)
  df90.3<-rbind(a90[1][[1]][4],yy3)
  df90.4<-rbind(a90[1][[1]][5],yy4)
  df90.5<-rbind(a90[1][[1]][6],yy5)
  df90.6<-rbind(a90[1][[1]][7],yy6)
  df90.7<-rbind(a90[1][[1]][8],yy7)
  df90.8<-rbind(a90[1][[1]][9],yy8)
  df90.9<-rbind(a90[1][[1]][10],yy9)
  df90.10<-rbind(a90[1][[1]][11],yy10)
  df90.11<-rbind(a90[1][[1]][12],yy11)
  df90.12<-rbind(a90[1][[1]][13],yy12)
  df90.13<-rbind(a90[1][[1]][14],yy13)
  df90.14<-rbind(a90[1][[1]][15],yy14)
  df90.15<-rbind(a90[1][[1]][16],yy15)
  df90.16<-rbind(a90[1][[1]][17],yy16)
  df90.17<-rbind(a90[1][[1]][18],yy17)
  df90.18<-rbind(a90[1][[1]][19],yy18)
  df90.19<-rbind(a90[1][[1]][20],yy19)
  df90.20<-rbind(a90[1][[1]][21],yy20)
  df90.21<-rbind(a90[1][[1]][22],yy21)
  df90.22<-rbind(a90[1][[1]][23],yy22)
  df90.23<-rbind(a90[1][[1]][24],yy23)
  df90.24<-rbind(a90[1][[1]][25],yy24)
  df90.25<-rbind(a90[1][[1]][26],yy25)
  df90.26<-rbind(a90[1][[1]][27],yy26)
  df90.27<-rbind(a90[1][[1]][28],yy27)
  df90.28<-rbind(a90[1][[1]][29],yy28)
  df90.29<-rbind(a90[1][[1]][30],yy29)
  df90.30<-rbind(a90[1][[1]][31],yy30)
  df90.31<-rbind(a90[1][[1]][32],yy31)
  df90.32<-rbind(a90[1][[1]][33],yy32)
  df90.33<-rbind(a90[1][[1]][34],yy33)
  df90.34<-rbind(a90[1][[1]][35],yy34)
  df90.35<-rbind(a90[1][[1]][36],yy35)
  df90.36<-rbind(a90[1][[1]][37],yy36)
  df90.37<-rbind(a90[1][[1]][38],yy37)
  df90.38<-rbind(a90[1][[1]][39],yy38)
  df90.39<-rbind(a90[1][[1]][40],yy39)
  df90.40<-rbind(a90[1][[1]][41],yy40)
  df90.41<-rbind(a90[1][[1]][42],yy41)
  df90.42<-rbind(a90[1][[1]][43],yy42)
  df90.43<-rbind(a90[1][[1]][44],yy43)
  df90.44<-rbind(a90[1][[1]][45],yy44)
  df90.45<-rbind(a90[1][[1]][46],yy45)
  df90.46<-rbind(a90[1][[1]][47],yy46)
  df90.47<-rbind(a90[1][[1]][48],yy47)
  df90.48<-rbind(a90[1][[1]][49],yy48)
  df90.49<-rbind(a90[1][[1]][50],yy49)
  
  y0.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.0)))
  y0.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.0)))
  y0.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.0)))
  y0.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.0)))
  
  y1.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.1)))
  y1.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.1)))
  y1.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.1)))
  y1.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.1)))
  
  y2.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.2)))
  y2.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.2)))
  y2.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.2)))
  y2.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.2)))
  
  y3.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.3)))
  y3.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.3)))
  y3.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.3)))
  y3.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.3)))
  
  y4.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.4)))
  y4.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.4)))
  y4.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.4)))
  y4.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.4)))
  
  y5.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.5)))
  y5.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.5)))
  y5.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.5)))
  y5.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.5)))
  
  y6.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.6)))
  y6.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.6)))
  y6.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.6)))
  y6.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.6)))
  
  y7.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.7)))
  y7.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.7)))
  y7.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.7)))
  y7.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.7)))
  
  y8.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.8)))
  y8.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.8)))
  y8.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.8)))
  y8.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.8)))
  
  y9.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.9)))
  y9.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.9)))
  y9.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.9)))
  y9.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.9)))
  
  y10.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.10)))
  y10.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.10)))
  y10.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.10)))
  y10.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.10)))
  
  y11.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.11)))
  y11.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.11)))
  y11.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.11)))
  y11.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.11)))
  
  y12.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.12)))
  y12.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.12)))
  y12.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.12)))
  y12.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.12)))
  
  y13.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.13)))
  y13.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.13)))
  y13.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.13)))
  y13.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.13)))
  
  y14.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.14)))
  y14.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.14)))
  y14.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.14)))
  y14.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.14)))
  
  y15.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.15)))
  y15.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.15)))
  y15.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.15)))
  y15.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.15)))
  
  y16.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.16)))
  y16.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.16)))
  y16.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.16)))
  y16.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.16)))
  
  y17.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.17)))
  y17.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.17)))
  y17.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.17)))
  y17.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.17)))
  
  y18.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.18)))
  y18.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.18)))
  y18.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.18)))
  y18.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.18)))
  
  y19.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.19)))
  y19.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.19)))
  y19.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.19)))
  y19.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.19)))
  y20.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.20)))
  y20.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.20)))
  y20.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.20)))
  y20.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.20)))
  
  y21.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.21)))
  y21.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.21)))
  y21.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.21)))
  y21.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.21)))
  
  y22.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.22)))
  y22.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.22)))
  y22.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.22)))
  y22.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.22)))
  
  y23.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.23)))
  y23.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.23)))
  y23.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.23)))
  y23.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.23)))
  
  y24.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.24)))
  y24.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.24)))
  y24.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.24)))
  y24.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.24)))
  
  y25.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.25)))
  y25.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.25)))
  y25.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.25)))
  y25.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.25)))
  
  y26.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.26)))
  y26.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.26)))
  y26.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.26)))
  y26.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.26)))
  
  y27.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.27)))
  y27.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.27)))
  y27.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.27)))
  y27.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.27)))
  
  y28.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.28)))
  y28.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.28)))
  y28.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.28)))
  y28.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.28)))
  
  y29.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.29)))
  y29.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.29)))
  y29.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.29)))
  y29.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.29)))
  y30.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.30)))
  y30.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.30)))
  y30.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.30)))
  y30.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.30)))
  
  y31.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.31)))
  y31.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.31)))
  y31.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.31)))
  y31.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.31)))
  
  y32.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.32)))
  y32.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.32)))
  y32.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.32)))
  y32.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.32)))
  
  y33.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.33)))
  y33.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.33)))
  y33.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.33)))
  y33.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.33)))
  
  y34.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.34)))
  y34.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.34)))
  y34.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.34)))
  y34.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.34)))
  
  y35.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.35)))
  y35.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.35)))
  y35.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.35)))
  y35.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.35)))
  
  y36.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.36)))
  y36.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.36)))
  y36.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.36)))
  y36.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.36)))
  
  y37.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.37)))
  y37.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.37)))
  y37.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.37)))
  y37.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.37)))
  
  y38.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.38)))
  y38.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.38)))
  y38.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.38)))
  y38.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.38)))
  
  y39.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.39)))
  y39.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.39)))
  y39.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.39)))
  y39.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.39)))
  y40.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.40)))
  y40.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.40)))
  y40.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.40)))
  y40.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.40)))
  
  y41.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.41)))
  y41.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.41)))
  y41.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.41)))
  y41.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.41)))
  
  y42.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.42)))
  y42.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.42)))
  y42.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.42)))
  y42.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.42)))
  
  y43.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.43)))
  y43.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.43)))
  y43.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.43)))
  y43.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.43)))
  
  y44.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.44)))
  y44.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.44)))
  y44.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.44)))
  y44.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.44)))
  
  y45.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.45)))
  y45.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.45)))
  y45.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.45)))
  y45.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.45)))
  
  y46.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.46)))
  y46.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.46)))
  y46.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.46)))
  y46.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.46)))
  
  y47.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.47)))
  y47.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.47)))
  y47.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.47)))
  y47.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.47)))
  
  y48.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.48)))
  y48.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.48)))
  y48.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.48)))
  y48.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.48)))
  
  y49.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.49)))
  y49.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.49)))
  y49.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.49)))
  y49.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.49)))
  
  wei<-cbind(y0.wei,y1.wei,y2.wei,y3.wei,y4.wei,y5.wei,y6.wei,y7.wei,y8.wei,y9.wei,y10.wei,y11.wei,y12.wei,y13.wei,y14.wei,y15.wei,y16.wei,y17.wei,y18.wei,y19.wei,y20.wei,y21.wei,y22.wei,y23.wei,y24.wei,y25.wei,y26.wei,y27.wei,y28.wei,y29.wei,y30.wei,y31.wei,y32.wei,y33.wei,y34.wei,y35.wei,y36.wei,y37.wei,y38.wei,y39.wei,y40.wei,y41.wei,y42.wei,y43.wei,y44.wei,y45.wei,y46.wei,y47.wei,y48.wei,y49.wei)
  gum<-cbind(y0.gum,y1.gum,y2.gum,y3.gum,y4.gum,y5.gum,y6.gum,y7.gum,y8.gum,y9.gum,y10.gum,y11.gum,y12.gum,y13.gum,y14.gum,y15.gum,y16.gum,y17.gum,y18.gum,y19.gum,y20.gum,y21.gum,y22.gum,y23.gum,y24.gum,y25.gum,y26.gum,y27.gum,y28.gum,y29.gum,y30.gum,y31.gum,y32.gum,y33.gum,y34.gum,y35.gum,y36.gum,y37.gum,y38.gum,y39.gum,y40.gum,y41.gum,y42.gum,y43.gum,y44.gum,y45.gum,y46.gum,y47.gum,y48.gum,y49.gum)
  pe3<-cbind(y0.pe3,y1.pe3,y2.pe3,y3.pe3,y4.pe3,y5.pe3,y6.pe3,y7.pe3,y8.pe3,y9.pe3,y10.pe3,y11.pe3,y12.pe3,y13.pe3,y14.pe3,y15.pe3,y16.pe3,y17.pe3,y18.pe3,y19.pe3,y20.pe3,y21.pe3,y22.pe3,y23.pe3,y24.pe3,y25.pe3,y26.pe3,y27.pe3,y28.pe3,y29.pe3,y30.pe3,y31.pe3,y32.pe3,y33.pe3,y34.pe3,y35.pe3,y36.pe3,y37.pe3,y38.pe3,y39.pe3,y40.pe3,y41.pe3,y42.pe3,y43.pe3,y44.pe3,y45.pe3,y46.pe3,y47.pe3,y48.pe3,y49.pe3)
  ln3<-cbind(y0.ln3,y1.ln3,y2.ln3,y3.ln3,y4.ln3,y5.ln3,y6.ln3,y7.ln3,y8.ln3,y9.ln3,y10.ln3,y11.ln3,y12.ln3,y13.ln3,y14.ln3,y15.ln3,y16.ln3,y17.ln3,y18.ln3,y19.ln3,y20.ln3,y21.ln3,y22.ln3,y23.ln3,y24.ln3,y25.ln3,y26.ln3,y27.ln3,y28.ln3,y29.ln3,y30.ln3,y31.ln3,y32.ln3,y33.ln3,y34.ln3,y35.ln3,y36.ln3,y37.ln3,y38.ln3,y39.ln3,y40.ln3,y41.ln3,y42.ln3,y43.ln3,y44.ln3,y45.ln3,y46.ln3,y47.ln3,y48.ln3,y49.ln3)
  
  return(list(wei,gum,pe3,ln3))
  
}

### Ein 30 tägiges HQ am Anfang, dann ein 0.99 HQ berechnen
maxmonth<-function(dfs,year,quantile){
  dff<-dfs[[1]]
  df<-dfs[[1]]
  a90<-par.80y(dfm=dff,year = year,quantile=quantile)
  quantile=0.99
  ys0<-seq(1,year*365+11,1)
  ys1<-seq(1,(year*365+10+1*365),1)
  ys2<-seq(1,(year*365+10+2*365),1)
  ys3<-seq(1,(year*365+10+3*365),1)
  ys4<-seq(1,(year*365+10+4*365),1)
  ys5<-seq(1,(year*365+10+5*365),1)
  ys6<-seq(1,(year*365+10+6*365),1)
  ys7<-seq(1,(year*365+10+7*365),1)
  ys8<-seq(1,(year*365+10+8*365),1)
  ys9<-seq(1,(year*365+10+9*365),1)
  ys10<-seq(1,(year*365+10+10*365),1)
  ys11<-seq(1,(year*365+10+11*365),1)
  ys12<-seq(1,(year*365+10+12*365),1)
  ys13<-seq(1,(year*365+10+13*365),1)
  ys14<-seq(1,(year*365+10+14*365),1)
  ys15<-seq(1,(year*365+10+15*365),1)
  ys16<-seq(1,(year*365+10+16*365),1)
  ys17<-seq(1,(year*365+10+17*365),1)
  ys18<-seq(1,(year*365+10+18*365),1)
  ys19<-seq(1,(year*365+10+19*365),1)
  ys20<-seq(1,(year*365+10+20*365),1)
  ys21<-seq(1,(year*365+10+21*365),1)
  ys22<-seq(1,(year*365+10+22*365),1)
  ys23<-seq(1,(year*365+10+23*365),1)
  ys24<-seq(1,(year*365+10+24*365),1)
  ys25<-seq(1,(year*365+10+25*365),1)
  ys26<-seq(1,(year*365+10+26*365),1)
  ys27<-seq(1,(year*365+10+27*365),1)
  ys28<-seq(1,(year*365+10+28*365),1)
  ys29<-seq(1,(year*365+10+29*365),1)
  ys30<-seq(1,(year*365+10+30*365),1)
  ys31<-seq(1,(year*365+10+31*365),1)
  ys32<-seq(1,(year*365+10+32*365),1)
  ys33<-seq(1,(year*365+10+33*365),1)
  ys34<-seq(1,(year*365+10+34*365),1)
  ys35<-seq(1,(year*365+10+35*365),1)
  ys36<-seq(1,(year*365+10+36*365),1)
  ys37<-seq(1,(year*365+10+37*365),1)
  ys38<-seq(1,(year*365+10+38*365),1)
  ys39<-seq(1,(year*365+10+39*365),1)
  ys40<-seq(1,(year*365+10+40*365),1)
  ys41<-seq(1,(year*365+10+41*365),1)
  ys42<-seq(1,(year*365+10+42*365),1)
  ys43<-seq(1,(year*365+10+43*365),1)
  ys44<-seq(1,(year*365+10+44*365),1)
  ys45<-seq(1,(year*365+10+45*365),1)
  ys46<-seq(1,(year*365+10+46*365),1)
  ys47<-seq(1,(year*365+10+47*365),1)
  ys48<-seq(1,(year*365+10+48*365),1)
  ys49<-seq(1,(year*365+10+49*365),1)
  
  yy0<-as.matrix(df[[4]][ys0])
  yy1<-as.matrix(df[[4]][ys1])
  yy2<-as.matrix(df[[4]][ys2])
  yy3<-as.matrix(df[[4]][ys3])
  yy4<-as.matrix(df[[4]][ys4])
  yy5<-as.matrix(df[[4]][ys5])
  yy6<-as.matrix(df[[4]][ys6])
  yy7<-as.matrix(df[[4]][ys7])
  yy8<-as.matrix(df[[4]][ys8])
  yy9<-as.matrix(df[[4]][ys9])
  yy10<-as.matrix(df[[4]][ys10])
  yy11<-as.matrix(df[[4]][ys11])
  yy12<-as.matrix(df[[4]][ys12])
  yy13<-as.matrix(df[[4]][ys13])
  yy14<-as.matrix(df[[4]][ys14])
  yy15<-as.matrix(df[[4]][ys15])
  yy16<-as.matrix(df[[4]][ys16])
  yy17<-as.matrix(df[[4]][ys17])
  yy18<-as.matrix(df[[4]][ys18])
  yy19<-as.matrix(df[[4]][ys19])
  yy20<-as.matrix(df[[4]][ys20])
  yy21<-as.matrix(df[[4]][ys21])
  yy22<-as.matrix(df[[4]][ys22])
  yy23<-as.matrix(df[[4]][ys23])
  yy24<-as.matrix(df[[4]][ys24])
  yy25<-as.matrix(df[[4]][ys25])
  yy26<-as.matrix(df[[4]][ys26])
  yy27<-as.matrix(df[[4]][ys27])
  yy28<-as.matrix(df[[4]][ys28])
  yy29<-as.matrix(df[[4]][ys29])
  yy30<-as.matrix(df[[4]][ys30])
  yy31<-as.matrix(df[[4]][ys31])
  yy32<-as.matrix(df[[4]][ys32])
  yy33<-as.matrix(df[[4]][ys33])
  yy34<-as.matrix(df[[4]][ys34])
  yy35<-as.matrix(df[[4]][ys35])
  yy36<-as.matrix(df[[4]][ys36])
  yy37<-as.matrix(df[[4]][ys37])
  yy38<-as.matrix(df[[4]][ys38])
  yy39<-as.matrix(df[[4]][ys39])
  yy40<-as.matrix(df[[4]][ys40])
  yy41<-as.matrix(df[[4]][ys41])
  yy42<-as.matrix(df[[4]][ys42])
  yy43<-as.matrix(df[[4]][ys43])
  yy44<-as.matrix(df[[4]][ys44])
  yy45<-as.matrix(df[[4]][ys45])
  yy46<-as.matrix(df[[4]][ys46])
  yy47<-as.matrix(df[[4]][ys47])
  yy48<-as.matrix(df[[4]][ys48])
  yy49<-as.matrix(df[[4]][ys49])
  
  df90.0<-rbind(a90[1][[1]][1],yy0)
  df90.1<-rbind(a90[1][[1]][2],yy1)
  df90.2<-rbind(a90[1][[1]][3],yy2)
  df90.3<-rbind(a90[1][[1]][4],yy3)
  df90.4<-rbind(a90[1][[1]][5],yy4)
  df90.5<-rbind(a90[1][[1]][6],yy5)
  df90.6<-rbind(a90[1][[1]][7],yy6)
  df90.7<-rbind(a90[1][[1]][8],yy7)
  df90.8<-rbind(a90[1][[1]][9],yy8)
  df90.9<-rbind(a90[1][[1]][10],yy9)
  df90.10<-rbind(a90[1][[1]][11],yy10)
  df90.11<-rbind(a90[1][[1]][12],yy11)
  df90.12<-rbind(a90[1][[1]][13],yy12)
  df90.13<-rbind(a90[1][[1]][14],yy13)
  df90.14<-rbind(a90[1][[1]][15],yy14)
  df90.15<-rbind(a90[1][[1]][16],yy15)
  df90.16<-rbind(a90[1][[1]][17],yy16)
  df90.17<-rbind(a90[1][[1]][18],yy17)
  df90.18<-rbind(a90[1][[1]][19],yy18)
  df90.19<-rbind(a90[1][[1]][20],yy19)
  df90.20<-rbind(a90[1][[1]][21],yy20)
  df90.21<-rbind(a90[1][[1]][22],yy21)
  df90.22<-rbind(a90[1][[1]][23],yy22)
  df90.23<-rbind(a90[1][[1]][24],yy23)
  df90.24<-rbind(a90[1][[1]][25],yy24)
  df90.25<-rbind(a90[1][[1]][26],yy25)
  df90.26<-rbind(a90[1][[1]][27],yy26)
  df90.27<-rbind(a90[1][[1]][28],yy27)
  df90.28<-rbind(a90[1][[1]][29],yy28)
  df90.29<-rbind(a90[1][[1]][30],yy29)
  df90.30<-rbind(a90[1][[1]][31],yy30)
  df90.31<-rbind(a90[1][[1]][32],yy31)
  df90.32<-rbind(a90[1][[1]][33],yy32)
  df90.33<-rbind(a90[1][[1]][34],yy33)
  df90.34<-rbind(a90[1][[1]][35],yy34)
  df90.35<-rbind(a90[1][[1]][36],yy35)
  df90.36<-rbind(a90[1][[1]][37],yy36)
  df90.37<-rbind(a90[1][[1]][38],yy37)
  df90.38<-rbind(a90[1][[1]][39],yy38)
  df90.39<-rbind(a90[1][[1]][40],yy39)
  df90.40<-rbind(a90[1][[1]][41],yy40)
  df90.41<-rbind(a90[1][[1]][42],yy41)
  df90.42<-rbind(a90[1][[1]][43],yy42)
  df90.43<-rbind(a90[1][[1]][44],yy43)
  df90.44<-rbind(a90[1][[1]][45],yy44)
  df90.45<-rbind(a90[1][[1]][46],yy45)
  df90.46<-rbind(a90[1][[1]][47],yy46)
  df90.47<-rbind(a90[1][[1]][48],yy47)
  df90.48<-rbind(a90[1][[1]][49],yy48)
  df90.49<-rbind(a90[1][[1]][50],yy49)
  
  y0.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.0)))
  y0.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.0)))
  y0.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.0)))
  y0.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.0)))
  
  y1.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.1)))
  y1.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.1)))
  y1.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.1)))
  y1.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.1)))
  
  y2.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.2)))
  y2.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.2)))
  y2.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.2)))
  y2.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.2)))
  
  y3.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.3)))
  y3.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.3)))
  y3.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.3)))
  y3.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.3)))
  
  y4.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.4)))
  y4.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.4)))
  y4.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.4)))
  y4.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.4)))
  
  y5.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.5)))
  y5.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.5)))
  y5.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.5)))
  y5.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.5)))
  
  y6.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.6)))
  y6.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.6)))
  y6.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.6)))
  y6.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.6)))
  
  y7.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.7)))
  y7.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.7)))
  y7.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.7)))
  y7.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.7)))
  
  y8.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.8)))
  y8.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.8)))
  y8.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.8)))
  y8.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.8)))
  
  y9.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.9)))
  y9.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.9)))
  y9.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.9)))
  y9.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.9)))
  
  y10.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.10)))
  y10.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.10)))
  y10.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.10)))
  y10.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.10)))
  
  y11.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.11)))
  y11.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.11)))
  y11.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.11)))
  y11.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.11)))
  
  y12.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.12)))
  y12.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.12)))
  y12.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.12)))
  y12.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.12)))
  
  y13.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.13)))
  y13.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.13)))
  y13.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.13)))
  y13.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.13)))
  
  y14.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.14)))
  y14.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.14)))
  y14.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.14)))
  y14.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.14)))
  
  y15.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.15)))
  y15.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.15)))
  y15.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.15)))
  y15.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.15)))
  
  y16.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.16)))
  y16.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.16)))
  y16.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.16)))
  y16.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.16)))
  
  y17.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.17)))
  y17.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.17)))
  y17.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.17)))
  y17.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.17)))
  
  y18.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.18)))
  y18.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.18)))
  y18.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.18)))
  y18.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.18)))
  
  y19.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.19)))
  y19.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.19)))
  y19.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.19)))
  y19.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.19)))
  y20.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.20)))
  y20.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.20)))
  y20.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.20)))
  y20.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.20)))
  
  y21.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.21)))
  y21.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.21)))
  y21.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.21)))
  y21.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.21)))
  
  y22.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.22)))
  y22.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.22)))
  y22.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.22)))
  y22.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.22)))
  
  y23.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.23)))
  y23.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.23)))
  y23.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.23)))
  y23.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.23)))
  
  y24.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.24)))
  y24.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.24)))
  y24.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.24)))
  y24.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.24)))
  
  y25.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.25)))
  y25.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.25)))
  y25.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.25)))
  y25.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.25)))
  
  y26.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.26)))
  y26.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.26)))
  y26.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.26)))
  y26.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.26)))
  
  y27.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.27)))
  y27.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.27)))
  y27.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.27)))
  y27.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.27)))
  
  y28.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.28)))
  y28.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.28)))
  y28.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.28)))
  y28.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.28)))
  
  y29.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.29)))
  y29.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.29)))
  y29.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.29)))
  y29.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.29)))
  y30.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.30)))
  y30.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.30)))
  y30.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.30)))
  y30.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.30)))
  
  y31.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.31)))
  y31.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.31)))
  y31.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.31)))
  y31.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.31)))
  
  y32.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.32)))
  y32.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.32)))
  y32.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.32)))
  y32.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.32)))
  
  y33.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.33)))
  y33.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.33)))
  y33.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.33)))
  y33.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.33)))
  
  y34.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.34)))
  y34.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.34)))
  y34.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.34)))
  y34.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.34)))
  
  y35.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.35)))
  y35.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.35)))
  y35.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.35)))
  y35.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.35)))
  
  y36.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.36)))
  y36.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.36)))
  y36.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.36)))
  y36.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.36)))
  
  y37.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.37)))
  y37.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.37)))
  y37.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.37)))
  y37.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.37)))
  
  y38.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.38)))
  y38.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.38)))
  y38.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.38)))
  y38.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.38)))
  
  y39.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.39)))
  y39.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.39)))
  y39.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.39)))
  y39.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.39)))
  y40.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.40)))
  y40.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.40)))
  y40.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.40)))
  y40.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.40)))
  
  y41.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.41)))
  y41.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.41)))
  y41.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.41)))
  y41.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.41)))
  
  y42.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.42)))
  y42.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.42)))
  y42.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.42)))
  y42.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.42)))
  
  y43.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.43)))
  y43.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.43)))
  y43.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.43)))
  y43.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.43)))
  
  y44.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.44)))
  y44.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.44)))
  y44.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.44)))
  y44.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.44)))
  
  y45.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.45)))
  y45.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.45)))
  y45.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.45)))
  y45.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.45)))
  
  y46.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.46)))
  y46.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.46)))
  y46.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.46)))
  y46.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.46)))
  
  y47.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.47)))
  y47.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.47)))
  y47.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.47)))
  y47.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.47)))
  
  y48.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.48)))
  y48.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.48)))
  y48.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.48)))
  y48.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.48)))
  
  y49.wei<-quawei(f=quantile, para=pelwei(samlmu(df90.49)))
  y49.gum<-quagum(f=quantile, para=pelgum(samlmu(df90.49)))
  y49.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df90.49)))
  y49.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df90.49)))
  
  wei<-cbind(y0.wei,y1.wei,y2.wei,y3.wei,y4.wei,y5.wei,y6.wei,y7.wei,y8.wei,y9.wei,y10.wei,y11.wei,y12.wei,y13.wei,y14.wei,y15.wei,y16.wei,y17.wei,y18.wei,y19.wei,y20.wei,y21.wei,y22.wei,y23.wei,y24.wei,y25.wei,y26.wei,y27.wei,y28.wei,y29.wei,y30.wei,y31.wei,y32.wei,y33.wei,y34.wei,y35.wei,y36.wei,y37.wei,y38.wei,y39.wei,y40.wei,y41.wei,y42.wei,y43.wei,y44.wei,y45.wei,y46.wei,y47.wei,y48.wei,y49.wei)
  gum<-cbind(y0.gum,y1.gum,y2.gum,y3.gum,y4.gum,y5.gum,y6.gum,y7.gum,y8.gum,y9.gum,y10.gum,y11.gum,y12.gum,y13.gum,y14.gum,y15.gum,y16.gum,y17.gum,y18.gum,y19.gum,y20.gum,y21.gum,y22.gum,y23.gum,y24.gum,y25.gum,y26.gum,y27.gum,y28.gum,y29.gum,y30.gum,y31.gum,y32.gum,y33.gum,y34.gum,y35.gum,y36.gum,y37.gum,y38.gum,y39.gum,y40.gum,y41.gum,y42.gum,y43.gum,y44.gum,y45.gum,y46.gum,y47.gum,y48.gum,y49.gum)
  pe3<-cbind(y0.pe3,y1.pe3,y2.pe3,y3.pe3,y4.pe3,y5.pe3,y6.pe3,y7.pe3,y8.pe3,y9.pe3,y10.pe3,y11.pe3,y12.pe3,y13.pe3,y14.pe3,y15.pe3,y16.pe3,y17.pe3,y18.pe3,y19.pe3,y20.pe3,y21.pe3,y22.pe3,y23.pe3,y24.pe3,y25.pe3,y26.pe3,y27.pe3,y28.pe3,y29.pe3,y30.pe3,y31.pe3,y32.pe3,y33.pe3,y34.pe3,y35.pe3,y36.pe3,y37.pe3,y38.pe3,y39.pe3,y40.pe3,y41.pe3,y42.pe3,y43.pe3,y44.pe3,y45.pe3,y46.pe3,y47.pe3,y48.pe3,y49.pe3)
  ln3<-cbind(y0.ln3,y1.ln3,y2.ln3,y3.ln3,y4.ln3,y5.ln3,y6.ln3,y7.ln3,y8.ln3,y9.ln3,y10.ln3,y11.ln3,y12.ln3,y13.ln3,y14.ln3,y15.ln3,y16.ln3,y17.ln3,y18.ln3,y19.ln3,y20.ln3,y21.ln3,y22.ln3,y23.ln3,y24.ln3,y25.ln3,y26.ln3,y27.ln3,y28.ln3,y29.ln3,y30.ln3,y31.ln3,y32.ln3,y33.ln3,y34.ln3,y35.ln3,y36.ln3,y37.ln3,y38.ln3,y39.ln3,y40.ln3,y41.ln3,y42.ln3,y43.ln3,y44.ln3,y45.ln3,y46.ln3,y47.ln3,y48.ln3,y49.ln3)
  
  return(list(wei,gum,pe3,ln3))
  
}

# only to save it, so I dont have to redo it
par.80y.dif<-function(df,quantile,year){
  ys0<-seq(1,year*365+11,1)
  ys1<-seq(1,(year*365+10+1*365),1)
  ys2<-seq(1,(year*365+10+2*365),1)
  ys3<-seq(1,(year*365+10+3*365),1)
  ys4<-seq(1,(year*365+10+4*365),1)
  ys5<-seq(1,(year*365+10+5*365),1)
  ys6<-seq(1,(year*365+10+6*365),1)
  ys7<-seq(1,(year*365+10+7*365),1)
  ys8<-seq(1,(year*365+10+8*365),1)
  ys9<-seq(1,(year*365+10+9*365),1)
  ys10<-seq(1,(year*365+10+10*365),1)
  ys11<-seq(1,(year*365+10+11*365),1)
  ys12<-seq(1,(year*365+10+12*365),1)
  ys13<-seq(1,(year*365+10+13*365),1)
  ys14<-seq(1,(year*365+10+14*365),1)
  ys15<-seq(1,(year*365+10+15*365),1)
  ys16<-seq(1,(year*365+10+16*365),1)
  ys17<-seq(1,(year*365+10+17*365),1)
  ys18<-seq(1,(year*365+10+18*365),1)
  ys19<-seq(1,(year*365+10+19*365),1)
  ys20<-seq(1,(year*365+10+20*365),1)
  ys21<-seq(1,(year*365+10+21*365),1)
  ys22<-seq(1,(year*365+10+22*365),1)
  ys23<-seq(1,(year*365+10+23*365),1)
  ys24<-seq(1,(year*365+10+24*365),1)
  ys25<-seq(1,(year*365+10+25*365),1)
  ys26<-seq(1,(year*365+10+26*365),1)
  ys27<-seq(1,(year*365+10+27*365),1)
  ys28<-seq(1,(year*365+10+28*365),1)
  ys29<-seq(1,(year*365+10+29*365),1)
  ys30<-seq(1,(year*365+10+30*365),1)
  ys31<-seq(1,(year*365+10+31*365),1)
  ys32<-seq(1,(year*365+10+32*365),1)
  ys33<-seq(1,(year*365+10+33*365),1)
  ys34<-seq(1,(year*365+10+34*365),1)
  ys35<-seq(1,(year*365+10+35*365),1)
  ys36<-seq(1,(year*365+10+36*365),1)
  ys37<-seq(1,(year*365+10+37*365),1)
  ys38<-seq(1,(year*365+10+38*365),1)
  ys39<-seq(1,(year*365+10+39*365),1)
  ys40<-seq(1,(year*365+10+40*365),1)
  ys41<-seq(1,(year*365+10+41*365),1)
  ys42<-seq(1,(year*365+10+42*365),1)
  ys43<-seq(1,(year*365+10+43*365),1)
  ys44<-seq(1,(year*365+10+44*365),1)
  ys45<-seq(1,(year*365+10+45*365),1)
  ys46<-seq(1,(year*365+10+46*365),1)
  ys47<-seq(1,(year*365+10+47*365),1)
  ys48<-seq(1,(year*365+10+48*365),1)
  ys49<-seq(1,(year*365+10+49*365),1)
  ys50<-seq(1,(year*365+10+50*365),1)
  ys51<-seq(1,(year*365+10+51*365),1)
  ys52<-seq(1,(year*365+10+52*365),1)
  ys53<-seq(1,(year*365+10+53*365),1)
  ys54<-seq(1,(year*365+10+54*365),1)
  ys55<-seq(1,(year*365+10+55*365),1)
  ys56<-seq(1,(year*365+10+56*365),1)
  ys57<-seq(1,(year*365+10+57*365),1)
  ys58<-seq(1,(year*365+10+58*365),1)
  ys59<-seq(1,(year*365+10+59*365),1)
  ys60<-seq(1,(year*365+10+60*365),1)
  ys61<-seq(1,(year*365+10+61*365),1)
  ys62<-seq(1,(year*365+10+62*365),1)
  ys63<-seq(1,(year*365+10+63*365),1)
  ys64<-seq(1,(year*365+10+64*365),1)
  ys65<-seq(1,(year*365+10+65*365),1)
  ys66<-seq(1,(year*365+10+66*365),1)
  ys67<-seq(1,(year*365+10+67*365),1)
  ys68<-seq(1,(year*365+10+68*365),1)
  ys69<-seq(1,(year*365+10+69*365),1)
  ys70<-seq(1,(year*365+10+70*365),1)
  ys71<-seq(1,(year*365+10+71*365),1)
  ys72<-seq(1,(year*365+10+72*365),1)
  ys73<-seq(1,(year*365+10+73*365),1)
  ys74<-seq(1,(year*365+10+74*365),1)
  ys75<-seq(1,(year*365+10+75*365),1)
  ys76<-seq(1,(year*365+10+76*365),1)
  ys77<-seq(1,(year*365+10+77*365),1)
  ys78<-seq(1,(year*365+10+78*365),1)
  ys79<-seq(1,(year*365+10+79*365),1)
  ys80<-seq(1,(year*365+10+80*365),1)
  
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
  yy31<-df[[4]][ys31]
  yy32<-df[[4]][ys32]
  yy33<-df[[4]][ys33]
  yy34<-df[[4]][ys34]
  yy35<-df[[4]][ys35]
  yy36<-df[[4]][ys36]
  yy37<-df[[4]][ys37]
  yy38<-df[[4]][ys38]
  yy39<-df[[4]][ys39]
  yy40<-df[[4]][ys40]
  yy41<-df[[4]][ys41]
  yy42<-df[[4]][ys42]
  yy43<-df[[4]][ys43]
  yy44<-df[[4]][ys44]
  yy45<-df[[4]][ys45]
  yy46<-df[[4]][ys46]
  yy47<-df[[4]][ys47]
  yy48<-df[[4]][ys48]
  yy49<-df[[4]][ys49]
  yy50<-df[[4]][ys50]
  yy51<-df[[4]][ys51]
  yy52<-df[[4]][ys52]
  yy53<-df[[4]][ys53]
  yy54<-df[[4]][ys54]
  yy55<-df[[4]][ys55]
  yy56<-df[[4]][ys56]
  yy57<-df[[4]][ys57]
  yy58<-df[[4]][ys58]
  yy59<-df[[4]][ys59]
  yy60<-df[[4]][ys60]
  yy61<-df[[4]][ys61]
  yy62<-df[[4]][ys62]
  yy63<-df[[4]][ys63]
  yy64<-df[[4]][ys64]
  yy65<-df[[4]][ys65]
  yy66<-df[[4]][ys66]
  yy67<-df[[4]][ys67]
  yy68<-df[[4]][ys68]
  yy69<-df[[4]][ys69]
  yy70<-df[[4]][ys70]
  yy71<-df[[4]][ys71]
  yy72<-df[[4]][ys72]
  yy73<-df[[4]][ys73]
  yy74<-df[[4]][ys74]
  yy75<-df[[4]][ys75]
  yy76<-df[[4]][ys76]
  yy77<-df[[4]][ys77]
  yy78<-df[[4]][ys78]
  yy79<-df[[4]][ys79]
  yy80<-df[[4]][ys80]
  
  
  y0.wei<-quawei(f=quantile, para=pelwei(samlmu(yy0)))
  y0.gum<-quagum(f=quantile, para=pelgum(samlmu(yy0)))
  y0.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy0)))
  y0.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy0)))
  
  y1.wei<-quawei(f=quantile, para=pelwei(samlmu(yy1)))
  y1.gum<-quagum(f=quantile, para=pelgum(samlmu(yy1)))
  y1.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy1)))
  y1.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy1)))
  
  y2.wei<-quawei(f=quantile, para=pelwei(samlmu(yy2)))
  y2.gum<-quagum(f=quantile, para=pelgum(samlmu(yy2)))
  y2.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy2)))
  y2.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy2)))
  
  y3.wei<-quawei(f=quantile, para=pelwei(samlmu(yy3)))
  y3.gum<-quagum(f=quantile, para=pelgum(samlmu(yy3)))
  y3.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy3)))
  y3.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy3)))
  
  y4.wei<-quawei(f=quantile, para=pelwei(samlmu(yy4)))
  y4.gum<-quagum(f=quantile, para=pelgum(samlmu(yy4)))
  y4.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy4)))
  y4.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy4)))
  
  y5.wei<-quawei(f=quantile, para=pelwei(samlmu(yy5)))
  y5.gum<-quagum(f=quantile, para=pelgum(samlmu(yy5)))
  y5.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy5)))
  y5.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy5)))
  
  y6.wei<-quawei(f=quantile, para=pelwei(samlmu(yy6)))
  y6.gum<-quagum(f=quantile, para=pelgum(samlmu(yy6)))
  y6.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy6)))
  y6.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy6)))
  
  y7.wei<-quawei(f=quantile, para=pelwei(samlmu(yy7)))
  y7.gum<-quagum(f=quantile, para=pelgum(samlmu(yy7)))
  y7.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy7)))
  y7.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy7)))
  
  y8.wei<-quawei(f=quantile, para=pelwei(samlmu(yy8)))
  y8.gum<-quagum(f=quantile, para=pelgum(samlmu(yy8)))
  y8.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy8)))
  y8.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy8)))
  
  y9.wei<-quawei(f=quantile, para=pelwei(samlmu(yy9)))
  y9.gum<-quagum(f=quantile, para=pelgum(samlmu(yy9)))
  y9.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy9)))
  y9.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy9)))
  
  y10.wei<-quawei(f=quantile, para=pelwei(samlmu(yy10)))
  y10.gum<-quagum(f=quantile, para=pelgum(samlmu(yy10)))
  y10.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy10)))
  y10.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy10)))
  
  y11.wei<-quawei(f=quantile, para=pelwei(samlmu(yy11)))
  y11.gum<-quagum(f=quantile, para=pelgum(samlmu(yy11)))
  y11.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy11)))
  y11.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy11)))
  
  y12.wei<-quawei(f=quantile, para=pelwei(samlmu(yy12)))
  y12.gum<-quagum(f=quantile, para=pelgum(samlmu(yy12)))
  y12.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy12)))
  y12.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy12)))
  
  y13.wei<-quawei(f=quantile, para=pelwei(samlmu(yy13)))
  y13.gum<-quagum(f=quantile, para=pelgum(samlmu(yy13)))
  y13.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy13)))
  y13.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy13)))
  
  y14.wei<-quawei(f=quantile, para=pelwei(samlmu(yy14)))
  y14.gum<-quagum(f=quantile, para=pelgum(samlmu(yy14)))
  y14.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy14)))
  y14.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy14)))
  
  y15.wei<-quawei(f=quantile, para=pelwei(samlmu(yy15)))
  y15.gum<-quagum(f=quantile, para=pelgum(samlmu(yy15)))
  y15.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy15)))
  y15.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy15)))
  
  y16.wei<-quawei(f=quantile, para=pelwei(samlmu(yy16)))
  y16.gum<-quagum(f=quantile, para=pelgum(samlmu(yy16)))
  y16.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy16)))
  y16.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy16)))
  
  y17.wei<-quawei(f=quantile, para=pelwei(samlmu(yy17)))
  y17.gum<-quagum(f=quantile, para=pelgum(samlmu(yy17)))
  y17.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy17)))
  y17.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy17)))
  
  y18.wei<-quawei(f=quantile, para=pelwei(samlmu(yy18)))
  y18.gum<-quagum(f=quantile, para=pelgum(samlmu(yy18)))
  y18.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy18)))
  y18.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy18)))
  
  y19.wei<-quawei(f=quantile, para=pelwei(samlmu(yy19)))
  y19.gum<-quagum(f=quantile, para=pelgum(samlmu(yy19)))
  y19.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy19)))
  y19.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy19)))
  
  y20.wei<-quawei(f=quantile, para=pelwei(samlmu(yy20)))
  y20.gum<-quagum(f=quantile, para=pelgum(samlmu(yy20)))
  y20.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy20)))
  y20.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy20)))
  
  y21.wei<-quawei(f=quantile, para=pelwei(samlmu(yy21)))
  y21.gum<-quagum(f=quantile, para=pelgum(samlmu(yy21)))
  y21.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy21)))
  y21.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy21)))
  
  y22.wei<-quawei(f=quantile, para=pelwei(samlmu(yy22)))
  y22.gum<-quagum(f=quantile, para=pelgum(samlmu(yy22)))
  y22.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy22)))
  y22.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy22)))
  
  y23.wei<-quawei(f=quantile, para=pelwei(samlmu(yy23)))
  y23.gum<-quagum(f=quantile, para=pelgum(samlmu(yy23)))
  y23.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy23)))
  y23.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy23)))
  
  y24.wei<-quawei(f=quantile, para=pelwei(samlmu(yy24)))
  y24.gum<-quagum(f=quantile, para=pelgum(samlmu(yy24)))
  y24.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy24)))
  y24.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy24)))
  
  y25.wei<-quawei(f=quantile, para=pelwei(samlmu(yy25)))
  y25.gum<-quagum(f=quantile, para=pelgum(samlmu(yy25)))
  y25.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy25)))
  y25.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy25)))
  
  y26.wei<-quawei(f=quantile, para=pelwei(samlmu(yy26)))
  y26.gum<-quagum(f=quantile, para=pelgum(samlmu(yy26)))
  y26.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy26)))
  y26.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy26)))
  
  y27.wei<-quawei(f=quantile, para=pelwei(samlmu(yy27)))
  y27.gum<-quagum(f=quantile, para=pelgum(samlmu(yy27)))
  y27.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy27)))
  y27.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy27)))
  
  y28.wei<-quawei(f=quantile, para=pelwei(samlmu(yy28)))
  y28.gum<-quagum(f=quantile, para=pelgum(samlmu(yy28)))
  y28.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy28)))
  y28.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy28)))
  
  y29.wei<-quawei(f=quantile, para=pelwei(samlmu(yy29)))
  y29.gum<-quagum(f=quantile, para=pelgum(samlmu(yy29)))
  y29.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy29)))
  y29.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy29)))
  
  y30.wei<-quawei(f=quantile, para=pelwei(samlmu(yy30)))
  y30.gum<-quagum(f=quantile, para=pelgum(samlmu(yy30)))
  y30.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy30)))
  y30.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy30)))
  
  y31.wei<-quawei(f=quantile, para=pelwei(samlmu(yy31)))
  y31.gum<-quagum(f=quantile, para=pelgum(samlmu(yy31)))
  y31.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy31)))
  y31.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy31)))
  
  y32.wei<-quawei(f=quantile, para=pelwei(samlmu(yy32)))
  y32.gum<-quagum(f=quantile, para=pelgum(samlmu(yy32)))
  y32.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy32)))
  y32.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy32)))
  
  y33.wei<-quawei(f=quantile, para=pelwei(samlmu(yy33)))
  y33.gum<-quagum(f=quantile, para=pelgum(samlmu(yy33)))
  y33.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy33)))
  y33.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy33)))
  
  y34.wei<-quawei(f=quantile, para=pelwei(samlmu(yy34)))
  y34.gum<-quagum(f=quantile, para=pelgum(samlmu(yy34)))
  y34.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy34)))
  y34.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy34)))
  
  y35.wei<-quawei(f=quantile, para=pelwei(samlmu(yy35)))
  y35.gum<-quagum(f=quantile, para=pelgum(samlmu(yy35)))
  y35.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy35)))
  y35.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy35)))
  
  y36.wei<-quawei(f=quantile, para=pelwei(samlmu(yy36)))
  y36.gum<-quagum(f=quantile, para=pelgum(samlmu(yy36)))
  y36.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy36)))
  y36.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy36)))
  
  y37.wei<-quawei(f=quantile, para=pelwei(samlmu(yy37)))
  y37.gum<-quagum(f=quantile, para=pelgum(samlmu(yy37)))
  y37.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy37)))
  y37.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy37)))
  
  y38.wei<-quawei(f=quantile, para=pelwei(samlmu(yy38)))
  y38.gum<-quagum(f=quantile, para=pelgum(samlmu(yy38)))
  y38.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy38)))
  y38.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy38)))
  
  y39.wei<-quawei(f=quantile, para=pelwei(samlmu(yy39)))
  y39.gum<-quagum(f=quantile, para=pelgum(samlmu(yy39)))
  y39.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy39)))
  y39.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy39)))
  
  y40.wei<-quawei(f=quantile, para=pelwei(samlmu(yy40)))
  y40.gum<-quagum(f=quantile, para=pelgum(samlmu(yy40)))
  y40.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy40)))
  y40.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy40)))
  
  y41.wei<-quawei(f=quantile, para=pelwei(samlmu(yy41)))
  y41.gum<-quagum(f=quantile, para=pelgum(samlmu(yy41)))
  y41.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy41)))
  y41.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy41)))
  
  y42.wei<-quawei(f=quantile, para=pelwei(samlmu(yy42)))
  y42.gum<-quagum(f=quantile, para=pelgum(samlmu(yy42)))
  y42.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy42)))
  y42.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy42)))
  
  y43.wei<-quawei(f=quantile, para=pelwei(samlmu(yy43)))
  y43.gum<-quagum(f=quantile, para=pelgum(samlmu(yy43)))
  y43.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy43)))
  y43.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy43)))
  
  y44.wei<-quawei(f=quantile, para=pelwei(samlmu(yy44)))
  y44.gum<-quagum(f=quantile, para=pelgum(samlmu(yy44)))
  y44.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy44)))
  y44.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy44)))
  
  y45.wei<-quawei(f=quantile, para=pelwei(samlmu(yy45)))
  y45.gum<-quagum(f=quantile, para=pelgum(samlmu(yy45)))
  y45.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy45)))
  y45.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy45)))
  
  y46.wei<-quawei(f=quantile, para=pelwei(samlmu(yy46)))
  y46.gum<-quagum(f=quantile, para=pelgum(samlmu(yy46)))
  y46.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy46)))
  y46.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy46)))
  
  y47.wei<-quawei(f=quantile, para=pelwei(samlmu(yy47)))
  y47.gum<-quagum(f=quantile, para=pelgum(samlmu(yy47)))
  y47.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy47)))
  y47.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy47)))
  
  y48.wei<-quawei(f=quantile, para=pelwei(samlmu(yy48)))
  y48.gum<-quagum(f=quantile, para=pelgum(samlmu(yy48)))
  y48.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy48)))
  y48.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy48)))
  
  y49.wei<-quawei(f=quantile, para=pelwei(samlmu(yy49)))
  y49.gum<-quagum(f=quantile, para=pelgum(samlmu(yy49)))
  y49.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy49)))
  y49.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy49)))
  
  y50.wei<-quawei(f=quantile, para=pelwei(samlmu(yy50)))
  y50.gum<-quagum(f=quantile, para=pelgum(samlmu(yy50)))
  y50.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy50)))
  y50.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy50)))
  
  y51.wei<-quawei(f=quantile, para=pelwei(samlmu(yy51)))
  y51.gum<-quagum(f=quantile, para=pelgum(samlmu(yy51)))
  y51.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy51)))
  y51.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy51)))
  
  y52.wei<-quawei(f=quantile, para=pelwei(samlmu(yy52)))
  y52.gum<-quagum(f=quantile, para=pelgum(samlmu(yy52)))
  y52.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy52)))
  y52.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy52)))
  
  y53.wei<-quawei(f=quantile, para=pelwei(samlmu(yy53)))
  y53.gum<-quagum(f=quantile, para=pelgum(samlmu(yy53)))
  y53.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy53)))
  y53.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy53)))
  
  y54.wei<-quawei(f=quantile, para=pelwei(samlmu(yy54)))
  y54.gum<-quagum(f=quantile, para=pelgum(samlmu(yy54)))
  y54.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy54)))
  y54.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy54)))
  
  y55.wei<-quawei(f=quantile, para=pelwei(samlmu(yy55)))
  y55.gum<-quagum(f=quantile, para=pelgum(samlmu(yy55)))
  y55.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy55)))
  y55.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy55)))
  
  y56.wei<-quawei(f=quantile, para=pelwei(samlmu(yy56)))
  y56.gum<-quagum(f=quantile, para=pelgum(samlmu(yy56)))
  y56.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy56)))
  y56.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy56)))
  
  y57.wei<-quawei(f=quantile, para=pelwei(samlmu(yy57)))
  y57.gum<-quagum(f=quantile, para=pelgum(samlmu(yy57)))
  y57.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy57)))
  y57.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy57)))
  
  y58.wei<-quawei(f=quantile, para=pelwei(samlmu(yy58)))
  y58.gum<-quagum(f=quantile, para=pelgum(samlmu(yy58)))
  y58.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy58)))
  y58.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy58)))
  
  y59.wei<-quawei(f=quantile, para=pelwei(samlmu(yy59)))
  y59.gum<-quagum(f=quantile, para=pelgum(samlmu(yy59)))
  y59.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy59)))
  y59.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy59)))
  
  y60.wei<-quawei(f=quantile, para=pelwei(samlmu(yy60)))
  y60.gum<-quagum(f=quantile, para=pelgum(samlmu(yy60)))
  y60.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy60)))
  y60.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy60)))
  
  y61.wei<-quawei(f=quantile, para=pelwei(samlmu(yy61)))
  y61.gum<-quagum(f=quantile, para=pelgum(samlmu(yy61)))
  y61.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy61)))
  y61.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy61)))
  
  y62.wei<-quawei(f=quantile, para=pelwei(samlmu(yy62)))
  y62.gum<-quagum(f=quantile, para=pelgum(samlmu(yy62)))
  y62.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy62)))
  y62.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy62)))
  
  y63.wei<-quawei(f=quantile, para=pelwei(samlmu(yy63)))
  y63.gum<-quagum(f=quantile, para=pelgum(samlmu(yy63)))
  y63.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy63)))
  y63.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy63)))
  
  y64.wei<-quawei(f=quantile, para=pelwei(samlmu(yy64)))
  y64.gum<-quagum(f=quantile, para=pelgum(samlmu(yy64)))
  y64.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy64)))
  y64.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy64)))
  
  y65.wei<-quawei(f=quantile, para=pelwei(samlmu(yy65)))
  y65.gum<-quagum(f=quantile, para=pelgum(samlmu(yy65)))
  y65.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy65)))
  y65.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy65)))
  
  y66.wei<-quawei(f=quantile, para=pelwei(samlmu(yy66)))
  y66.gum<-quagum(f=quantile, para=pelgum(samlmu(yy66)))
  y66.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy66)))
  y66.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy66)))
  
  y67.wei<-quawei(f=quantile, para=pelwei(samlmu(yy67)))
  y67.gum<-quagum(f=quantile, para=pelgum(samlmu(yy67)))
  y67.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy67)))
  y67.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy67)))
  
  y68.wei<-quawei(f=quantile, para=pelwei(samlmu(yy68)))
  y68.gum<-quagum(f=quantile, para=pelgum(samlmu(yy68)))
  y68.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy68)))
  y68.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy68)))
  
  y69.wei<-quawei(f=quantile, para=pelwei(samlmu(yy69)))
  y69.gum<-quagum(f=quantile, para=pelgum(samlmu(yy69)))
  y69.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy69)))
  y69.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy69)))
  
  y70.wei<-quawei(f=quantile, para=pelwei(samlmu(yy70)))
  y70.gum<-quagum(f=quantile, para=pelgum(samlmu(yy70)))
  y70.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy70)))
  y70.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy70)))
  
  y71.wei<-quawei(f=quantile, para=pelwei(samlmu(yy71)))
  y71.gum<-quagum(f=quantile, para=pelgum(samlmu(yy71)))
  y71.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy71)))
  y71.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy71)))
  
  y72.wei<-quawei(f=quantile, para=pelwei(samlmu(yy72)))
  y72.gum<-quagum(f=quantile, para=pelgum(samlmu(yy72)))
  y72.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy72)))
  y72.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy72)))
  
  y73.wei<-quawei(f=quantile, para=pelwei(samlmu(yy73)))
  y73.gum<-quagum(f=quantile, para=pelgum(samlmu(yy73)))
  y73.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy73)))
  y73.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy73)))
  
  y74.wei<-quawei(f=quantile, para=pelwei(samlmu(yy74)))
  y74.gum<-quagum(f=quantile, para=pelgum(samlmu(yy74)))
  y74.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy74)))
  y74.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy74)))
  
  y75.wei<-quawei(f=quantile, para=pelwei(samlmu(yy75)))
  y75.gum<-quagum(f=quantile, para=pelgum(samlmu(yy75)))
  y75.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy75)))
  y75.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy75)))
  
  y76.wei<-quawei(f=quantile, para=pelwei(samlmu(yy76)))
  y76.gum<-quagum(f=quantile, para=pelgum(samlmu(yy76)))
  y76.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy76)))
  y76.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy76)))
  
  y77.wei<-quawei(f=quantile, para=pelwei(samlmu(yy77)))
  y77.gum<-quagum(f=quantile, para=pelgum(samlmu(yy77)))
  y77.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy77)))
  y77.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy77)))
  
  y78.wei<-quawei(f=quantile, para=pelwei(samlmu(yy78)))
  y78.gum<-quagum(f=quantile, para=pelgum(samlmu(yy78)))
  y78.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy78)))
  y78.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy78)))
  
  y79.wei<-quawei(f=quantile, para=pelwei(samlmu(yy79)))
  y79.gum<-quagum(f=quantile, para=pelgum(samlmu(yy79)))
  y79.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy79)))
  y79.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy79)))
  
  y80.wei<-quawei(f=quantile, para=pelwei(samlmu(yy80)))
  y80.gum<-quagum(f=quantile, para=pelgum(samlmu(yy80)))
  y80.pe3<-quape3(f=quantile, para=pelpe3(samlmu(yy80)))
  y80.ln3<-qualn3(f=quantile, para=pelln3(samlmu(yy80)))
  
  
  
  wei<-cbind(y0.wei,y1.wei,y2.wei,y3.wei,y4.wei,y5.wei,y6.wei,y7.wei,y8.wei,y9.wei,y10.wei,y11.wei,y12.wei,y13.wei,y14.wei,y15.wei,y16.wei,y17.wei,y18.wei,y19.wei,y20.wei,y21.wei,y22.wei,y23.wei,y24.wei,y25.wei,y26.wei,y27.wei,y28.wei,y29.wei,y30.wei,y31.wei,y32.wei,y33.wei,y34.wei,y35.wei,y36.wei,y37.wei,y38.wei,y39.wei,y40.wei,y41.wei,y42.wei,y43.wei,y44.wei,y45.wei,y46.wei,y47.wei,y48.wei,y49.wei,y50.wei,y51.wei,y52.wei,y53.wei,y54.wei,y55.wei,y56.wei,y57.wei,y58.wei,y59.wei,y60.wei,y61.wei,y62.wei,y63.wei,y64.wei,y65.wei,y66.wei,y67.wei,y68.wei,y69.wei,y70.wei,y71.wei,y72.wei,y73.wei,y74.wei,y75.wei,y76.wei,y77.wei,y78.wei,y79.wei,y80.wei)
  gum<-cbind(y0.gum,y1.gum,y2.gum,y3.gum,y4.gum,y5.gum,y6.gum,y7.gum,y8.gum,y9.gum,y10.gum,y11.gum,y12.gum,y13.gum,y14.gum,y15.gum,y16.gum,y17.gum,y18.gum,y19.gum,y20.gum,y21.gum,y22.gum,y23.gum,y24.gum,y25.gum,y26.gum,y27.gum,y28.gum,y29.gum,y30.gum,y31.gum,y32.gum,y33.gum,y34.gum,y35.gum,y36.gum,y37.gum,y38.gum,y39.gum,y40.gum,y41.gum,y42.gum,y43.gum,y44.gum,y45.gum,y46.gum,y47.gum,y48.gum,y49.gum,y50.gum,y51.gum,y52.gum,y53.gum,y54.gum,y55.gum,y56.gum,y57.gum,y58.gum,y59.gum,y60.gum,y61.gum,y62.gum,y63.gum,y64.gum,y65.gum,y66.gum,y67.gum,y68.gum,y69.gum,y70.gum,y71.gum,y72.gum,y73.gum,y74.gum,y75.gum,y76.gum,y77.gum,y78.gum,y79.gum,y80.gum)
  pe3<-cbind(y0.pe3,y1.pe3,y2.pe3,y3.pe3,y4.pe3,y5.pe3,y6.pe3,y7.pe3,y8.pe3,y9.pe3,y10.pe3,y11.pe3,y12.pe3,y13.pe3,y14.pe3,y15.pe3,y16.pe3,y17.pe3,y18.pe3,y19.pe3,y20.pe3,y21.pe3,y22.pe3,y23.pe3,y24.pe3,y25.pe3,y26.pe3,y27.pe3,y28.pe3,y29.pe3,y30.pe3,y31.pe3,y32.pe3,y33.pe3,y34.pe3,y35.pe3,y36.pe3,y37.pe3,y38.pe3,y39.pe3,y40.pe3,y41.pe3,y42.pe3,y43.pe3,y44.pe3,y45.pe3,y46.pe3,y47.pe3,y48.pe3,y49.pe3,y50.pe3,y51.pe3,y52.pe3,y53.pe3,y54.pe3,y55.pe3,y56.pe3,y57.pe3,y58.pe3,y59.pe3,y60.pe3,y61.pe3,y62.pe3,y63.pe3,y64.pe3,y65.pe3,y66.pe3,y67.pe3,y68.pe3,y69.pe3,y70.pe3,y71.pe3,y72.pe3,y73.pe3,y74.pe3,y75.pe3,y76.pe3,y77.pe3,y78.pe3,y79.pe3,y80.pe3)
  ln3<-cbind(y0.ln3,y1.ln3,y2.ln3,y3.ln3,y4.ln3,y5.ln3,y6.ln3,y7.ln3,y8.ln3,y9.ln3,y10.ln3,y11.ln3,y12.ln3,y13.ln3,y14.ln3,y15.ln3,y16.ln3,y17.ln3,y18.ln3,y19.ln3,y20.ln3,y21.ln3,y22.ln3,y23.ln3,y24.ln3,y25.ln3,y26.ln3,y27.ln3,y28.ln3,y29.ln3,y30.ln3,y31.ln3,y32.ln3,y33.ln3,y34.ln3,y35.ln3,y36.ln3,y37.ln3,y38.ln3,y39.ln3,y40.ln3,y41.ln3,y42.ln3,y43.ln3,y44.ln3,y45.ln3,y46.ln3,y47.ln3,y48.ln3,y49.ln3,y50.ln3,y51.ln3,y52.ln3,y53.ln3,y54.ln3,y55.ln3,y56.ln3,y57.ln3,y58.ln3,y59.ln3,y60.ln3,y61.ln3,y62.ln3,y63.ln3,y64.ln3,y65.ln3,y66.ln3,y67.ln3,y68.ln3,y69.ln3,y70.ln3,y71.ln3,y72.ln3,y73.ln3,y74.ln3,y75.ln3,y76.ln3,y77.ln3,y78.ln3,y79.ln3,y80.ln3)
  
  return(list(wei,gum,pe3,ln3))
  
}

###### Plot für lmom
ploti<-function(dfp,dfr,names,quantile1,on,years){
  if (on=="ON"){
  ye<-quant(dfr,year=years,qu=quantile1)
  df<-param(dfp,quantile=quantile1,year=years)
  ylim.max<-max(cbind(df[[1]],df[[2]],df[[3]],df[[4]],ye$yy1))*1.1
  ylim.min<-min(cbind(df[[1]],df[[2]],df[[3]],df[[4]],ye$yy1))
  wei<-as.numeric(df[[1]])
  gum<-as.numeric(df[[2]])
  pe3<-as.numeric(df[[3]])
  ln3<-as.numeric(df[[4]])
  plot(wei,type="l",ylim=c(ylim.min,ylim.max),lty=2,col=1,xlab = "Year-Shift",
       ylab="Discharge",main=paste("lmom Quantiles (",quantile1,")",years,"yrs"),lwd=2)
  legend("topright",inset=.005,title="Functions",c("Wei","Gum","Pe3","Ln3"),fill=c(1:4),horiz=T)
  lines(gum,lty=3,col=2,lwd=2)
  lines(pe3,lty=4,col=3,lwd=2)
  lines(ln3,lty=5,col=4,lwd=2)
  points(ye$yy1:30,cex=1,pch=8,col=2)
  abline(h = seq(1,10000,100),v=c(1:30),lwd=0.5)
  mtext(names, side=3, line=3,las=0, col="seagreen")
    
  } else { if (on=="OFF"){
    df<-param(dfp,quantile=quantile1,year=years)
    ylim.max<-max(cbind(df[[1]],df[[2]],df[[3]],df[[4]]))*1.1
    ylim.min<-min(cbind(df[[1]],df[[2]],df[[3]],df[[4]]))
    wei<-as.numeric(df[[1]])
    gum<-as.numeric(df[[2]])
    pe3<-as.numeric(df[[3]])
    ln3<-as.numeric(df[[4]])
    plot(wei,type="l",ylim=c(ylim.min,ylim.max),lty=2,col=1,xlab = "Year-Shift",
         ylab="Discharge",main=paste("lmom Quantiles (",quantile1,")",years,"yrs"),lwd=2)
    legend("topright",inset=.005,title="Functions",c("Wei","Gum","Pe3","Ln3"),fill=c(1:4),horiz=T)
    lines(gum,lty=3,col=2,lwd=2)
    lines(pe3,lty=4,col=3,lwd=2)
    lines(ln3,lty=5,col=4,lwd=2)
    abline(h = seq(1,10000,100),v=c(1:30),lwd=0.5)
    mtext(names, side=3, line=3,las=0, col="seagreen")  
  }
  }
}
ploti.80<-function(dfp,dfr,names,quantile1,years){
    df<-par.80y(dfp,quantile=quantile1,year=years)
    ylim.max<-max(cbind(df[[1]],df[[2]],df[[3]],df[[4]]))*1.1
    ylim.min<-min(cbind(df[[1]],df[[2]],df[[3]],df[[4]]))
    wei<-as.numeric(df[[1]])
    gum<-as.numeric(df[[2]])
    pe3<-as.numeric(df[[3]])
    ln3<-as.numeric(df[[4]])
    plot(wei,type="l",ylim=c(ylim.min,ylim.max),lty=2,col=1,xlab = "Year-Expand",
         ylab="Discharge",main=paste("lmom Quantiles (",quantile1,")",years,"yrs"),lwd=2)
    legend("topright",inset=.005,title="Functions",c("Wei","Gum","Pe3","Ln3"),fill=c(1:4),horiz=T)
    lines(gum,lty=3,col=2,lwd=2)
    lines(pe3,lty=4,col=3,lwd=2)
    lines(ln3,lty=5,col=4,lwd=2)
    abline(h = seq(1,ylim.max,(ylim.max/100)),v=c(1:81),lwd=0.5)
    mtext(names, side=3, line=3,las=0, col="seagreen")  
}

### Sequenzen für 30 Jahre über einen data Frame, Jahre können eingefügt werden - 
### über yyall$yy[0-30]; data.frame listko[1][[1]] einlesen, dann wird der [[4]] (data) gelesen
seq30y<-function(df,year){
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
  
  
  return(data.frame(yy0,yy1,yy2,yy3,yy4,yy5,yy6,yy7,yy8,yy9,yy10,yy11,yy12,yy13,yy14,yy15,yy16,yy17,yy18,yy19,yy20,yy21,yy22,yy23,yy24,yy25,yy26,yy27,yy28,yy29,yy30))
}
quant<-function(df,year,qu){
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
  yy0<-quantile(df[[4]][ys0],c(qu))
  yy1<-quantile(df[[4]][ys1],c(qu))
  yy2<-quantile(df[[4]][ys2],c(qu))
  yy3<-quantile(df[[4]][ys3],c(qu))
  yy4<-quantile(df[[4]][ys4],c(qu))
  yy5<-quantile(df[[4]][ys5],c(qu))
  yy6<-quantile(df[[4]][ys6],c(qu))
  yy7<-quantile(df[[4]][ys7],c(qu))
  yy8<-quantile(df[[4]][ys8],c(qu))
  yy9<-quantile(df[[4]][ys9],c(qu))
  yy10<-quantile(df[[4]][ys10],c(qu))
  yy11<-quantile(df[[4]][ys11],c(qu))
  yy12<-quantile(df[[4]][ys12],c(qu))
  yy13<-quantile(df[[4]][ys13],c(qu))
  yy14<-quantile(df[[4]][ys14],c(qu))
  yy15<-quantile(df[[4]][ys15],c(qu))
  yy16<-quantile(df[[4]][ys16],c(qu))
  yy17<-quantile(df[[4]][ys17],c(qu))
  yy18<-quantile(df[[4]][ys18],c(qu))
  yy19<-quantile(df[[4]][ys19],c(qu))
  yy20<-quantile(df[[4]][ys20],c(qu))
  yy21<-quantile(df[[4]][ys21],c(qu))
  yy22<-quantile(df[[4]][ys22],c(qu))
  yy23<-quantile(df[[4]][ys23],c(qu))
  yy24<-quantile(df[[4]][ys24],c(qu))
  yy25<-quantile(df[[4]][ys25],c(qu))
  yy26<-quantile(df[[4]][ys26],c(qu))
  yy27<-quantile(df[[4]][ys27],c(qu))
  yy28<-quantile(df[[4]][ys28],c(qu))
  yy29<-quantile(df[[4]][ys29],c(qu))
  yy30<-quantile(df[[4]][ys30],c(qu))
  
  #return(c(yy0,yy1,yy2,yy3,yy4,yy5,yy6,yy7,yy8,yy9,yy10,yy11,yy12,yy13,yy14,yy15,yy16,yy17,yy18,yy19,yy20,yy21,yy22,yy23,yy24,yy25,yy26,yy27,yy28,yy29,yy30))
  return(data.frame(yy0,yy1,yy2,yy3,yy4,yy5,yy6,yy7,yy8,yy9,yy10,yy11,yy12,yy13,yy14,yy15,yy16,yy17,yy18,yy19,yy20,yy21,yy22,yy23,yy24,yy25,yy26,yy27,yy28,yy29,yy30))
}
quantc<-function(df,year,qu){
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
  yy0<-quantile(df[[4]][ys0],c(qu))
  yy1<-quantile(df[[4]][ys1],c(qu))
  yy2<-quantile(df[[4]][ys2],c(qu))
  yy3<-quantile(df[[4]][ys3],c(qu))
  yy4<-quantile(df[[4]][ys4],c(qu))
  yy5<-quantile(df[[4]][ys5],c(qu))
  yy6<-quantile(df[[4]][ys6],c(qu))
  yy7<-quantile(df[[4]][ys7],c(qu))
  yy8<-quantile(df[[4]][ys8],c(qu))
  yy9<-quantile(df[[4]][ys9],c(qu))
  yy10<-quantile(df[[4]][ys10],c(qu))
  yy11<-quantile(df[[4]][ys11],c(qu))
  yy12<-quantile(df[[4]][ys12],c(qu))
  yy13<-quantile(df[[4]][ys13],c(qu))
  yy14<-quantile(df[[4]][ys14],c(qu))
  yy15<-quantile(df[[4]][ys15],c(qu))
  yy16<-quantile(df[[4]][ys16],c(qu))
  yy17<-quantile(df[[4]][ys17],c(qu))
  yy18<-quantile(df[[4]][ys18],c(qu))
  yy19<-quantile(df[[4]][ys19],c(qu))
  yy20<-quantile(df[[4]][ys20],c(qu))
  yy21<-quantile(df[[4]][ys21],c(qu))
  yy22<-quantile(df[[4]][ys22],c(qu))
  yy23<-quantile(df[[4]][ys23],c(qu))
  yy24<-quantile(df[[4]][ys24],c(qu))
  yy25<-quantile(df[[4]][ys25],c(qu))
  yy26<-quantile(df[[4]][ys26],c(qu))
  yy27<-quantile(df[[4]][ys27],c(qu))
  yy28<-quantile(df[[4]][ys28],c(qu))
  yy29<-quantile(df[[4]][ys29],c(qu))
  yy30<-quantile(df[[4]][ys30],c(qu))
  
  return(c(yy0,yy1,yy2,yy3,yy4,yy5,yy6,yy7,yy8,yy9,yy10,yy11,yy12,yy13,yy14,yy15,yy16,yy17,yy18,yy19,yy20,yy21,yy22,yy23,yy24,yy25,yy26,yy27,yy28,yy29,yy30))
  #return(data.frame(yy0,yy1,yy2,yy3,yy4,yy5,yy6,yy7,yy8,yy9,yy10,yy11,yy12,yy13,yy14,yy15,yy16,yy17,yy18,yy19,yy20,yy21,yy22,yy23,yy24,yy25,yy26,yy27,yy28,yy29,yy30))
}
# quantc und qunt unterscheiden sich bezüglich data.frame ausgabe (quant) und c() ausgabe
ye<-quant(listko[2][[1]],10,qu=0.9)
ye$yy20

#### Sequenzen von 1 bis 80 Jahre plus parameter für lmom
seq80y<-function(df,year){
  ys0<-seq(1,year*365+11,1)
  ys1<-seq(1,(year*365+10+1*365),1)
  ys2<-seq(1,(year*365+10+2*365),1)
  ys3<-seq(1,(year*365+10+3*365),1)
  ys4<-seq(1,(year*365+10+4*365),1)
  ys5<-seq(1,(year*365+10+5*365),1)
  ys6<-seq(1,(year*365+10+6*365),1)
  ys7<-seq(1,(year*365+10+7*365),1)
  ys8<-seq(1,(year*365+10+8*365),1)
  ys9<-seq(1,(year*365+10+9*365),1)
  ys10<-seq(1,(year*365+10+10*365),1)
  ys11<-seq(1,(year*365+10+11*365),1)
  ys12<-seq(1,(year*365+10+12*365),1)
  ys13<-seq(1,(year*365+10+13*365),1)
  ys14<-seq(1,(year*365+10+14*365),1)
  ys15<-seq(1,(year*365+10+15*365),1)
  ys16<-seq(1,(year*365+10+16*365),1)
  ys17<-seq(1,(year*365+10+17*365),1)
  ys18<-seq(1,(year*365+10+18*365),1)
  ys19<-seq(1,(year*365+10+19*365),1)
  ys20<-seq(1,(year*365+10+20*365),1)
  ys21<-seq(1,(year*365+10+21*365),1)
  ys22<-seq(1,(year*365+10+22*365),1)
  ys23<-seq(1,(year*365+10+23*365),1)
  ys24<-seq(1,(year*365+10+24*365),1)
  ys25<-seq(1,(year*365+10+25*365),1)
  ys26<-seq(1,(year*365+10+26*365),1)
  ys27<-seq(1,(year*365+10+27*365),1)
  ys28<-seq(1,(year*365+10+28*365),1)
  ys29<-seq(1,(year*365+10+29*365),1)
  ys30<-seq(1,(year*365+10+30*365),1)
  ys31<-seq(1,(year*365+10+31*365),1)
  ys32<-seq(1,(year*365+10+32*365),1)
  ys33<-seq(1,(year*365+10+33*365),1)
  ys34<-seq(1,(year*365+10+34*365),1)
  ys35<-seq(1,(year*365+10+35*365),1)
  ys36<-seq(1,(year*365+10+36*365),1)
  ys37<-seq(1,(year*365+10+37*365),1)
  ys38<-seq(1,(year*365+10+38*365),1)
  ys39<-seq(1,(year*365+10+39*365),1)
  ys40<-seq(1,(year*365+10+40*365),1)
  ys41<-seq(1,(year*365+10+41*365),1)
  ys42<-seq(1,(year*365+10+42*365),1)
  ys43<-seq(1,(year*365+10+43*365),1)
  ys44<-seq(1,(year*365+10+44*365),1)
  ys45<-seq(1,(year*365+10+45*365),1)
  ys46<-seq(1,(year*365+10+46*365),1)
  ys47<-seq(1,(year*365+10+47*365),1)
  ys48<-seq(1,(year*365+10+48*365),1)
  ys49<-seq(1,(year*365+10+49*365),1)
  ys50<-seq(1,(year*365+10+50*365),1)
  ys51<-seq(1,(year*365+10+51*365),1)
  ys52<-seq(1,(year*365+10+52*365),1)
  ys53<-seq(1,(year*365+10+53*365),1)
  ys54<-seq(1,(year*365+10+54*365),1)
  ys55<-seq(1,(year*365+10+55*365),1)
  ys56<-seq(1,(year*365+10+56*365),1)
  ys57<-seq(1,(year*365+10+57*365),1)
  ys58<-seq(1,(year*365+10+58*365),1)
  ys59<-seq(1,(year*365+10+59*365),1)
  ys60<-seq(1,(year*365+10+60*365),1)
  ys61<-seq(1,(year*365+10+61*365),1)
  ys62<-seq(1,(year*365+10+62*365),1)
  ys63<-seq(1,(year*365+10+63*365),1)
  ys64<-seq(1,(year*365+10+64*365),1)
  ys65<-seq(1,(year*365+10+65*365),1)
  ys66<-seq(1,(year*365+10+66*365),1)
  ys67<-seq(1,(year*365+10+67*365),1)
  ys68<-seq(1,(year*365+10+68*365),1)
  ys69<-seq(1,(year*365+10+69*365),1)
  ys70<-seq(1,(year*365+10+70*365),1)
  ys71<-seq(1,(year*365+10+71*365),1)
  ys72<-seq(1,(year*365+10+72*365),1)
  ys73<-seq(1,(year*365+10+73*365),1)
  ys74<-seq(1,(year*365+10+74*365),1)
  ys75<-seq(1,(year*365+10+75*365),1)
  ys76<-seq(1,(year*365+10+76*365),1)
  ys77<-seq(1,(year*365+10+77*365),1)
  ys78<-seq(1,(year*365+10+78*365),1)
  ys79<-seq(1,(year*365+10+79*365),1)
  ys80<-seq(1,(year*365+10+80*365),1)
  
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
  yy31<-df[[4]][ys31]
  yy32<-df[[4]][ys32]
  yy33<-df[[4]][ys33]
  yy34<-df[[4]][ys34]
  yy35<-df[[4]][ys35]
  yy36<-df[[4]][ys36]
  yy37<-df[[4]][ys37]
  yy38<-df[[4]][ys38]
  yy39<-df[[4]][ys39]
  yy40<-df[[4]][ys40]
  yy41<-df[[4]][ys41]
  yy42<-df[[4]][ys42]
  yy43<-df[[4]][ys43]
  yy44<-df[[4]][ys44]
  yy45<-df[[4]][ys45]
  yy46<-df[[4]][ys46]
  yy47<-df[[4]][ys47]
  yy48<-df[[4]][ys48]
  yy49<-df[[4]][ys49]
  yy50<-df[[4]][ys50]
  yy51<-df[[4]][ys51]
  yy52<-df[[4]][ys52]
  yy53<-df[[4]][ys53]
  yy54<-df[[4]][ys54]
  yy55<-df[[4]][ys55]
  yy56<-df[[4]][ys56]
  yy57<-df[[4]][ys57]
  yy58<-df[[4]][ys58]
  yy59<-df[[4]][ys59]
  yy60<-df[[4]][ys60]
  yy61<-df[[4]][ys61]
  yy62<-df[[4]][ys62]
  yy63<-df[[4]][ys63]
  yy64<-df[[4]][ys64]
  yy65<-df[[4]][ys65]
  yy66<-df[[4]][ys66]
  yy67<-df[[4]][ys67]
  yy68<-df[[4]][ys68]
  yy69<-df[[4]][ys69]
  yy70<-df[[4]][ys70]
  yy71<-df[[4]][ys71]
  yy72<-df[[4]][ys72]
  yy73<-df[[4]][ys73]
  yy74<-df[[4]][ys74]
  yy75<-df[[4]][ys75]
  yy76<-df[[4]][ys76]
  yy77<-df[[4]][ys77]
  yy78<-df[[4]][ys78]
  yy79<-df[[4]][ys79]
  yy80<-df[[4]][ys80]
  
  
  return(data.frame(yy0,yy1,yy2,yy3,yy4,yy5,yy6,yy7,yy8,yy9,yy10,yy11,yy12,yy13,yy14,yy15,yy16,yy17,yy18,yy19,yy20,yy21,yy22,yy23,yy24,yy25,yy26,yy27,yy28,yy29,yy30,yy31,yy32,yy33,yy34,yy35,yy36,yy37,yy38,yy39,yy40,yy41,yy42,yy43,yy44,yy45,yy46,yy47,yy48,yy49,yy50,yy51,yy52,yy53,yy54,yy55,yy56,yy57,yy58,yy59,yy60,yy61,yy62,yy63,yy64,yy65,yy66,yy67,yy68,yy69,yy70,yy71,yy72,yy73,yy74,yy75,yy76,yy77,yy78,yy79,yy80))
}
# only to save the function and maybe not have to redo it
seq80y.dif<-function(df,year){
  ys0<-seq(1,year*365+11,1)
  ys1<-seq(1,(year*365+10+1*365),1)
  ys2<-seq(1,(year*365+10+2*365),1)
  ys3<-seq(1,(year*365+10+3*365),1)
  ys4<-seq(1,(year*365+10+4*365),1)
  ys5<-seq(1,(year*365+10+5*365),1)
  ys6<-seq(1,(year*365+10+6*365),1)
  ys7<-seq(1,(year*365+10+7*365),1)
  ys8<-seq(1,(year*365+10+8*365),1)
  ys9<-seq(1,(year*365+10+9*365),1)
  ys10<-seq(1,(year*365+10+10*365),1)
  ys11<-seq(1,(year*365+10+11*365),1)
  ys12<-seq(1,(year*365+10+12*365),1)
  ys13<-seq(1,(year*365+10+13*365),1)
  ys14<-seq(1,(year*365+10+14*365),1)
  ys15<-seq(1,(year*365+10+15*365),1)
  ys16<-seq(1,(year*365+10+16*365),1)
  ys17<-seq(1,(year*365+10+17*365),1)
  ys18<-seq(1,(year*365+10+18*365),1)
  ys19<-seq(1,(year*365+10+19*365),1)
  ys20<-seq(1,(year*365+10+20*365),1)
  ys21<-seq(1,(year*365+10+21*365),1)
  ys22<-seq(1,(year*365+10+22*365),1)
  ys23<-seq(1,(year*365+10+23*365),1)
  ys24<-seq(1,(year*365+10+24*365),1)
  ys25<-seq(1,(year*365+10+25*365),1)
  ys26<-seq(1,(year*365+10+26*365),1)
  ys27<-seq(1,(year*365+10+27*365),1)
  ys28<-seq(1,(year*365+10+28*365),1)
  ys29<-seq(1,(year*365+10+29*365),1)
  ys30<-seq(1,(year*365+10+30*365),1)
  ys31<-seq(1,(year*365+10+31*365),1)
  ys32<-seq(1,(year*365+10+32*365),1)
  ys33<-seq(1,(year*365+10+33*365),1)
  ys34<-seq(1,(year*365+10+34*365),1)
  ys35<-seq(1,(year*365+10+35*365),1)
  ys36<-seq(1,(year*365+10+36*365),1)
  ys37<-seq(1,(year*365+10+37*365),1)
  ys38<-seq(1,(year*365+10+38*365),1)
  ys39<-seq(1,(year*365+10+39*365),1)
  ys40<-seq(1,(year*365+10+40*365),1)
  ys41<-seq(1,(year*365+10+41*365),1)
  ys42<-seq(1,(year*365+10+42*365),1)
  ys43<-seq(1,(year*365+10+43*365),1)
  ys44<-seq(1,(year*365+10+44*365),1)
  ys45<-seq(1,(year*365+10+45*365),1)
  ys46<-seq(1,(year*365+10+46*365),1)
  ys47<-seq(1,(year*365+10+47*365),1)
  ys48<-seq(1,(year*365+10+48*365),1)
  ys49<-seq(1,(year*365+10+49*365),1)
  ys50<-seq(1,(year*365+10+50*365),1)
  ys51<-seq(1,(year*365+10+51*365),1)
  ys52<-seq(1,(year*365+10+52*365),1)
  ys53<-seq(1,(year*365+10+53*365),1)
  ys54<-seq(1,(year*365+10+54*365),1)
  ys55<-seq(1,(year*365+10+55*365),1)
  ys56<-seq(1,(year*365+10+56*365),1)
  ys57<-seq(1,(year*365+10+57*365),1)
  ys58<-seq(1,(year*365+10+58*365),1)
  ys59<-seq(1,(year*365+10+59*365),1)
  ys60<-seq(1,(year*365+10+60*365),1)
  ys61<-seq(1,(year*365+10+61*365),1)
  ys62<-seq(1,(year*365+10+62*365),1)
  ys63<-seq(1,(year*365+10+63*365),1)
  ys64<-seq(1,(year*365+10+64*365),1)
  ys65<-seq(1,(year*365+10+65*365),1)
  ys66<-seq(1,(year*365+10+66*365),1)
  ys67<-seq(1,(year*365+10+67*365),1)
  ys68<-seq(1,(year*365+10+68*365),1)
  ys69<-seq(1,(year*365+10+69*365),1)
  ys70<-seq(1,(year*365+10+70*365),1)
  ys71<-seq(1,(year*365+10+71*365),1)
  ys72<-seq(1,(year*365+10+72*365),1)
  ys73<-seq(1,(year*365+10+73*365),1)
  ys74<-seq(1,(year*365+10+74*365),1)
  ys75<-seq(1,(year*365+10+75*365),1)
  ys76<-seq(1,(year*365+10+76*365),1)
  ys77<-seq(1,(year*365+10+77*365),1)
  ys78<-seq(1,(year*365+10+78*365),1)
  ys79<-seq(1,(year*365+10+79*365),1)
  ys80<-seq(1,(year*365+10+80*365),1)
  
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
  yy31<-df[[4]][ys31]
  yy32<-df[[4]][ys32]
  yy33<-df[[4]][ys33]
  yy34<-df[[4]][ys34]
  yy35<-df[[4]][ys35]
  yy36<-df[[4]][ys36]
  yy37<-df[[4]][ys37]
  yy38<-df[[4]][ys38]
  yy39<-df[[4]][ys39]
  yy40<-df[[4]][ys40]
  yy41<-df[[4]][ys41]
  yy42<-df[[4]][ys42]
  yy43<-df[[4]][ys43]
  yy44<-df[[4]][ys44]
  yy45<-df[[4]][ys45]
  yy46<-df[[4]][ys46]
  yy47<-df[[4]][ys47]
  yy48<-df[[4]][ys48]
  yy49<-df[[4]][ys49]
  yy50<-df[[4]][ys50]
  yy51<-df[[4]][ys51]
  yy52<-df[[4]][ys52]
  yy53<-df[[4]][ys53]
  yy54<-df[[4]][ys54]
  yy55<-df[[4]][ys55]
  yy56<-df[[4]][ys56]
  yy57<-df[[4]][ys57]
  yy58<-df[[4]][ys58]
  yy59<-df[[4]][ys59]
  yy60<-df[[4]][ys60]
  yy61<-df[[4]][ys61]
  yy62<-df[[4]][ys62]
  yy63<-df[[4]][ys63]
  yy64<-df[[4]][ys64]
  yy65<-df[[4]][ys65]
  yy66<-df[[4]][ys66]
  yy67<-df[[4]][ys67]
  yy68<-df[[4]][ys68]
  yy69<-df[[4]][ys69]
  yy70<-df[[4]][ys70]
  yy71<-df[[4]][ys71]
  yy72<-df[[4]][ys72]
  yy73<-df[[4]][ys73]
  yy74<-df[[4]][ys74]
  yy75<-df[[4]][ys75]
  yy76<-df[[4]][ys76]
  yy77<-df[[4]][ys77]
  yy78<-df[[4]][ys78]
  yy79<-df[[4]][ys79]
  yy80<-df[[4]][ys80]
}

#### PLOT EV (lmom)
evWeiPl<-function(df,year,quantile,names,ally,allyn,func){
  xy<-seq30y(df,year)
  y0<-xy$yy0
  y3<-xy$yy3
  y6<-xy$yy6
  y9<-xy$yy9
  y12<-xy$yy12
  y15<-xy$yy15
  y20<-xy$yy20
  y25<-xy$yy25
  y30<-xy$yy30
  evplot(ally,main=paste("EV-Plot, Quantiles (",quantile,")",",",year,"yrs"),lwd=3,lty=2,type="l",col=1)
  #legend("topleft",inset=.005,title="Functions",c("EV","Wei","Gum","Pe3","Ln3"),fill=c(1:5),horiz=T)
  mtext(names, side=3, line=3,las=0, col="seagreen")
  abline(h = seq(1,max(ally),(max(ally)/100)),v=seq(0,10,0.1),lwd=0.5)
  
  if(func=="weibull")
  {
    xlm.wei0<-pelwei(samlmu(y0))
    evdistq(quawei, xlm.wei0,type="l",col=2)
    weiqu0<-quawei(f=quantile, para=xlm.wei0)
    xlm.wei1<-pelwei(samlmu(y3))
    evdistq(quawei, xlm.wei1,type="l",col=3)
    weiqu1<-quawei(f=quantile, para=xlm.wei1)
    xlm.wei2<-pelwei(samlmu(y6))
    evdistq(quawei, xlm.wei2,type="l",col=4)
    weiqu2<-quawei(f=quantile, para=xlm.wei2)
    xlm.wei3<-pelwei(samlmu(y9))
    evdistq(quawei, xlm.wei3,type="l",col=5)
    weiqu3<-quawei(f=quantile, para=xlm.wei3)
    xlm.wei4<-pelwei(samlmu(y12))
    evdistq(quawei, xlm.wei4,type="l",col=6)
    weiqu4<-(quawei(f=quantile, para=xlm.wei4))
    xlm.wei5<-pelwei(samlmu(y15))
    evdistq(quawei, xlm.wei5,type="l",col=7)
    weiqu5<-(quawei(f=quantile, para=xlm.wei5))
    
    xlm.wei6<-pelwei(samlmu(y20))
    evdistq(quawei, xlm.wei6,type="l",col=8)
    weiqu6<-(quawei(f=quantile, para=xlm.wei6))
    xlm.wei7<-pelwei(samlmu(y25))
    evdistq(quawei, xlm.wei7,type="l",col=9)
    weiqu7<-(quawei(f=quantile, para=xlm.wei7))
    xlm.wei8<-pelwei(samlmu(y30))
    evdistq(quawei, xlm.wei8,type="l",col=10)
    weiqu8<-(quawei(f=quantile, para=xlm.wei8))
    
    
    legend("topleft",inset=.005,cex=0.70,title="Year-Shift",c("yy0","yy3","yy6","yy9","yy12","yy15","yy20","yy25","yy30"),fill=c(2:10),horiz=T)
    (return(cbind(weiqu0,weiqu1,weiqu2,weiqu3,weiqu4,weiqu5,weiqu6,weiqu7,weiqu8)))
  } 
  else 
  { 
    if(func=="gumbel")
    {
      xlm.gum0<-pelgum(samlmu(y0))
      evdistq(quagum, xlm.gum0,type="l",col=2)
      gumqu0<-quagum(f=quantile, para=xlm.gum0)
      xlm.gum1<-pelgum(samlmu(y3))
      evdistq(quagum, xlm.gum1,type="l",col=3)
      gumqu1<-quagum(f=quantile, para=xlm.gum1)
      xlm.gum2<-pelgum(samlmu(y6))
      evdistq(quagum, xlm.gum2,type="l",col=4)
      gumqu2<-quagum(f=quantile, para=xlm.gum2)
      xlm.gum3<-pelgum(samlmu(y9))
      evdistq(quagum, xlm.gum3,type="l",col=5)
      gumqu3<-quagum(f=quantile, para=xlm.gum3)
      xlm.gum4<-pelgum(samlmu(y12))
      evdistq(quagum, xlm.gum4,type="l",col=6)
      gumqu4<-quagum(f=quantile, para=xlm.gum4)
      xlm.gum5<-pelgum(samlmu(y15))
      evdistq(quagum, xlm.gum5,type="l",col=7)
      gumqu5<-quagum(f=quantile, para=xlm.gum5)
      xlm.gum6<-pelgum(samlmu(y20))
      evdistq(quagum, xlm.gum6,type="l",col=8)
      gumqu6<-quagum(f=quantile, para=xlm.gum6)
      xlm.gum7<-pelgum(samlmu(y25))
      evdistq(quagum, xlm.gum7,type="l",col=9)
      gumqu7<-quagum(f=quantile, para=xlm.gum7)
      xlm.gum8<-pelgum(samlmu(y30))
      evdistq(quagum, xlm.gum8,type="l",col=10)
      gumqu8<-quagum(f=quantile, para=xlm.gum8)
      
      legend("topleft",inset=.005,cex=0.70,title="Year-Shift",c("yy0","yy3","yy6","yy9","yy12","yy15","yy20","yy25","yy30"),fill=c(2:10),horiz=T)
      (return(cbind(gumqu0,gumqu1,gumqu2,gumqu3,gumqu4,gumqu5,gumqu6,gumqu7,gumqu8)))
    }
    
    else 
    {
      if(func=="pearson")
      {
        xlm.pe30<-pelpe3(samlmu(y0))
        evdistq(quape3, xlm.pe30,type="l",col=2)
        pe3qu0<-quape3(f=quantile, para=xlm.pe30)
        xlm.pe31<-pelpe3(samlmu(y3))
        evdistq(quape3, xlm.pe31,type="l",col=3)
        pe3qu1<-quape3(f=quantile, para=xlm.pe31)
        xlm.pe32<-pelpe3(samlmu(y6))
        evdistq(quape3, xlm.pe32,type="l",col=4)
        pe3qu2<-quape3(f=quantile, para=xlm.pe32)
        xlm.pe33<-pelpe3(samlmu(y9))
        evdistq(quape3, xlm.pe33,type="l",col=5)
        pe3qu3<-quape3(f=quantile, para=xlm.pe33)
        xlm.pe34<-pelpe3(samlmu(y12))
        evdistq(quape3, xlm.pe34,type="l",col=6)
        pe3qu4<-quape3(f=quantile, para=xlm.pe34)
        xlm.pe35<-pelpe3(samlmu(y15))
        evdistq(quape3, xlm.pe35,type="l",col=7)
        pe3qu5<-quape3(f=quantile, para=xlm.pe35)
        
        xlm.pe36<-pelpe3(samlmu(y20))
        evdistq(quape3, xlm.pe36,type="l",col=8)
        pe3qu6<-quape3(f=quantile, para=xlm.pe36)
        xlm.pe37<-pelpe3(samlmu(y25))
        evdistq(quape3, xlm.pe37,type="l",col=9)
        pe3qu7<-quape3(f=quantile, para=xlm.pe37)
        xlm.pe38<-pelpe3(samlmu(y30))
        evdistq(quape3, xlm.pe38,type="l",col=10)
        pe3qu8<-quape3(f=quantile, para=xlm.pe38)
        
        
        
        legend("topleft",inset=.005,cex=0.70,title="Year-Shift",c("yy0","yy3","yy6","yy9","yy12","yy15","yy20","yy25","yy30"),fill=c(2:10),horiz=T)
        (return(cbind(pe3qu0,pe3qu1,pe3qu2,pe3qu3,pe3qu4,pe3qu5,pe3qu6,pe3qu7,pe3qu8)))
      }
      
      else {
        if(func=="lognorm")
        {
          xlm.ln30<-pelln3(samlmu(y0))
          evdistq(qualn3, xlm.ln30,type="l",col=2)
          ln3qu0<-qualn3(f=quantile, para=xlm.ln30)
          xlm.ln31<-pelln3(samlmu(y3))
          evdistq(qualn3, xlm.ln31,type="l",col=3)
          ln3qu1<-qualn3(f=quantile, para=xlm.ln31)
          xlm.ln32<-pelln3(samlmu(y6))
          evdistq(qualn3, xlm.ln32,type="l",col=4)
          ln3qu2<-(qualn3(f=quantile, para=xlm.ln32))
          xlm.ln33<-pelln3(samlmu(y9))
          evdistq(qualn3, xlm.ln33,type="l",col=5)
          ln3qu3<-(qualn3(f=quantile, para=xlm.ln33))
          xlm.ln34<-pelln3(samlmu(y12))
          evdistq(qualn3, xlm.ln34,type="l",col=6)
          ln3qu4<-(qualn3(f=quantile, para=xlm.ln34))
          xlm.ln35<-pelln3(samlmu(y15))
          evdistq(qualn3, xlm.ln35,type="l",col=7)
          ln3qu5<-(qualn3(f=quantile, para=xlm.ln35))
          
          xlm.ln36<-pelln3(samlmu(y20))
          evdistq(qualn3, xlm.ln36,type="l",col=8)
          ln3qu6<-(qualn3(f=quantile, para=xlm.ln36))
          xlm.ln37<-pelln3(samlmu(y25))
          evdistq(qualn3, xlm.ln37,type="l",col=9)
          ln3qu7<-(qualn3(f=quantile, para=xlm.ln37))
          xlm.ln38<-pelln3(samlmu(y30))
          evdistq(qualn3, xlm.ln38,type="l",col=10)
          ln3qu8<-(qualn3(f=quantile, para=xlm.ln38))
          
          legend("topleft",inset=.005,cex=0.70,title="Year-Shift",c("yy0","yy3","yy6","yy9","yy12","yy15","yy20","yy25","yy30"),fill=c(2:10),horiz=T)
          (return(cbind(ln3qu0,ln3qu1,ln3qu2,ln3qu3,ln3qu4,ln3qu5,ln3qu6,ln3qu7,ln3qu8)))
        }
        else {
          if(func=="all")
          {
            xlm.wei<-pelwei(samlmu(ally))
            evdistq(quawei, xlm.wei,type="l",col=1)
            weiqu<-(quawei(f=quantile, para=xlm.wei))
            
            xlm.gum<-pelgum(samlmu(ally))
            evdistq(quagum, xlm.gum,type="l",col=2)
            gumqu<-(quagum(f=quantile, para=xlm.gum))
            
            xlm.pe3<-pelpe3(samlmu(ally))
            evdistq(quape3, xlm.pe3,type="l",col=3)
            pe3qu<-(quape3(f=quantile, para=xlm.pe3))
            
            xlm.ln3<-pelln3(samlmu(ally))
            evdistq(qualn3, xlm.ln3,type="l",col=4)
            ln3qu<-(qualn3(f=quantile, para=xlm.ln3))
            legend("topleft",inset=.005,title=paste("Functions",",",allyn),c("EV","Wei","Gum","Pe3","Ln3"),fill=c(1,1,2,3,4),horiz=T)
            (return(cbind(weiqu,gumqu,pe3qu,ln3qu)))
          }
        }
      }  
    } 
  }
}

##### Plot EV (MLE, außer LN3) <- funktioniert, aber fehlerhaft, überarbeiten
ev.mle<-function(df,names,ally,allyn){
  evplot(df[[4]],main=paste("EV-Plot", "MLE"),lwd=3,lty=2,type="l",col=1)
  mtext(names, side=3, line=3,las=0, col="seagreen")
  
  gum.mle<-gum.fit(ally)
  gu<-c(gum.mle$mle[1],gum.mle$mle[2])
  evdistq(quagum, gu,type="l",col=2)
  
  wei3.fit0<-function(shape,scale,thres) { 
    -sum(dweibull3(ally,shape,scale,thres,log=T)) }
  wei.mle<-mle2(wei3.fit0,start=list(shape=1,scale=1),data=list(thres=signif(min(ally)-1,1)))
  wei.mle2<-mle2(wei3.fit0,start=list(shape=wei.mle@coef[1],thres=1),data=list(scale=wei.mle@coef[2]))
  w<-c(wei.mle2@coef[1],wei.mle@coef[2],wei.mle2@coef[2])
  evdistq(quawei, w,type="l",col=1)
  
  
  ln3.b<-function(shape,scale,thres) { 
    -sum(dlnorm3(gg,shape,scale,thres,log=T)) }
  ln1 <- mle2(ln3.b,start=list(shape=1,scale=1),data=list(thres=signif(min(gg)-1,1)))
  ln2 <- mle2(ln3.b,start=list(shape=ln1@coef[1],thres=signif(min(gg)-1,1)),data=list(scale=ln1@coef[2]))
  ml.ln3<-qualn3(0.99,para=c(ln2@coef[2],ln1@coef[2],ln2@coef[1]))
  
  ln3.fit0<-function(shape,scale,thres) { 
    -sum(dlnorm3(ally,shape,scale,thres,log=T)) }
  ln3.mle<-mle2(ln3.fit0,start=list(shape=1,scale=1),data=list(thres=signif(min(ally)-1,1)))
  ln3.mle2<-mle2(ln3.fit0,start=list(shape=1,thres=signif(min(ally)-1,1)),data=list(ln3.mle@coef[2])))
  ln<-c(ln3.mle2@coef[2],ln3.mle@coef[2],ln3.mle2@coef[1])
  evdistq(qualn3, ln,type="l",col=4)
  
  pe3.fit0<-function(scale,rate,shape) { 
    -sum(dgamma(ally,scale,rate,shape,log=T)) }
  pe3.mle<-mle2(pe3.fit0,start=list(shape=3,scale=500),data=list(ally))
  pe<-c(100,pe3.mle@coef[2],pe3.mle@coef[1])
  evdistq(quape3, para=pe,type="l",col=3)
  
  legend("topleft",inset=.005,title=paste("Functions",allyn),c("EV","Wei","Gum","Pe3","Ln3"),fill=c(1,1,2,3,4),horiz=T)
}

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

### Plot MLE (Quantilen, ohne ln3, da nicht plot tauglich)  
ploti2<-function(df,dfr,names,quantile1,years,on){
  if (on=="ON"){
  ye<-quant(dfr,year=years,qu=quantile1)
  ylim.max<-max(cbind(df[[1]],df[[2]],df[[4]],ye$yy1))*1.1
  ylim.min<-min(cbind(df[[1]],df[[2]],df[[4]],ye$yy1))
  wei<-as.numeric(df[[1]])
  gum<-as.numeric(df[[2]])
  ln3<-as.numeric(df[[3]])
  pe3<-as.numeric(df[[4]])
  plot(wei,type="l",ylim=c(ylim.min,ylim.max),lty=2,col=1,xlab = "Year-Shift",
       ylab="Discharge",main=paste("MLE Quantiles (",quantile1,")",years,"yrs"),lwd=2)
  legend("topright",inset=.005,title="Functions",c("Wei","Gum","Pe3","ln3"),fill=c(1:4),horiz=T)
  lines(gum,lty=3,col=2,lwd=2)
  lines(ln3,lty=3,col=4,lwd=2)
  lines(pe3,lty=3,col=3,lwd=2)
  points(ye$yy1:30,cex=1,pch=8,col=2)
  abline(h = seq(1,10000,100),v=c(1:30),lwd=0.5)
  mtext(names, side=3, line=3,las=0, col="seagreen")
    
  }
  else { if (on=="OFF"){
    ylim.max<-max(cbind(df[[1]],df[[2]],df[[4]]))*1.1
    ylim.min<-min(cbind(df[[1]],df[[2]],df[[4]]))
    wei<-as.numeric(df[[1]])
    gum<-as.numeric(df[[2]])
    ln3<-as.numeric(df[[3]])
    pe3<-as.numeric(df[[4]])
    plot(wei,type="l",ylim=c(ylim.min,ylim.max),lty=2,col=1,xlab = "Year-Shift",
         ylab="Discharge",main=paste("MLE Quantiles (",quantile1,")",years,"yrs"),lwd=2)
    legend("topright",inset=.005,title="Functions",c("Wei","Gum","Pe3","ln3"),fill=c(1:4),horiz=T)
    lines(gum,lty=3,col=2,lwd=2)
    #lines(ln3,lty=3,col=4,lwd=2)
    lines(pe3,lty=3,col=3,lwd=2)
    abline(h = seq(1,10000,100),v=c(1:30),lwd=0.5)
    mtext(names, side=3, line=3,las=0, col="seagreen")
  }
  }
  
}

##### Values lmom,mle,obs
## Nicht verwendet (da MLE noch drinnen ist)
#data.all<-function(dfall,yearall,quall){
  mle1<-param.mle(df=dfall,quantile=quall,year=yearall)
  lmom1<-param(df=dfall,quantile=quall,year=yearall)
  qua1<-quant(df=dfall,year=yearall,qu=quall)
  li.wei<-c(mle1[[1]][[1]],lmom1[[1]][[1]][[1]],qua1)
  li.gum<-c(mle1[[1]][[2]],lmom1[[1]][[1]][[2]],qua1)
  li.ln3<-c(mle1[[1]][[3]],lmom1[[1]][[1]][[4]],qua1)
  li.pe3<-c(mle1[[1]][[4]],lmom1[[1]][[1]][[3]],qua1)
  return(list(li.wei,li.gum,li.ln3,li.pe3))
}
#Alle daten, außer MLE <- Überarbeiten
yyall<-seq30y(listko[1][[1]],year = 30)
data.all<-function(dfal,yearall,quall){
  #mle1<-param.mle(df=dfall,quantile=quall,year=yearall)
  dfall<-dfal[[1]]
  lmom<-param(df=dfall,quantile=quall,year=yearall)
  qua<-quantc(df=dfall,year=yearall,qu=quall)
  li.wei<-as.vector(unlist(lmom[[1]]))
  li.gum<-as.vector(unlist(lmom[[2]]))
  li.ln3<-as.vector(unlist(lmom[[4]]))
  li.pe3<-as.vector(unlist(lmom[[3]]))
  quanti<-as.vector(unlist(qua@.Data))
  n<-c(dput(names(dfal[1])),yearall,quall)
  lili<-(list(li.wei,li.gum,li.ln3,li.pe3,quanti,n))
  names(lili)<-c("Wei","Gum","LN3","PE3","Obs","Names")
  return(lili)
}
#### alle daten, außer MLE, Vioplot mit R-square adjusted
#### auch die summe der % aller im Directory -> zuerst in den Results die sum.wei etc. laufen lassen,
#### davor die compList anpassen bezüglich Jahre und Quantilen
data.all.r<-function(dfal,yearall,quall){
  #mle1<-param.mle(df=dfall,quantile=quall,year=yearall)
  dfall<-dfal[[1]]
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
  fit.all<-lm(formula = listi$Obs~listi$Wei+listi$Gum+listi$LN3+listi$PE3)
  fit.wei<-lm(formula = listi$Obs~listi$Wei)
  fit.gum<-lm(formula = listi$Obs~listi$Gum)
  fit.ln3<-lm(formula = listi$Obs~listi$LN3)
  fit.pe3<-lm(formula = listi$Obs~listi$PE3)
  r.adj.all<-summary(fit.all)$adj.r.squared
  r.adj.wei<-summary(fit.wei)$adj.r.squared
  r.adj.gum<-summary(fit.gum)$adj.r.squared
  r.adj.ln3<-summary(fit.ln3)$adj.r.squared
  r.adj.pe3<-summary(fit.pe3)$adj.r.squared
  wei.bp <- listi$Wei
  gum.bp <- listi$Gum
  ln3.bp <- listi$LN3
  pe3.bp <- listi$PE3
  obs.bp<-listi$Obs
  wei.mean <- mean(listi$Wei)
  gum.mean <- mean(listi$Gum)
  ln3.mean <- mean(listi$LN3)
  pe3.mean <- mean(listi$PE3)
  obs.mean<-mean(listi$Obs)
  # minus
  adj.wei<-signif((sum(obs.bp-wei.bp)),4)
  adj.gum<-signif((sum(obs.bp-gum.bp)),4)
  adj.ln3<-signif((sum(obs.bp-ln3.bp)),4)
  adj.pe3<-signif((sum(obs.bp-pe3.bp)),4)
  adj.wei.p<-signif((adj.wei/sum(obs.bp)*100),4)
  adj.gum.p<-signif((adj.gum/sum(obs.bp)*100),4)
  adj.ln3.p<-signif((adj.ln3/sum(obs.bp)*100),4)
  adj.pe3.p<-signif((adj.pe3/sum(obs.bp)*100),4)
  
  vioplot(wei.bp,gum.bp,ln3.bp,pe3.bp,obs.bp, names=c("wei", "gum", "ln3","pe3","Obs"), col=2)
  mtext(c("R.Adj.Squ:",signif(r.adj.wei,4),signif(r.adj.gum,4),signif(r.adj.ln3,4),signif(r.adj.pe3,4),"all:",
          c(signif(r.adj.all,4))),side = 3,line = -1,at = c(0.6,1,2,3,4,4.7,5))
  mtext(c("Mean(m^3/s):",signif(wei.mean,4),signif(gum.mean,4),signif(ln3.mean,4),signif(pe3.mean,4),signif(obs.mean,4)),
        side = 3,line = 3,at = c(0.5,1,2,3,4,5))
  mtext(c("Minus (%):",adj.wei.p,adj.gum.p,adj.ln3.p,adj.pe3.p),side = 3,line = 2,at = c(0.5,1,2,3,4))
  mtext(c("All % dif.:",signif(sum.wei.p,4),signif(sum.gum.p,4),signif(sum.ln3.p,4),signif(sum.pe3.p,4)),
        side=3,line=1,at = c(0.5,1,2,3,4))
  mtext(c("Value(m^3/s):",signif(adj.wei.p/100*wei.mean,4),signif(adj.gum.p/100*gum.mean,4),
          signif(adj.ln3.p/100*ln3.mean,4),signif(adj.pe3.p/100*pe3.mean,4)),side=3,line=0,at = c(0.5,1,2,3,4))
  mtext(c(names(dfal),"Years:",yearall,"Quantile:",quall),side=1,line=3,at=c(1,2.7,3,4.7,5))
  R.Adj.Squ<-data.frame(r.adj.wei,r.adj.gum,r.adj.ln3,r.adj.pe3,r.adj.all)
  listi2<-(list(li.wei,li.gum,li.ln3,li.pe3,quanti,n,R.Adj.Squ))
  names(listi2)<-c("Wei","Gum","LN3","PE3","Obs","Names","R.Adj.Squ")
  return(listi2)
}
#### Vioplot und Diagnostics <- Fertig machen. Vioplot mit Diagnostik-Values dazu(?!) 

#### Alle Daten, kein Plot nur Listen. Möchte ich aufsummieren und schauen, welche Funktion über 
#### alle Flüsse über/unterschätzt;;; Nur lmom, noch keine MLE ;;; Nur Minus ()
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
  listix<-(list(li.wei,li.gum,li.ln3,li.pe3,quanti,n))
  names(listix)<-c("Wei","Gum","LN3","PE3","Obs","Names")
  wei.bp <- listix$Wei
  gum.bp <- listix$Gum
  ln3.bp <- listix$LN3
  pe3.bp <- listix$PE3
  obs.bp<-listix$Obs
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

#### Aufsummierte Funktionen, die vom Obs abweichen (nur lmom); Pro Funktion, eine Funktion
#### n deswegen, damit ich die Länge habe und bei den Results berechnen kann
n<-as.numeric(1:length(compList)) ### Vorsicht bei length(compList); könnte nicht richtig sein
SumWei<-function(complist){
  wei.p<-(unlist(complist)[[5]])
  return(wei.p)
}
Sumgum<-function(complist){
  wei.p<-(unlist(complist)[[6]])
  return(wei.p)
}
Sumln3<-function(complist){
  wei.p<-(unlist(complist)[[7]])
  return(wei.p)
}
Sumpe3<-function(complist){
  wei.p<-(unlist(complist)[[8]])
  return(wei.p)
}

### (neu) shift 30 Jahre (AM-Methode)
shift<-matrix(NA,50,3)
colnames(shift)<-c("30","100","1000")
seqq<-NULL
shift_30<-function(df,quantile,...){  
  nSubsets<-(length(df[[4]])/365.25)
  outList<-vector("list",length=nSubsets)
  maxOL<-vector("numeric",length=nSubsets)
  totRow<-nrow(df[[4]])
  
  for (i in seq_len(nSubsets)){
    rowsToGrab<-seq(i*365.25-365.25,(365.25*i),1)
    outList[[i]]<-df[[4]][rowsToGrab]
    maxOL[i]<-max(outList[[i]],na.rm=T)
  } 
  
  shift<-matrix(NA,50,4)
  colnames(shift)<-c("Wei","Gum","PE3","LN3")
  
  for (i in c(1:50)){
    
    seqq<-seq(i,29+i,1)
    y0.wei<-quawei(f=quantile, para=pelwei(samlmu(maxOL[seqq]),bound=0))
    y0.gum<-quagum(f=quantile, para=pelgum(samlmu(maxOL[seqq])))
    y0.pe3<-quape3(f=quantile, para=pelpe3(samlmu(maxOL[seqq])))
    y0.ln3<-qualn3(f=quantile, para=pelln3(samlmu(maxOL[seqq]),bound=0))
    shift[i,1]<-y0.wei
    shift[i,2]<-y0.gum
    shift[i,3]<-y0.pe3
    shift[i,4]<-y0.ln3
    
  }
  return(shift)
}

### MLE Gumbel 30/100/1000 return Periods (neu)
shift<-matrix(NA,50,3)
colnames(shift)<-c("30","100","1000")
seqq<-NULL
shift_30_mle<-function(df,rp,...){
  
  for (i in seq_along(1:50)){
    seqq<-seq(i*365.25-365.25,(29+i)*365.25,1)
    y0.gum<-fevd(df[[4]][seqq],type="Gumbel")
    rl<-ci(y0.gum,return.period = rp)
    shift[i,1]<-rl[4]
    shift[i,2]<-rl[5]
    shift[i,3]<-rl[6]
  }
  return(shift)
}
### (AM) add HQ1000 to shift 30
shift<-matrix(NA,50,3)
colnames(shift)<-c("30","100","1000")
seqq<-NULL
shift_30_add<-function(df,quantile,...){
  df1<-shift_30(df,quantile=quantile)
  df[[4]][60*365.25]<-max(df[[4]]+1)
  df_c<-list(1,2,3,df[[4]])
  df_c1<-shift_30(df_c,quantile=quantile)
  wei<-df_c1[,1]-df1[,1]
  gum<-df_c1[,2]-df1[,2]
  pe3<-df_c1[,3]-df1[,3]
  ln3<-df_c1[,4]-df1[,4]
  
  wei_p<-(df_c1[,1]-df1[,1])/df_c1[,1]
  gum_p<-(df_c1[,2]-df1[,2])/df_c1[,2]
  pe3_p<-(df_c1[,3]-df1[,3])/df_c1[,3]
  ln3_p<-(df_c1[,4]-df1[,4])/df_c1[,4]
  o<-list(df1,df_c1,wei,gum,pe3,ln3,wei_p,gum_p,pe3_p,ln3_p)
  names(o)<-c("df1","df_c1","wei","gum","pe3","ln3","wei_p","gum_p","pe3_p","ln3_p")
  return(list(o))
}

### 80 years cummulated with and without add of HQ1000 (AM)
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
    y0.wei<-quawei(f=quantile, para=pelwei(samlmu(maxOL[seqq]),bound=0))
    y0.gum<-quagum(f=quantile, para=pelgum(samlmu(maxOL[seqq])))
    y0.pe3<-quape3(f=quantile, para=pelpe3(samlmu(maxOL[seqq])))
    y0.ln3<-qualn3(f=quantile, para=pelln3(samlmu(maxOL[seqq]),bound=0))
    cummul_80[i,1]<-y0.wei
    cummul_80[i,2]<-y0.gum
    cummul_80[i,3]<-y0.pe3
    cummul_80[i,4]<-y0.ln3
    
  }
  return(cummul_80)
}
cummul_80_f2<-function(df,quantile,...){
  
  cummul_80<-matrix(NA,50,4)
  colnames(cummul_80)<-c("Wei","Gum","PE3","LN3")
  
  for (i in c(1:50)){
    
    seqq<-seq(1,29+i,1)
    y0.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][seqq]),bound=0))
    y0.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][seqq])))
    y0.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][seqq])))
    y0.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][seqq]),bound=0))
    cummul_80[i,1]<-y0.wei
    cummul_80[i,2]<-y0.gum
    cummul_80[i,3]<-y0.pe3
    cummul_80[i,4]<-y0.ln3
    
  }
  return(cummul_80)
}
cum_80_add<-function(df,quantile,...){
  df1<-cummul_80_f(df,quantile=quantile)
  df_c<-list(1,2,3,append((max(df[[4]])+1),df[[4]]))
  df_c1<-cummul_80_f(df_c,quantile=quantile)
  wei<-df_c1[,1]-df1[,1]
  gum<-df_c1[,2]-df1[,2]
  pe3<-df_c1[,3]-df1[,3]
  ln3<-df_c1[,4]-df1[,4]
  
  wei_p<-(df_c1[,1]-df1[,1])/df_c1[,1]
  gum_p<-(df_c1[,2]-df1[,2])/df_c1[,2]
  pe3_p<-(df_c1[,3]-df1[,3])/df_c1[,3]
  ln3_p<-(df_c1[,4]-df1[,4])/df_c1[,4]
  o<-list(df1,df_c1,wei,gum,pe3,ln3,wei_p,gum_p,pe3_p,ln3_p)
  names(o)<-c("df1","df_c1","wei","gum","pe3","ln3","wei_p","gum_p","pe3_p","ln3_p")
  return(o)
}


##### Return Level
which(dbinom(1,listko[3][[1]][[4]],0.01)==(max(dbinom(1,listko[3][[1]][[4]],0.01))))
head(dbinom(1,listko[3][[1]][[4]],0.001))

dbinom(1,30,0.001)
listko[3][[1]][[4]][320]
mean(listko[3][[1]][[4]])
which(dpois(listko[3][[1]][[4]], lambda = 2050.856)==max(dpois(listko[3][[1]][[4]], lambda = 2050.856)))
ff<-(ppois(listko[3][[1]][[4]], lambda = 2050.856,lower=F))
head((rank((ff))))
tail(ff)
listko[3][[1]][[4]]
#################################################################################################
#################################################################################################
#################################################################################################
#################################################################################################
### 80 years 142 rivers 
shift<-matrix(NA,30,4)
colnames(shift)<-c("30","100","1000")
seqq<-NULL
shift_30<-function(df,quantile,...){  
  nSubsets<-(60)
  outList<-vector("list",length=nSubsets)
  maxOL<-vector("numeric",length=nSubsets)
  totRow<-nrow(df[[4]])
  
  for (i in seq_len(nSubsets)){
    rowsToGrab<-seq(i*365.25-365.25,(365.25*i),1)
    outList[[i]]<-df[[4]][rowsToGrab]
    maxOL[i]<-max(outList[[i]],na.rm=T)
  } 
  
  shift<-matrix(NA,30,4)
  colnames(shift)<-c("Wei","Gum","PE3","LN3")
  
  for (i in c(1:30)){
    
    seqq<-seq(i,29+i,1)
    y0.wei<-quawei(f=quantile, para=pelwei(samlmu(maxOL[seqq]),bound=0))
    y0.gum<-quagum(f=quantile, para=pelgum(samlmu(maxOL[seqq])))
    y0.pe3<-quape3(f=quantile, para=pelpe3(samlmu(maxOL[seqq])))
    y0.ln3<-qualn3(f=quantile, para=pelln3(samlmu(maxOL[seqq]),bound=0))
    shift[i,1]<-y0.wei
    shift[i,2]<-y0.gum
    shift[i,3]<-y0.pe3
    shift[i,4]<-y0.ln3
    
  }
  return(shift)
}


#################################################################################################
#################################################################################################
#################################################################################################
#################################################################################################
### 100 years 47 rivers;;; AM
## Repeat calculation of fake years 1000times, calculate HQ100 while moving from 1-30/31/32...1000years
## then plot ### TIME INTENSE
filist<-list()
set.seed(1)
filist<-list()
for (i in seq(1,1000,1)){
  filist[i]<-list(fakeit3(listko[[7]],year=1000))
}
filist_1000<-NULL
filist_1000<-lapply(filist[c(1:1000)],geth,year=1000,quantile=0.99)

plot(filist_1000[[1]][,1],type="l",lwd=5,col="red")
for (i in seq(1:1000)){
  lines(filist[[i]][,1],col=i)
}

## First: Fit 5 functions
## Second: Take Parameters, create 1000years
## Third: Shift for 30years HQ10/30/50/75/100 ### Not done yet, but for HQ100 done previously (shift_30)
## Fourth: Expand years 30->1000, gradually, for HQ10/30/50/75/100 and plot, when it gets flat
#shift_30<-function(df,quantile,...){  
  nSubsets<-(length(df[[4]])/365.25)
  outList<-vector("list",length=nSubsets)
  maxOL<-vector("numeric",length=nSubsets)
  totRow<-nrow(df[[4]])
  
  for (i in seq_len(nSubsets)){
    rowsToGrab<-seq(i*365.25-365.25,(365.25*i),1)
    outList[[i]]<-df[[4]][rowsToGrab]
    maxOL[i]<-max(outList[[i]],na.rm=T)
  } 
  
  shift<-matrix(NA,50,4)
  colnames(shift)<-c("Wei","Gum","PE3","LN3")
  
  for (i in c(1:50)){
    
    seqq<-seq(i,29+i,1)
    y0.wei<-quawei(f=quantile, para=pelwei(samlmu(maxOL[seqq]),bound=0))
    y0.gum<-quagum(f=quantile, para=pelgum(samlmu(maxOL[seqq])))
    y0.pe3<-quape3(f=quantile, para=pelpe3(samlmu(maxOL[seqq])))
    y0.ln3<-qualn3(f=quantile, para=pelln3(samlmu(maxOL[seqq]),bound=0))
    shift[i,1]<-y0.wei
    shift[i,2]<-y0.gum
    shift[i,3]<-y0.pe3
    shift[i,4]<-y0.ln3
    
  }
  return(shift)
}
# NR.1 and NR.2
fakeit3<-function(df,year,...){  
  nSubsets<-(length(df[[4]])/365.25)
  outList<-vector("list",length=nSubsets)
  maxOL<-vector("numeric",length=nSubsets)
  
  for (i in seq_len(nSubsets)){
    rowsToGrab<-seq(i*365.25-365.25,(365.25*i),1)
    outList[[i]]<-df[[4]][rowsToGrab]
    maxOL[i]<-max(outList[[i]],na.rm=T)
  } 
  
  lmwei<-pelwei(samlmu(maxOL),bound = 0)
  lmgum<-pelgum(samlmu(maxOL))
  lmpe3<-pelpe3(samlmu(maxOL))
  lmln3<-pelln3(samlmu(maxOL),bound=0)
  lmgev<-pelgev(samlmu(maxOL))
  
  faitma<-matrix(NA,year,5)
  colnames(faitma)<-c("Wei","Gum","PE3","LN3","Gev")
 
  faitma[,1]<-rweibull(year,scale = lmwei[2],shape=lmwei[3])
  faitma[,2]<-rgumbel(year,location=lmgum[1],scale =lmgum[2])
  faitma[,3]<-rpearsonIII(year,scale=lmpe3[2],shape=lmpe3[3],location=lmpe3[1])
  faitma[,4]<-rlnorm3(year,scale = lmln3[2],shape = lmln3[3])
  faitma[,5]<-rgev(year,shape =lmgev[3] ,scale =lmgev[2] ,location =lmgev[1])
  
  return(faitma)
} # only gets 1000 years fake data
faitlist<-NULL
faitlist2<-lapply(listko[c(1:6)],fakeit3,year=10000)
faitlist[[4]]<-NULL # because nr 47 doesnt work with gamma/pearson3


## NR.4 get accumulated HQ100, Starting with 30 years till 1000years calculation for HQ100, when does it stay 
# almost the same
# for HQ100
cummul_47<-matrix(NA,10000,15)
colnames(cummul_47)<-c("Wei","Gum","PE3","LN3","GEV","Weil","Guml","PE3l","LN3l","GEVl","Weih","Gumh","PE3h","LN3h","GEVh")
geth<-function(df,quantile,year,...){
  for (i in seq(1,970,1)){
    seqq<-seq(1,29+i,1)
    y0.wei<-quawei(f=quantile, para=pelwei(samlmu(df[,1][seqq]),bound=0))
    y0.gum<-quagum(f=quantile, para=pelgum(samlmu(df[,2][seqq])))
    y0.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[,3][seqq])))
    y0.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[,4][seqq]),bound=0))
    y0.gev<-quagev(f=quantile, para=pelgev(samlmu(df[,5][seqq])))
    cummul_47[i,1]<-y0.wei
    cummul_47[i,2]<-y0.gum
    cummul_47[i,3]<-y0.pe3
    cummul_47[i,4]<-y0.ln3
    cummul_47[i,5]<-y0.gev
    
  }
   
  return(cummul_47)
}
cummul_47
geth2<-function(df,quantile,cin,year,...){
  for (i in seq(1,9970,1)){
    seqq<-seq(1,29+i,1)
    cfin<-CI(df[,1][seqq],ci=cin)
    
    wlm<-samlmu(df[,1][seqq])
    wlm[1]<-cfin[1]
    y0.wei.l<-quawei(f=quantile, para=pelwei(wlm,bound=0))
    wlm<-samlmu(df[,1][seqq])    
    wlm[1]<-cfin[3]
    y0.wei.h<-quawei(f=quantile, para=pelwei(wlm,bound=0))
    y0.wei<-quawei(f=quantile, para=pelwei(samlmu(df[,1][seqq]),bound=0))
    

    glm<-samlmu(df[,1][seqq])
    glm[1]<-cfin[1]
    y0.gum.l<-quagum(f=quantile, para=pelgum(glm))
    glm<-samlmu(df[,1][seqq])    
    glm[1]<-cfin[3]
    y0.gum.h<-quagum(f=quantile, para=pelgum(glm))
    y0.gum<-quagum(f=quantile, para=pelgum(samlmu(df[,2][seqq])))

    
    plm<-samlmu(df[,1][seqq])
    plm[1]<-cfin[1]
    y0.pe3.l<-quape3(f=quantile, para=pelpe3(plm))
    plm<-samlmu(df[,1][seqq])    
    plm[1]<-cfin[3]
    y0.pe3.h<-quape3(f=quantile, para=pelpe3(plm))
    y0.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[,3][seqq])))

    llm<-samlmu(df[,1][seqq])
    llm[1]<-cfin[1]
    y0.ln3.l<-qualn3(f=quantile, para=pelln3(llm,bound=0))
    llm<-samlmu(df[,1][seqq])    
    llm[1]<-cfin[3]
    y0.ln3.h<-qualn3(f=quantile, para=pelln3(llm,bound=0))
    y0.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[,4][seqq]),bound=0))

    gelm<-samlmu(df[,1][seqq])
    gelm[1]<-cfin[1]
    y0.gev.l<-quagev(f=quantile, para=pelgev(gelm))
    gelm<-samlmu(df[,1][seqq])    
    gelm[1]<-cfin[3]
    y0.gev.h<-quagev(f=quantile, para=pelgev(gelm))
    y0.gev<-quagev(f=quantile, para=pelgev(samlmu(df[,5][seqq])))
    
    cummul_47[i,1]<-y0.wei
    cummul_47[i,2]<-y0.gum
    cummul_47[i,3]<-y0.pe3
    cummul_47[i,4]<-y0.ln3
    cummul_47[i,5]<-y0.gev
    cummul_47[i,6]<-y0.wei.l
    cummul_47[i,7]<-y0.gum.l
    cummul_47[i,8]<-y0.pe3.l
    cummul_47[i,9]<-y0.ln3.l
    cummul_47[i,10]<-y0.gev.l
    cummul_47[i,11]<-y0.wei.h
    cummul_47[i,12]<-y0.gum.h
    cummul_47[i,13]<-y0.pe3.h
    cummul_47[i,14]<-y0.ln3.h
    cummul_47[i,15]<-y0.gev.h
  }
  
  return(cummul_47)
}

faitrp_100_ci<-NULL
faitrp_100_ci<-lapply(faitlist2[c(1)],geth2,year=10000,quantile=0.9,cin=0.95)

plot(faitrp_100_ci[[1]][,1],type="l")
lines(faitrp_100_ci[[1]][,6],type="l",col="gray")
lines(faitrp_100_ci[[1]][,11],type="l",col="gray")
polygon(c(seq(1,9970,1),1), c(faitrp_100_ci[[1]][,6][1:9970],faitrp_100_ci[[1]][,11][1:9970]),
        col = "black",border=NA)
polygon(c(seq(1,9970,1),seq(9970,1,1)),c(faitrp_100_ci[[1]][,6][1:9970],faitrp_100_ci[[1]][,11][1:9970]),col=gray(0.8),border=NA)

faitrp_100_ci[[1]][,11]-faitrp_100_ci[[1]][,1]

faitrp_100<-lapply(faitlist[c(1:46)],geth,year=1000,quantile=0.99)
# for HQ 10
faitrp_10<-NULL
faitrp_10<-lapply(faitlist[c(1:46)],geth,year=1000,quantile=0.9)
# for HQ30
faitrp_30<-NULL
faitrp_30<-lapply(faitlist[c(1:46)],geth,year=1000,quantile=0.9666667)
#for HQ50
faitrp_50<-NULL
faitrp_50<-lapply(faitlist[c(1:46)],geth,year=1000,quantile=0.98)
# for HQ75
faitrp_75<-NULL
faitrp_75<-lapply(faitlist[c(1:46)],geth,year=1000,quantile=0.9866667)
# for HQ300
faitrp_300<-NULL
faitrp_300<-lapply(faitlist[c(1:46)],geth,year=1000,quantile=0.9966667)



### Plot 30-1000 year calculation
par(mfrow=c(3,1))
for (j in seq(1,3,1)){
plot(faitrp[[j]][,1],type="l",ylim=c(min(faitrp[[j]],na.rm=T),max(faitrp[[j]],na.rm=T)))
for (i in seq(1,5,1)){
  lines(faitrp[[j]][,i],col=i)
}
}


#################################################################################################
#################################################################################################
#################################################################################################
#################################################################################################
### 60 years 204 rivers 

##### AM
### (neu) shift 30 Jahre (AM-Methode)
shift<-matrix(NA,30,3)
colnames(shift)<-c("30","100","1000")
seqq<-NULL
shift_30<-function(df,quantile,...){  
  nSubsets<-(60)
  outList<-vector("list",length=nSubsets)
  maxOL<-vector("numeric",length=nSubsets)
  totRow<-nrow(df[[4]])
  
  for (i in seq_len(nSubsets)){
    rowsToGrab<-seq(i*365.25-365.25,(365.25*i),1)
    outList[[i]]<-df[[4]][rowsToGrab]
    maxOL[i]<-max(outList[[i]],na.rm=T)
  } 
  
  shift<-matrix(NA,30,4)
  colnames(shift)<-c("Wei","Gum","PE3","LN3")
  
  for (i in c(1:30)){
    
    seqq<-seq(i,29+i,1)
    y0.wei<-quawei(f=quantile, para=pelwei(samlmu(maxOL[seqq]),bound=0))
    y0.gum<-quagum(f=quantile, para=pelgum(samlmu(maxOL[seqq])))
    y0.pe3<-quape3(f=quantile, para=pelpe3(samlmu(maxOL[seqq])))
    y0.ln3<-qualn3(f=quantile, para=pelln3(samlmu(maxOL[seqq]),bound=0))
    shift[i,1]<-y0.wei
    shift[i,2]<-y0.gum
    shift[i,3]<-y0.pe3
    shift[i,4]<-y0.ln3
    
  }
  return(shift)
}


#################################################################################################
#################################################################################################
#################################################################################################
#################################################################################################
### 60 years 110 rivers 

##### AM
### (neu) shift 30 Jahre (AM-Methode)
shift<-matrix(NA,30,3)
colnames(shift)<-c("30","100","1000")
seqq<-NULL
shift_30<-function(df,quantile,...){  
  nSubsets<-(60)
  outList<-vector("list",length=nSubsets)
  maxOL<-vector("numeric",length=nSubsets)
  totRow<-nrow(df[[4]])
  
  for (i in seq_len(nSubsets)){
    rowsToGrab<-seq(i*365.25-365.25,(365.25*i),1)
    outList[[i]]<-df[[4]][rowsToGrab]
    maxOL[i]<-max(outList[[i]],na.rm=T)
  } 
  
  shift<-matrix(NA,30,4)
  colnames(shift)<-c("Wei","Gum","PE3","LN3")
  
  for (i in c(1:30)){
    
    seqq<-seq(i,29+i,1)
    y0.wei<-quawei(f=quantile, para=pelwei(samlmu(maxOL[seqq]),bound=0))
    y0.gum<-quagum(f=quantile, para=pelgum(samlmu(maxOL[seqq])))
    y0.pe3<-quape3(f=quantile, para=pelpe3(samlmu(maxOL[seqq])))
    y0.ln3<-qualn3(f=quantile, para=pelln3(samlmu(maxOL[seqq]),bound=0))
    shift[i,1]<-y0.wei
    shift[i,2]<-y0.gum
    shift[i,3]<-y0.pe3
    shift[i,4]<-y0.ln3
    
  }
  return(shift)
}

### MLE Gumbel 30/100/1000 return Periods (neu)
shift<-matrix(NA,50,3)
colnames(shift)<-c("30","100","1000")
seqq<-NULL
shift_30_mle<-function(df,rp,...){
  
  for (i in seq_along(1:50)){
    seqq<-seq(i*365.25-365.25,(29+i)*365.25,1)
    y0.gum<-fevd(df[[4]][seqq],type="Gumbel")
    rl<-ci(y0.gum,return.period = rp)
    shift[i,1]<-rl[4]
    shift[i,2]<-rl[5]
    shift[i,3]<-rl[6]
  }
  return(shift)
}
mean(listko[[31]][[4]])
y0.gum<-fevd(listko[[31]][[4]],type="Gumbel")
ci(y0.gum,return.period=100)
rl<-ci(y0.gum,return.period = 100)
### (AM) add HQ1000 to shift 30
shift<-matrix(NA,30,3)
colnames(shift)<-c("30","100","1000")
seqq<-NULL
shift_30_add<-function(df,quantile,...){
  df1<-shift_30(df,quantile=quantile)
  df[[4]][60*365.25]<-max(df[[4]]+1)
  df_c<-list(1,2,3,df[[4]])
  df_c1<-shift_30(df_c,quantile=quantile)
  wei<-df_c1[,1]-df1[,1]
  gum<-df_c1[,2]-df1[,2]
  pe3<-df_c1[,3]-df1[,3]
  ln3<-df_c1[,4]-df1[,4]
  
  wei_p<-(df_c1[,1]-df1[,1])/df_c1[,1]
  gum_p<-(df_c1[,2]-df1[,2])/df_c1[,2]
  pe3_p<-(df_c1[,3]-df1[,3])/df_c1[,3]
  ln3_p<-(df_c1[,4]-df1[,4])/df_c1[,4]
  o<-list(df1,df_c1,wei,gum,pe3,ln3,wei_p,gum_p,pe3_p,ln3_p)
  names(o)<-c("df1","df_c1","wei","gum","pe3","ln3","wei_p","gum_p","pe3_p","ln3_p")
  return(list(o))
}

### 80 years cummulated with and without add of HQ1000 (AM)
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
    y0.wei<-quawei(f=quantile, para=pelwei(samlmu(maxOL[seqq]),bound=0))
    y0.gum<-quagum(f=quantile, para=pelgum(samlmu(maxOL[seqq])))
    y0.pe3<-quape3(f=quantile, para=pelpe3(samlmu(maxOL[seqq])))
    y0.ln3<-qualn3(f=quantile, para=pelln3(samlmu(maxOL[seqq]),bound=0))
    cummul_80[i,1]<-y0.wei
    cummul_80[i,2]<-y0.gum
    cummul_80[i,3]<-y0.pe3
    cummul_80[i,4]<-y0.ln3
    
  }
  return(cummul_80)
}
cummul_80_f2<-function(df,quantile,...){
  
  cummul_80<-matrix(NA,50,4)
  colnames(cummul_80)<-c("Wei","Gum","PE3","LN3")
  
  for (i in c(1:50)){
    
    seqq<-seq(1,29+i,1)
    y0.wei<-quawei(f=quantile, para=pelwei(samlmu(df[[4]][seqq]),bound=0))
    y0.gum<-quagum(f=quantile, para=pelgum(samlmu(df[[4]][seqq])))
    y0.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[[4]][seqq])))
    y0.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[[4]][seqq]),bound=0))
    cummul_80[i,1]<-y0.wei
    cummul_80[i,2]<-y0.gum
    cummul_80[i,3]<-y0.pe3
    cummul_80[i,4]<-y0.ln3
    
  }
  return(cummul_80)
}
cum_80_add<-function(df,quantile,...){
  df1<-cummul_80_f(df,quantile=quantile)
  df_c<-list(1,2,3,append((max(df[[4]])+1),df[[4]]))
  df_c1<-cummul_80_f(df_c,quantile=quantile)
  wei<-df_c1[,1]-df1[,1]
  gum<-df_c1[,2]-df1[,2]
  pe3<-df_c1[,3]-df1[,3]
  ln3<-df_c1[,4]-df1[,4]
  
  wei_p<-(df_c1[,1]-df1[,1])/df_c1[,1]
  gum_p<-(df_c1[,2]-df1[,2])/df_c1[,2]
  pe3_p<-(df_c1[,3]-df1[,3])/df_c1[,3]
  ln3_p<-(df_c1[,4]-df1[,4])/df_c1[,4]
  o<-list(df1,df_c1,wei,gum,pe3,ln3,wei_p,gum_p,pe3_p,ln3_p)
  names(o)<-c("df1","df_c1","wei","gum","pe3","ln3","wei_p","gum_p","pe3_p","ln3_p")
  return(o)
}