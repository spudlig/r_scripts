libs <- c('lmom','boot','dplyr','evd','extRemes', 'FAdist', 'fields', 'MASS','gplots','dtw', 
          'ismev', 'maps', 'Hmisc', 'PearsonDS','stats', 'stats4','fitdistrplus','logspline','TSclust',
          'cluster','rworldmap','rworlxtra','pwr','ggExtra','ggplot2','corrplot')
lapply(libs, require, character.only = T)
# FIRST create two folders with names ye60_all_3 and ye100_all, then RUN g60sten.sh and g100sten.sh with console
# then set wd to the folder of interest
setwd("~/Studium/Watermanagement/Masterarbeit/data/ye60_all_3/")
#setwd("~/Desktop/MASTERARBEIT WIEDER/ye100_all/")

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
# dann via Hand die leeren .txt files aussortieren
largeList<-readTable(file_dir)
# Rename 
nam<-list.files("./",pattern=".day.txt")
nl<-sub(".day.txt","", nam,ignore.case = T)
namList<-grep("[0-9]{7}",nl,value=T)
namList<-paste("a",namList,sep="")
names(largeList)[c(1:length(file_dir))]<-namList
# Die DF in der Liste rechentauglich machen;;
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
### nur noch ohne -999
kickoutnine<-function(df){
  if(min(df[[4]])=="-999"){
    df=NULL
  } 
  return(df)
}
lltry<-lapply(formList[1:length(file_dir)],kickoutnine)
listko<-lltry[!sapply(lltry, is.null)]
for (i in seq_along(listko)){
  if(nrow(listko[[i]]) < 21915){
    listko[i]<-list(NULL)
  }
} ### VORSICHT mit < > als 21000 nrow !!!
listko<-listko[!sapply(listko, is.null)]

### depending on wich list (y60 or y100), remove the "#" and let it run!
listko_y60<-listko
#listko_y100<-listko

# get coordinates for table
rawmapll<-read.table("./long.txt",comment.char = "",sep=":",header=F)
rawmapll[,1]<-NULL
mapll<-matrix(NA,410,3) # 410 for y60/// 282 for y100
names<-dput(names(listko))
seqq1<-seq(1,1230,3) # 1230 for y60 /// 846 for y100
seqq2<-seq(2,1230,3)
seqq3<-seq(3,1230,3)
mapll[,1]<-as.character(rawmapll$V2[seqq1])
mapll[,2]<-(rawmapll$V2[seqq2])
mapll[,3]<-(rawmapll$V2[seqq3])
maa<-matrix(NA,227,3) # 227 for y60 /// 45 for y100

for (i in seq_along(maa)){
  for (j in seq_along(maa)){
    if(names[i] == paste("a",mapll[j],sep="")){
      maa[i,1]<-paste("a",mapll[j,1],sep="")
      maa[i,2]<-mapll[j,2]
      maa[i,3]<-mapll[j,3]
    }
  }
}
colnames(maa)<-c("Nr","Lat","Long")
samps<-maa[,c(2:3)] # depending on which list (listko_y60 or listko_y100)

##### AMS get max years. depending on wich list...
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
maxOL_list_y60<-lapply(listko_y60[c(1:227)],AMS_get)
#maxOL_listY100<-lapply(listko_y100[c(1:45)],AMS_get)
maxOL<-maxOL_list_y60[[15]]
#maxOL<-maxOL_list_y100[[15]]
################################################ Shift 30 Jahre (AM-Methode) for 227 rivers
shiftIT<-function(df,quantile,yrs_avail,yrs_cal,...){  
  nSubsets<-(length(df[[4]])/365.25)
  outList<-vector("list",length=nSubsets)
  maxOL<-vector("numeric",length=nSubsets)
  totRow<-nrow(df[[4]])
  
  for (i in seq_len(nSubsets)){
    rowsToGrab<-seq(i*365.25-365.25,(365.25*i),1)
    outList[[i]]<-df[[4]][rowsToGrab]
    maxOL[i]<-max(outList[[i]],na.rm=T)
  } 
  nyr<-yrs_avail/2
  nyrs<-yrs_cal-1
  shift<-matrix(NA,nyr,4)
  colnames(shift)<-c("Wei","Gum","PE3","LN3")
  
  for (i in seq_along(shift[,1])){
    
    seqq<-seq(i,nyrs+i,1)
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
} #yrs_cal means 30...50 years shift window
# yrs_avail means what listko is used (30,100). It is diveded by two in the function

sh<-shiftIT(listko[[31]],quantile=0.99,yrs_avail = 60,yrs_cal = 30)
shift<-lapply(listko[1:227],quantile=0.99,shiftIT,yrs_avail=60,yrs_cal=30)

# make df standardize
shiftst<-shift
for (i in seq_along(shiftst)){
  for (j in seq(1,4,1)){
    shiftst[[i]][,j]<-scale(shiftst[[i]][,j])
  }
}
plot(shiftst[[1]][,1],type="l",ylim=c(-3,3))
for (i in seq_along(shiftst)){
  lines(shiftst[[i]][,1],col=i,type="l")
}

#### GoF 
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
} # gamma as pearsonIII

plot(gof[,1],type="l",col="red",ylim=c(500,550))
lines(gof[,2],col="green")
lines(gof[,3],col="blue")
lines(gof[,4],col="orange")
lines(gof[,5],col="lightgreen")
lines(gof[,6],col="purple")

shift_30_227<-lapply(listko_y60[1:227],quantile=0.99,yrs_avail=60,yrs_cal=30,shiftIT)
shiftst_30_227<-shift_30_227
for (i in seq_along(shiftst_30_227)){
  for (j in seq(1,4,1)){
    shiftst_30_227[[i]][,j]<-scale(shiftst_30_227[[i]][,j])
  }
}
################################################ Shift 30,40,50 Jahre (AM-Methode) for 45 rivers (100years long)
######################## FIRST GET y100 list!!!!!!!!!
shiftIT<-function(df,quantile,yrs_avail,yrs_cal,...){  
  nSubsets<-(length(df[[4]])/365.25)
  outList<-vector("list",length=nSubsets)
  maxOL<-vector("numeric",length=nSubsets)
  totRow<-nrow(df[[4]])
  
  for (i in seq_len(nSubsets)){
    rowsToGrab<-seq(i*365.25-365.25,(365.25*i),1)
    outList[[i]]<-df[[4]][rowsToGrab]
    maxOL[i]<-max(outList[[i]],na.rm=T)
  } 
  nyrs<-yrs_avail-yrs_cal-1
  shift<-matrix(NA,nyrs,4)
  colnames(shift)<-c("Wei","Gum","PE3","LN3")
  
  for (i in seq_along(shift[,1])){
    
    seqq<-seq(i,yrs_cal+i,1)
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
} #yrs_avail means either 60 years (than the 30 
#year shift is calculated). can be applied for listko_y60 and listko_y100. for listko_y100 only 80 and 100
# should be applied for 40 year shift and 50year shift

shift_40<-lapply(listko_y100[1:45],quantile=0.99,yrs_avail=100,yrs_cal=40,shiftIT)
shift_50<-lapply(listko_y100[1:45],quantile=0.99,yrs_avail=100,yrs_cal=50,shiftIT)
shift_30<-lapply(listko_y100[1:45],quantile=0.99,yrs_avail=100,yrs_cal=30,shiftIT)

# make df standardize
shiftst_50<-shift_50
shiftst_40<-shift_40
shiftst_30<-shift_30
for (i in seq_along(shiftst_50)){
  for (j in seq(1,4,1)){
    shiftst_50[[i]][,j]<-scale(shiftst_50[[i]][,j])
  }
}
for (i in seq_along(shiftst_40)){
  for (j in seq(1,4,1)){
    shiftst_40[[i]][,j]<-scale(shiftst_40[[i]][,j])
  }
}
for (i in seq_along(shiftst_30)){
  for (j in seq(1,4,1)){
    shiftst_30[[i]][,j]<-scale(shiftst_30[[i]][,j])
  }
}
plot(shiftst_30[[1]][,1],type="l",ylim=c(-3,3))
lines(shiftst_30[[1]][,2],col="red")
lines(shiftst_30[[1]][,3],col="green")
lines(shiftst_30[[1]][,4],col="blue")
plot(shiftst_40[[1]][,1],type="l",ylim=c(-3,3))
lines(shiftst_40[[1]][,2],col="red")
lines(shiftst_40[[1]][,3],col="green")
lines(shiftst_40[[1]][,4],col="blue")
plot(shiftst_50[[1]][,1],type="l",ylim=c(-3,3))
lines(shiftst_50[[1]][,2],col="red")
lines(shiftst_50[[1]][,3],col="green")
lines(shiftst_50[[1]][,4],col="blue")

for (i in seq_along(shiftst_30)){
  lines(shiftst_30[[i]][,1],col=i,type="l")
}
lines(shiftst_50[[1]][,1],col="red")
lines(shiftst_40[[1]][,1],col="green")
lines(shiftst_30[[1]][,1],col="blue")

######################## Accumulate
accIT<-function(df,quantile,yrs_avail,...){  
  nSubsets<-(length(df[[4]])/365.25)
  outList<-vector("list",length=nSubsets)
  maxOL<-vector("numeric",length=nSubsets)
  totRow<-nrow(df[[4]])
  
  for (i in seq_len(nSubsets)){
    rowsToGrab<-seq(i*365.25-365.25,(365.25*i),1)
    outList[[i]]<-df[[4]][rowsToGrab]
    maxOL[i]<-max(outList[[i]],na.rm=T)
  } 
  
  nyrs<-yrs_avail-30
  shift<-matrix(NA,nyrs,4)
  colnames(shift)<-c("Wei","Gum","PE3","LN3")
  
  for (i in seq_along(shift[,1])){
    
    seqq<-seq(1,29+i,1)
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
} #yrs_avail means either 60 years (than the 30 

acc_y100<-lapply(listko_y100,quantile=0.99,yrs_avail=100,accIT)

acc_y100_st<-acc_y100
for (i in seq_along(acc_y100_st)){
  for (j in seq(1,4,1)){
    acc_y100_st[[i]][,j]<-scale(acc_y100_st[[i]][,j])
  }
}

plot(acc_y100[[3]][,1],type="l",ylim=c(6000,13000))
lines(acc_y100[[3]][,2],col="red")
lines(acc_y100[[3]][,3],col="green")
lines(acc_y100[[3]][,4],col="blue")

for (i in seq_along(acc_y100_st)){
  lines(acc_y100_st[[i]][,1],col=i,type="l")
}

######### MLE -> Leider zu wenig n (bei einem Shift von 30 Jahre viel zu wenig) (AM)
for (i in seq_len(nSubsets)){
  rowsToGrab<-seq(i*365.25-365.25,(365.25*i),1)
  outList[[i]]<-df[[4]][rowsToGrab]
  maxOL[i]<-max(outList[[i]],na.rm=T)
} 

### Funktioniert mit MLE, start Wert sind immer etwas fragwürdig, aber, wie auch im "Leitfade" angesrochen, bei nur 60-Werten ist der
### Fehler massiv hoch. Nicht brauchbar. (AM)

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


####################################################### get fake years
set.seed(1)
fakeyears<-matrix(NA,2000,4)
getfakeyears<-function(df,year,...){
  nSubsets<-(length(df[[4]])/365.25)
  outList<-vector("list",length=nSubsets)
  maxOL<-vector("numeric",length=nSubsets)
  totRow<-nrow(df[[4]])
  
  for (i in seq_len(nSubsets)){
    rowsToGrab<-seq(i*365.25-365.25,(365.25*i),1)
    outList[[i]]<-df[[4]][rowsToGrab]
    maxOL[i]<-max(outList[[i]],na.rm=T)
  } 
  
  nryr<-(year)
  
  lmwei<-pelwei(samlmu(maxOL),bound = 0)
  lmgum<-pelgum(samlmu(maxOL))
  lmpe3<-pelpe3(samlmu(maxOL))
  lmln3<-pelln3(samlmu(maxOL),bound=0)
  
  fakeyears[,1]<-rweibull(nryr,scale = lmwei[2],shape=lmwei[3])
  fakeyears[,2]<-rgumbel(nryr,location=lmgum[1],scale =lmgum[2])
  fakeyears[,3]<-rpearsonIII(nryr,scale=lmpe3[2],shape=lmpe3[3],location=lmpe3[1])
  fakeyears[,4]<-rlnorm3(nryr,scale = lmln3[2],shape = lmln3[3])
  ??gumbel
  
  return(fakeyears)
}
fakeyears<-getfakeyears(listko_y100[[3]],2000)

### Compare the accumulated 1:29+i years for weibull function with the shift i:29+i
### not yet with the set.seed accumulated plus CI
shift_fait_ac<-function(df,quantile,year,...){  
  shift<-matrix(NA,(year-30),4)
  colnames(shift)<-c("Wei","Gum","PE3","LN3")
  
  for (i in c(1:(year-30))){
    
    seqq<-seq(1,29+i,1)
    y0.wei<-quawei(f=quantile, para=pelwei(samlmu(df[,1][seqq]),bound=0))
    y0.gum<-quagum(f=quantile, para=pelgum(samlmu(df[,2][seqq])))
    y0.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[,3][seqq])))
    y0.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[,4][seqq]),bound=0))
    shift[i,1]<-y0.wei
    shift[i,2]<-y0.gum
    shift[i,3]<-y0.pe3
    shift[i,4]<-y0.ln3
    
  }
  return(shift)
} # accumulative (1:29+i)
shift_fait_nex<-function(df,quantile,year,...){  
  shift<-matrix(NA,(year-30),4)
  colnames(shift)<-c("Wei","Gum","PE3","LN3")
  
  for (i in c(1:(year-30))){
    
    seqq<-seq(i,29+i,1)
    y0.wei<-quawei(f=quantile, para=pelwei(samlmu(df[,1][seqq]),bound=0))
    y0.gum<-quagum(f=quantile, para=pelgum(samlmu(df[,2][seqq])))
    y0.pe3<-quape3(f=quantile, para=pelpe3(samlmu(df[,3][seqq])))
    y0.ln3<-qualn3(f=quantile, para=pelln3(samlmu(df[,4][seqq]),bound=0))
    shift[i,1]<-y0.wei
    shift[i,2]<-y0.gum
    shift[i,3]<-y0.pe3
    shift[i,4]<-y0.ln3
    
  }
  return(shift)
} # i:29+i

shift_fait_ac<-shift_fakeit_ac(fakeyears,quantile=0.99,year=10000)
shift_fait_nex<-shift_fakeit_nex(fakeyears,quantile=0.99,year=10000)
# compare the accumulative and the shift via plot
par(mfrow=c(2,2))
ntimes=400
plot(shift_fait_nex[,1][c(1:ntimes)],type="l",main="wei")
lines(shift_fait_ac[,1][c(1:ntimes)],col="red")
lines(rep(shift_fait_ac[,1][9000],ntimes),col="green")
plot(shift_fait_nex[,2][c(1:ntimes)],type="l",main="gum")
lines(shift_fait_ac[,2][c(1:ntimes)],col="red")
lines(rep(shift_fait_ac[,2][9000],ntimes),col="green")
plot(shift_fait_nex[,3][c(1:ntimes)],type="l",main="pe3")
lines(shift_fait_ac[,3][c(1:ntimes)],col="red")
lines(rep(shift_fait_ac[,3][9000],ntimes),col="green")
plot(shift_fait_nex[,4][c(1:ntimes)],type="l",main="ln3")
lines(shift_fait_ac[,4][c(1:ntimes)],col="red")
lines(rep(mean(shift_fait_ac[,4][4000:9900]),ntimes),col="green")
# CI for all functions accumulated 1:29+i <- ZEITINTENSIV!!!

alpha=0.05
boot_wei <- function(x,i) { 
  y0.wei<-quawei(f=0.99, para=pelwei(samlmu(x[i]),bound=0))
}
boot_gum <- function(x,i) { 
  y0.gum<-quagum(f=0.99, para=pelgum(samlmu(x[i])))
}
boot_pe3 <- function(x,i) { 
  y0.pe3<-quape3(f=0.99, para=pelpe3(samlmu(x[i])))
}
boot_ln3 <- function(x,i) { 
  y0.ln3<-qualn3(f=0.99, para=pelln3(samlmu(x[i]),bound=0))
}
bootwei<-matrix(NA,1000,3)
bootgum<-matrix(NA,1000,3)
bootpe3<-matrix(NA,1000,3)
bootln3<-matrix(NA,1000,3)
for (i in seq(1:9970)){
  seqq<-seq(1,29+i,1)  
  bootwei_0 <- suppressWarnings(boot(fakeyears[,1][seqq], boot_wei, R=10000))  
  bootgum_0 <- suppressWarnings(boot(fakeyears[,2][seqq], boot_gum, R=10000))  
  bootpe3_0 <- suppressWarnings(boot(fakeyears[,3][seqq], boot_pe3, R=10000))  
  bootln3_0 <- suppressWarnings(boot(fakeyears[,4][seqq], boot_ln3, R=10000))  
  if(i %in% seq(1,10000,100)){
    cat("loop", i, "\n") 
  }  
  for (j in seq(1:3)){
    bootwei[i,j]<-suppressWarnings(append((boot.ci(bootwei_0, conf=(1-alpha))$bca[4:5]),bootwei_0$t0[[1]],after=2)[[j]])
    bootgum[i,j]<-suppressWarnings(append((boot.ci(bootgum_0, conf=(1-alpha))$bca[4:5]),bootgum_0$t0[[1]],after=2)[[j]])
    bootpe3[i,j]<-suppressWarnings(append((boot.ci(bootpe3_0, conf=(1-alpha))$bca[4:5]),bootpe3_0$t0[[1]],after=2)[[j]])
    bootln3[i,j]<-suppressWarnings(append((boot.ci(bootln3_0, conf=(1-alpha))$bca[4:5]),bootln3_0$t0[[1]],after=2)[[j]])
  }
}

########## CI with shift 30 years for listko_y60 maxOL_list_y60. R=1000
CI_shift_60_wei<-matrix(NA,30,3)
CI_shift_60_gum<-matrix(NA,30,3)
CI_shift_60_pe3<-matrix(NA,30,3)
CI_shift_60_ln3<-matrix(NA,30,3)
CI_shift_y60_wei<-list()
CI_shift_y60_gum<-list()
CI_shift_y60_pe3<-list()
CI_shift_y60_ln3<-list()
for(j in seq(1,227,1)){
  if(j %in% seq(1,227,1)){
    cat("river", j, "\n") 
  }
  for (k in seq(1:30)){
    seqq<-seq(k,29+k,1)
    boot_shift60_wei<-suppressWarnings(boot(maxOL_list[[j]][seqq],boot_wei, R=1000))
    boot_shift60_gum<-suppressWarnings(boot(maxOL_list[[j]][seqq],boot_gum, R=1000))
    boot_shift60_pe3<-suppressWarnings(boot(maxOL_list[[j]][seqq],boot_pe3, R=1000))
    boot_shift60_ln3<-suppressWarnings(boot(maxOL_list[[j]][seqq],boot_ln3, R=1000))
    if(k %in% seq(1,30,1)){
      cat("loop", k, "\n") 
    }
    for (l in seq(1:3)){
      CI_shift_60_wei[k,l]<-suppressWarnings(append((boot.ci(boot_shift60_wei, conf=(1-alpha))$bca[4:5]),boot_shift60_wei$t0[[1]],after=2)[[l]])
      CI_shift_60_gum[k,l]<-suppressWarnings(append((boot.ci(boot_shift60_gum, conf=(1-alpha))$bca[4:5]),boot_shift60_gum$t0[[1]],after=2)[[l]])
      CI_shift_60_pe3[k,l]<-suppressWarnings(append((boot.ci(boot_shift60_pe3, conf=(1-alpha))$bca[4:5]),boot_shift60_pe3$t0[[1]],after=2)[[l]])
      CI_shift_60_ln3[k,l]<-suppressWarnings(append((boot.ci(boot_shift60_ln3, conf=(1-alpha))$bca[4:5]),boot_shift60_ln3$t0[[1]],after=2)[[l]])
    }
  }
  CI_shift_y60_wei[j]<-list(CI_shift_60_wei)
  CI_shift_y60_gum[j]<-list(CI_shift_60_gum)
  CI_shift_y60_pe3[j]<-list(CI_shift_60_pe3)
  CI_shift_y60_ln3[j]<-list(CI_shift_60_ln3)
}

########## CI with listko_y100 for shift of 30,40,50 years. Change maxOL_list according to interest
maxOL_list<-maxOL_listY100
CI_shift_100_wei_30<-matrix(NA,70,3)
CI_shift_100_gum_30<-matrix(NA,70,3)
CI_shift_100_pe3_30<-matrix(NA,70,3)
CI_shift_100_ln3_30<-matrix(NA,70,3)
CI_shift_y100_wei_30<-list()
CI_shift_y100_gum_30<-list()
CI_shift_y100_pe3_30<-list()
CI_shift_y100_ln3_30<-list()
CI_shift_100_wei_40<-matrix(NA,60,3)
CI_shift_100_gum_40<-matrix(NA,60,3)
CI_shift_100_pe3_40<-matrix(NA,60,3)
CI_shift_100_ln3_40<-matrix(NA,60,3)
CI_shift_y100_wei_40<-list()
CI_shift_y100_gum_40<-list()
CI_shift_y100_pe3_40<-list()
CI_shift_y100_ln3_40<-list()
CI_shift_100_wei_50<-matrix(NA,50,3)
CI_shift_100_gum_50<-matrix(NA,50,3)
CI_shift_100_pe3_50<-matrix(NA,50,3)
CI_shift_100_ln3_50<-matrix(NA,50,3)
CI_shift_y100_wei_50<-list()
CI_shift_y100_gum_50<-list()
CI_shift_y100_pe3_50<-list()
CI_shift_y100_ln3_50<-list()
## for maxOL_Y100 with shift 30 (seqq needs to go from 1:70, as its 100years)
for(j in seq(1,45,1)){
  if(j %in% seq(1,45,1)){
    cat("river", j, "\n") 
  }
  for (k in seq(1:70)){
    seqq<-seq(k,29+k,1)
    boot_shift100_wei_30<-suppressWarnings(boot(maxOL_list[[j]][seqq],boot_wei, R=1000))
    boot_shift100_gum_30<-suppressWarnings(boot(maxOL_list[[j]][seqq],boot_gum, R=1000))
    boot_shift100_pe3_30<-suppressWarnings(boot(maxOL_list[[j]][seqq],boot_pe3, R=1000))
    boot_shift100_ln3_30<-suppressWarnings(boot(maxOL_list[[j]][seqq],boot_ln3, R=1000))
    if(k %in% seq(1,70,1)){
      cat("loop", k, "\n") 
    }
    for (l in seq(1:3)){
      CI_shift_100_wei_30[k,l]<-suppressWarnings(append((boot.ci(boot_shift100_wei_30, conf=(1-alpha))$bca[4:5]),boot_shift100_wei_30$t0[[1]],after=2)[[l]])
      CI_shift_100_gum_30[k,l]<-suppressWarnings(append((boot.ci(boot_shift100_gum_30, conf=(1-alpha))$bca[4:5]),boot_shift100_gum_30$t0[[1]],after=2)[[l]])
      CI_shift_100_pe3_30[k,l]<-suppressWarnings(append((boot.ci(boot_shift100_pe3_30, conf=(1-alpha))$bca[4:5]),boot_shift100_pe3_30$t0[[1]],after=2)[[l]])
      CI_shift_100_ln3_30[k,l]<-suppressWarnings(append((boot.ci(boot_shift100_ln3_30, conf=(1-alpha))$bca[4:5]),boot_shift100_ln3_30$t0[[1]],after=2)[[l]])
    }
  }
  CI_shift_y100_wei_30[j]<-list(CI_shift_100_wei_30)
  CI_shift_y100_gum_30[j]<-list(CI_shift_100_gum_30)
  CI_shift_y100_pe3_30[j]<-list(CI_shift_100_pe3_30)
  CI_shift_y100_ln3_30[j]<-list(CI_shift_100_ln3_30)
}

## for maxOL_Y100 with shift 40
for(j in seq(1,45,1)){
  if(j %in% seq(1,45,1)){
    cat("river", j, "\n") 
  }
  for (k in seq(1:60)){
    seqq<-seq(k,39+k,1)
    boot_shift100_wei_40<-suppressWarnings(boot(maxOL_list[[j]][seqq],boot_wei, R=1000))
    boot_shift100_gum_40<-suppressWarnings(boot(maxOL_list[[j]][seqq],boot_wei, R=1000))
    boot_shift100_pe3_40<-suppressWarnings(boot(maxOL_list[[j]][seqq],boot_wei, R=1000))
    boot_shift100_ln3_40<-suppressWarnings(boot(maxOL_list[[j]][seqq],boot_wei, R=1000))
    if(k %in% seq(1,60,1)){
      cat("loop", k, "\n") 
    }
    for (l in seq(1:3)){
      CI_shift_100_wei_40[k,l]<-suppressWarnings(append((boot.ci(boot_shift100_wei_40, conf=(1-alpha))$bca[4:5]),boot_shift100_wei_40$t0[[1]],after=2)[[l]])
      CI_shift_100_gum_40[k,l]<-suppressWarnings(append((boot.ci(boot_shift100_gum_40, conf=(1-alpha))$bca[4:5]),boot_shift100_gum_40$t0[[1]],after=2)[[l]])
      CI_shift_100_pe3_40[k,l]<-suppressWarnings(append((boot.ci(boot_shift100_pe3_40, conf=(1-alpha))$bca[4:5]),boot_shift100_pe3_40$t0[[1]],after=2)[[l]])
      CI_shift_100_ln3_40[k,l]<-suppressWarnings(append((boot.ci(boot_shift100_ln3_40, conf=(1-alpha))$bca[4:5]),boot_shift100_ln3_40$t0[[1]],after=2)[[l]])
    }
  }
  CI_shift_y100_wei_40[j]<-list(CI_shift_100_wei_40)
  CI_shift_y100_gum_40[j]<-list(CI_shift_100_gum_40)
  CI_shift_y100_pe3_40[j]<-list(CI_shift_100_pe3_40)
  CI_shift_y100_ln3_40[j]<-list(CI_shift_100_ln3_40)
}

## for maxOL_Y100 with shift 50
for(j in seq(1,45,1)){
  if(j %in% seq(1,45,1)){
    cat("river", j, "\n") 
  }
  for (k in seq(1:50)){
    seqq<-seq(k,49+k,1)
    boot_shift100_wei_50<-suppressWarnings(boot(maxOL_list[[j]][seqq],boot_wei, R=1000))
    boot_shift100_gum_50<-suppressWarnings(boot(maxOL_list[[j]][seqq],boot_wei, R=1000))
    boot_shift100_pe3_50<-suppressWarnings(boot(maxOL_list[[j]][seqq],boot_wei, R=1000))
    boot_shift100_ln3_50<-suppressWarnings(boot(maxOL_list[[j]][seqq],boot_wei, R=1000))
    if(k %in% seq(1,50,1)){
      cat("loop", k, "\n") 
    }
    for (l in seq(1:3)){
      CI_shift_100_wei_50[k,l]<-suppressWarnings(append((boot.ci(boot_shift100_wei_50, conf=(1-alpha))$bca[4:5]),boot_shift100_wei_50$t0[[1]],after=2)[[l]])
      CI_shift_100_gum_50[k,l]<-suppressWarnings(append((boot.ci(boot_shift100_gum_50, conf=(1-alpha))$bca[4:5]),boot_shift100_gum_50$t0[[1]],after=2)[[l]])
      CI_shift_100_pe3_50[k,l]<-suppressWarnings(append((boot.ci(boot_shift100_pe3_50, conf=(1-alpha))$bca[4:5]),boot_shift100_pe3_50$t0[[1]],after=2)[[l]])
      CI_shift_100_ln3_50[k,l]<-suppressWarnings(append((boot.ci(boot_shift100_ln3_50, conf=(1-alpha))$bca[4:5]),boot_shift100_ln3_50$t0[[1]],after=2)[[l]])
    }
  }
  CI_shift_y100_wei_50[j]<-list(CI_shift_100_wei_50)
  CI_shift_y100_gum_50[j]<-list(CI_shift_100_gum_50)
  CI_shift_y100_pe3_50[j]<-list(CI_shift_100_pe3_50)
  CI_shift_y100_ln3_50[j]<-list(CI_shift_100_ln3_50)
}


### Plot fakeyears boot functions for 1000 years
p=seq(1,960,1)
par(mfrow=c(2,2))
plot(bootwei[,3],type="l",ylim=c(2500,4500),xlab = "Years",ylab="Discharge[m^3/s]",main="WEI")
lines(bootwei[,1],col="gray")
lines(bootwei[,2],col="gray")
polygon(c(p,rev(p)),c(bootwei[,1][c(1:960)],rev(bootwei[,2][c(1:960)])), col = "grey50")
lines(bootwei[,3],col="black")
lines(rep(bootwei[,3][[960]],960))

plot(bootgum[,3],type="l",ylim=c(3000,5700),xlab = "Years",ylab="Discharge[m^3/s]",main="GUM")
lines(bootgum[,1],col="gray")
lines(bootgum[,2],col="gray")
polygon(c(p,rev(p)),c(bootgum[,1][c(1:960)],rev(bootgum[,2][c(1:960)])), col = "grey50")
lines(bootgum[,3],col="black")
lines(rep(bootgum[,3][[960]],960))

plot(bootpe3[,3],type="l",ylim=c(4000,8500),xlab = "Years",ylab="Discharge[m^3/s]",main="PE3")
lines(bootpe3[,1],col="gray")
lines(bootpe3[,2],col="gray")
polygon(c(p,rev(p)),c(bootpe3[,1][c(1:960)],rev(bootpe3[,2][c(1:960)])), col = "grey50")
lines(bootpe3[,3],col="black")
lines(rep(bootpe3[,3][[960]],960))

plot(bootln3[,3],type="l",,ylim=c(2500,5700),xlab = "Years",ylab="Discharge[m^3/s]",main="LN3")
lines(bootln3[,1],col="gray")
lines(bootln3[,2],col="gray")
polygon(c(p,rev(p)),c(bootln3[,1][c(1:960)],rev(bootln3[,2][c(1:960)])), col = "grey50")
lines(bootln3[,3],col="black")
lines(rep(bootln3[,3][[960]],960))

####################################### y100 shift with 30/40/50/80/100/150/200/300 over fakeyears
shiftIT_fake<-function(df,quantile,yrs_avail,yrs_cal,...){  
  maxOL<-df
  nyrs<-yrs_avail-yrs_cal-1
  shift<-matrix(NA,nyrs,4)
  colnames(shift)<-c("Wei","Gum","PE3","LN3")
  
  for (i in seq_along(shift[,1])){
    
    seqq<-seq(i,yrs_cal+i,1)
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
} #yrs_avail means either 60 years (than the 30 
#year shift is calculated). can be applied for listko_y60 and listko_y100. for listko_y100 only 80 and 100
# should be applied for 40 year shift and 50year shift

#for weibull fakeyears 30 years shift
shift_fake_wei30<-shiftIT_fake(fakeyears[,1],0.99,2000,30)

#for weibull fakeyears 40 years shift
shift_fake_wei40<-shiftIT_fake(fakeyears[,1],0.99,2000,40)

#for weibull fakeyears 50 years shift
shift_fake_wei50<-shiftIT_fake(fakeyears[,1],0.99,2000,50)

#for weibull fakeyears 80 years shift
shift_fake_wei80<-shiftIT_fake(fakeyears[,1],0.99,2000,80)

#for weibull fakeyears 100 years shift
shift_fake_wei100<-shiftIT_fake(fakeyears[,1],0.99,2000,100)

#for weibull fakeyears 1500 years shift
shift_fake_wei150<-shiftIT_fake(fakeyears[,1],0.99,2000,150)

#for weibull fakeyears 200 years shift
shift_fake_wei200<-shiftIT_fake(fakeyears[,1],0.99,2000,200)

#for weibull fakeyears 300 years shift
shift_fake_wei300<-shiftIT_fake(fakeyears[,1],0.99,2000,300)

#for weibull fakeyears 500 years shift
shift_fake_wei500<-shiftIT_fake(fakeyears[,1],0.99,2000,500)

### Plot fakeyears boot functions for 1000 years
p=seq(1,960,1)
par(mfrow=c(2,2))
plot(bootwei[,3],type="l",ylim=c(2500,4500),xlab = "Years",ylab="Discharge[m^3/s]",main="WEI")
lines(bootwei[,1],col="gray")
lines(bootwei[,2],col="gray")
polygon(c(p,rev(p)),c(bootwei[,1][c(1:960)],rev(bootwei[,2][c(1:960)])), col = "grey50")
lines(bootwei[,3],col="black")
lines(rep(bootwei[,3][[960]],960))

plot(bootgum[,3],type="l",ylim=c(3000,5700),xlab = "Years",ylab="Discharge[m^3/s]",main="GUM")
lines(bootgum[,1],col="gray")
lines(bootgum[,2],col="gray")
polygon(c(p,rev(p)),c(bootgum[,1][c(1:960)],rev(bootgum[,2][c(1:960)])), col = "grey50")
lines(bootgum[,3],col="black")
lines(rep(bootgum[,3][[960]],960))

plot(bootpe3[,3],type="l",ylim=c(4000,8500),xlab = "Years",ylab="Discharge[m^3/s]",main="PE3")
lines(bootpe3[,1],col="gray")
lines(bootpe3[,2],col="gray")
polygon(c(p,rev(p)),c(bootpe3[,1][c(1:960)],rev(bootpe3[,2][c(1:960)])), col = "grey50")
lines(bootpe3[,3],col="black")
lines(rep(bootpe3[,3][[960]],960))

plot(bootln3[,3],type="l",,ylim=c(2500,5700),xlab = "Years",ylab="Discharge[m^3/s]",main="LN3")
lines(bootln3[,1],col="gray")
lines(bootln3[,2],col="gray")
polygon(c(p,rev(p)),c(bootln3[,1][c(1:960)],rev(bootln3[,2][c(1:960)])), col = "grey50")
lines(bootln3[,3],col="black")
lines(rep(bootln3[,3][[960]],960))


########################################################## Calculate difference

###### y100 shift
mean(CI_shift_y100_wei_50[[3]][,3])
mean(CI_shift_y100_wei_40[[3]][,3])
mean(CI_shift_y100_wei_30[[3]][,3])
var(CI_shift_y100_wei_50[[3]][,3])
var(CI_shift_y100_wei_40[[3]][,3])
var(CI_shift_y100_wei_30[[3]][,3])
sd(CI_shift_y100_wei_50[[3]][,3])
sd(CI_shift_y100_wei_40[[3]][,3])
sd(CI_shift_y100_wei_30[[3]][,3])
densityplot(~CI_shift_y100_wei_50[[3]][,3]+CI_shift_y100_wei_40[[3]][,3]+CI_shift_y100_wei_30[[3]][,3])


####### fakeyears, dont know if usefula, in the boot file there is a better df

df_wei_fk<-data.frame(rbind(as.matrix(bootwei30[complete.cases(bootwei30),][,3]),as.matrix(bootwei40[complete.cases(bootwei40),][,3]),as.matrix(bootwei50[complete.cases(bootwei50),][,3]),
                            as.matrix(bootwei80[complete.cases(bootwei80),][,3]),as.matrix(bootwei100[complete.cases(bootwei100),][,3]),as.matrix(bootwei200[complete.cases(bootwei200),][,3]),
                            as.matrix(bootwei300[complete.cases(bootwei300),][,3])),
                      rbind(as.matrix(rep("30 years",NROW(bootwei30[complete.cases(bootwei30),][,3]))),as.matrix(rep("40 years",NROW(bootwei40[complete.cases(bootwei40),][,3]))),as.matrix(rep("50 years",NROW(bootwei50[complete.cases(bootwei50),][,3]))),
                            as.matrix(rep("80 years",NROW(bootwei80[complete.cases(bootwei80),][,3]))),as.matrix(rep("100 years",NROW(bootwei100[complete.cases(bootwei100),][,3]))),as.matrix(rep("200 years",NROW(bootwei200[complete.cases(bootwei200),][,3]))),
                            as.matrix(rep("300 years",NROW(bootwei300[complete.cases(bootwei300),][,3])))))
colnames(df_wei_fk)<-c("value","years")

### get f-value ANOVA
plotmeans(df_wei_fk$value~df_wei_fk$years,digits=2, ccol="red", mean.labels=T, main="Plot of discharge means by different window shift")
boxplot(df_wei_fk$value~df_wei_fk$years, main="Plot of discharge means by different window shift", xlab="Window Shift", ylab="Discharge", col=rainbow(7))
btwn<-aov(df_wei_fk$value~df_wei_fk$years)
summary(btwn)
########################################### Try
df<-data.frame(cbind(shift_fake_wei500[c(1:1500)],shift_fake_wei300[c(1:1500)],shift_fake_wei30[c(1:1500)],shift_fake_wei50[c(1:1500)]))
colnames(df)<-c("ye500","ye300","ye30","ye50")
df
plot_center = ggplot(df,aes(x=ye300,y=ye30)) + 
  geom_point() +
  geom_smooth(method="lm")

ggMarginal(plot_center, type="histogram")
