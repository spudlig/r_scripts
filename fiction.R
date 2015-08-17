### Function: Takes the river [df] dependend, takes via AM the first 30 years, fits calculates the L-MOM
### and fits for wei/gum/pe3/ln3. Then Produces, depending on the [year], random variables.
### Then sifts in a 30year intervall over the random max-years and calcuates [quantile] dependend
### a HQ100 (e.g.)
fakeit<-function(df,quantile,year,...){  
  nSubsets<-(length(df[[4]])/365.25)
  outList<-vector("list",length=nSubsets)
  maxOL<-vector("numeric",length=nSubsets)
  totRow<-nrow(df[[4]])
  
  for (i in seq_len(nSubsets)){
    rowsToGrab<-seq(i*365.25-365.25,(365.25*i),1)
    outList[[i]]<-df[[4]][rowsToGrab]
    maxOL[i]<-max(outList[[i]],na.rm=T)
  } 
  
  nryr<-(year*365.25)
  
  lmwei<-pelwei(samlmu(maxOL[1:30]),bound = 0)
  lmgum<-pelgum(samlmu(maxOL[1:30]))
  lmpe3<-pelpe3(samlmu(maxOL[1:30]))
  lmln3<-pelln3(samlmu(maxOL[1:30]),bound=0)
  
  dfwei<-rweibull(nryr,scale = lmwei[2],shape=lmwei[3])
  dfgum<-rgumbel(nryr,location=lmgum[2],scale =lmgum[1])
  dfpe3<-rpearsonIII(nryr,scale=lmpe3[2],shape=lmpe3[3],location=lmpe3[1])
  dfln3<-rlnorm3(nryr,scale = lmln3[2],shape = lmln3[3])
  
  shift<-matrix(NA,(year-30),4)
  colnames(shift)<-c("Wei","Gum","PE3","LN3")
  
  for (i in c(1:(year-30))){
    
    seqq<-seq(i,29+i,1)
    y0.wei<-quawei(f=quantile, para=pelwei(samlmu(dfwei[seqq]),bound=0))
    y0.gum<-quagum(f=quantile, para=pelgum(samlmu(dfgum[seqq])))
    y0.pe3<-quape3(f=quantile, para=pelpe3(samlmu(dfpe3[seqq])))
    y0.ln3<-qualn3(f=quantile, para=pelln3(samlmu(dfln3[seqq]),bound=0))
    shift[i,1]<-y0.wei
    shift[i,2]<-y0.gum
    shift[i,3]<-y0.pe3
    shift[i,4]<-y0.ln3
    
  }
  return(shift)
}
fakeit2<-function(df,quantile,year,...){  
  nSubsets<-(length(df[[4]])/365.25)
  outList<-vector("list",length=nSubsets)
  maxOL<-vector("numeric",length=nSubsets)
  totRow<-nrow(df[[4]])
  
  for (i in seq_len(nSubsets)){
    rowsToGrab<-seq(i*365.25-365.25,(365.25*i),1)
    outList[[i]]<-df[[4]][rowsToGrab]
    maxOL[i]<-max(outList[[i]],na.rm=T)
  } 
  
  nryr<-(year*365.25)
  
  lmwei<-pelwei(samlmu(maxOL),bound = 0)
  lmgum<-pelgum(samlmu(maxOL))
  lmpe3<-pelpe3(samlmu(maxOL))
  lmln3<-pelln3(samlmu(maxOL),bound=0)
  
  dfwei<-rweibull(nryr,scale = lmwei[2],shape=lmwei[3])
  dfgum<-rgumbel(nryr,location=lmgum[2],scale =lmgum[1])
  dfpe3<-rpearsonIII(nryr,scale=lmpe3[2],shape=lmpe3[3],location=lmpe3[1])
  dfln3<-rlnorm3(nryr,scale = lmln3[2],shape = lmln3[3])
  
  shift<-matrix(NA,(year-30),4)
  colnames(shift)<-c("Wei","Gum","PE3","LN3")
  
  for (i in c(1:(year-30))){
    
    seqq<-seq(1,29+i,1)
    y0.wei<-quawei(f=quantile, para=pelwei(samlmu(dfwei[seqq]),bound=0))
    y0.gum<-quagum(f=quantile, para=pelgum(samlmu(dfgum[seqq])))
    y0.pe3<-quape3(f=quantile, para=pelpe3(samlmu(dfpe3[seqq])))
    y0.ln3<-qualn3(f=quantile, para=pelln3(samlmu(dfln3[seqq]),bound=0))
    shift[i,1]<-y0.wei
    shift[i,2]<-y0.gum
    shift[i,3]<-y0.pe3
    shift[i,4]<-y0.ln3
    
  }
  return(shift)
}

### Calculates the AM for the whole time series. For Comparison Reasons. maxOL as output, then
### input into - listko40 deswegen, da die Reihe über die ganze Länge geht
### listko40[[2]] und listko[[8]] sind je 6142150
listko_y60<-listko
nSubsets<-(length(listko_y100[[4]][[4]])/365.25)
outList<-vector("list",length=nSubsets)
maxOL<-vector("numeric",length=nSubsets)
totRow<-nrow(listko_y100[[4]][[4]])
for (i in seq_len(nSubsets)){
  rowsToGrab<-seq(i*365.25-365.25,(i*365.25),1)
  outList[[i]]<-listko_y100[[4]][[4]][rowsToGrab]
  maxOL[i]<-max(outList[[i]],na.rm=T)
}
quawei(f=0.99,para=c(pelwei(samlmu(maxOL))))
quagum(f=0.99,para=c(pelgum(samlmu(maxOL))))
quape3(f=0.99,para=c(pelpe3(samlmu(maxOL))))
qualn3(f=0.99,para=c(pelln3(samlmu(maxOL))))

(shift_30(listko[[7]],quantile = 0.99))
lapply(listko_y100,quantile=0.99,shift_30)


plot(shift_30(listko_y100[[7]],quantile = 0.99)[,1],type="l")
lines(rep(mean(faitmape3),50),col="red")
lines(rep(ffpe,50),col="green")
lines(rep(shift_30(listko[[7]],quantile = 0.99)[1,3],50),type="l",col=6)
lines(rep(shift_30(listko[[7]],quantile = 0.99)[50,3],50),type="l",col=5)
faitma<-matrix(NA,100,1)
maxOL
faitmam<-matrix(NA,1,1)
faitmam[,1]<-mean(replicate(100,mean((fakeit(listko[[i]],quantile=0.99,year=10000))[,1])))
faitmam[,1]<-mean(fakeit2(listko[[1]],quantile=0.99,year=10000))

### Calculation: For all 110/40 Rivers the fakeit function into the faitma (depending on how many rivers
### one has to change the row nr)
set.seed(1)
faitma<-matrix(NA,1,4)
for (i in seq_along(faitma)){
  faitma[1,1]<-mean(replicate(100,mean((fakeit2(listko[[8]],quantile=0.99,year=1000))[,1])))
  faitma[1,2]<-mean(replicate(100,mean((fakeit2(listko[[8]],quantile=0.99,year=1000))[,2])))
  faitma[1,3]<-mean(replicate(100,mean((fakeit2(listko[[8]],quantile=0.99,year=1000))[,3])))
  faitma[1,4]<-mean(replicate(100,mean((fakeit2(listko[[8]],quantile=0.99,year=1000))[,4])))
}
set.seed(1)
faitmam<-matrix(NA,1000,4)
faitmam[,1]<-(replicate(100,mean((fakeit2(listko[[8]],quantile=0.99,year=1000))[,1])))
faitmam[,2]<-(replicate(100,mean((fakeit2(listko[[8]],quantile=0.99,year=1000))[,2])))
faitmam[,3]<-(replicate(100,mean((fakeit2(listko[[8]],quantile=0.99,year=1000))[,3])))
faitmam[,4]<-(replicate(100,mean((fakeit2(listko[[8]],quantile=0.99,year=1000))[,4])))

set.seed(1)
faitmam9<-matrix(NA,1000,4)
faitmam9[,1]<-(replicate(100,mean((fakeit2(listko[[9]],quantile=0.99,year=1000))[,1])))
faitmam9[,2]<-(replicate(100,mean((fakeit2(listko[[9]],quantile=0.99,year=1000))[,2])))
faitmam9[,3]<-(replicate(100,mean((fakeit2(listko[[9]],quantile=0.99,year=1000))[,3])))
faitmam9[,4]<-(replicate(100,mean((fakeit2(listko[[9]],quantile=0.99,year=1000))[,4])))

set.seed(1)
faitmah9<-matrix(NA,100,4)
faitmah9[,1]<-(replicate(100,mean((fakeit2(listko[[9]],quantile=0.99,year=100))[,1])))
faitmah9[,2]<-(replicate(100,mean((fakeit2(listko[[9]],quantile=0.99,year=100))[,2])))
faitmah9[,3]<-(replicate(100,mean((fakeit2(listko[[9]],quantile=0.99,year=100))[,3])))
faitmah9[,4]<-(replicate(100,mean((fakeit2(listko[[9]],quantile=0.99,year=100))[,4])))

set.seed(1)
faitmat9<-matrix(NA,10000,4)
faitmat9[,1]<-(replicate(100,mean((fakeit2(listko[[9]],quantile=0.99,year=10000))[,1])))
faitmat9[,2]<-(replicate(100,mean((fakeit2(listko[[9]],quantile=0.99,year=10000))[,2])))
faitmat9[,3]<-(replicate(100,mean((fakeit2(listko[[9]],quantile=0.99,year=10000))[,3])))
faitmat9[,4]<-(replicate(100,mean((fakeit2(listko[[9]],quantile=0.99,year=10000))[,4])))

### compare the faitma - always a bit confusing, no order

mean(faitmat9[,1])
mean(faitmah9[,4])
(shift_30(listko[[9]],quantile = 0.99)[1,4])
mean(faitmam9[,4])
qualn3(f=0.99,para=c(pelln3(samlmu(maxOL))))
quawei(f=0.99,para=c(pelwei(samlmu(maxOL))))

(shift_30(listko[[8]],quantile = 0.99)[1,1])
mean(faitmam[,1])
quawei(f=0.99,para=c(pelwei(samlmu(maxOL))))
(shift_30(listko[[8]],quantile = 0.99)[1,2])
mean(faitmam[,2])
quagum(f=0.99,para=c(pelgum(samlmu(maxOL))))
(shift_30(listko[[8]],quantile = 0.99)[1,3])
mean(faitmam[,3])
quape3(f=0.99,para=c(pelpe3(samlmu(maxOL))))
(shift_30(listko[[8]],quantile = 0.99)[1,4])
mean(faitmam[,4])
qualn3(f=0.99,para=c(pelln3(samlmu(maxOL))))


matrix_fait<-list()
matrix_fait<-lapply(listko_y60[1:3],quantile=0.99,year=10000,fakeit2)
plot(matrix_fait[[1]][,4],type="l")
matrix_fait[[1]]
proc.time(lapply(listko_y60[1:3],quantile=0.99,year=10000,fakeit2))
gc.time(lapply(listko_y60[1:3],quantile=0.99,year=10000,fakeit2))
#############

### Calculation; Dependend on i, several Rivers via fakeit Function
### Produces 10000 Random max years, for wei/gum/pe3/ln3. Then moves with 30years over them, and
### calculates the mean of all the 10000y/30y. This is repeated 100 times (depending on how many rivers
### one has to change the row nr)

faitma<-matrix(NA,5,4)
for (i in seq_along(faitmaweim)){
  faitma[i,1]<-mean(replicate(100,mean((fakeit(listko[[i]],quantile=0.99,year=50))[,1])))
  faitma[i,2]<-mean(replicate(100,mean((fakeit(listko[[i]],quantile=0.99,year=50))[,2])))
  faitma[i,3]<-mean(replicate(100,mean((fakeit(listko[[i]],quantile=0.99,year=50))[,3])))
  faitma[i,4]<-mean(replicate(100,mean((fakeit(listko[[i]],quantile=0.99,year=50))[,4])))
}

mean(faitmawei1)

mean(faitmaln31)
mean((shift_30(listko[[1]],quantile = 0.99)[,4]))
mean(shift_30(listko[[2]],quantile = 0.99)[,2])
length(listko[[12]][[4]])
?rnorm
faitma
kk<-fakeit(listko[[1]],quantile=0.99,year=10000)
kkt<-fakeit(listko[[1]],quantile=0.99,year=1000)
kkh<-fakeit(listko[[1]],quantile=0.99,year=100)
kkn<-shift_30(listko[[1]],quantile=0.99)
head(kkn)
max(kk[,1])
max(kk[,2])
max(kk[,3])
max(kk[,4])
max(kkn[,1])
kkn[c(1:30),1]
# Weibul
mean(kk[,1])
mean(kkt[,1])
mean(kkh[,1])
mean(kkn[,1])

max(kkh[,1])
max(kkt[,1])
max(kk[,1])
max(kkn[,1])
#Gumbel
mean(kk[,2])
mean(kkt[,2])
mean(kkh[,2])
mean(kkn[,2])

max(kkh[,2])
max(kkt[,2])
max(kk[,2])
max(kkn[,2])
# pe3
mean(kk[,3])
mean(kkt[,3])
mean(kkh[,3])
mean(kkn[,3])

max(kk[,3])
max(kkt[,3])
max(kkh[,3])
max(kkn[,3])
#ln3
mean(kk[,4])
mean(kkt[,4])
mean(kkh[,4])
mean(kkn[,4])

max(kk[,4])
max(kkt[,4])
max(kkh[,4])
max(kkn[,4])


max(kk[,1])-max(kkt[,1])
max(kk[,2])-max(kkt[,2])
max(kk[,3])-max(kkt[,3])
max(kk[,4])-max(kkt[,4])
var(kk[,1])-var(kkt[,1])
var(kk[,2])-var(kkt[,2])
var(kk[,3])-var(kkt[,3])
var(kk[,4])-var(kkt[,4])
var(kkt[,1])
var(kk[,1])


kj<-shift_30(listko[[1]],quantile=0.99)

plot(kj[,1])
plot(kk[,1],ylim=c(280,600))
lines(kj[,1],col="red")
setwd("./Studium//Watermanagement/Masterarbeit/")
llk<-matrix(NA,90,4)
dev.off()
plot(kk[,1],type="l",ylim=c(250,600))
for(i in seq_along(c(1:20))){
  llk<-fakeit(listko[[1]],quantile=0.99,year=1000)
  lines(llk[,1],type="l",col=c(i))
}

saveGIF({for(i in seq_along(c(1:10))){ 
  llk<-fakeit(listko[[1]],quantile=0.99,year=90)
  plot(llk[,1],type="l")
      }})

fakeit<-function(df,year){
  nr_yr<-(year*365.25)
  
  lmwei<-pelwei(samlmu(df[[4]]),bound = 0)
  lmgum<-pelgum(samlmu(df[[4]]))
  lmpe3<-pelpe3(samlmu(df[[4]]))
  lmln3<-pelln3(samlmu(df[[4]]),bound=0)
  
  dfwei<-rweibull(n=nr_yr,scale=lmwei[2],shape=lmwei[3])
  dfgum<-rgumbel(n=nr_yr,scale=lmwei[2],shape=lmwei[3])
  dfpe3<-rpearsonIII(n=nr_yr,scale=lmwei[2],shape=lmwei[3])
  dfln3<-rlnorm3(n=nr_yr,scale=lmwei[2],shape=lmwei[3])
  
  
  
}

lmwei<-pelwei(samlmu(listko[[1]][[4]]),bound=0)
lmgum<-pelgum(samlmu(listko[[1]][[4]]))
lmpe3<-pelpe3(samlmu(listko[[1]][[4]]))
lmln3<-pelln3(samlmu(listko[[1]][[4]]),bound=0)

lmwei
lmgum
lmpe3
lmln3
?rgumbel
trwei<-rweibull(365.25*60,scale = lmwei[2],shape=lmwei[3])
trgum<-rgumbel(365.25*60,location=lmgum[2],scale =lmgum[1])
trpe3<-rpearsonIII(365.25*60,scale=lmpe3[2],shape=lmpe3[3],location=lmpe3[1])
trln3<-rlnorm3(365.25*60,scale = lmln3[2],shape = lmln3[3])
summary(trln3)
summary(trwei)
summary(trgum)
summary(trpe3)
summary(listko[[1]][[4]])


hist(trwei)
hist(trgum)
hist(trln3)
hist(trpe3)
hist(listko[[1]][[4]],freq=FALSE,col="gray")

mean(trwei)
mean(trgum)
mean(trpe3)
mean(trln3)
mean(listko[[1]][[4]])

summary(trln3)
summary(trwei)
summary(trgum)
summary(trpe3)
summary(listko[[1]][[4]])
