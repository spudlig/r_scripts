######### CI with bootst - zuerst werden die functionen definiert, die wiederholt werden müssen, dann die matrix fürs
######### reinschreiben und anschließend die for schleife.
#### schleife lässt über die maximalwerte über 30 jahre iterieren, unterdrückt die warunungen und benütz die funktionen.
#### funktionen deswegen immer mit 1:4 2:4 etc. geschreiben, da ich immer unterschiedliche momente für die CI hernehmen möchte.
library(lmom)
library(boot)
library(FAdist)
library(PearsonDS)
nboot <- 10000 # Number of simulations
alpha <- .05 # alpha level
rawmax<-maxOL # raw data (AM)

## generate random 10000years with 100year input from listko_y100[[1]]. Change the fakeyears matrix, if you want to change
## the generated number of years.
set.seed(1)
fakeyears<-matrix(NA,10000,4)
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


return(fakeyears)
}
fakeyears<-getfakeyears(listko_y100[[1]],10000)

shift_fait<-list()
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
plot(shift_fait_nex[,1][c(1:200)],type="l")
lines(shift_fait_ac[,1][c(1:200)],col="red")
plot(shift_fait_nex[,2][c(1:200)],type="l")
lines(shift_fait_ac[,2][c(1:200)],col="red")
plot(shift_fait_nex[,3][c(1:200)],type="l")
lines(shift_fait_ac[,3][c(1:200)],col="red")
plot(shift_fait_nex[,4][c(1:200)],type="l")
lines(shift_fait_ac[,4][c(1:200)],col="red")
#### Works (!!!) only for 1000 years, not longer (try with 10000 and a small boot R and look at the error)
### 1. set up function 2. boot function 3. for loop 4. all wei/gum/pe3/ln3
# 1. seqq needs to be booted 10000 times - use boot_xxx function 2. then the boot.ci needs to be calculated 
# 3. extract 
# CI for all functions accumulated <- ZEITINTENSIV!!!
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
system.time()
plot.new()
p=seq(1,960,1)
par(mfrow=c(2,2))
plot(bootwei[,3],type="l",ylim=c(2500,4500),xlab = "Years",ylab="Discharge[m^3/s]",main="WEI")
lines(bootwei[,1],col="gray")
lines(bootwei[,2],col="gray")
polygon(c(p,rev(p)),c(bootwei[,1][c(1:960)],rev(bootwei[,2][c(1:960)])), col = "grey50")
lines(bootwei[,3],col="black")
lines(rep(shift_fait[,1][[9970]],960))

plot(bootgum[,3],type="l",ylim=c(3000,5700),xlab = "Years",ylab="Discharge[m^3/s]",main="GUM")
lines(bootgum[,1],col="gray")
lines(bootgum[,2],col="gray")
polygon(c(p,rev(p)),c(bootgum[,1][c(1:960)],rev(bootgum[,2][c(1:960)])), col = "grey50")
lines(bootgum[,3],col="black")
lines(rep(shift_fait[,2][[9000]],960))

plot(bootpe3[,3],type="l",ylim=c(4000,8500),xlab = "Years",ylab="Discharge[m^3/s]",main="PE3")
lines(bootpe3[,1],col="gray")
lines(bootpe3[,2],col="gray")
polygon(c(p,rev(p)),c(bootpe3[,1][c(1:960)],rev(bootpe3[,2][c(1:960)])), col = "grey50")
lines(bootpe3[,3],col="black")
lines(rep(shift_fait[,3][[9000]],960))

plot(bootln3[,3],type="l",,ylim=c(2500,5700),xlab = "Years",ylab="Discharge[m^3/s]",main="LN3")
lines(bootln3[,1],col="gray")
lines(bootln3[,2],col="gray")
polygon(c(p,rev(p)),c(bootln3[,1][c(1:960)],rev(bootln3[,2][c(1:960)])), col = "grey50")
lines(bootln3[,3],col="black")
lines(rep(shift_fait[,4][[9000]],960))

wei_lm<-(lm(rep(shift_fait[,1][9000],960)~bootwei[,3][c(1:960)]))
summary(wei_lm)


# 
