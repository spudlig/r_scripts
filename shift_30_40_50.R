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

sh<-shift_30(listko[[31]],quantile=0.99)
shift<-lapply(listko[1:227],quantile=0.99,shift_30)

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

#### Get max years
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
maxOL_list_y60<-lapply(listko_y60[c(1:227)],AMS_get)
maxOL_listY100<-lapply(listko_y100[c(1:45)],AMS_get)

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

plot(gof[,1],type="l",col="red")
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
################################################ Shift 30,40,50 Jahre (AM-Methode) for 45 rivers

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

shift_50<-lapply(listko_y100[1:45],quantile=0.99,yrs_avail=100,yrs_cal=50,shiftIT)
shift_40<-lapply(listko_y100[1:45],quantile=0.99,yrs_avail=100,yrs_cal=40,shiftIT)
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
} #yrs_avail means either 60 years 

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






