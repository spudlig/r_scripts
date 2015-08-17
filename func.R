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
aha<-cummul_80_f(listko[[9]],quantile = 0.99)
aha2<-lapply(listko[1:40],quantile=0.99,cummul_80_f)

plot(aha2[[1]][,1],type="l",ylim=c(10))
plot(aha2[[1]][,1],type="l",ylim=c(min(aha2[[1]]),max(aha2[[1]])))
lines(aha2[[1]][,2],col=2)
lines(aha2[[1]][,3],col=3)
lines(aha2[[1]][,4],col=4)
aha2[[1]][,1]-aha2[[1]][,2]
aha22<-append(max(aha2[[1]]),aha2[[1]][,1])
cummul_80_f(aha22,quantile) 
cum_80_add<-function(df,quantile,...){
  df_c<-list(1,2,3,append(max(df[[4]]),df[[4]]))
  df2_c<-cummul_80_f(df_c,quantile=quantile)
  df3<-cummul_80_f(df,quantile=quantile)
  wei<-df2_c[,1]-df3[,1]
  gum<-df2_c[,2]-df3[,2]
  pe3<-df2_c[,3]-df3[,3]
  ln3<-df2_c[,4]-df3[,4]
  o<-list(df2_c,df3,wei,gum,pe3,ln3)
  o<-names(c("80_cum_add","80_cum","minus_wei","minus_gum","minus_pe3","minus_ln3"))
  return(o)
}
nana<-listko
dput(nana)
aha3<-lapply(listko[1:40],quantile=0.99,cum_80_add)
plot(aha3[[1]][,1],type="l",ylim=c(min(aha3[[1]]),max(aha3[[1]])),ylab)
lines(aha3[[1]][,2],col=2)
lines(aha3[[1]][,3],col=3)
lines(aha3[[1]][,4],col=4)
aha3[[1]]
