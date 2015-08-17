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
aha3<-lapply(listko[1:40],quantile=0.99,cum_80_add)
aha3<-lapply(listko[1:40],quantile=0.99,cum_80_add)


mat<-matrix(NA,50,4)
for (i in seq_along(aha3)){
  mat[i,1]<-sum(aha3[[i]]$wei_p)/50
  mat[i,2]<-sum(aha3[[i]]$gum_p)/50
  mat[i,3]<-sum(aha3[[i]]$pe3_p)/50
  mat[i,4]<-sum(aha3[[i]]$ln3_p)/50
}
mat2<-matrix(NA,1,4)
mat2[,1]<-sum(mat[,1],na.rm=T)
mat2[,2]<-sum(mat[,2],na.rm=T)
mat2[,3]<-sum(mat[,3],na.rm=T)
mat2[,4]<-sum(mat[,4],na.rm=T)

aha3[[1]]$wei_p[1]

mat_wei<-matrix(NA,40,50)
mat_gum<-matrix(NA,40,50)
mat_pe3<-matrix(NA,40,50)
mat_ln3<-matrix(NA,40,50)

for (i in seq_along(1:40)){
  for (j in seq_along(1:50)){
  mat_wei[i,j]<-aha3[[i]]$wei_p[j]
  }
}
for (i in seq_along(1:40)){
  for (j in seq_along(1:50)){
    mat_gum[i,j]<-aha3[[i]]$gum_p[j]
  }
}
for (i in seq_along(1:40)){
  for (j in seq_along(1:50)){
    mat_pe3[i,j]<-aha3[[i]]$pe3_p[j]
  }
}
for (i in seq_along(1:40)){
  for (j in seq_along(1:50)){
    mat_ln3[i,j]<-aha3[[i]]$ln3_p[j]
  }
}

mat_all<-matrix(NA,50,4)
for(i in c(1:50)){
  mat_all[i,1]<-sum(mat_wei[,i])/50
  mat_all[i,2]<-sum(mat_gum[,i])/50
  mat_all[i,3]<-sum(mat_pe3[,i])/50
  mat_all[i,4]<-sum(mat_ln3[,i])/50
}
colnames(mat_all)<-c("wei","gum","pe3","ln3")

plot(mat_all[,1],type="l",ylim=c(min(mat_all),max(mat_all)),xlab="years",ylab="percent [%]")
lines(mat_all[,2],col=2)
lines(mat_all[,3],col=3)
lines(mat_all[,4],col=4)
mat_all[,3]
?samlmu
