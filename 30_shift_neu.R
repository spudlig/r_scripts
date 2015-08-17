dev.off()
par(mfrow=c(2,1))
??para
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
sh<-shift_30(listko[[1]],quantile=0.99)
shift<-lapply(listko[1:40],quantile=0.99,shift_30)
plot(shift[[3]][,1],type="l")
lines(shift[[5]][,2],col=2)
lines(shift[[5]][,3],col=3)
lines(shift[[5]][,4],col=4)


shift_30_add<-function(df,quantile,...){
  df1<-shift_30(df,quantile=quantile)
  df_c<-list(1,2,3,append((max(df[[4]])+1),df[[4]]))
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
  return(o)
}
sh_add<-shift_30_add(listko[[1]],quantile=0.99)

#plot(shift[[1]][,2],type="l",ylim=(c(min(maxx),max(maxx))))
plot(shift[[3]][,2]/meann[3,],type="l",ylim=c(.5,1.5))

for (i in seq_along(1:40)){
  lines(shift[[i]][,2]/meann[i,],col=i)
}



maxx<-matrix(NA,40,1)
for (i in seq_along(1:40)){
  maxx[i,]<-max(shift[[i]][,2])
}

meann<-matrix(NA,40,1)
for (i in seq_along(1:40)){
  meann[i,]<-mean(shift[[i]][,2])
}
