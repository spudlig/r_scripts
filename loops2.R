### 2xMatrix
da_ma<-matrix(NA,42,2)
min_ma<-matrix(NA,7,2)
### What comes in
sam<-as.matrix(sample(c(1:42),42,replace=F))
r<-rep(2001:2007,each=6)
x<-c(2001:2007)
### Filling
da_ma<-cbind(as.numeric(r),sam)
min_ma[,1]<-as.numeric(x)
min_m<-min_ma[,2]
### loops
for (i in seq(along=min_ma[,1])){
  min_ma[i,2]<-min(da_ma[da_ma[,1]==min_ma[i,1],])
}
### Loop for annual sum 
for (i in seq(along=Annualsum[,1])){
  Annualsum[i,2]<-sum(Q_D[Q_D==Q_Data[,2] & Q_Data[,1]==Annualsum[i,1]])
}

