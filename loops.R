sqr<-seq(1,150,1)
sqr.sqr<-matrix(NA,length(sqr),2)
sqr.sqr[,1]<-sqr
sqr.out<-matrix(NA,1,2)
sqr.t<-matrix(NA,1,1)
for (i in nrow(sqr.sqr)){
  sqr.sqr[,3]<-sqr^2
  sqr.sqr[,2]<-sqrt(sqr)
  sqr.out[,1]<-max(sqr.sqr[,2])
  sqr.out[,2]<-min(sqr.sqr[,3])

}
sqr.null<-matrix(NA,length(sqr),1)

for (i in seq(along=sqr)){
  if(sqr[i]<10){
    sqr.null[i]<-as.matrix(sqr[i])
  } 
  else if (sqr[i]>50){
    sqr.null[i]<-as.matrix(sqr[i])
  }
  else if (sqr[i]>100){
    sqr.null[i]<-500
  }
  else {sqr.null[i]<-NA}
}

p<-seq(1,100,1)
r<-matrix(NA,length(p),1)

for (i in seq(along=p)){ 
if (p[i]<30){
  r<-as.matrix(seq(p[i]))
} 
} 

as.matrix(ifelse(sqr<10 | sqr>100,sqr,NA))

for (i in seq(along=sqr)){
if(sqr==20 && sqr>130){
  sqr.null<-as.matrix(sqr[i])
}
}


#################
#################

sam<-as.matrix(sample(c(1:42),42,replace=F))
r<-rep(1:7,each=6)
da_ma<-matrix(NA,42,2)
min_ma<-matrix(NA,7,2)
se<-as.matrix(seq(1:4))
v<-c(1:7)
min_ma[,1]<-v

da_ma<-cbind(as.numeric(r),sam)


for (i in seq(along=da_ma)){
  if (da_ma[,1]==1){
    min_ma[1,]<-min(da_ma[,2])
  }
}

  else if (da_ma [,1==2]){
    min_ma[,2]<-min(da_ma[,2])
  else if (da_ma [,1==3]){
    min_ma[,3]<-min(da_ma[,2])
  else if (da_ma [,1==4]){
    min_ma[,4]<-min(da_ma[,2])
  }  
  }
}


for (i in 1:ncol(min_ma)){
  if (da_ma[i,1]==se[i,1]){
    min_ma[1,i]<-min(da_ma[da_ma[i,2]==se[i,1],])
  }
}


min_ma[,1]<-c(1:4)
for (i in 1:nrow(min_ma)){
  min<-da_ma[da_ma[,1]==min_ma[i,1]]
  min_ma[i,2]<-min(min[i,2],na.rm=T)
  
}

###############
###############

da_ma<-as.matrix(sample(c(1:140),140,replace=T))
r<-rep(2001:2007,each=20)
da_ma<-cbind(as.numeric(r),da_ma)
v<-c(2001:2007)
min_ma<-matrix(NA,length(v),2)
min_ma[,1]<-v

for (i in 1:nrow(min_ma)) {
  mim<-da_ma[da_ma[,1]==min_ma[i,1],]
  min_ma[i,2]<-sum(mim[i,2],na.rm=T)
}
#
Q_Data<-as.matrix(Data[,2])
year<-format(Date,format="%Y")
month<-format(Date,format="%m")

Q_Data<-cbind(as.numeric(year),Q_Data)
x<-c(1951:2011)
Annualsum<-matrix(NA,length(x),2)
Annualsum[,1]<-x

for (i in 1:nrow(Annualsum)) {
  Annual_Q<-Q_Data[Q_Data[,1]==Annualsum[i,1],]
  Annualsum[i,2]<-sum(Annual_Q[i,2],na.rm=T)
}


try<-cbind(Q_Data[1:365,1],Q_Data[1:365,2])
sum(try[,2])
mean(try[,2])
