setwd("/Users//oliverw/Studium/Watermanagement/Masterarbeit/data/")
data<-read.table("./Salzach_Q_Tagesmittel_18_02_15.csv",skip=22,header=F,sep=";",dec = ",")
date<-(data$V1)
date<-strptime(date,format = "%d.%m.%Y %H:%M")
Q_data<-as.matrix(data[,2])
year<-format(date,"%Y")
month<-format(date,"%m")
days<-format(date,"%d")
summary(Q_data)
MAM7_Data<-cbind(as.numeric(days),Q_data)

22281/14

### MAM (7)

## 2xMatrix
#
days_7<-rep(2001:5183,each=7)
MAM7_Data<-cbind(as.numeric(days_7),Q_data)
#
x7<-c(2001:5183)
x7_matrix<-matrix(NA,length(x7),2)
x7_matrix[,1]<-x7
Q_d<-Q_data

x7_matrix2<-x7_matrix

## loop
for (i in seq(along=x7_matrix[,1])){
  x7_matrix[i,2]<-min(MAM7_Data[MAM7_Data[,1]==x7_matrix[i,1],])
}


for (i in seq(along=x7_matrix2[,1])){
  x7_matrix2[i,2]<-min(Q_d[Q_d==MAM7_Data[,2] & MAM7_Data[,1]==x7_matrix2[i,1]])
}

x7_matrix2==x7_matrix
### MAM (14)
#
days_14<-rep(1:1591,each=14)
MAM14_Data<-cbind(as.numeric(days_14),Q_data[1:22274])
#
x14<-c(1:1591)
x14_matrix<-matrix(NA,length(x14),2)
x14_matrix[,1]<-x14
## loop
for (i in seq(along=x14_matrix[,1])){
  x14_matrix[i,2]<-min(Q_d[Q_d==MAM14_Data[,2] & MAM14_Data[,1]==x14_matrix[i,1]])
}



