naotab<-read.table("../NAO/monthly/norm.nao.monthly.b5001.current.ascii.txt",sep="",header=F,dec=".")
naomat<-as.matrix(naotab[,3])
plot(naotab[,3],type="l")
color2D.matplot(naomat,main="NAO",ylab="month",xlab="",
                extremes=c(1:30),show.legend=TRUE) 
mean(naotab[1:(12*30),3])

listko_y60<-listko
names_y60<-names(listko_y60)
#names_y100<-names_y100[-41] ### NUR EINMAL, da sie nicht die Länge über 36525 aufweisen !!! (NUR BEI 47)
#names_y100<-names_y100[-33] ### NUR EINMAL, da sie nicht die Länge über 36525 aufweisen !!! (NUR BEI 47)

names_y60<-names(listko_y60)
nam_y100<-gsub('a','',names_y100)
nam_y60<-gsub('a','',names_y60)
NROW(nam_y60)
for (i in seq_along(names_y60)){
write.table(file=paste("../ye60_all_no9/",nam_y60[i],".day",sep=""),x=listko_y60[[i]],sep=";",dec=".")
  
}

write.csv(file="../Names/nam_y60.csv",x=nam_y60)
#setwd("./Studium/Watermanagement/Masterarbeit/data/ye60_all_2/")

names<-read.csv("../ye60_all_no9/info.txt",sep=":",dec=".",comment.char = "",header=F)

names_on<-gsub('#','',names$V1)
names<-cbind(names_on,as.character(names$V2))


names_list<-matrix(NA,227,7)
for(i in seq(1,227,1)){
  seqq<-rep(c(1:7),1)
  seqq<-seqq+(i*7)
  names_list[i,c(1:7)]<-names[seqq,2]
  }
na<-matrix(NA,1,7)
na[1,c(1:7)]<-names[c(1:7),2]
names_list<-rbind(na,names_list)
#names_list<-names_list[-205,]
names_list[1,c(1:7)]<-names[c(1:7),2]
names_li<-cbind(names_list[,4],names_list[,c(2:3)],round(as.numeric(names_list[,5]),2),round(as.numeric(names_list[,6]),2),round(as.numeric(names_list[,7]),2),names_list[,1])
#names_li<-names_li[-46,] #NUR EINMAL! Da bei den 47 zuviele drinnen waren
for (i in seq(1,227,1)){
  for (j in seq(4,7,1)){
    names_li[i,j]<-paste("$",names_li[i,j],"$",sep="")
    
  }
}
for (i in seq(1,227,1)){
  for (j in seq(1,6,1)){
names_li[i,j]<-paste(names_li[i,j],"","&",sep="")
    
  }
}
for (i in seq(1,227,1)){
  for (j in seq(7,7,1)){
    names_li[i,j]<-paste(names_li[i,j],"","\\",sep="")
    
  }
}
for (i in seq(1,227,1)){
  for (j in seq(7,7,1)){
    names_li[i,j]<-paste(names_li[i,j],"","\\",sep="")
    
  }
}
for (i in seq(1,227,1)){
  for (j in seq(7,7,1)){
    names_li[i,j]<-paste(names_li[i,j],"","\\","cmidrule{2-7}",sep="")
    
  }
}


write.table(file="../table.txt",x=names_li,dec=".",quote=FALSE,row.names=FALSE)




