### Download from web page
library("utils")
if(pattern=("203[0-9][0-9][0-9]")){
  
nam<-list.files("./",pattern=".day.txt")
}
for (i in seq_along(0:9)){
  for (j in seq_along(0:9)){
    for (k in seq_along(0:9)){
      
      if (regmatches(url.show(paste("http://ehyd.gv.at/eHYD/MessstellenExtraData/owf?id=203",i,j,k,"&file=4",sep="")==
                                regexpr(paste("203",i,j,k,sep=""))){
        download.file(url = paste("http://ehyd.gv.at/eHYD/MessstellenExtraData/owf?id=203",i,j,k,"&file=4",sep=""),
                     destfile = paste("../203",i,j,k,".txt",sep=""))
      }
      
    }
  }
  
}

url<-list(NULL)
for (i in seq_along(0:9)){      
  int<-list.files(paste("http://ehyd.gv.at/eHYD/MessstellenExtraData/owf?id=20342",i,"&file=4",sep=""))
      }
  
}

url<-url.show(paste("http://ehyd.gv.at/eHYD/MessstellenExtraData/owf?id=20342",i,"&file=4",sep=""),delete.file=t)
if (regmatches(url[i],regexpr(paste("20342",i,sep=""),url[i],perl=T))==paste("20342",i,sep="")){
  download.file(url = paste("http://ehyd.gv.at/eHYD/MessstellenExtraData/owf?id=20342",i,"&file=4",sep=""),
                destfile = paste("../20342",i,".txt",sep=""))
  
listt<-read.table("../Internet/Export_output.txt",sep=",",header=T)
m<-regexpr("2[0-9]{}",listt,perl=T)
regmatches(listt[,2],m)
dli<-function(X){
download.file(url = paste("http://ehyd.gv.at/eHYD/MessstellenExtraData/owf?id=",X,"&file=4",sep=""),
              destfile = paste("../Internet/",X,".txt",sep=""))
}
listtt<-as.matrix(listt[,2])
apply(listtt,1,dli)

setwd("../Internet/")

path<-"./"
file_dir<-dir(path,pattern=".txt")
readt<-function(path){
  tab<-try(read.csv(path,sep=";",dec=",",header=F,skip=35),silent = T)
  if(class(tab)=='try-error') { 
    tab=NULL
  }
    else { if(class(tab)!='try-error'){
    tab=read.csv(path,sep=";",dec=",",header=F)
  }}
  return(tab)
}
ehydList<-NULL
ehydList<-lapply(file_dir[c(1:40)],readt)
if (ehydList[[2]][[2]]=="L\xfccke"){
  ehydList<-NULL
}
dd<-ehydList[[2]][[2]]=="L\xfccke"
ehydList[[2]][[2]]
dd
ddf<-unlist(ehydList[[2]][2])
fd<-cbind(ddf,dd)
ffd<-fd[dd==F,]
summary(ffd)
ehydList[[2]][ehydList=="L\xfccke"]
80*365.25+40
for (i in seq_along(ehydList)){
  if (length(ehydList[[i]][[2]])<=29260){
    ehydList[[i]]<-NULL
  }
  
}
str(ehydList)


(file_dir[[2]])
gj<-try(read.csv("./200063.txt",sep=";",dec=",",blank.lines.skip=T,fill=F),silent = T)
kk<-paste(path,file_dir[[1]],sep="")

gj<-read.table("/Users//oliverw/Studium/Watermanagement//2014 WS/ExtremeS/homew/2/file2.txt",sep=";",dec=",",header=F,as.is=T)
ta<-list()
ta<-try(read.csv("./200022.txt",sep=";",dec=",",header=F,blank.lines.skip=T,fill=T),silent = T)
if(class(ta)=='try-error') { 
  ta=NULL 
} 
ta




plot(1/2x^2) 
