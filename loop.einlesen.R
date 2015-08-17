install.packages("stringr")
library(stringr)
### How many years each file
setwd("./Studium/Watermanagement//Masterarbeit/data/")
getwd()
path="./try2/"
out.file<-""
file.names<-dir(path,pattern=".txt")
for(i in 1:length(file.names)){
  file[i]<-as.matrix(read.table(file.names[i],header=F,sep=";",dec=".",stringsAsFactors=F))
  out.file<-as.matrix(file[,2]-file[,1])
}
str(out.file)
tem<-read.table("./IDs.txt")
y60<-cbind(tem,out.file[,1])
names(y60)[2]<-("IDs and Amount")
write.table(y60,"./years.dif.txt",sep="\t")
min(out.file)
### 
path=./try/
read.table()
getwd()
nam<-list.files("./try/",patter=".day.txt")

assign(paste("6",i,sep=""),nam[[i]])

for (i in seq_along(nam)){
  ge<-read.table("./try/",sep=";",dec=".")
  names(a)[i]<-assign(paste('X',i,sep=""),nam[i])
}
getwd()
setwd("./try/")
path="./"
out.file<-""
file.names<-dir(path,pattern=".txt")
for(i in 1:length(file.names)){
  file<-read.table(file.names[i],header=F,sep=";",dec=".")
  out.file<-rbind(file)
}
out.file

temp.file[]=""
dataset=""
file_list<-list.files()
for (file in file_list){
  temp.file[i]<-read.table(file,header=F,sep=";",dec=".")
  dataset<-as.data.frame(cbind(temp.file[i],dataset))
}
file_list<-list.files()
for (file in file_list){
if (exists("dataset")){
  temp_dataset <-read.table(file, header=TRUE, sep=";",dec=".")
  dataset<-rbind(dataset, temp_dataset)
  rm(temp_dataset)
}
}

path<-("./try/")
dir.file<-dir(path,pattern=".txt")
dir.names<-list.files("./try/")
for (file in seq_along(dir.file)){
   temp.table<-

    for (names in seq_along(dir.names)){
      assign(dir.names[names],x[names])
      data.table<-read.table()
}
}
