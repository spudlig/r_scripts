### 60 Jahre split 30/30
###
setwd("./try/")
path="./"
out.file=""
dir.files<-dir(path,pattern=".txt")
temp.file<-(list.files(path))
for (file in seq_along(dir.files)){
  mat<-read.table(dir.files[file],header=F,sep=";",dec=".")
  out.file<-cbind(mat)
}

d<-lapply(dir.files,read.table)
str(d)
summary(d)
d[]
ma<-NULL
ma<-d[[1]]
ma2<-d[[2]]
ma3<-d[[3]]


