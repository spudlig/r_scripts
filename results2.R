# Vom Directory in eine Liste
largeList<-readTable(file_dir)
# Rename 
nam<-list.files("./",pattern=".day.txt")
nl<-sub(".day.txt","", nam,ignore.case = T)
namList<-grep("[0-9]{7}",nl,value=T)
namList<-paste("a",namList,sep="")
names(largeList)[c(1:length(file_dir))]<-namList
# Die DF in der Liste rechentauglich machen
formList<-lapply(largeList[c(1:length(file_dir))],get.matrix)
### nur noch ohne -999 -> sind genau 9 (bei y120)
lltry<-lapply(formList[1:length(file_dir)],kickoutnine)
listko<-lltry[!sapply(lltry, is.null)]


