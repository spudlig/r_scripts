split_1<-listko_y100
split_max<-function(df,...){
nSubsets<-(100)
outList<-vector("list",length=nSubsets)
maxOL<-vector("numeric",length=nSubsets)
totRow<-nrow(df[[4]])

for (i in seq_len(nSubsets)){
  rowsToGrab<-seq(i*365.25-365.25,(365.25*i),1)
  outList[[i]]<-df[[4]][rowsToGrab]
  maxOL[i]<-max(outList[[i]],na.rm=T)
}
return(maxOL)
}
max_split<-list()
max_split<-lapply(listko_y100[1:47],split_max)

split_1<-max_split[1:50]
split_2<-max_split[51:100]


