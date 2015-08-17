### Versuch 30 Jahr zu sliden
# 40 Jahre sind 365*40+10 (10 Tage für die Schaltjahre, alle 4 Jahre)
# für die ersten 40 Jahre, für die zweiten 40 Jahre 

y0<-c(2*356:y2)
y1
y2
y1<-1*365+365*40+10
y2<-2*365+365*40+10
y3<-3*365+365*40+10
y4<-4*365+365*40+10
y5<-5*365+365*40+10

yFun<-function(df){
  y0<-365*40+10
  y1<-1*365+365*40+10
  y2<-2*365+365*40+10
  y3<-3*365+365*40+10
  y4<-4*365+365*40+10
  y5<-5*365+365*40+10
  y6<-6*365+365*40+10
  y7<-7*365+365*40+10
  y8<-8*365+365*40+10
  y9<-9*365+365*40+10
  yseq0<-seq(1,y0,1)
  yseq1<-seq(1*365,y1,1)
  yseq2<-seq(2*365,y2,1)
  yseq3<-seq(3*365,y3,1)
  yseq4<-seq(4*365,y4,1)
  yseq5<-seq(6*365,y5,1)
  yseq6<-seq(7*365,y6,1)
  yseq7<-seq(8*365,y7,1)
  yseq8<-seq(9*365,y8,1)
  yseq9<-seq(10*365,y9,1)
  yAll<-(as.numeric(df[[4]]))
  y40<-mean(yAll[yseq0])
  y41<-mean(yAll[yseq1])
  y42<-mean(yAll[yseq2])
  y43<-mean(yAll[yseq3])
  y44<-mean(yAll[yseq4])
  y45<-mean(yAll[yseq5])
  y46<-mean(yAll[yseq6])
  y47<-mean(yAll[yseq7])
  y48<-mean(yAll[yseq8])
  y49<-mean(yAll[yseq9])
  
  matr<-c(y40,y41,y42,y43,y44,y45,y46,y47,y48,y49)
  return(matr)
}
dfS<-(lapply(formList[c(1:length(file_dir))],yFun))
min(dfS[[1]])
dfS[1]
min(formList[[14]][4])
### y120 DF mit jeweils keinem -999 in den Daten der formList (formatierten Liste) 
### 2,4,5,6,7,8,10,11,12 ->c("6335060.day.txt", "6335500.day.txt", "6337100.day.txt",
### "6337200.day.txt", "6337400.day.txt", "6337514.day.txt", "6340120.day.txt", 
### "6343100.day.txt", "6343500.day.txt")


plot(dfS[[6]],type="line")
lines(dfS[[2]])
