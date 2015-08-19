install.packages("devtools")
library("devtools")
setwd("../")
create("packages")
boot_wei <- function(x,i) { 
  y0.wei<-quawei(f=0.99, para=pelwei(samlmu(x[i]),bound=0))
}

setwd("~/Studium/Watermanagement/Masterarbeit/data/data/")


#### function for one input
# file name needs to be put in within ""
# window_years -> in years, start_date : as "1903-01-01", then in the function there is a ;--:--; added
# set wd where the file is located (setwd(""))
# HQ 30,50,80,100,300,500 possible
AMS<-function(df,...){
  nSubsets<-NROW(df)/365.25
  outList<-vector("list",length=nSubsets)
  maxOL<-vector("numeric",length=nSubsets)
  totRow<-nrow(df)
  
  for (i in seq_len(nSubsets)){
    rowsToGrab<-seq(i*365.25-365.25,(365.25*i),1)
    outList[i]<-df[rowsToGrab]
    maxOL[i]<-max(outList[[i]],na.rm=T)
  } 
  return(maxOL)
}

shift_it<-function(df,quantile,yrs_avail,yrs_cal,...){  
  nSubsets<-(length(df)/365.25)
  outList<-vector("list",length=nSubsets)
  maxOL<-vector("numeric",length=nSubsets)
  
  for (i in seq_len(nSubsets)){
    rowsToGrab<-seq(i*365.25-365.25,(365.25*i),1)
    outList[[i]]<-df[rowsToGrab]
    maxOL[[i]]<-max(outList[[i]],na.rm=T)
  } 
  nyrs<-yrs_avail-yrs_cal-1
  shift<-matrix(NA,nyrs,4)
  colnames(shift)<-c("Wei","Gum","PE3","LN3")
  
  for (i in seq_along(shift[,1])){
    
    seqq<-seq(i,yrs_cal+i,1)
    y0.wei<-quawei(f=quantile, para=pelwei(samlmu(maxOL[seqq]),bound=0))
    y0.gum<-quagum(f=quantile, para=pelgum(samlmu(maxOL[seqq])))
    y0.pe3<-quape3(f=quantile, para=pelpe3(samlmu(maxOL[seqq])))
    y0.ln3<-qualn3(f=quantile, para=pelln3(samlmu(maxOL[seqq]),bound=0))
    shift[i,1]<-y0.wei
    shift[i,2]<-y0.gum
    shift[i,3]<-y0.pe3
    shift[i,4]<-y0.ln3
    
  }
  return(shift)
}


readt<-function(file_name,window_years,start_date,HQ,...){
  window_n<-window_years*365.25
  path<-paste("./",file_name,sep="")
  tab<-read.table(path,sep=";",dec=".",skip=1,header=T)
  tab<-tab[,-c(2,4,5)]
  tab[,2]<-as.numeric(tab[,2])
  years_start<-as.character(tab[1,1])
  years_end<-as.character(tail(tabl1[,1]))[6]
  where_NA<-tab[grep("-999", tab[,2]),]
  wd_start<-tab[grep(start_date, tab[,1]),]
  seq_wd<-c(as.numeric(rownames(wd_start)):(as.numeric(window_n)+as.numeric(rownames(wd_start))))
  if (min(tab[,2])<(-1)){
    warning("There are missing values in the selected time window, please select another data file or window ---
    Look into the output (second list) in order to see the year(s) of the missing value(s), maybe
            your selected time window doesn't fall in")
  } 
  else{
    print(paste("Time Series Begin:",years_start," - Time Series End:",years_end,
                " - Your window:",sep=""))
  }
  
  if(HQ==30){
  quan<-(1-1/30)  
  } else{
    if(HQ==50){
      quan<-(1-1/50)  
    }
  else{
    if(HQ==80){
      quan<-(1-1/80)  
    }
  else{
    if(HQ==100){
      quan<-(1-1/100)  
    }
  else{
    if(HQ==150){
      quan<-(1-1/150)  
    }
  else{
    if(HQ==200){
      quan<-(1-1/200)  
    }
  else{
    if(HQ==300){
      quan<-(1-1/300)  
    }
  }}}}}}
  maxOL<-as.matrix(AMS(tab[,2][seq_wd]))
  shift<-matrix(NA,1,4)
  colnames(shift)<-c("Wei","Gum","PE3","LN3")
    y0.wei<-quawei(f=quan, para=pelwei(samlmu(maxOL),bound=0))
    y0.gum<-quagum(f=quan, para=pelgum(samlmu(maxOL)))
    y0.pe3<-quape3(f=quan, para=pelpe3(samlmu(maxOL)))
    y0.ln3<-qualn3(f=quan, para=pelln3(samlmu(maxOL),bound=0))
    shift[1,1]<-y0.wei
    shift[1,2]<-y0.gum
    shift[1,3]<-y0.pe3
    shift[1,4]<-y0.ln3
    
  listt<-list(tab,where_NA,maxOL,shift)
  names(listt)<-c("Raw Data","NAs Dates","AMS",paste("HQ: ",HQ,sep=""))
    return(listt)
} 

#"6128101.day" - no -999
tabl<-readt("6128101.day",window_years=30,start_date="1970-01-01",HQ=100)

tabl$AMS
else{
  if(HQ != c(30,50,80,100,150,200,300){
    print("The chosen return level is not supported; Choose either '30,50,80,100,150,200 or 300'")
  }
}
