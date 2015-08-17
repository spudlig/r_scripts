library(copula)
library(evd)
library(gmm)
library(relaimpo)
library(aplpack)
library(vioplot)
library(lattice)
library(latticeExtra)
library(grid)
library(dplyr)
library(ggvis)
library(magrittr)
library(bbmle)
library(FAdist)
library(nsRFA)
library(PearsonDS)
library(survival)
library(MASS)
library(ismev) 
library(fBasics)
library(lmom)
library(Hmisc)
library(extRemes)

### Einlesen, und in eine formatierte Liste bringen
#setwd("./Studium/Watermanagement/Masterarbeit/data/y120/")
setwd("~/Studium/Watermanagement/Masterarbeit/data/year60/")

# Vom Directory in eine Liste
path<-"./"
file_dir<-dir(path,pattern=".day.txt")
readt<-function(path){
  tab<-read.table(path,sep=";",dec=".",header=F)
  return(tab)
}
readTable<-function(file_dir){
  table<-lapply(file_dir,readt)
  return(table)
}

# Die DF in der Liste rechentauglich machen
get.matrix<-function(table){
  data<-table[[4]]
  date<-as.Date(table[[1]], "%Y-%m-%d")
  year<-format(date,format="%Y")
  month<-format(date,format="%m")
  day<-format(date,format="%d")
  mat<-data.frame(year,month,day,data)
  return(mat)
}

### nur noch ohne -999 -> sind genau 9 (bei y120) 21.04.15, jetzt Versuch mit y80
kickoutnine<-function(df){
  if(min(df[[4]])=="-999"){
    df=NULL
  } 
  return(df)
}

