install.packages("devtools")
library("devtools")
setwd("../")
create("packages")
boot_wei <- function(x,i) { 
  y0.wei<-quawei(f=0.99, para=pelwei(samlmu(x[i]),bound=0))
}


#######
dat_seq<-rep((seq(c(ISOdate(1948,1,1)), by = "day", length.out = c(60*365.25+1))),227)
dat_seq_max<-rep(seq(1948,1977,1),227)

listko_df<-matrix(NA,227*60*365.25+227,2)
maxOL_df<-matrix(NA,227*60,2)
shiftst_df<-matrix(NA,227*30,5)

for(j in seq(1:227)){
  listko_df[c((1+j*60*365.25-60*365.25):(j*60*365.25)),1]<-listko[[j]][,4][1:(60*365.25)]
  listko_df[c((1+j*60*365.25-60*365.25):(j*60*365.25)),2]<-rep(names(listko[j]),(60*365.25))
}


for(j in seq(1:227)){
  maxOL_df[c((1+j*60-60):(j*60)),1]<-maxOL_list[[j]]
  maxOL_df[c((1+j*60-60):(j*60)),2]<-rep(names(maxOL_list[j]),(60))
}

for(j in seq(1:227)){
  shiftst_df[c((1+j*30-30):(j*30)),1]<-shiftst[[j]][,1]
  shiftst_df[c((1+j*30-30):(j*30)),2]<-shiftst[[j]][,2]
  shiftst_df[c((1+j*30-30):(j*30)),3]<-shiftst[[j]][,3]
  shiftst_df[c((1+j*30-30):(j*30)),4]<-shiftst[[j]][,4]
  shiftst_df[c((1+j*30-30):(j*30)),5]<-rep(names(shiftst[j]),(30))
}

listko_df<-data.frame(listko_df,listko_df,dat_seq)
maxOL_dff<-data.frame(maxOL_df[,1],maxOL_df[,2],dat_seq_max)
shiftst_dff<-data.frame(shiftst_df,dat_seq_max)


#xyplot(listko_dff[,1]~listko_dff[,3]|listko_dff[,2],type="smooth")
#xyplot(maxOL_dff[,1]~maxOL_dff[,3]|maxOL_dff[,2],type=c("l","smooth"))
xyplot(shiftst_dff[,1]~shiftst_dff[,6]|shiftst_dff[,5],type=c("l","smooth"))


