### two settings are implimented in the function (yet):
# First: non shifting settings (the default), which means that the user sets a date within the time serie and calculates a HQxxx (default is set to HQ100, but 30,40,50,80,10,150,200 and 300 is implimented as well)
# The start_date and end_date can be choosen as soon as the file_name is real and in the working directory, then the time series start and end is printed (after running the function)
# default options: HQ=100, boot=FALSE,alpha for bootstrapping=0.05, repetations for bootstrapping=5000 (takes a while), GOF=FALSE -> Anderson Darling Test is run with the A2 (distance), and 
# P value (1-p-value) via Monte Carlo Method. See nsRFA-package for further details. Monte_Carlo=2000 by default.
# plot_CI="FALSE" by default, but when boot="TRUE" and plot_CI="TRUE" one gets a plot as well.

# Second: shifting settings, which means that one can choose the window range with which the time window can shift over the whole time series (between the start_date and end_date)
# default window_years=30, shift="FALSE" and has to be set "TRUE", if one want the CI intervalls to be plotted, set plot_CI_shift=TRUE. GOF same as above.


#### What needs to be done:
# Install packages (if not done yet, see below)
# library them (see below)
# Set working directory to folder with GRDC-data (needs to be a nr with a ".day" ending)
# Then run the two functions (AMS a read_ts)

### minimum in order to run read_ts:
# File_name="..." e.g. "6128101.day" ; needs to be in the working directory; and needs to have a .day ending (GRDC - data type) as well as within "...")
# It also shows you in the beginning, if any missing values (NAs) are in the series

### Then one can see the time series start and end and one can set other arguments. e.g.:
# window_years=... e.g. 30 ; is the amount of years for the moving window (30 should be considered as a minimum)
# start_date="..." e.g. "1950-01-01" ; is the date from which the HQxxx is being calculated
# end_date="..."
# ...

##################################################################################################################
### if one didnt install the packages yet, un-hashtag the install.packages as well (second lines below):
libs <- c('lmom','boot','fitdistrplus','nsRFA','lattice','latticeExtra')
#lapply(libs, FUN = function(X){do.call("install.packages", list(libs))})
lapply(libs, require, character.only = T)
setwd("~/Studium/Watermanagement/Masterarbeit/data/data/")

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

read_ts<-function(file_name="NONE",start_date="NONE",end_date="NONE",window_years=30,HQ=100,shift="FALSE",boot="FALSE",alpha=0.05,rep=5000,plot_CI_shift="FALSE",plot_CI="FALSE",GOF="FALSE",Monte_Carlo=2000,...){
  ### get time series
  if(file_name=="NONE"){
    stop("Please selecte an existing file_name and working directory")
  }
  path<-paste("./",file_name,sep="")
  tab<-read.table(path,sep=";",dec=".",skip=1,header=T)
  tab<-tab[,-c(2,4,5)]
  tab[,2]<-as.numeric(tab[,2])
  ### basic infos
  nyears_avail<-NROW(tab[,2])/365.25 # get total years available
  window_n<-window_years*365.25 # get window years in days
  years_start<-as.character(tab[1,1]) # TS start
  years_end<-as.character(tail(tab[,1]))[6] # TS end
  wd_start<-tab[grep(start_date, tab[,1]),] # window start (character)
  wd_end<-tab[grep(start_date, tab[,1]),] # window start (character)
  where_NA<-tab[grep("-999", tab[,2]),]# are NAs in the TS, and if warn, otherwise print details when ts start and ends:
  colnames(where_NA)<-c("NAs_Dates")
  if(start_date=="NONE"&&end_date=="NONE"){
    if (min(tab[,2])<(-1)){
      warning(paste("There are missing values in your time serie.
            Look into the output (NAs_Dates) in order to see the year(s) of the missing value(s), so you can choose a
              time window that doesnt have any NAs. ","  Time Series Begin: ",years_start," - Time Series End: ",years_end,sep=""))
      return(where_NA)
      stop()
    }else{
      print("No NAs detected")
    }
  }else{
    if(start_date=="NONE"||end_date=="NONE"){
      stop(paste("select a start_date/end_date: ","Time Series Begin: ",years_start," - Time Series End: ",years_end,sep=""))
    }else{
    start<-as.numeric(rownames(tab[grep(start_date,tab[,1],value=FALSE),]))
    end<-as.numeric(rownames(tab[grep(end_date,tab[,1],value=FALSE),]))
    seq_wd<-tab[start:end,2]  # get seq for window
    if(min(seq_wd)<(-1)){
      warning("There are missing values in your time window.
            Look into the output (NAs_Dates) in order to see the year(s) of the missing value(s), so you can choose a
              time window that doesnt have any NAs")
      return(where_NA)      
      stop()
    }
    }}
  nyr_avail<-(NROW(seq_wd)/365.25) # get the years available for the window
  nyr<-(nyr_avail-window_years) # get value for matrix length according to window length and total available years. Important for shift
  nyrs<-(window_years) # because its shorter. IMPORTANT for sequences in shift; -1 because we want 1:30
  if(nyr_avail<=nyrs){
    print("Your available time series is too short for your time window - change either the window length or choose another time series")
  } # check whether the time series is shorter than the choosen window
  ### get AMS for window without shifting
  maxOL<-as.matrix(suppressWarnings(AMS(seq_wd)))
  colnames(maxOL)<-c("Annual Maximum Values")
  ### get AMS for total TS with shift
  maxOLsh<-as.matrix(suppressWarnings(AMS(seq_wd)))
  colnames(maxOLsh)<-c("Annual Maximum Values")
  # matrices for bootstrap-function, gof
  cal<-matrix(NA,1,5)
  colnames(cal)<-c("Weibull","Gumbel","Pearson_III","LogNormal_III","GEV_Distr")
  CI_func<-matrix(NA,5,3)
  colnames(CI_func)<-c("lower bound","upper bound","value")
  rownames(CI_func)<-c("Weibull","Gumbel","Pearson-III","Log-Normal III","GEV-Distribution")
  CI_shift_wei<-matrix(NA,nyr,3)
  colnames(CI_shift_wei)<-c("lower bound","upper bound","value")
  CI_shift_gum<-matrix(NA,nyr,3)
  colnames(CI_shift_gum)<-c("lower bound","upper bound","value")
  CI_shift_pe3<-matrix(NA,nyr,3)
  colnames(CI_shift_pe3)<-c("lower bound","upper bound","value")
  CI_shift_ln3<-matrix(NA,nyr,3)
  colnames(CI_shift_ln3)<-c("lower bound","upper bound","value")
  CI_shift_gev<-matrix(NA,nyr,3)
  colnames(CI_shift_gev)<-c("lower bound","upper bound","value")
  shift_m<-matrix(NA,nyr,5)
  colnames(shift_m)<-c("Weibull","Gumbel","Pearson-III","Log-Normal III","GEV-Distribution")
  gof_sh<-matrix(NA,nyr,8)
  colnames(gof_sh)<-c("Gumbel - A2","P ","Pearson III - A2","P ","Log-Normal III - A2","P ","GEV- Distribution - A2","P ")
  gof_HQ<-matrix(NA,1,8)
  colnames(gof_HQ)<-c("Gumbel - A2","P ","Pearson III - A2","P ","Log-Normal III - A2","P ","GEV- Distribution - A2","P ")
  ### not well done, but puts the HQ 30,40,50 into quantiles for calculations
  if(HQ==30){
    quan<-(1-1/30)  
  }else{
    if(HQ==50){
      quan<-(1-1/50)  
    }else{
      if(HQ==80){
        quan<-(1-1/80)  
      }else{
        if(HQ==100){
          quan<-(1-1/100)  
        }else{
          if(HQ==150){
            quan<-(1-1/150)  
          }else{
            if(HQ==200){
              quan<-(1-1/200)  
            }else{
              if(HQ==300){
                quan<-(1-1/300)  
              }
            }}}}}}
  ### shift=TRUE; boot=FALSE; calculates a shift over the TS. calculates the AMS for the whole TS
  boot_wei <- function(x,i) { 
    y0.wei<-quawei(f=quan, para=pelwei(samlmu(x[i]),bound=0))
  }
  boot_gum <- function(x,i) { 
    y0.gum<-quagum(f=quan, para=pelgum(samlmu(x[i])))
  }
  boot_pe3 <- function(x,i) { 
    y0.pe3<-quape3(f=quan, para=pelpe3(samlmu(x[i])))
  }
  boot_ln3 <- function(x,i) { 
    y0.ln3<-qualn3(f=quan, para=pelln3(samlmu(x[i]),bound=0))
  }
  boot_gev <- function(x,i) { 
    y0.ln3<-quagev(f=quan, para=pelgev(samlmu(x[i])))
  }
  if(shift=="TRUE"&&boot=="FALSE"){
    for (i in seq(nyr)){
      
      seqq<-seq(i,nyrs+i,1)
      y0.wei<-quawei(f=quan, para=pelwei(samlmu(maxOLsh[seqq]),bound=0))
      y0.gum<-quagum(f=quan, para=pelgum(samlmu(maxOLsh[seqq])))
      y0.pe3<-quape3(f=quan, para=pelpe3(samlmu(maxOLsh[seqq])))
      y0.ln3<-qualn3(f=quan, para=pelln3(samlmu(maxOLsh[seqq]),bound=0))
      y0.gev<-quagev(f=quan, para=pelgev(samlmu(maxOLsh[seqq])))
      shift_m[i,1]<-y0.wei
      shift_m[i,2]<-y0.gum
      shift_m[i,3]<-y0.pe3
      shift_m[i,4]<-y0.ln3
      shift_m[i,5]<-y0.gev
      
    }}else{
      if(shift=="TRUE"&&boot=="TRUE"){
        for (i in seq(nyr)){
          seqq<-seq(i,nyrs+i,1)
          boot_shift_wei<-suppressWarnings(boot(maxOLsh[seqq],boot_wei, R=rep))
          boot_shift_gum<-suppressWarnings(boot(maxOLsh[seqq],boot_gum, R=rep))
          boot_shift_pe3<-suppressWarnings(boot(maxOLsh[seqq],boot_pe3, R=rep))
          boot_shift_ln3<-suppressWarnings(boot(maxOLsh[seqq],boot_ln3, R=rep))
          boot_shift_gev<-suppressWarnings(boot(maxOLsh[seqq],boot_gev, R=rep))
          
          for (j in seq(1:3)){
            CI_shift_wei[i,j]<-suppressWarnings(append((boot.ci(boot_shift_wei, conf=(1-alpha))$bca[4:5]),boot_shift_wei$t0[[1]],after=2)[[j]])
            CI_shift_gum[i,j]<-suppressWarnings(append((boot.ci(boot_shift_gum, conf=(1-alpha))$bca[4:5]),boot_shift_gum$t0[[1]],after=2)[[j]])
            CI_shift_pe3[i,j]<-suppressWarnings(append((boot.ci(boot_shift_pe3, conf=(1-alpha))$bca[4:5]),boot_shift_pe3$t0[[1]],after=2)[[j]])
            CI_shift_ln3[i,j]<-suppressWarnings(append((boot.ci(boot_shift_ln3, conf=(1-alpha))$bca[4:5]),boot_shift_ln3$t0[[1]],after=2)[[j]])
            CI_shift_gev[i,j]<-suppressWarnings(append((boot.ci(boot_shift_gev, conf=(1-alpha))$bca[4:5]),boot_shift_gev$t0[[1]],after=2)[[j]])
          }
        }} else{
          if(shift=="FALSE"&&boot=="FALSE"){
            cal[1,1]<-quawei(f=quan, para=pelwei(samlmu(maxOL),bound=0))
            cal[1,2]<-quagum(f=quan, para=pelgum(samlmu(maxOL)))
            cal[1,3]<-quape3(f=quan, para=pelpe3(samlmu(maxOL)))
            cal[1,4]<-qualn3(f=quan, para=pelln3(samlmu(maxOL),bound=0))
            cal[1,5]<-quagev(f=quan, para=pelgev(samlmu(maxOL)))
          }
          else{
            if(shift=="FALSE"&&boot=="TRUE"){
              boot_shift_wei<-suppressWarnings(boot(maxOL,boot_wei, R=rep))
              boot_shift_gum<-suppressWarnings(boot(maxOL,boot_gum, R=rep))
              boot_shift_pe3<-suppressWarnings(boot(maxOL,boot_pe3, R=rep))
              boot_shift_ln3<-suppressWarnings(boot(maxOL,boot_ln3, R=rep))
              boot_shift_gev<-suppressWarnings(boot(maxOL,boot_gev, R=rep))
              
              for (j in seq(1:3)){
                CI_func[1,j]<-suppressWarnings(append((boot.ci(boot_shift_wei, conf=(1-alpha))$bca[4:5]),boot_shift_wei$t0[[1]],after=2)[[j]])
                CI_func[2,j]<-suppressWarnings(append((boot.ci(boot_shift_gum, conf=(1-alpha))$bca[4:5]),boot_shift_gum$t0[[1]],after=2)[[j]])
                CI_func[3,j]<-suppressWarnings(append((boot.ci(boot_shift_pe3, conf=(1-alpha))$bca[4:5]),boot_shift_pe3$t0[[1]],after=2)[[j]])
                CI_func[4,j]<-suppressWarnings(append((boot.ci(boot_shift_ln3, conf=(1-alpha))$bca[4:5]),boot_shift_ln3$t0[[1]],after=2)[[j]])
                CI_func[5,j]<-suppressWarnings(append((boot.ci(boot_shift_gev, conf=(1-alpha))$bca[4:5]),boot_shift_gev$t0[[1]],after=2)[[j]])
              }
            }  
          }}}
  if(GOF=="TRUE"&&shift=="TRUE"){
    for (i in seq(nyr)){
      seqq<-seq(i,nyrs+i,1)
      gof_sh[i,c(1:2)]<-gofGUMBELtest(maxOLsh[seqq], Nsim=Monte_Carlo)
      gof_sh[i,c(3:4)]<-gofP3test(maxOLsh[seqq], Nsim=Monte_Carlo)
      gof_sh[i,c(5:6)]<-gofLOGNORMtest(maxOLsh[seqq], Nsim=Monte_Carlo)
      gof_sh[i,c(7:8)]<-gofGEVtest(maxOLsh[seqq], Nsim=Monte_Carlo)  
    }}else{
      if(GOF=="TRUE"&&shift=="FALSE"){
        gof_HQ[1,c(1:2)]<-gofGUMBELtest(maxOL, Nsim=Monte_Carlo)
        gof_HQ[1,c(3:4)]<-gofP3test(maxOL, Nsim=Monte_Carlo)
        gof_HQ[1,c(5:6)]<-gofLOGNORMtest(maxOL, Nsim=Monte_Carlo)
        gof_HQ[1,c(7:8)]<-gofGEVtest(maxOL, Nsim=Monte_Carlo)  
      }
    }
  
  if(plot_CI_shift=="TRUE"&&boot=="FALSE"&&shift=="TRUE"){
    print("You have to calculate the confident intervalls via setting boot=TRUE (default boot=5000 - can take a while)")
  }else{
    if(plot_CI_shift=="TRUE"&&boot=="TRUE"&&shift=="FALSE"){
      print("You have to set shift=TRUE in order to calculate a shift - or set plot_CI_shift=FALSE")
    }else{
      if(plot_CI_shift=="TRUE"&&boot=="TRUE"&&shift=="TRUE"){
        plot.new()
        par(mfrow=c(2,3))
        p=seq(1,nyr,1)
        plot(CI_shift_wei[,3],type="l",ylim=c((min(CI_shift_wei[,1])*0.95),(max(CI_shift_wei[,2])*1.05)),xlab = "Years",ylab="Discharge[m^3/s]",main="Weibull")
        lines(CI_shift_wei[,1],col="black")
        lines(CI_shift_wei[,2],col="black")
        polygon(c(p,rev(p)),c(CI_shift_wei[,1],rev(CI_shift_wei[,2])), col = "grey50")
        lines(CI_shift_wei[,3],col="black")
        
        plot(CI_shift_gum[,3],type="l",ylim=c((min(CI_shift_gum[,1])*0.95),(max(CI_shift_gum[,2])*1.05)),xlab = "Years",ylab="Discharge[m^3/s]",main="Gumbel")
        lines(CI_shift_gum[,1],col="black")
        lines(CI_shift_gum[,2],col="black")
        polygon(c(p,rev(p)),c(CI_shift_gum[,1],rev(CI_shift_gum[,2])), col = "grey50")
        lines(CI_shift_gum[,3],col="black")
        
        plot(CI_shift_pe3[,3],type="l",ylim=c((min(CI_shift_pe3[,1])*0.95),(max(CI_shift_pe3[,2])*1.05)),xlab = "Years",ylab="Discharge[m^3/s]",main="Pearson III")
        lines(CI_shift_pe3[,1],col="black")
        lines(CI_shift_pe3[,2],col="black")
        polygon(c(p,rev(p)),c(CI_shift_pe3[,1],rev(CI_shift_pe3[,2])), col = "grey50")
        lines(CI_shift_pe3[,3],col="black")
        
        plot(CI_shift_ln3[,3],type="l",ylim=c((min(CI_shift_ln3[,1])*0.95),(max(CI_shift_ln3[,2])*1.05)),xlab = "Years",ylab="Discharge[m^3/s]",main="Log-Normal III")
        lines(CI_shift_ln3[,1],col="black")
        lines(CI_shift_ln3[,2],col="black")
        polygon(c(p,rev(p)),c(CI_shift_ln3[,1],rev(CI_shift_ln3[,2])), col = "grey50")
        lines(CI_shift_ln3[,3],col="black")
        
        plot(CI_shift_gev[,3],type="l",ylim=c((min(CI_shift_gev[,1])*0.95),(max(CI_shift_gev[,2])*1.05)),xlab = "Years",ylab="Discharge[m^3/s]",main="Generalized Extreme Distribution")
        lines(CI_shift_gev[,1],col="black")
        lines(CI_shift_gev[,2],col="black")
        polygon(c(p,rev(p)),c(CI_shift_gev[,1],rev(CI_shift_gev[,2])), col = "grey50")
        lines(CI_shift_gev[,3],col="black")
        
        plot(CI_shift_wei[,3],type="l",ylim=c((min(CI_shift_wei[,1])*0.95),(max(CI_shift_wei[,2])*1.15)),xlab = "Years",ylab="Discharge[m^3/s]",main="All Functions")
        legend(0,(max(CI_shift_wei[,2])*1.15),lty=1,c("Weibull","Gumbel","Pearson III","Log-Normal III","GEV-Distribution"),col=c("black","red","green","blue","orange"))
        lines(CI_shift_wei[,1],col="black")
        lines(CI_shift_wei[,2],col="black")
        polygon(c(p,rev(p)),c(CI_shift_wei[,1],rev(CI_shift_wei[,2])), col = "grey50")
        lines(CI_shift_wei[,3],col="black")
        lines(CI_shift_gum[,3],col="red")
        lines(CI_shift_pe3[,3],col="green")
        lines(CI_shift_ln3[,3],col="blue")
        lines(CI_shift_gev[,3],col="orange")
      }
    }
  }
  
  if(plot_CI=="TRUE"&&boot=="FALSE"&&shift=="TRUE"){
    print("You have to calculate the confident intervalls via setting boot=TRUE and shift=FALSE (default boot=5000 - can take a while)")
  }else{
      if(plot_CI=="TRUE"&&boot=="TRUE"&&shift=="FALSE"){
        plot_CI<-dotplot(CI_func,cex=2,lwd=c(""),pch=c(20,20,18),xlab=c("Discharge m^3/s"),auto.key =
                  list(space = "right", points = TRUE, lines = FALSE))        

    }
  }
  
  CI_shift<-list(CI_shift_wei,CI_shift_gum,CI_shift_pe3,CI_shift_ln3,CI_shift_gev)
  names(CI_shift)<-c("CI_shift_weibull","CI_shift_gumbel","CI_shift_pearsonIII","CI_shift_logNormal_III","CI_shift_gev")
  GoF_AD_Test<-list(gof_sh,gof_HQ)
  names(GoF_AD_Test)<-c(paste("Shift_AD_HQ",HQ,sep=""),paste("Time_Window_AD_HQ",HQ,sep=""))
  listt<-list(tab,where_NA,maxOL,cal,shift_m,CI_shift,CI_func,GoF_AD_Test)
  names(listt)<-c("Raw_Data","NAs_Dates","AMS",paste("HQ_",HQ,sep=""),"Shift_time_window","CI_shift",paste("CI_HQ_",HQ,sep=""),"GoF_AD_Test")
  print(plot_CI)
  return(listt)
} 

tabl<-read_ts(file_name="NONE")

tabl$NAs_Dates

