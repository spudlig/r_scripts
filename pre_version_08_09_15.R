### Readme not public yet.
### get coordnates for googleVis
### GoF and LN4 in! Yeah

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
stat_prop<-function(df,...){
  mean<-mean(as.numeric(df))
  std<-sd(as.numeric(df))
  vari<-var(as.numeric(df))
  skew<-skewness(as.numeric(df))
  kurt<-kurtosis(as.numeric(df))
  matr<-data.frame(mean,std,vari,skew,kurt)
  return(matr)
}

read_ts<-function(file_name="NONE",start_date="NONE",end_date="NONE",window_years=30,HQ=100,shift="FALSE",boot="FALSE",alpha=0.05,rep=5000,plot_CI_shift="FALSE",plot_CI="FALSE",GOF="FALSE",Monte_Carlo=2000,bound_wei=0,bound_ln3=0,qqpl="FALSE",logIV_factor=1.4,lower_bound_ln4=0,...){
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
  if(start_date=="NONE"||end_date=="NONE"){
    if (min(tab[,2])<(-1)){
      warning(paste("There are missing values in your time serie.
            Look into the output (NAs_Dates) in order to see the year(s) of the missing value(s), so you can choose a
              time window that doesnt have any NAs. ","  Time Series Begin: ",years_start," - Time Series End: ",years_end,sep=""))
      return(where_NA)
      stop()
    }else{
      print("No NAs detected")
      stop(paste("select a start_date/end_date: ","Time Series Begin: ",years_start," - Time Series End: ",years_end,sep=""))
    }
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
    }
  nyr_avail<-(NROW(seq_wd)/365.25) # get the years available for the window
  nyr<-(nyr_avail-window_years) # get value for matrix length according to window length and total available years. Important for shift
  nyrs<-(window_years) # because its shorter. IMPORTANT for sequences in shift;
  if(nyr_avail<=nyrs){
    print("Your available time series is too short for your time window - change either the window length or choose another time series")
  } # check whether the time series is shorter than the choosen window
  ### get AMS for window without shifting
  maxOL<-as.matrix(suppressWarnings(AMS(seq_wd)))
  colnames(maxOL)<-c("Annual Maximum Values")
  ### get AMS for total TS with shift
  maxOLsh<-as.matrix(suppressWarnings(AMS(seq_wd)))
  colnames(maxOLsh)<-c("Annual Maximum Values")
  n<-length(maxOL)
  prob<-(1:n)/(n+1)
  # matrices for bootstrap-function, gof
  g<-list() # get list for qqplot and summary
  cal<-matrix(NA,1,6)
  colnames(cal)<-c("Weibull","Gumbel","Pearson_III","LogNormal_III","GEV_Distr","LogNormal_IV")
  CI_func<-matrix(NA,6,3)
  colnames(CI_func)<-c("lower bound","upper bound","value")
  rownames(CI_func)<-c("Weibull","Gumbel","Pearson-III","Log-Normal_III","GEV-Distribution","Log-Normal_IV")
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
  CI_shift_ln4<-matrix(NA,nyr,3)
  colnames(CI_shift_ln4)<-c("lower bound","upper bound","value")
  shift_m<-matrix(NA,nyr,6)
  colnames(shift_m)<-c("Weibull","Gumbel","Pearson-III","Log-Normal III","GEV-Distribution","Log-Normal IV")
  gof_sh<-matrix(NA,nyr,10)
  colnames(gof_sh)<-c("Gumbel - A2","P ","Pearson III - A2","P ","Log-Normal III - A2","P ","GEV- Distribution - A2","P ","Log-Normal IV - A2","P ")
  gof_HQ<-matrix(NA,1,10)
  colnames(gof_HQ)<-c("Gumbel - A2","P ","Pearson III - A2","P ","Log-Normal III - A2","P ","GEV- Distribution - A2","P ","Log-Normal IV - A2","P ")
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
    y0.wei<-quawei(f=quan, para=pelwei(samlmu(x[i]),bound=bound_wei))
  }
  boot_gum <- function(x,i) { 
    y0.gum<-quagum(f=quan, para=pelgum(samlmu(x[i])))
  }
  boot_pe3 <- function(x,i) { 
    y0.pe3<-quape3(f=quan, para=pelpe3(samlmu(x[i])))
  }
  boot_ln3 <- function(x,i) { 
    y0.ln3<-qualn3(f=quan, para=pelln3(samlmu(x[i]),bound=bound_ln3))
  }
  boot_gev <- function(x,i) { 
    y0.ln3<-quagev(f=quan, para=pelgev(samlmu(x[i])))
  }
  if(logIV_factor<1){
    stop("logIV_factor is below 1. Please enter a higher number.")
  }
  boot_ln4<-function(x,i){
    y0.ln4<-qlnormTrunc(p = quan, meanlog = pelln3(samlmu(x[i]),bound=0)[[2]], sdlog = pelln3(samlmu(x[i]),bound=0)[[3]], min = 0, max = (max(x[i])*logIV_factor))
  }
  if(shift=="TRUE"){
    stsh<-stat_prop(maxOLsh)
  }else{
    st<-stat_prop(maxOL)
  }
  if(shift=="TRUE"&&boot=="FALSE"){
    for (i in seq(nyr)){
      
      seqq<-seq(i,nyrs+i,1)
      y0.wei<-quawei(f=quan, para=pelwei(samlmu(maxOLsh[seqq]),bound=bound_wei))
      y0.gum<-quagum(f=quan, para=pelgum(samlmu(maxOLsh[seqq])))
      y0.pe3<-quape3(f=quan, para=pelpe3(samlmu(maxOLsh[seqq])))
      y0.ln3<-qualn3(f=quan, para=pelln3(samlmu(maxOLsh[seqq]),bound=bound_ln3))
      y0.gev<-quagev(f=quan, para=pelgev(samlmu(maxOLsh[seqq])))
      y0.ln4<-qlnormTrunc(p = quan, meanlog = pelln3(samlmu(maxOLsh[seqq]),bound=0)[[2]], sdlog = pelln3(samlmu(maxOLsh[seqq]),bound=0)[[3]], min = 0, max = (max(maxOLsh[seqq])*logIV_factor))
      shift_m[i,1]<-y0.wei
      shift_m[i,2]<-y0.gum
      shift_m[i,3]<-y0.pe3
      shift_m[i,4]<-y0.ln3
      shift_m[i,5]<-y0.gev
      shift_m[i,6]<-y0.ln4
      
    }}else{
      if(shift=="TRUE"&&boot=="TRUE"){
        for (i in seq(nyr)){
          seqq<-seq(i,nyrs+i,1)
          boot_shift_wei<-suppressWarnings(boot(maxOLsh[seqq],boot_wei, R=rep))
          boot_shift_gum<-suppressWarnings(boot(maxOLsh[seqq],boot_gum, R=rep))
          boot_shift_pe3<-suppressWarnings(boot(maxOLsh[seqq],boot_pe3, R=rep))
          boot_shift_ln3<-suppressWarnings(boot(maxOLsh[seqq],boot_ln3, R=rep))
          boot_shift_gev<-suppressWarnings(boot(maxOLsh[seqq],boot_gev, R=rep))
          boot_shift_ln4<-suppressWarnings(boot(maxOLsh[seqq],boot_ln4, R=rep))
          if(i %in% seq(nyr)){
            cat("loop", i, "\n")}
          for (j in seq(1:3)){
            CI_shift_wei[i,j]<-suppressWarnings(append((boot.ci(boot_shift_wei, conf=(1-alpha))$bca[4:5]),boot_shift_wei$t0[[1]],after=2)[[j]])
            CI_shift_gum[i,j]<-suppressWarnings(append((boot.ci(boot_shift_gum, conf=(1-alpha))$bca[4:5]),boot_shift_gum$t0[[1]],after=2)[[j]])
            CI_shift_pe3[i,j]<-suppressWarnings(append((boot.ci(boot_shift_pe3, conf=(1-alpha))$bca[4:5]),boot_shift_pe3$t0[[1]],after=2)[[j]])
            CI_shift_ln3[i,j]<-suppressWarnings(append((boot.ci(boot_shift_ln3, conf=(1-alpha))$bca[4:5]),boot_shift_ln3$t0[[1]],after=2)[[j]])
            CI_shift_gev[i,j]<-suppressWarnings(append((boot.ci(boot_shift_gev, conf=(1-alpha))$bca[4:5]),boot_shift_gev$t0[[1]],after=2)[[j]])
            CI_shift_ln4[i,j]<-suppressWarnings(append((boot.ci(boot_shift_ln4, conf=(1-alpha))$bca[4:5]),boot_shift_ln4$t0[[1]],after=2)[[j]])
          }
        }} else{
          if(shift=="FALSE"&&boot=="FALSE"){
            st<-stat_prop(maxOL)
            cal[1,1]<-quawei(f=quan, para=pelwei(samlmu(maxOL),bound=bound_wei))
            cal[1,2]<-quagum(f=quan, para=pelgum(samlmu(maxOL)))
            cal[1,3]<-quape3(f=quan, para=pelpe3(samlmu(maxOL)))
            cal[1,4]<-qualn3(f=quan, para=pelln3(samlmu(maxOL),bound=bound_ln3))
            cal[1,5]<-quagev(f=quan, para=pelgev(samlmu(maxOL)))
            cal[1,5]<-qlnormTrunc(p = quan, meanlog = pelln3(samlmu(maxOL),bound=0)[[2]], sdlog = pelln3(samlmu(maxOL),bound=0)[[3]], min = 0, max = (max(maxOL)*logIV_factor))
          }
          else{
            if(shift=="FALSE"&&boot=="TRUE"){
              boot_shift_wei<-suppressWarnings(boot(maxOL,boot_wei, R=rep))
              boot_shift_gum<-suppressWarnings(boot(maxOL,boot_gum, R=rep))
              boot_shift_pe3<-suppressWarnings(boot(maxOL,boot_pe3, R=rep))
              boot_shift_ln3<-suppressWarnings(boot(maxOL,boot_ln3, R=rep))
              boot_shift_gev<-suppressWarnings(boot(maxOL,boot_gev, R=rep))
              boot_shift_ln4<-suppressWarnings(boot(maxOL,boot_ln4, R=rep))
              
              for (j in seq(1:3)){
                CI_func[1,j]<-suppressWarnings(append((boot.ci(boot_shift_wei, conf=(1-alpha))$bca[4:5]),boot_shift_wei$t0[[1]],after=2)[[j]])
                CI_func[2,j]<-suppressWarnings(append((boot.ci(boot_shift_gum, conf=(1-alpha))$bca[4:5]),boot_shift_gum$t0[[1]],after=2)[[j]])
                CI_func[3,j]<-suppressWarnings(append((boot.ci(boot_shift_pe3, conf=(1-alpha))$bca[4:5]),boot_shift_pe3$t0[[1]],after=2)[[j]])
                CI_func[4,j]<-suppressWarnings(append((boot.ci(boot_shift_ln3, conf=(1-alpha))$bca[4:5]),boot_shift_ln3$t0[[1]],after=2)[[j]])
                CI_func[5,j]<-suppressWarnings(append((boot.ci(boot_shift_gev, conf=(1-alpha))$bca[4:5]),boot_shift_gev$t0[[1]],after=2)[[j]])
                CI_func[6,j]<-suppressWarnings(append((boot.ci(boot_shift_ln4, conf=(1-alpha))$bca[4:5]),boot_shift_ln4$t0[[1]],after=2)[[j]])
              }
            }  
          }}}
  # statistical properties and GoF
  st<-stat_prop(maxOL)
  stsh<-stat_prop(maxOLsh)
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
  if(qqpl=="TRUE"){
    plot_gof<-function(df,bound_wei=bound_wei,bound_ln3=bound_ln3,lower_bound_ln4=lower_bound_ln4,logIV_factor=logIV_factor,prob=prob,...){
      par(mfrow=c(2,3))
      aa<-summary(lm(quawei(prob, pelwei(samlmu(df),bound=bound_wei))~df))[c(4,8,10)]
      a<-plot(sort(quawei(prob, pelwei(samlmu(df),bound=bound_wei))), sort(df) , xlab = 'Theoretical Quantiles from maxOL', ylab = 'Sample Quantiles from maxOL', main = 'Weibull Quantile-Quantile Plot')
      abline(0,1)
      legend(10,5000,paste(names(aa[2]),": ",round(aa[[2]],6),sep=""))
      names(aa)<-"Weibull"
      bb<-summary(lm(quagum(prob, pelgum(samlmu(df)))~df))[c(4,8,10)]
      b<-plot(sort(quagum(prob, pelgum(samlmu(df)))), sort(df) , xlab = 'Theoretical Quantiles from maxOL', ylab = 'Sample Quantiles from maxOL', main = 'Gumbel III Quantile-Quantile Plot')
      abline(0,1)
      legend(10,5000,paste(names(bb[2]),": ",round(bb[[2]],6),sep=""))
      names(bb)<-"Gumbel"
      cc<-summary(lm(quape3(prob, pelpe3(samlmu(df)))~df))[c(4,8,10)]
      c<-plot(sort(quape3(prob, pelpe3(samlmu(df)))), sort(df) , xlab = 'Theoretical Quantiles from maxOL', ylab = 'Sample Quantiles from maxOL', main = 'Pearson III Quantile-Quantile Plot')
      abline(0,1)
      legend(10,5000,paste(names(cc[2]),": ",round(cc[[2]],6),sep=""))
      names(cc)<-"Pearson III"
      dd<-summary(lm(qualn3(prob, pelln3(samlmu(df),bound=bound_ln3))~df))[c(4,8,10)]
      d<-plot(sort(qualn3(prob, pelln3(samlmu(df),bound=bound_ln3))), sort(df), xlab = 'Theoretical Quantiles from maxOL', ylab = 'Sample Quantiles from maxOL', main = 'Log-Normal III Quantile-Quantile Plot')
      abline(0,1)
      legend(10,5000,paste(names(dd[2]),": ",round(dd[[2]],6),sep=""))
      names(dd)<-"Log-Normal III"
      ee<-summary(lm(quagev(prob, pelgev(samlmu(df)))~df))[c(4,8,10)]
      e<-plot(sort(quagev(prob, pelgev(samlmu(df)))), sort(df) , xlab = 'Theoretical Quantiles from maxOL', ylab = 'Sample Quantiles from maxOL', main = 'GEV III Quantile-Quantile Plot')
      abline(0,1)
      legend(10,5000,paste(names(ee[2]),": ",round(ee[[2]],6),sep=""))
      names(ee)<-"GEV-Distribution"
      ff<-summary(lm(qlnormTrunc(prob, meanlog = pelln3(samlmu(df),bound=0)[[2]], sdlog = pelln3(samlmu(df),bound=0)[[3]], min = lower_bound_ln4, max = (max(df)*logIV_factor))~df))[c(4,8,10)]
      f<-plot(sort(qlnormTrunc(prob, meanlog = pelln3(samlmu(df),bound=0)[[2]], sdlog = pelln3(samlmu(df),bound=0)[[3]], min = lower_bound_ln4, max = (max(df)*logIV_factor))), sort(df) , xlab = 'Theoretical Quantiles from maxOL', ylab = 'Sample Quantiles from maxOL', main = 'Log-Normal IV Quantile-Quantile Plot')
      abline(0,1)
      legend(10,5000,paste(names(ff[2]),": ",round(ff[[2]],6),sep=""))
      names(ff)<-"Log-Normal IV"
      
      return(list(c(a,b,c,d,e,f),c(aa,bb,cc,dd,ee,ff)))
    }
    g<-plot_gof(maxOL,bound_wei = bound_wei,bound_ln3 = bound_ln3,lower_bound_ln4=lower_bound_ln4,logIV_factor=logIV_factor,prob=prob)
  }
  
  if(plot_CI_shift=="TRUE"&&boot=="FALSE"&&shift=="TRUE"){
    print("You have to calculate the confident intervalls via setting boot=TRUE (default boot=5000 - can take a while)")
  }else{
    if(plot_CI_shift=="TRUE"&&boot=="TRUE"&&shift=="FALSE"){
      print("You have to set shift=TRUE in order to calculate a shift - or set plot_CI_shift=FALSE")
    }else{
      if(plot_CI_shift=="TRUE"&&boot=="TRUE"&&shift=="TRUE"){
        plot.new()
        par(mfrow=c(2,4))
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
        
        plot(CI_shift_ln4[,3],type="l",ylim=c((min(CI_shift_ln4[,1])*0.95),(max(CI_shift_ln4[,2])*1.05)),xlab = "Years",ylab="Discharge[m^3/s]",main="Log-Normal IV")
        lines(CI_shift_ln4[,1],col="black")
        lines(CI_shift_ln4[,2],col="black")
        polygon(c(p,rev(p)),c(CI_shift_ln4[,1],rev(CI_shift_ln4[,2])), col = "grey50")
        lines(CI_shift_ln4[,3],col="black")
        
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
        lines(CI_shift_ln4[,3],col="brown")
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
  
  CI_shift<-list(CI_shift_wei,CI_shift_gum,CI_shift_pe3,CI_shift_ln3,CI_shift_gev,CI_shift_ln4)
  names(CI_shift)<-c("CI_shift_weibull","CI_shift_gumbel","CI_shift_pearsonIII","CI_shift_logNormal_III","CI_shift_gev","CI_shift_ln4")
  GoF_AD_Test<-list(gof_sh,gof_HQ)
  names(GoF_AD_Test)<-c(paste("Shift_AD_HQ",HQ,sep=""),paste("Time_Window_AD_HQ",HQ,sep=""))
  listt<-list(tab,where_NA,maxOL,cal,shift_m,CI_shift,CI_func,GoF_AD_Test,list(st,stsh),g)
  names(listt)<-c("Raw_Data","NAs_Dates","AMS",paste("HQ_",HQ,sep=""),"Shift_time_window","CI_shift",paste("CI_HQ_",HQ,sep=""),"GoF_AD_Test","Statistic_parameters","qqplot_statistics")
  print(plot_CI)
  print(g[[1]])
  return(listt)
} 

tabl<-read_ts(file_name=" ;) ",start_date = "1901-01-01",end_date = "2008-01-01",qqpl="TRUE")
