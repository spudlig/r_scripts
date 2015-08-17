setwd("~/Studium/Watermanagement/Masterarbeit/data/ye60_all_3/")
setwd("~/Studium/Watermanagement/Masterarbeit/WorkSpace/selected/")
### Save Listko // Open it <- Start from here
#save(listko,file="../Listko_80y_40df.RData")
load("./CI_shift_y100_gum_50.RData")
save(samps_y60,file="~/Studium/Watermanagement/Masterarbeit/WorkSpace/selected/
     samps_y60.RData")
load("~/Studium/Watermanagement/Masterarbeit/WorkSpace/selected/samps_y100.RData")
### Listko 40 Rivers with +80
load("../Listko_80y_40df.RData")
listko40<-listko
### Listko 110 Rivers with exactly 60yers
load("../Listko_60y_all_204.RData")
listko_y60<-listko
### Listko  Rivers with exactly 80yers
#load("../Listko_80y_all.RData")
load("../WorkSpace/selected/listko_y100.RData")
detach()
load("~/Studium/Watermanagement/Masterarbeit/WorkSpace/listko_100y_all_47.RData")
# load session 09.Juni.2015
load("~/Studium/Watermanagement/Masterarbeit/data/Maps_Juni_16_2015.RData")
### Find min nrow in each listko
max_47<-matrix(NA,47,1)
max_226<-matrix(NA,227,1)
for (i in seq(1:227)){
#listko_y100<-listko_y100[-33]
  max_226[i,]<-NROW(listko_y60[[i]])
}
NROW(listko_y60[[41]])
# delete Nr 6935052 (nr. 41) and 6731170 (nr. 33) in listko_y100, not 100 years long
listko_y100[[33]]<-NULL
#listko_y100<-listko_y100[-33]


# get coordinates for table
rawmapll<-read.table("./long.txt",comment.char = "",sep=":",header=F)
rawmapll[,1]<-NULL
mapll<-matrix(NA,410,3) # 410 for y60/// 282 for y100
names<-dput(names(listko))
seqq1<-seq(1,1230,3) # 1230 for y60 /// 846 for y100
seqq2<-seq(2,1230,3)
seqq3<-seq(3,1230,3)
mapll[,1]<-as.character(rawmapll$V2[seqq1])
mapll[,2]<-(rawmapll$V2[seqq2])
mapll[,3]<-(rawmapll$V2[seqq3])
maa<-matrix(NA,227,3) # 227 for y60 /// 45 for y100

for (i in seq_along(maa)){
  for (j in seq_along(maa)){
    if(names[i] == paste("a",mapll[j],sep="")){
      maa[i,1]<-paste("a",mapll[j,1],sep="")
      maa[i,2]<-mapll[j,2]
      maa[i,3]<-mapll[j,3]
    }
  }
}
colnames(maa)<-c("Nr","Lat","Long")
maa_y60<-maa
#maa<-maa[-41,] #IMPORTANT! because two rivers are not 100years long - Nr 6935052 (nr. 41) and 6731170 (nr. 33)
#maa<-maa[-33,] #IMPORTANT! thats why with the 47 rivers 100years long all they need to be thrown out
samps<-maa[,c(2:3)]

# Vom Directory in eine Liste
largeList<-readTable(file_dir)
# Rename 
nam<-list.files("./",pattern=".day.txt")
nl<-sub(".day.txt","", nam,ignore.case = T)
namList<-grep("[0-9]{7}",nl,value=T)
namList<-paste("a",namList,sep="")
names(largeList)[c(1:length(file_dir))]<-namList
# Die DF in der Liste rechentauglich machen;;
formList<-lapply(largeList[c(1:length(file_dir))],get.matrix)
### nur noch ohne -999
lltry<-lapply(formList[1:length(file_dir)],kickoutnine)
listko<-lltry[!sapply(lltry, is.null)]
for (i in seq_along(listko)){
  if(nrow(listko[[i]]) < 21915){
  listko[i]<-list(NULL)
}
names(listko)
} ### VORSICHT mit < > als 21000 nrow !!!
listko<-listko[!sapply(listko, is.null)]
### Save Listko // Open it <- Start from here
#save(listko,file="../Listko_80y_40df.RData")
#save(listko,file="../listko_60y.RData")
#save(listko,file="../listko_80y_all.RData")
save(listko,file="../listko_100y_all_47.RData")
save(listko,file="../listko_60y_all_227.RData")
#load("../Listko_80y_40df.RData")
#load("../Listko_60y.RData")
load("../Listko_80y_all.RData")

##### AMS get max years.
maxOL<-AMS_get(listko[[1]])
maxOL_list<-lapply(listko_y60[c(1:227)],AMS_get)

### Statisitsche Kenngrößen <- dafür muss man noch das ganze Skript laufen lassen
dfStatKenn<-lapply(listko[c(1:length(listko))],stat.kenn)
dfStatKenn

###### Parameters EV (lmom)
la<-lapply(listko[c(3)],quantile=0.99,year=30,param)
la_80<-lapply(listko[c(1:40)],year=30,quantile=0.99,par.80y)
la_80

#### für ein 0.90/99/999 hochwasser am Anfang, dann ein 0.90 Hochwasser berechnet
q90.90<-inc.hqs.90.90(listko[3],year=30,quantile=0.90)
q99.90<-inc.hqs.99.90(listko[3],year=30,quantile=0.99)
q999.90<-inc.hqs.999.90(listko[3],year=30,quantile=0.999)
#### für ein 0.90/99/999 hochwasser am Anfang, dann ein 0.99 Hochwasser berechnet
qviel.90<-inc.hqs.90.99(listko[3],year=30,quantile=0.999999999999999)
q99.99<-inc.hqs.99.99(listko[3],year=30,quantile=0.99)
q999.99<-inc.hqs.999.99(listko[3],year=30,quantile=0.999)
#### für ein 0.90/99/999 hochwasser am Anfang, dann ein 0.999 Hochwasser berechnet
q90.999<-inc.hqs.90.999(listko[3],year=30,quantile=0.90)
q99.999<-inc.hqs.99.999(listko[3],year=30,quantile=0.99)
q999.999<-inc.hqs.999.999(listko[3],year=30,quantile=0.999)
#### Plot ein 999 Hochwasser am Anfang, dann ein 0.90 Hochwasser berechnet für ALLE Funktionen
plot(c(1:50),q999.90[1][[1]],type="l",ylim=c(200,270))
lines(as.numeric(q999.90[[2]]),col="red")
lines(as.numeric(q999.90[[3]]),col="green")
lines(as.numeric(q999.90[[4]]),col="blue")
#### Plot 0.90/99/999 Hochwasser am Anfang, dann ein 0.99 Hochwasser berechnet + Minus für
#### je eine Funktion
plot(c(1:50),qviel.90[1][[1]],type="l")
lines(as.numeric(q999.99[[1]]),col="green")
lines(as.numeric(la_80[[1]][[1]]),col="red")
(qviel.90[[1]]-la_80[[1]][[1]])

#### Extract 40 Days of the Time series with the highes mean discharge and add it at the beginning of
#### the TS, then calculate the summed years (30->80yrs) and a 0.99 quantile HQ
max_month<-max_month_80y(listko[[2]],quantile=0.99,year=30)
max_month3<-max_month_80y(listko[[3]],quantile=0.99,year=30)
maxMonthList<-lapply(listko[c(2:40)],quantile=0.99,year=30,FUN=max_month_80y)
save(maxMonthList,file="../MaxMonthList.RData")
load(load("../MaxMonthList.RData"))


## calculate lmom of the MaxMonth, plot and add the data without the added MaxMonth. 
## max_month[[4]][[x]] x are the wei,gum,pe3,ln3;; la_80[[1]][[x]], x is either wei,gum,pe3,ln3
plot(c(1:50),la_80[[3]][[1]],type="l",ylim=c(min(la_80[[3]][[1]])*0.96,max(la_80[[3]][[1]])*1.08))
lines(as.numeric(maxMonthList[[1]][[4]][[1]]),col="red")
maxMonthList[[1]][[4]][[1]]
la_80[[2]][[1]]
maxMonthList[[1]][[4]][[1]]-la_80[[3]][[1]]
maxMonthList[[1]][[4]][[4]]-la_80[[3]][[4]]


###### Parameters MLE <- MLE_run_before nicht vergessen (ln3; zeta,mu,sigma = thres,scale,shape)
la99_50<-lapply(listko[1],quantile=0.99,year=50,param.mle)
la90_50<-lapply(listko[1],quantile=0.90,year=50,param.mle)
la99_30<-lapply(listko[1],quantile=0.99,year=30,param.mle)
la90_30<-lapply(listko[1],quantile=0.90,year=30,param.mle)

###### Plot lmom
### Je 30 jahre verschoben
lapply(listko[13],dfr=listko[3][[1]],names=list(names(la)[3]),quantile1=0.99,years=30,on="OFF",FUN=ploti)
### Von 30-80 Jahre Summiert
lapply(listko[3],dfr=listko[3][[1]],names=list(names(la)[3]),quantile1=0.99,years=30,FUN=ploti.80)
lapply(new_hq,dfr=neq_hq,names=list(names(la)[3]),quantile1=0.99,years=30,FUN=ploti.80)

plot(listko[[3]][[4]])

###### Plot MLE
lapply(la99_50[1],dfr=listko[1][[1]],names=list(names(la)[1]),quantile1=0.99,years=50,on="ON",ploti2)
lapply(la90_50[1],dfr=listko[1][[1]],names=list(names(la)[1]),quantile1=0.90,years=50,on="ON",ploti2)
lapply(la99_30[1],dfr=listko[1][[1]],names=list(names(la)[1]),quantile1=0.99,years=30,on="ON",ploti2)
lapply(la90_30[1],dfr=listko[1][[1]],names=list(names(la)[1]),quantile1=0.90,years=30,on="ON",ploti2)

###### Plot EV (lmom) Immer yyall und la (Parameters EV lmom) (mit year,qu,names -> gleich), 
###### dann f berechnen ;;; Beim Plot output steht (falls func="all") ein Quantilen Wert dabei -> Unwichtig für "all", 
###### wird nur bei func="Weibull" etc. schlagend
yyall<-seq30y(listko[3][[1]],year = 30)
f<-lapply(listko[3],year=30,quantile=0.99,names=list(names(la)[3]),
          func="all",ally=(yyall$yy0),allyn="yy0",evWeiPl)
f
###### Plot EV (MLE) <- noch nicht fertig, fehlerhaft 
lapply(listko[3],names=list(names(listko)[1]),ally=(yyall$yy0),allyn="yy0",FUN=ev.mle)

##### Values lmom,mle,obs <- geht noch ohne MLE
listi<-data.all(dfal=listko[1],yearall=30,quall=0.95)
cor(listi$Gum,listi$Obs)

#### Vioplot mit allen Daten, außer MLE, Vioplot mit R-square adjusted
#### auch die % dabei für die Vektor Minus Rechnung; inklusive Prozent
#### Davor compList erstellen und sum.wei.p etc % für Alle - je nachdem 
#### was für eine compList erstellt wurde, mit welchen quantile all, kommt etwas unterschiedliches raus
listi2<-data.all.r(dfal=listko[3],yearall=30,quall=0.99) # Zuerst compList erstellen (!!!)

#### (compList ->)Daten Vergleichen (via Minus Absolut Zahlen und Prozente, Liste erstellen und über alle
#### im Directory laufen lassen) 
compList<-lapply(listko[1:40],yearall=30,quall=0.99,FUN = list.compare)

#### Aufsummierte Funktionen (prozent und nur lmom) ohne a6xxxx nr. nur noch das ganze Directory
#### muss dann geändert werden, wenn ich mehr im Directory hab (zur Zeit nur 9)
#### Nicht vergessen die length(compList) dem [1:x] anzupassen und umbekehrt
#### compList yearall;quall anpassen -> Zuerst
wei.p<-lapply(compList[1:40],FUN=SumWei)
sum.wei.p<-sum(unlist(wei.p))/length(compList)
gum.p<-lapply(compList[1:40],FUN=Sumgum)
sum.gum.p<-sum(unlist(gum.p))/length(compList)
ln3.p<-lapply(compList[1:40],FUN=Sumln3)
sum.ln3.p<-sum(unlist(ln3.p))/length(compList)
pe3.p<-lapply(compList[1:40],FUN=Sumpe3)
sum.pe3.p<-sum(unlist(pe3.p))/length(compList)
dif.all.p<-cbind(sum.wei.p,sum.gum.p,sum.ln3.p,sum.pe3.p)

#### Plot Parameter
par(mfrow=c(3,1))
par(mfrow=c(2,2))
par(mfrow=c(2,3))

### (AM) shift over 30 years
sh<-shift_30(listko[[31]],quantile=0.99)
shift<-lapply(listko_y60[1:204],quantile=0.99,shift_30)

# plot shift (AM)
plot(shift[[31]][,1],type="l",ylim=c(min(shift[[31]]),max(shift[[31]])+350))
lines(shift[[31]][,2],col=2)
lines(shift[[31]][,3],col=3)
lines(shift[[31]][,4],col=4)
lines(sh_add[[1]]$df_c1[,1],lty=4,col=1,lwd=2)
lines(sh_add[[1]]$df_c1[,2],lty=4,col=2,lwd=2)
lines(sh_add[[1]]$df_c1[,3],lty=4,col=3,lwd=2)
lines(sh_add[[1]]$df_c1[,4],lty=4,col=4,lwd=2)
lala<-listko[[31]][4]
laal1<-listko[[31]][4]
lala[[1]][60*365.25]<-max(listko[[31]][4])
par(mfrow=c(2,1))
xyplot(lala[[1]]~c(1:30742,type="l"))
plot(laal1[[1]],type="l")
laa1<-list(lala)
str(lala[[1]])
dev.off()
nrow(listko[[31]][4])
sh_add[[1]]$df_c1[,2]
-shift[[31]]
xyplot(lala[[1]]~c(1:30742),type=c("l"),cex=1,panel.axis=F,lwd=3,lty=1,col="black")
       #par.settings = list(axis.line = list(col = "transparent"))
       #scales=list(x=list(at=NULL),alternating=2)
       #xlab=NULL,ylab=NULL,ylim=c(min(lala[[1]])-100,max(llshif1)+100),
       #key=list(corner=c(0.9,0.8), columns=1,rows=4
        #        lines=list(col=c("black","red","green","blue"),lty=1,lwd=3)
         #       text=list(c("Weibull","Gumbel","Pe3","Logn3"),cex=2)
      # ))

### (AM) shift plus add HQ1000
sh_add<-shift_30_add(listko[[31]],quantile=0.99)
sh_add_l<-lapply(listko[1:40],quantile=0.99,shift_30_add)

### (AM) standardize shift 30yrs
### (AM) standardize and plot
# make df standardize
shiftst<-shift
for (i in seq_along(shiftst)){
  for (j in seq(1,4,1)){
  shiftst[[i]][,j]<-scale(shiftst[[i]][,j])
  }
}

plot(shiftst[[1]][,1],type="l")
for (i in seq_along(shiftst)){
  lines(shiftst[[i]][,1],col=i,type="l")
}

# plot
plot(shift[[31]][,2]/meanngum[31,1],type="l",ylim=c(.5,1.5)) # plot standardized, then add other lines
for (i in seq_along(1:110)){
  lines(shift[[i]][,2]/meanngum[i,],col=i)
} # add lines to plot

### MLE Gumbel 30/100/1000 return Periods 
ahaa<-lapply(listko[1:40],rp=c(30,100,1000),shift_30_mle) # zeitintensiv
ahha<-shift_30_mle(listko[[31]],rp=100)
meann<-matrix(NA,40,3)
for (i in seq_along(1:40)){
  meann[i,1]<-mean(ahaa[[i]][,1])
  meann[i,2]<-mean(ahaa[[i]][,2])
  meann[i,3]<-mean(ahaa[[i]][,3])
} # mean
ahaa[[31]][,2]/meann[31,2]
dev.off()
plot(ahaa[[31]][,2]/meann[31,2],type=c("p"),ylim=c(0.8,1.25),lwd=3,col="red")
for (i in seq_along(1:40)){
  lines((ahaa[[i]][,2]/meann[i,2]),col=i)
  #lines((ahaa[[31]][,3]/meann[31,2]),col="red")
  #lines((ahaa[[31]][,1]/meann[31,2]),col="red")
} # lines (plot)

### MLE Gumbel 30/100/1000 return Periods plot (AM). Mit nur einem HQ100, konstant, ein sehr fluktuierendes, dann
### die restlichen Werte darüber (30 Jahre, über 50 Jahre Shift) und anschließend ein HQ1000
plot(ahaa[[31]][,2]/meann[31,2],type="l",lwd=3,col="red",main="6342925 - ISAR,  Landau")
const_HQ100<-matrix(rep(ahaa[[31]][1,2],50),50,1)
lines(const_HQ100/meann[31,2],col="3")
lines(ahaa[[31]][,3]/meann[31,2])
legend(35,1.04,c("Germany","Area: 8467 km^2","Series Start: 1925", "Series End 1999","GRDC_NO:6342925","Average Discharge: 167.8 m3/s"),lty=c(5,5))

### Vergleich Gumbel MLE mit 0.966667/0.99/0.999 (AM)
### d für HQ30/h->HQ100/t->HQ1000
maxyear<-aggregate(listko[[3]][[4]],by=list(listko[[3]][[1]]),max)
AM<-maxyear$x
gum.fit<-gum.fit(AM)
d_ml<-quagum(f = 0.99,para = c(pelgum(samlmu(AM))))
d_lm<-quagum(f = 0.99,para = c(gum.fit$mle))
h_ml<-quagum(f = 0.99,para = c(pelgum(samlmu(AM))))
h_lm<-quagum(f = 0.99,para = c(gum.fit$mle))
t_ml<-quagum(f = 0.99,para = c(pelgum(samlmu(AM))))
t_lm<-quagum(f = 0.99,para = c(gum.fit$mle))
(d_lm-d_ml)/d_lm
(h_lm-h_ml)/h_lm
(t_lm-t_ml)/t_lm


#### 80 years cummulated with hq1000 (AM) and plot
aha3<-lapply(listko[1:40],quantile=0.99,cum_80_add)
aha3[[31]]
mat<-matrix(NA,50,4) ## Summiert bei jeder Funktion seperiert, einen DF
for (i in seq_along(aha3)){
  mat[i,1]<-sum(aha3[[i]]$wei_p)/50
  mat[i,2]<-sum(aha3[[i]]$gum_p)/50
  mat[i,3]<-sum(aha3[[i]]$pe3_p)/50
  mat[i,4]<-sum(aha3[[i]]$ln3_p)/50
}
plot(mat[,1]*100,type="l",ylim=c(min(mat[c(1:40),c(1:4)])*100,max(mat[c(1:40),c(1:4)])*100),xlab="years",ylab="percent [%]",
      main=paste("Wei: ",mat2[1],"(%)",", Gum: ",mat2[2],"(%)",", Pe3: ",mat2[3],"(%)",", LN3: ",mat2[4],"(%)",sep=""),cex.main=0.9)
lines(mat[,2]*100,col=2)
lines(mat[,3]*100,col=3)
lines(mat[,4]*100,col=4)
lines(mat_all[,1]*100,col=1)
lines(mat_all[,2]*100,col=2)
lines(mat_all[,3]*100,col=3)
lines(mat_all[,4]*100,col=4)


## Summiert für jede Funktion, für alle DF einen einzigen Wert
mat2<-matrix(NA,1,4)
mat2[,1]<-sum(mat[,1]*100/50,na.rm=T)
mat2[,2]<-sum(mat[,2]*100/50,na.rm=T)
mat2[,3]<-sum(mat[,3]*100/50,na.rm=T)
mat2[,4]<-sum(mat[,4]*100/50,na.rm=T)

### Für je eine Funktion, alle Prozent in ein DF über die aha3 an der 1 bis i-ten Stelle geschreiben
mat_wei<-matrix(NA,40,50)
mat_gum<-matrix(NA,40,50)
mat_pe3<-matrix(NA,40,50)
mat_ln3<-matrix(NA,40,50)

for (i in seq_along(1:40)){
  for (j in seq_along(1:50)){
    mat_wei[i,j]<-aha3[[i]]$wei_p[j]
  }
} # Gum
for (i in seq_along(1:40)){
  for (j in seq_along(1:50)){
    mat_gum[i,j]<-aha3[[i]]$gum_p[j]
  }
} # Wei
for (i in seq_along(1:40)){
  for (j in seq_along(1:50)){
    mat_pe3[i,j]<-aha3[[i]]$pe3_p[j]
  }
} # Pe3
for (i in seq_along(1:40)){
  for (j in seq_along(1:50)){
    mat_ln3[i,j]<-aha3[[i]]$ln3_p[j]
  }
} # LN3

### Für je eine Funktion, alle Prozent aufsummiert über die aha3 an der 1 bis i-ten Stelle
mat_all<-matrix(NA,50,4)
for(i in c(1:50)){
  mat_all[i,1]<-sum(mat_wei[,i])/50
  mat_all[i,2]<-sum(mat_gum[,i])/50
  mat_all[i,3]<-sum(mat_pe3[,i])/50
  mat_all[i,4]<-sum(mat_ln3[,i])/50
}
colnames(mat_all)<-c("wei","gum","pe3","ln3")

plot(mat_all[,1]*100,type="l",ylim=c(min(mat_all)*100,max(mat_all)*100),xlab="years",ylab="percent [%]")
lines(mat_all[,2]*100,col=2)
lines(mat_all[,3]*100,col=3)
lines(mat_all[,4]*100,col=4)
mat_all
#################################### Which distribution fits best. then which is significantly different
str(shift)
#shift<-shift_30_227
shift<-shift_30
#shift<-shift_40
#shift<-shift_50
a<-descdist(shiftst[[1]][,4])
points(x=(1-a$kurtosis),y = a$skewness)
plot(sort(shift[[3]][,1]),t="l")
plot(runif(n = 70,min = 9003,max=10885),type="l")
plot(sort(runif(n = 70,min = 9003,max=10885)),t="l")
densityplot(~shift[[3]][,1])
?descdist
10-a$kurtosis
###############################################################################################
######################### Nützliches #########################################################
gum.mle<-fevd(listko[[3]][[4]])
gum.mle
return.level(gum.mle)
ah<-ci(gum.mle, return.period=c(30,100,10000))
findpars(gum.mle)
plot(density(listko[[14]][[4]]))

### Get the function code
getAnywhere('cummul_80_f')
methods(mean)

## Vioplot (noch als Funktion abspeichern)
summary(lm(listi2$Wei~listi2$Obs))$adj.r.squared
summary(lm(listi2$Gum~listi2$Obs))$adj.r.squared
summary(lm(listi2$LN3~listi2$Obs))$adj.r.squared
summary(lm(listi2$PE3~listi2$Obs))$adj.r.squared

##### Plot lmom,mle,obs 
xyplot(listi$Wei~listi$Obs,main=listi$Names,ylab=names(listi)[1],xlab=names(listi)[5])

##### Matrix of correlations and p-values (mit listi2)
rcorr(cbind(listi2[1][[1]],listi2[2][[1]],listi2[3][[1]],listi2[4][[1]],listi2[5][[1]]),type=c("pearson","spearman"))

# Add a column to PV that you can visually check the regular expression matches.
PV$Match <- grepl(pattern = "(de|te)$", PV$Word)

# Subset PV data frame to show only TRUE matches
PV <- PV[PV$Match == FALSE, ]


layout( matrix( c(1,1,0,2),ncol=2),widths=c(1,5) )