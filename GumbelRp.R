### 19.05.2015
### MLE Gumbel 30/100/1000 return Periods  
  shift<-matrix(NA,50,3)
  colnames(shift)<-c("30","100","1000")
  seqq<-NULL
shift_30_mle<-function(df,rp,...){
    
  for (i in seq_along(1:50)){
    seqq<-seq(i*365.25-365.25,(29+i)*365.25,1)
    y0.gum<-fevd(df[[4]][seqq],type="Gumbel")
    rl<-ci(y0.gum,return.period = rp)
    shift[i,1]<-rl[4]
    shift[i,2]<-rl[5]
    shift[i,3]<-rl[6]
  }
  return(shift)
  }
  
ahaa<-lapply(listko[1:40],rp=c(30,100,1000),shift_30_mle) # zeitintensiv

meann<-matrix(NA,40,3)
for (i in seq_along(1:40)){
  meann[i,1]<-mean(ahaa[[i]][,1])
  meann[i,2]<-mean(ahaa[[i]][,2])
  meann[i,3]<-mean(ahaa[[i]][,3])
} # mean
plot(ahaa[[3]][,2]/meann[3,2],type="l",ylim=c(0.7,1.4),lwd=3,col="red")
for (i in seq_along(1:40)){
  lines((ahaa[[i]][,2]/meann[i,2]),col=i)
  lines((ahaa[[3]][,3]/meann[3,2]),col="red")
  lines((ahaa[[3]][,1]/meann[3,2]),col="red")
} # lines (plot)

ahaa[[3]]

### Vergleich Gumbel MLE mit 0.966667/0.99/0.999
ffgg<-unlist(listko[[3]][[1]])

maxyear<-aggregate(listko[[3]][[4]],by=list(listko[[3]][[1]]),max)
  AM<-maxyear$x
gum.fit<-gum.fit(AM)
lmomgum<-pelgum(samlmu(AM))
lmomgum
quagum(f = 0.96666667,para = c(pelgum(samlmu(AM))))
  
### Nützliches
gum.mle<-fevd(listko[[3]][[4]])
gum.mle
return.level(gum.mle)
ah<-ci(gum.mle, return.period=c(30,100,10000))
findpars(gum.mle)
plot(density(listko[[14]][[4]]))
methods("ci")
getAnywhere(ci) 
