libs <- c('lattice','gtable','hexbin','ggplot2','corrplot', 'latticeExtra', 'gridExtra', 'MASS', 
          'colorspace', 'plyr', 'Hmisc', 'scales','plotrix')
lapply(libs, require, character.only = T)
######################################################
################################# shift listko_y60 // a6142200 is for y100 shiftst[2] and for y60 shiftst[15]
listko<-listko_y60
shiftst<-shiftst_30_227
names(shiftst[15])
################################### shift 30 (standardized)
# only one river
xyplot(shiftst[[15]][,1]+shiftst[[15]][,2]+shiftst[[15]][,3]+shiftst[[15]][,4]~c(1948:1977),type=c("l","smooth"),
       col.line=c("black","red","darkgreen","blue"),xlab="Years",ylab="Standardized Discharge",grid=TRUE)

# all rivers separately 
xyplot_shift<-xyplot(reg_df$value~reg_df$years|reg_df$GRDCNR,grid=TRUE,type=c("l"),
                     groups=reg_df$func,col.line=c("black","red","darkgreen","blue"),
                     par.settings=simpleTheme(col=c("black","red","darkgreen","blue")),
                     lwd=2,xlab="Years",ylab="Standardized Discharge",auto.key=list(text=
                      c("Weibull","Gumbel","Pearson III", "Log-Normal III"),relation="same",
                      x=0.91,y=.94,corner=c(0.8,0.8,1,1),col=c("black","red","darkgreen","blue"),
                      pch=8,cex=1.8,lines=TRUE,points=FALSE))
xyplot_shift[1:55]
xyplot_shift[56:115]
xyplot_shift[116:175]
xyplot_shift[176:227]
#all rivers together with a xyplot as points and loess - not good. shouldnt be used
xyplot(reg_df$value~reg_df$years,grid=TRUE,type=c("p","smooth"),groups=reg_df$func,col.lines=c("black","red","darkgreen","blue"),col=c("black","red","darkgreen","blue"))
# all rivers smooth and non smooth
only_shift<-xyplot(reg_df$value~reg_df$years|reg_df$func,groups=reg_df$GRDCNR,type=c("","l"),grid=TRUE,
       xlab="Years",ylab="Standardized Discharge")
smooth_shift<-xyplot(reg_df$value~reg_df$years|reg_df$func,groups=reg_df$GRDCNR,type=c("","smooth"),grid=TRUE,
       xlab="Years",ylab="")
print(only_shift, pos = c(0, 0, 0.52, 1), more = TRUE)
print(smooth_shift, pos = c(0.48, 0, 1, 1))

# all rivers grouped by func, only smoothed
xyplot_shift<-xyplot(reg_df$value~reg_df$years|reg_df$func,grid=TRUE,type=c("smooth","smooth","smooth","smooth"),
col.line=c("black","red","darkgreen","blue"),groups=reg_df$GRDC,
lwd=2,xlab="Years",ylab="Standardized Discharge")
(xyplot_shift)
# all rivers only density
xyplot(reg_df$value~reg_df$years|reg_df$func,panel=panel.smoothScatter,auto.key=TRUE,grid=TRUE)

# plot hexbinplot with smooth line
(hexbinplot(reg_df$value~reg_df$years|reg_df$func,auto.key=TRUE,
           xlab="Years",ylab="Standardized Discharge",
           aspect = 1, bins=50))+as.layer(xyplot_shift2)


# plot density plot with smooth line
xyplot_shift2<-xyplot(reg_df$value~reg_df$years|reg_df$func,type=c("smooth","smooth","smooth","smooth"),
                     col.line=c("orange"),grid=TRUE,
                     lwd=2,xlab="Years",ylab="Standardized Discharge")
xyplot(reg_df$value~reg_df$years|reg_df$func,panel=panel.smoothScatter,xlab="Years",
       ylab="Standardized Discharge")+as.layer(xyplot_shift2)

####simple shift for accumulated

shiftst
shiftst_30_227<-shiftst
shiftst_30_227
############################### Correlation Matrix
################### Correlation 227 Rivers, shift 30 years. get correlation first
#Weibull
shkw<-matrix(NA,30,227)
for (i in seq(1,227,1)){
  for (j in seq(1,30,1)){
    shkw[j,i]<-shiftst[[i]][j,1]
    
  }
}
colnames(shkw)<-dput(names(shiftst))
rownames(shkw)<-seq(1948,1977,1)
shkwt<-t(shkw)
reswt<-cor(shkwt,shkwt)
resw<-cor(shkw,shkw)

#Gumbel
shkg<-matrix(NA,30,227)
for (i in seq(1,227,1)){
  for (j in seq(1,30,1)){
    shkg[j,i]<-shiftst[[i]][j,2]
    
  }
}
colnames(shkg)<-dput(names(shift[1:227]))
rownames(shkg)<-seq(1948,1977,1)
shkgt<-t(shkg)
resgt<-cor(shkgt,shkgt)
resg<-cor(shkg,shkg)

#Pe3
shkp<-matrix(NA,30,227)
for (i in seq(1,227,1)){
  for (j in seq(1,30,1)){
    shkp[j,i]<-shiftst[[i]][j,3]
    
  }
}
colnames(shkp)<-dput(names(shift))
rownames(shkp)<-seq(1948,1977,1)
shkpt<-t(shkp)
respt<-cor(shkpt,shkpt)
resp<-cor(shkp,shkp)

shkl<-matrix(NA,30,227)
for (i in seq(1,227,1)){
  for (j in seq(1,30,1)){
    shkl[j,i]<-shiftst[[i]][j,4]
    
  }
}
colnames(shkl)<-dput(names(shift))
rownames(shkl)<-seq(1948,1977,1)
shklt<-t(shkl)
reslt<-cor(shklt,shklt)

############################### Regions. corrplot for y60
### plot all regions
resw_plot<-corrplot(resw,order="hclust",addrect=6,cl.ratio = 0.1, cl.align = "r",tl.cex=0.5)
comp<-dput(maa_y60[,1])


#### find regions via k-means/k-medoid/k-median

## Region1 6233460:6855402; 1:92. get values and coordinates
region1<-matrix(NA,92,92)
colnames(region1)<-dput(colnames(resw_plot)[1:92])
rownames(region1)<-dput(rownames(resw_plot)[1:92])
for (i in seq(1:92)){
  for(j in seq(1:92)){
    region1[i,j]<-resw_plot[i,j]
    
  }
}
reg1_cor_y60<-matrix(NA,92,3)
for (i in seq(1:92)){
  for (j in seq(1:227))
    if(colnames(region1)[i]==comp[j]){
      reg1_cor_y60[i,]<-maa_y60[j,]
    }
}
# Region 1 - correlation via years - not useful
shiftforcor1<-list()
for (i in seq(1:92)){
  for (j in seq(1:227))
    if(colnames(region1)[i]==comp[j]){
      shiftforcor1[i]<-shiftst_30_227[j]
      names(shiftforcor1)[i]<-names(shiftst_30_227)[j]
    }
}
shkw_r1<-matrix(NA,30,92)
for (i in seq(1,92,1)){
  for (j in seq(1,30,1)){
    shkw_r1[j,i]<-shiftforcor1[[i]][j,1]
  }
}
colnames(shkw_r1)<-dput(names(shiftforcor1))
rownames(shkw_r1)<-seq(1948,1977,1)
shkwt_r1<-t(shkw_r1)
reswt_r1<-cor(shkwt_r1,shkwt_r1)
resw_r1<-cor(shkw_r1,shkw_r1)
#corrplot(reswt_r1,tl.cex = 0.7)
#corrplot(resw_r1,tl.cex = 0.7)

######region2 /// 93:126 6855403:6357502
region2<-matrix(NA,34,34)
colnames(region2)<-dput(colnames(resw_plot)[93:126])
rownames(region2)<-dput(rownames(resw_plot)[93:126])
for (i in seq(1:34)){
  for(j in seq(1:34)){
    region2[i,j]<-resw_plot[(i+92),(j+92)]
    
  }
}
reg2_cor_y60<-matrix(NA,34,3)
for (i in seq(1:34)){
  for (j in seq(1:227))
    if(colnames(region2)[i]==comp[j]){
      reg2_cor_y60[i,]<-maa_y60[j,]
    }
}
# Region 2 - correlation via years
shiftforcor2<-list()
for (i in seq(1:34)){
  for (j in seq(1:227))
    if(colnames(region2)[i]==comp[j]){
      shiftforcor2[i]<-shiftst_30_227[j]
      names(shiftforcor2)[i]<-names(shiftst_30_227)[j]
    }
}
shkw_r2<-matrix(NA,30,34)
for (i in seq(1,34,1)){
  for (j in seq(1,30,1)){
    shkw_r2[j,i]<-shiftforcor2[[i]][j,1]
  }
}

colnames(shkw_r2)<-dput(names(shiftforcor2))
rownames(shkw_r2)<-seq(1948,1977,1)
shkwt_r2<-t(shkw_r2)
reswt_r2<-cor(shkwt_r2,shkwt_r2)
resw_r2<-cor(shkw_r2,shkw_r2)
#corrplot(reswt_r2,tl.cex = 0.7)
#corrplot(resw_r2,tl.cex = 0.7)


######region3 127:154   6854200:6607651
region3<-matrix(NA,28,28)
colnames(region3)<-dput(colnames(resw_plot)[127:154])
rownames(region3)<-dput(rownames(resw_plot)[127:154])
for (i in seq(1:28)){
  for(j in seq(1:28)){
    region3[i,j]<-resw_plot[(i+126),(j+126)]
    
  }
}
reg3_cor_y60<-matrix(NA,28,3)
for (i in seq(1:28)){
  for (j in seq(1:227))
    if(colnames(region3)[i]==comp[j]){
      reg3_cor_y60[i,]<-maa_y60[j,]
    }
}
# Region 3 - correlation via years
shiftforcor3<-list()
for (i in seq(1:28)){
  for (j in seq(1:227))
    if(colnames(region3)[i]==comp[j]){
      shiftforcor3[i]<-shiftst_30_227[j]
      names(shiftforcor3)[i]<-names(shiftst_30_227)[j]
    }
}
shkw_r3<-matrix(NA,30,28)
for (i in seq(1,28,1)){
  for (j in seq(1,30,1)){
    shkw_r3[j,i]<-shiftforcor3[[i]][j,1]
  }
}

colnames(shkw_r3)<-dput(names(shiftforcor3))
rownames(shkw_r3)<-seq(1948,1977,1)
shkwt_r3<-t(shkw_r3)
reswt_r3<-cor(shkwt_r3,shkwt_r3)
resw_r3<-cor(shkw_r3,shkw_r3)
#corrplot(reswt_r3,tl.cex = 0.7)
#corrplot(resw_r3,tl.cex = 0.7)


####region4 155:172
region4<-matrix(NA,18,18)
colnames(region4)<-dput(colnames(resw_plot)[155:172])
rownames(region4)<-dput(rownames(resw_plot)[155:172])
for (i in seq(1:18)){
  for(j in seq(1:18)){
    region4[i,j]<-resw_plot[(i+154),(j+154)]
    
  }
}
reg4_cor_y60<-matrix(NA,18,3)
for (i in seq(1:18)){
  for (j in seq(1:227))
    if(colnames(region4)[i]==comp[j]){
      reg4_cor_y60[i,]<-maa_y60[j,]
    }
}
# Region 4 - correlation via years
shiftforcor4<-list()
for (i in seq(1:18)){
  for (j in seq(1:227))
    if(colnames(region4)[i]==comp[j]){
      shiftforcor4[i]<-shiftst_30_227[j]
      names(shiftforcor4)[i]<-names(shiftst_30_227)[j]
    }
}
shkw_r4<-matrix(NA,30,18)
for (i in seq(1,18,1)){
  for (j in seq(1,30,1)){
    shkw_r4[j,i]<-shiftforcor4[[i]][j,1]
  }
}

colnames(shkw_r4)<-dput(names(shiftforcor4))
rownames(shkw_r4)<-seq(1948,1977,1)
shkwt_r4<-t(shkw_r4)
reswt_r4<-cor(shkwt_r4,shkwt_r4)
resw_r4<-cor(shkw_r4,shkw_r4)
#corrplot(reswt_r4,tl.cex = 0.7)
#corrplot(resw_r4,tl.cex = 0.7)


#region5 173:201   6545200:6935145
region5<-matrix(NA,29,29)
colnames(region5)<-dput(colnames(resw_plot)[173:201])
rownames(region5)<-dput(rownames(resw_plot)[173:201])
for (i in seq(1:29)){
  for(j in seq(1:29)){
    region5[i,j]<-resw_plot[(i+172),(j+172)]
    
  }
}
reg5_cor_y60<-matrix(NA,29,3)
for (i in seq(1:29)){
  for (j in seq(1:227))
    if(colnames(region5)[i]==comp[j]){
      reg5_cor_y60[i,]<-maa_y60[j,]
    }
}
# Region 5 - correlation via years
shiftforcor5<-list()
for (i in seq(1:29)){
  for (j in seq(1:227))
    if(colnames(region5)[i]==comp[j]){
      shiftforcor5[i]<-shiftst_30_227[j]
      names(shiftforcor5)[i]<-names(shiftst_30_227)[j]
    }
}
shkw_r5<-matrix(NA,30,29)
for (i in seq(1,29,1)){
  for (j in seq(1,30,1)){
    shkw_r5[j,i]<-shiftforcor5[[i]][j,1]
  }
}


colnames(shkw_r5)<-dput(names(shiftforcor5))
rownames(shkw_r5)<-seq(1948,1977,1)
shkwt_r5<-t(shkw_r5)
reswt_r5<-cor(shkwt_r5,shkwt_r5)
resw_r5<-cor(shkw_r5,shkw_r5)
#corrplot(reswt_r5,tl.cex = 0.7)
#corrplot(resw_r5,tl.cex = 0.7)

#region6 202:227   6343555:6342910
region6<-matrix(NA,26,26)
colnames(region6)<-dput(colnames(resw_plot)[202:227])
rownames(region6)<-dput(rownames(resw_plot)[202:227])
for (i in seq(1:26)){
  for(j in seq(1:26)){
    region6[i,j]<-resw_plot[(i+201),(j+201)]
    
  }
}
reg6_cor_y60<-matrix(NA,26,3)
for (i in seq(1:26)){
  for (j in seq(1:227))
    if(colnames(region6)[i]==comp[j]){
      reg6_cor_y60[i,]<-maa_y60[j,]
    }
}
# Region 6 - correlation via years
shiftforcor6<-list()
for (i in seq(1:26)){
  for (j in seq(1:227))
    if(colnames(region6)[i]==comp[j]){
      shiftforcor6[i]<-shiftst_30_227[j]
      names(shiftforcor6)[i]<-names(shiftst_30_227)[j]
    }
}
shkw_r6<-matrix(NA,30,26)
for (i in seq(1,26,1)){
  for (j in seq(1,30,1)){
    shkw_r6[j,i]<-shiftforcor6[[i]][j,1]
  }
}

colnames(shkw_r6)<-dput(names(shiftforcor6))
rownames(shkw_r6)<-seq(1948,1977,1)
shkwt_r6<-t(shkw_r6)
reswt_r6<-cor(shkwt_r6,shkwt_r6)
resw_r6<-cor(shkw_r6,shkw_r6)
#corrplot(reswt_r6,tl.cex = 0.7)
#corrplot(resw_r6,tl.cex = 0.7)

#################### Plot p-value of corrplot with sign/insign
cor.mtest <- function(mat, conf.level = 0.95) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  diag(lowCI.mat) <- diag(uppCI.mat) <- 1
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], conf.level = conf.level)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
      lowCI.mat[i, j] <- lowCI.mat[j, i] <- tmp$conf.int[1]
      uppCI.mat[i, j] <- uppCI.mat[j, i] <- tmp$conf.int[2]
    }
  }
  return(list(p.mat, lowCI.mat, uppCI.mat))
}

res1_r1 <- cor.mtest(reswt_r1, 0.95)
res1_r2 <- cor.mtest(reswt_r2, 0.95)
res1_r3 <- cor.mtest(reswt_r3, 0.95)
res1_r4 <- cor.mtest(reswt_r4, 0.95)
res1_r5 <- cor.mtest(reswt_r5, 0.95)
res1_r6 <- cor.mtest(reswt_r6, 0.95)
res2 <- cor.mtest(mtcars, 0.99)
## specialized the insignificant value according to the significant level
#corrplot(resw_r1, p.mat = res1_r1[[1]], insig = "blank",sig.level=0.05,tl.cex = 0.7)
#corrplot(resw_r2, p.mat = res1_r2[[1]], insig = "blank",sig.level=0.05,tl.cex = 0.7)
#corrplot(resw_r3, p.mat = res1_r3[[1]], insig = "blank",sig.level=0.05,tl.cex = 0.7)
#corrplot(resw_r4, p.mat = res1_r4[[1]], insig = "blank",sig.level=0.05,tl.cex = 0.7)
#corrplot(resw_r5, p.mat = res1_r5[[1]], insig = "blank",sig.level=0.05,tl.cex = 0.7)
#corrplot(resw_r6, p.mat = res1_r6[[1]], insig = "blank",sig.level=0.05,tl.cex = 0.7)

##################################
################################# get DF for regions and plot 
######## region 1
region1_shiftst<-list()
for(i in seq(1:92)){
  for (j in seq(1:227)){
    if((colnames(region1))[i]==names(shiftst[j]))
      region1_shiftst[i]<-shiftst[j]
  }}
for(i in seq(1:92)){
  for (j in seq(1:227)){
    if((colnames(region1))[i]==names(shiftst[j]))
      names(region1_shiftst)[i]<-names(shiftst[j])
  }}

reg1<-matrix(NA,(92*30*4),1)
for(i in seq(1:92)){
  reg1[c((i*30-29):(i*30)),1]<-region1_shiftst[[i]][,1]
  reg1[c((i*30-29+(92*30)):(i*30+(92*30))),1]<-region1_shiftst[[i]][,2]
  reg1[c((i*30-29+(92*30*2)):(i*30+(92*30*2))),1]<-region1_shiftst[[i]][,3]
  reg1[c((i*30-29+(92*30*3)):(i*30+(92*30*3))),1]<-region1_shiftst[[i]][,4]
}
re1_nam<-matrix(NA,(92*30*4),1)
for(i in seq(1:92)){
  re1_nam[c((i*30-29):(i*30)),1]<-rep(noquote(names(region1_shiftst))[i],30)
  re1_nam[c((i*30-29+(92*30)):(i*30+(92*30))),1]<-rep(noquote(names(region1_shiftst))[i],30)
  re1_nam[c((i*30-29+(92*30*2)):(i*30+(92*30*2))),1]<-rep(noquote(names(region1_shiftst))[i],30)
  re1_nam[c((i*30-29+(92*30*3)):(i*30+(92*30*3))),1]<-rep(noquote(names(region1_shiftst))[i],30)
}
re1_lon<-matrix(NA,(92*30*4),1)
re1_lat<-matrix(NA,(92*30*4),1)
for(i in seq(1:92)){
  re1_lon[c((i*30-29):(i*30)),1]<-rep(reg1_cor_y60[i,2],30)
  re1_lon[c((i*30-29+(92*30)):(i*30+(92*30))),1]<-rep(reg1_cor_y60[i,2],30)
  re1_lon[c((i*30-29+(92*30*2)):(i*30+(92*30*2))),1]<-rep(reg1_cor_y60[i,2],30)
  re1_lon[c((i*30-29+(92*30*3)):(i*30+(92*30*3))),1]<-rep(reg1_cor_y60[i,2],30)
  re1_lat[c((i*30-29):(i*30)),1]<-rep(reg1_cor_y60[i,3],30)
  re1_lat[c((i*30-29+(92*30)):(i*30+(92*30))),1]<-rep(reg1_cor_y60[i,3],30)
  re1_lat[c((i*30-29+(92*30*2)):(i*30+(92*30*2))),1]<-rep(reg1_cor_y60[i,3],30)
  re1_lat[c((i*30-29+(92*30*3)):(i*30+(92*30*3))),1]<-rep(reg1_cor_y60[i,3],30)
}
reg1_shiftst_df<-data.frame(rep(rep(c(1948:1977),92),4),c(rep("wei",(92*30)),rep("gum",(92*30)),
  rep("pe3",(92*30)),rep("ln3",(92*30))),reg1[,1],re1_nam,rep("Region 1",11040),
  as.numeric(re1_lon),as.numeric(re1_lat))
names(reg1_shiftst_df)[1]<-"years"
names(reg1_shiftst_df)[2]<-"func"
names(reg1_shiftst_df)[3]<-"value"
names(reg1_shiftst_df)[4]<-"GRDCNR"
names(reg1_shiftst_df)[5]<-"region"
names(reg1_shiftst_df)[6]<-"lon"
names(reg1_shiftst_df)[7]<-"lat"

### region 2
region2_shiftst<-list()
for(i in seq(1:34)){
  for (j in seq(1:227)){
    if((colnames(region2))[i]==names(shiftst[j]))
      region2_shiftst[i]<-shiftst[j]
  }}
for(i in seq(1:34)){
  for (j in seq(1:227)){
    if((colnames(region2))[i]==names(shiftst[j]))
      names(region2_shiftst)[i]<-names(shiftst[j])
  }}
reg2<-matrix(NA,(34*30*4),1)
for(i in seq(1:34)){
  reg2[c((i*30-29):(i*30)),1]<-region2_shiftst[[i]][,1]
  reg2[c((i*30-29+(34*30)):(i*30+(34*30))),1]<-region2_shiftst[[i]][,2]
  reg2[c((i*30-29+(34*30*2)):(i*30+(34*30*2))),1]<-region2_shiftst[[i]][,3]
  reg2[c((i*30-29+(34*30*3)):(i*30+(34*30*3))),1]<-region2_shiftst[[i]][,4]
}
re2_nam<-matrix(NA,(34*30*4),1)
for(i in seq(1:34)){
  re2_nam[c((i*30-29):(i*30)),1]<-rep(noquote(names(region2_shiftst))[i],30)
  re2_nam[c((i*30-29+(34*30)):(i*30+(34*30))),1]<-rep(noquote(names(region2_shiftst))[i],30)
  re2_nam[c((i*30-29+(34*30*2)):(i*30+(34*30*2))),1]<-rep(noquote(names(region2_shiftst))[i],30)
  re2_nam[c((i*30-29+(34*30*3)):(i*30+(34*30*3))),1]<-rep(noquote(names(region2_shiftst))[i],30)
}
re2_lon<-matrix(NA,(34*30*4),1)
re2_lat<-matrix(NA,(34*30*4),1)
for(i in seq(1:34)){
  re2_lon[c((i*30-29):(i*30)),1]<-rep(reg2_cor_y60[i,2],30)
  re2_lon[c((i*30-29+(34*30)):(i*30+(34*30))),1]<-rep(reg2_cor_y60[i,2],30)
  re2_lon[c((i*30-29+(34*30*2)):(i*30+(34*30*2))),1]<-rep(reg2_cor_y60[i,2],30)
  re2_lon[c((i*30-29+(34*30*3)):(i*30+(34*30*3))),1]<-rep(reg2_cor_y60[i,2],30)
  re2_lat[c((i*30-29):(i*30)),1]<-rep(reg2_cor_y60[i,3],30)
  re2_lat[c((i*30-29+(34*30)):(i*30+(34*30))),1]<-rep(reg2_cor_y60[i,3],30)
  re2_lat[c((i*30-29+(34*30*2)):(i*30+(34*30*2))),1]<-rep(reg2_cor_y60[i,3],30)
  re2_lat[c((i*30-29+(34*30*3)):(i*30+(34*30*3))),1]<-rep(reg2_cor_y60[i,3],30)
}
#re2_nam<-rep(rep(dput(noquote(names(region2_shiftst))),30),4)
reg2_shiftst_df<-data.frame(rep(rep(c(1948:1977),34),4),c(rep("wei",(34*30)),rep("gum",(34*30)),
  rep("pe3",(34*30)),rep("ln3",(34*30))),reg2[,1],re2_nam,rep("Region 2",4080),
  as.numeric(re2_lon),as.numeric(re2_lat))
names(reg2_shiftst_df)[1]<-"years"
names(reg2_shiftst_df)[2]<-"func"
names(reg2_shiftst_df)[3]<-"value"
names(reg2_shiftst_df)[4]<-"GRDCNR"
names(reg2_shiftst_df)[5]<-"region"
names(reg2_shiftst_df)[6]<-"lon"
names(reg2_shiftst_df)[7]<-"lat"

### region 3
region3_shiftst<-list()
for(i in seq(1:28)){
  for (j in seq(1:227)){
    if((colnames(region3))[i]==names(shiftst[j]))
      region3_shiftst[i]<-shiftst[j]
  }}
for(i in seq(1:28)){
  for (j in seq(1:227)){
    if((colnames(region3))[i]==names(shiftst[j]))
      names(region3_shiftst)[i]<-names(shiftst[j])
  }}
reg3<-matrix(NA,(28*30*4),1)
for(i in seq(1:28)){
  reg3[c((i*30-29):(i*30)),1]<-region3_shiftst[[i]][,1]
  reg3[c((i*30-29+(28*30)):(i*30+(28*30))),1]<-region3_shiftst[[i]][,2]
  reg3[c((i*30-29+(28*30*2)):(i*30+(28*30*2))),1]<-region3_shiftst[[i]][,3]
  reg3[c((i*30-29+(28*30*3)):(i*30+(28*30*3))),1]<-region3_shiftst[[i]][,4]
}

re3_nam<-matrix(NA,(28*30*4),1)
for(i in seq(1:28)){
  re3_nam[c((i*30-29):(i*30)),1]<-rep(noquote(names(region3_shiftst))[i],30)
  re3_nam[c((i*30-29+(28*30)):(i*30+(28*30))),1]<-rep(noquote(names(region3_shiftst))[i],30)
  re3_nam[c((i*30-29+(28*30*2)):(i*30+(28*30*2))),1]<-rep(noquote(names(region3_shiftst))[i],30)
  re3_nam[c((i*30-29+(28*30*3)):(i*30+(28*30*3))),1]<-rep(noquote(names(region3_shiftst))[i],30)
}
re3_lon<-matrix(NA,(28*30*4),1)
re3_lat<-matrix(NA,(28*30*4),1)
for(i in seq(1:28)){
  re3_lon[c((i*30-29):(i*30)),1]<-rep(reg3_cor_y60[i,2],30)
  re3_lon[c((i*30-29+(28*30)):(i*30+(28*30))),1]<-rep(reg3_cor_y60[i,2],30)
  re3_lon[c((i*30-29+(28*30*2)):(i*30+(28*30*2))),1]<-rep(reg3_cor_y60[i,2],30)
  re3_lon[c((i*30-29+(28*30*3)):(i*30+(28*30*3))),1]<-rep(reg3_cor_y60[i,2],30)
  re3_lat[c((i*30-29):(i*30)),1]<-rep(reg3_cor_y60[i,3],30)
  re3_lat[c((i*30-29+(28*30)):(i*30+(28*30))),1]<-rep(reg3_cor_y60[i,3],30)
  re3_lat[c((i*30-29+(28*30*2)):(i*30+(28*30*2))),1]<-rep(reg3_cor_y60[i,3],30)
  re3_lat[c((i*30-29+(28*30*3)):(i*30+(28*30*3))),1]<-rep(reg3_cor_y60[i,3],30)
}

#re3_nam<-rep(rep(dput(noquote(names(region3_shiftst))),30),4)
reg3_shiftst_df<-data.frame(rep(rep(c(1948:1977),28),4),c(rep("wei",(28*30)),rep("gum",(28*30)),
  rep("pe3",(28*30)),rep("ln3",(28*30))),reg3[,1],re3_nam,rep("Region 3",3360),
  as.numeric(re3_lon),as.numeric(re3_lat))
names(reg3_shiftst_df)[1]<-"years"
names(reg3_shiftst_df)[2]<-"func"
names(reg3_shiftst_df)[3]<-"value"
names(reg3_shiftst_df)[4]<-"GRDCNR"
names(reg3_shiftst_df)[5]<-"region"
names(reg3_shiftst_df)[6]<-"lon"
names(reg3_shiftst_df)[7]<-"lat"

### region 4
region4_shiftst<-list()
for(i in seq(1:18)){
  for (j in seq(1:227)){
    if((colnames(region4))[i]==names(shiftst[j]))
      region4_shiftst[i]<-shiftst[j]
  }}
for(i in seq(1:18)){
  for (j in seq(1:227)){
    if((colnames(region4))[i]==names(shiftst[j]))
      names(region4_shiftst)[i]<-names(shiftst[j])
  }}
reg4<-matrix(NA,(18*30*4),1)
for(i in seq(1:18)){
  reg4[c((i*30-29):(i*30)),1]<-region4_shiftst[[i]][,1]
  reg4[c((i*30-29+(18*30)):(i*30+(18*30))),1]<-region4_shiftst[[i]][,2]
  reg4[c((i*30-29+(18*30*2)):(i*30+(18*30*2))),1]<-region4_shiftst[[i]][,3]
  reg4[c((i*30-29+(18*30*3)):(i*30+(18*30*3))),1]<-region4_shiftst[[i]][,4]
}

re4_nam<-matrix(NA,(18*30*4),1)
for(i in seq(1:18)){
  re4_nam[c((i*30-29):(i*30)),1]<-rep(noquote(names(region4_shiftst))[i],30)
  re4_nam[c((i*30-29+(18*30)):(i*30+(18*30))),1]<-rep(noquote(names(region4_shiftst))[i],30)
  re4_nam[c((i*30-29+(18*30*2)):(i*30+(18*30*2))),1]<-rep(noquote(names(region4_shiftst))[i],30)
  re4_nam[c((i*30-29+(18*30*3)):(i*30+(18*30*3))),1]<-rep(noquote(names(region4_shiftst))[i],30)
}
re4_lon<-matrix(NA,(18*30*4),1)
re4_lat<-matrix(NA,(18*30*4),1)
for(i in seq(1:18)){
  re4_lon[c((i*30-29):(i*30)),1]<-rep(reg4_cor_y60[i,2],30)
  re4_lon[c((i*30-29+(18*30)):(i*30+(18*30))),1]<-rep(reg4_cor_y60[i,2],30)
  re4_lon[c((i*30-29+(18*30*2)):(i*30+(18*30*2))),1]<-rep(reg4_cor_y60[i,2],30)
  re4_lon[c((i*30-29+(18*30*3)):(i*30+(18*30*3))),1]<-rep(reg4_cor_y60[i,2],30)
  re4_lat[c((i*30-29):(i*30)),1]<-rep(reg4_cor_y60[i,3],30)
  re4_lat[c((i*30-29+(18*30)):(i*30+(18*30))),1]<-rep(reg4_cor_y60[i,3],30)
  re4_lat[c((i*30-29+(18*30*2)):(i*30+(18*30*2))),1]<-rep(reg4_cor_y60[i,3],30)
  re4_lat[c((i*30-29+(18*30*3)):(i*30+(18*30*3))),1]<-rep(reg4_cor_y60[i,3],30)
}


#re4_nam<-rep(rep(dput(noquote(names(region4_shiftst))),30),4)
reg4_shiftst_df<-data.frame(rep(rep(c(1948:1977),18),4),c(rep("wei",(18*30)),rep("gum",(18*30)),
  rep("pe3",(18*30)),rep("ln3",(18*30))),reg4[,1],re4_nam,rep("Region 4",2160),
  as.numeric(re4_lon),as.numeric(re4_lat))
names(reg4_shiftst_df)[1]<-"years"
names(reg4_shiftst_df)[2]<-"func"
names(reg4_shiftst_df)[3]<-"value"
names(reg4_shiftst_df)[4]<-"GRDCNR"
names(reg4_shiftst_df)[5]<-"region"
names(reg4_shiftst_df)[6]<-"lon"
names(reg4_shiftst_df)[7]<-"lat"

### region 5
region5_shiftst<-list()
for(i in seq(1:29)){
  for (j in seq(1:227)){
    if((colnames(region5))[i]==names(shiftst[j]))
      region5_shiftst[i]<-shiftst[j]
  }}
for(i in seq(1:29)){
  for (j in seq(1:227)){
    if((colnames(region5))[i]==names(shiftst[j]))
      names(region5_shiftst)[i]<-names(shiftst[j])
  }}
reg5<-matrix(NA,(29*30*4),1)
for(i in seq(1:29)){
  reg5[c((i*30-29):(i*30)),1]<-region5_shiftst[[i]][,1]
  reg5[c((i*30-29+(29*30)):(i*30+(29*30))),1]<-region5_shiftst[[i]][,2]
  reg5[c((i*30-29+(29*30*2)):(i*30+(29*30*2))),1]<-region5_shiftst[[i]][,3]
  reg5[c((i*30-29+(29*30*3)):(i*30+(29*30*3))),1]<-region5_shiftst[[i]][,4]
}
re5_nam<-matrix(NA,(29*30*4),1)
for(i in seq(1:29)){
  re5_nam[c((i*30-29):(i*30)),1]<-rep(noquote(names(region5_shiftst))[i],30)
  re5_nam[c((i*30-29+(29*30)):(i*30+(29*30))),1]<-rep(noquote(names(region5_shiftst))[i],30)
  re5_nam[c((i*30-29+(29*30*2)):(i*30+(29*30*2))),1]<-rep(noquote(names(region5_shiftst))[i],30)
  re5_nam[c((i*30-29+(29*30*3)):(i*30+(29*30*3))),1]<-rep(noquote(names(region5_shiftst))[i],30)
}
re5_lon<-matrix(NA,(29*30*4),1)
re5_lat<-matrix(NA,(29*30*4),1)
for(i in seq(1:29)){
  re5_lon[c((i*30-29):(i*30)),1]<-rep(reg5_cor_y60[i,2],30)
  re5_lon[c((i*30-29+(29*30)):(i*30+(29*30))),1]<-rep(reg5_cor_y60[i,2],30)
  re5_lon[c((i*30-29+(29*30*2)):(i*30+(29*30*2))),1]<-rep(reg5_cor_y60[i,2],30)
  re5_lon[c((i*30-29+(29*30*3)):(i*30+(29*30*3))),1]<-rep(reg5_cor_y60[i,2],30)
  re5_lat[c((i*30-29):(i*30)),1]<-rep(reg5_cor_y60[i,3],30)
  re5_lat[c((i*30-29+(29*30)):(i*30+(29*30))),1]<-rep(reg5_cor_y60[i,3],30)
  re5_lat[c((i*30-29+(29*30*2)):(i*30+(29*30*2))),1]<-rep(reg5_cor_y60[i,3],30)
  re5_lat[c((i*30-29+(29*30*3)):(i*30+(29*30*3))),1]<-rep(reg5_cor_y60[i,3],30)
}

#re5_nam<-rep(rep(dput(noquote(names(region5_shiftst))),30),4)
reg5_shiftst_df<-data.frame(rep(rep(c(1948:1977),29),4),c(rep("wei",(29*30)),rep("gum",(29*30)),
  rep("pe3",(29*30)),rep("ln3",(29*30))),reg5[,1],re5_nam,rep("Region 5",3480),
  as.numeric(re5_lon),as.numeric(re5_lat))
names(reg5_shiftst_df)[1]<-"years"
names(reg5_shiftst_df)[2]<-"func"
names(reg5_shiftst_df)[3]<-"value"
names(reg5_shiftst_df)[4]<-"GRDCNR"
names(reg5_shiftst_df)[5]<-"region"
names(reg5_shiftst_df)[6]<-"lon"
names(reg5_shiftst_df)[7]<-"lat"

### region 6
region6_shiftst<-list()
for(i in seq(1:26)){
  for (j in seq(1:227)){
    if((colnames(region6))[i]==names(shiftst[j]))
      region6_shiftst[i]<-shiftst[j]
  }}
for(i in seq(1:26)){
  for (j in seq(1:227)){
    if((colnames(region6))[i]==names(shiftst[j]))
      names(region6_shiftst)[i]<-names(shiftst[j])
  }}


reg6<-matrix(NA,(26*30*4),1)
for(i in seq(1:26)){
  reg6[c((i*30-29):(i*30)),1]<-region6_shiftst[[i]][,1]
  reg6[c((i*30-29+(26*30)):(i*30+(26*30))),1]<-region6_shiftst[[i]][,2]
  reg6[c((i*30-29+(26*30*2)):(i*30+(26*30*2))),1]<-region6_shiftst[[i]][,3]
  reg6[c((i*30-29+(26*30*3)):(i*30+(26*30*3))),1]<-region6_shiftst[[i]][,4]
}

re6_nam<-matrix(NA,(26*30*4),1)
for(i in seq(1:26)){
  re6_nam[c((i*30-29):(i*30)),1]<-rep(noquote(names(region6_shiftst))[i],30)
  re6_nam[c((i*30-29+(26*30)):(i*30+(26*30))),1]<-rep(noquote(names(region6_shiftst))[i],30)
  re6_nam[c((i*30-29+(26*30*2)):(i*30+(26*30*2))),1]<-rep(noquote(names(region6_shiftst))[i],30)
  re6_nam[c((i*30-29+(26*30*3)):(i*30+(26*30*3))),1]<-rep(noquote(names(region6_shiftst))[i],30)
}

re6_lon<-matrix(NA,(26*30*4),1)
re6_lat<-matrix(NA,(26*30*4),1)
for(i in seq(1:26)){
  re6_lon[c((i*30-29):(i*30)),1]<-rep(reg6_cor_y60[i,2],30)
  re6_lon[c((i*30-29+(26*30)):(i*30+(26*30))),1]<-rep(reg6_cor_y60[i,2],30)
  re6_lon[c((i*30-29+(26*30*2)):(i*30+(26*30*2))),1]<-rep(reg6_cor_y60[i,2],30)
  re6_lon[c((i*30-29+(26*30*3)):(i*30+(26*30*3))),1]<-rep(reg6_cor_y60[i,2],30)
  re6_lat[c((i*30-29):(i*30)),1]<-rep(reg6_cor_y60[i,3],30)
  re6_lat[c((i*30-29+(26*30)):(i*30+(26*30))),1]<-rep(reg6_cor_y60[i,3],30)
  re6_lat[c((i*30-29+(26*30*2)):(i*30+(26*30*2))),1]<-rep(reg6_cor_y60[i,3],30)
  re6_lat[c((i*30-29+(26*30*3)):(i*30+(26*30*3))),1]<-rep(reg6_cor_y60[i,3],30)
}

#re6_nam<-rep(rep(dput(noquote(names(region6_shiftst))),30),4)
reg6_shiftst_df<-data.frame(rep(rep(c(1948:1977),26),4),c(rep("wei",(26*30)),rep("gum",(26*30)),
 rep("pe3",(26*30)),rep("ln3",(26*30))),reg6[,1],re6_nam,rep("region 6",3120),
 as.numeric(re6_lon),as.numeric(re6_lat))
names(reg6_shiftst_df)[1]<-"years"
names(reg6_shiftst_df)[2]<-"func"
names(reg6_shiftst_df)[3]<-"value"
names(reg6_shiftst_df)[4]<-"GRDCNR"
names(reg6_shiftst_df)[5]<-"region"
names(reg6_shiftst_df)[6]<-"lon"
names(reg6_shiftst_df)[7]<-"lat"

##### Combine all regions
reg_df<-rbind(reg1_shiftst_df,reg2_shiftst_df,reg3_shiftst_df,reg4_shiftst_df,reg5_shiftst_df,reg6_shiftst_df)

#### Plot the reg_df
myAwesomePlot <-xyplot(value ~ years|region,reg_df,
                        scales = list(x = list(log = 10, equispaced.log = FALSE)),
                        type = c("p", "smooth"), grid = TRUE, col.line = "darkorange", lwd = 4,
                        ylab="Standardized Discharge",col.regions="red",
                       trellis.par.set = list(col = c("orange", "blue")),
                       trellis.par.set=list(strip.background=list(col=c('green','red','blue'))),
                       strip = function(..., bg) {
                         strip.default(...,
                                       bg = trellis.par.get("strip.background")$col[which.packet()])
                       })
print(myAwesomePlot)
dev.off()
myOtherPlot <- (stripplot(value ~ region,
                         subset(reg_df, subset = years %in% c(1948,1963,1977)),
                         groups = years, auto.key = list(reverse.rows = TRUE),
                         jitter.data = TRUE, type = c("p"), fun = median,
                         ylab.right=TRUE,ylab="Standardized Discharge"))
print(myOtherPlot)
myotherplot<-bwplot(value ~ region|years,data=reg_df, auto.key = list(reverse.rows = TRUE),
                    jitter.data = TRUE, type = c("p"), fun = median,
                    ylab.right=TRUE,ylab="Standardized Discharge")
print(myotherplot)
my.theme <- trellis.par.get()
scatter.lattice <- xyplot(value ~ years | region, 
                          data = reg_df, 
                          type = c("", "smooth"), grid = TRUE, col.line = "darkorange", lwd = 4,
                          panel = function(x, y, ...) {
                            panel.xyplot(x, y, ...)
                            lm1 <- lm(y ~ x)
                            lm1sum <- summary(lm1)
                            r2 <- lm1sum$adj.r.squared
                            panel.abline(a = lm1$coefficients[1], 
                                         b = lm1$coefficients[2])
                            panel.text(labels = bquote(italic(R)^2 == .(format(r2,digits = 3))),x = 1970,
                                       y = -4)},
                          xscale.components = xscale.components.subticks,
                          yscale.components = yscale.components.subticks,
                          as.table = TRUE,
                          trellis.par.set = list(col = c("orange", "blue")),
                          trellis.par.set=list(strip.background=list(col=c('green','red','blue'))),
                          strip = function(..., bg) {strip.default(...,
                                        bg = trellis.par.get("strip.background")$col[which.packet()])})


l.sc <- update(scatter.lattice, par.settings = my.theme)
l.sc.smooth <- update(scatter.lattice, aspect = 1, 
                      par.settings = my.theme, 
                      between = list(x = 0.3, y = 0.3),
                      panel = panel.smoothScatter,
                      trellis.par.set = list(col = c("orange", "blue")),
                      trellis.par.set=list(strip.background=list(col=c('green','red','blue'))),
                      strip = function(..., bg) {
                        strip.default(...,
                                      bg = trellis.par.get("strip.background")$col[which.packet()])
                      })

print(l.sc.smooth)
print(l.sc)
print(scatter.lattice)
plot.new()

opl<-otherplot+as.layer(xyplot(value ~ years|region,reg_df,
                          scales = list(x = list(log = 10, equispaced.log = FALSE)),
                          type = c("p", "smooth"), grid = TRUE, col.line = "darkorange", lwd = 4,
                          ylab="Standardized Discharge"))
### -> Good One
ahja<-l.sc.smooth+as.layer(scatter.lattice)

print(ahja, pos = c(0, 0, 0.52, 1), more = TRUE)
print(ahja2, pos = c(0.48, 0, 1, 1))


######################### mapping of y60 (get coordinates in upper section)
###########Get map, high resolution necessary. xlim, ylim according to long and latit
newmap <- getMap(resolution = "high")
plot(newmap,xlim = c(-20,35),ylim = c(35,73),asp = 1)
points(reg1_cor_y60[,3],reg1_cor_y60[,2],col="#F5DEB3",pch=16)
points(reg2_cor_y60[,3],reg2_cor_y60[,2],col="#66CDAA",pch=16)
points(reg3_cor_y60[,3],reg3_cor_y60[,2],col="#00FFFF",pch=16)
points(reg4_cor_y60[,3],reg4_cor_y60[,2],col="#87CEFA",pch=16)
points(reg5_cor_y60[,3],reg5_cor_y60[,2],col="#DDA0DD",pch=16)
points(reg6_cor_y60[,3],reg6_cor_y60[,2],col="#F08080",pch=16)

a<-str(trellis.par.get("superpose.symbol"))

#plot(newmap,xlim = c(-20,35),ylim = c(35,73),asp = 1,more = TRUE)

print(ahja, pos = c(0.0, 0.00, 0.45, 0.45), more = TRUE)
print(otplot, pos = c(0.48, 0, 1, 1))




########################################## TRY and ERROR!!!!!!!!!!!!! ONLY MAYBE USEFUL SOMEDAY. NOT USED!
my.theme <- trellis.par.get()
scatter.lattice<-xyplot(value ~ years|region,reg_df,
       scales = list(x = list(log = 10, equispaced.log = FALSE)),
       type = c("p", "smooth"), grid = TRUE, col.line = "darkorange", lwd = 4,
       ylab="Standardized Discharge",
       par.settings = list(col = c("orange", "blue")),
          panel = function(x, y, ...) {
                  panel.xyplot(x, y, ...)
                  lm1 <- lm(y ~ x)
                  lm1sum <- summary(lm1)
                  r2 <- lm1sum$adj.r.squared
                    panel.text(labels = 
                            bquote(italic(R)^2 == 
                              .(format(r2, 
                                 digits = 3))),
                            x = 1970, y = -4)
                  panel.smoother(x, y, method = "lm", 
                 col = "black", 
                 col.se = "black",
                 alpha.se = 0.3)},
      xscale.components = xscale.components.subticks,
      yscale.components = yscale.components.subticks,
      as.table = TRUE)

l.sc <- update(scatter.lattice, par.settings = my.theme)
l.sc.smooth <- update(scatter.lattice, aspect = 1, 
                      par.settings = my.theme, 
                      between = list(x = 0.3, y = 0.3),
                      panel = panel.smoothScatter)

print(l.sc.smooth)
print(l.sc)




otherplot<-hexbinplot(value ~ years|region, reg_df,
           scales = list(x = list(log = 10, equispaced.log = FALSE)),
           aspect = 1, bins=50,ylab="Standardized Discharge")

c(wireframe(reg_df), contourplot(reg_df))
wireframe(c(reg_df[[3]],reg_df[[1]],reg_df[[4]]))


new_map<-get_map(location = c(c(-20,35),c(35,73)),col="bw",maptype="roadmap",zoom=14)
p<-ggmap(new_map)
#p<-newmap+geom_point(data=reg_df,aes(x = lon, y = lat, size = value, colour = func))
p <- p + scale_x_continuous(expand = c(0.005, 0))
p <- p + scale_y_continuous(expand = c(0.005, 0)) # before creating the overlay

overlay <- stat_density2d(data = reg_df
                          , aes(x = lon, y = lat, fill = ..level.. , alpha = ..level..)
                          , size = 1, bins = 10, geom = "polygon")
p <- p + overlay

p <- p + scale_fill_gradient("Density")
p <- p + scale_alpha(range = c(0.9, 0), guide = FALSE)
p <- p + guides(fill = guide_colorbar(barwidth = 1.5, barheight = 16))
p <- p + geom_point(data = reg_df
                    , aes(x = lon, y = lat, colour = region)
                    , alpha = 0.5, size = 2
                    , position = "jitter")
print(p)
p1<- p + facet_wrap( ~ years)
print(p1)



reg_df[3]
colnames(shiftst[[15]])
st<-shiftst[[15]]
len<-c(1:30)
a<-xyplot(st[,1]~len,type="l",par.settings = list(axis.line = list(col = "transparent")),
          xlab="Years",ylab="Standardized Values",main="BRATISLAVA, Danube River",xlab.top="GRDC Nr. 6142200")
a2<-xyplot(st[,2]~len,type="l",par.settings = list(axis.line = list(col = "transparent")),
           xlab="Years",ylab="Standardized Values",main="BRATISLAVA, Danube River",xlab.top="GRDC Nr. 6142200")
a3<-xyplot(st[,3]~len,type="l",par.settings = list(axis.line = list(col = "transparent")),
           xlab="Years",ylab="Standardized Values",main="BRATISLAVA, Danube River",xlab.top="GRDC Nr. 6142200")
a4<-xyplot(st[,4]~len,type="l",par.settings = list(axis.line = list(col = "transparent")),
           xlab="Years",ylab="Standardized Values",main="BRATISLAVA, Danube River",xlab.top="GRDC Nr. 6142200")
a5<-corrplot(st,methode="circle",cl.ratio = 0.1, cl.align = "r",tl.cex=0.7,is.corr = FALSE)
update(c(a+as.layer(a2)+as.layer(a3)+as.layer(a4)+a5))

r1_shst_plot<-list()
for (i in seq(1:92)){
  r1_shst_plot[[i]]<-xyplot(region1_shiftst[[i]]~c(1:30),type=c("p","smooth"),
                            par.settings = list(axis.line = list(col = "transparent")),
                            xlab="Years",ylab="Standardized Values",main=names(region1_shiftst[i]),
                            panel=function(x,y,...){
                              panel.xyplot(x,y,...)
                              panel.abline(h=median(region1_shiftst[[i]]),lty=2,col=2)
                            })
}
region1_shiftst[1]
nn<-paste("r1_shst_plot","[[",c(1:92),"]],",sep="")
noquote(nn)
grid.arrange(r1_shst_plot[[1]],  r1_shst_plot[[2]],  r1_shst_plot[[3]],  r1_shst_plot[[4]],
             r1_shst_plot[[5]],  r1_shst_plot[[6]],  r1_shst_plot[[7]],  r1_shst_plot[[8]],
             ncol=2,nrow=4)
grid.arrange(r1_shst_plot[[9]],  r1_shst_plot[[10]], r1_shst_plot[[11]], r1_shst_plot[[12]],
             r1_shst_plot[[13]], r1_shst_plot[[14]], r1_shst_plot[[15]], r1_shst_plot[[16]],
             ncol=2,nrow=4)
grid.arrange(r1_shst_plot[[17]], r1_shst_plot[[18]], r1_shst_plot[[19]], r1_shst_plot[[20]],
             r1_shst_plot[[21]], r1_shst_plot[[22]], r1_shst_plot[[23]], r1_shst_plot[[24]],
             ncol=2,nrow=4)
grid.arrange(r1_shst_plot[[25]], r1_shst_plot[[26]], r1_shst_plot[[27]], r1_shst_plot[[28]],
             r1_shst_plot[[29]], r1_shst_plot[[30]], r1_shst_plot[[31]], r1_shst_plot[[32]],
             ncol=2,nrow=4)

region1_shiftst<-list()
for(i in seq(1:92)){
  for (j in seq(1:227)){
    if((colnames(region1))[i]==names(shiftst[j]))
      region1_shiftst[i]<-shiftst[j]
    names(region1_shiftst)[i]<-(names(shiftst[j]))
  }}


r1_shst_plot<-list()
for (i in seq(1:92)){
  r1_shst_plot[[i]]<-bwplot(region1_shiftst[[i]]~c(1:30),type=c("p","smooth"),panel=function(x,y,...){
    panel.bwplot(x,y,...)
    panel.abline(h=median(region1_shiftst[[i]]),lty=2,col=2)
  })
}

nn<-paste("r1_shst_plot","[[",c(1:92),"]],",sep="")
noquote(nn)
grid.arrange(r1_shst_plot[[1]],  r1_shst_plot[[2]],  r1_shst_plot[[3]],  r1_shst_plot[[4]],
             r1_shst_plot[[5]],  r1_shst_plot[[6]],  r1_shst_plot[[7]],  r1_shst_plot[[8]],
             ncol=2,nrow=4)
grid.arrange(r1_shst_plot[[9]],  r1_shst_plot[[10]], r1_shst_plot[[11]], r1_shst_plot[[12]],
             r1_shst_plot[[13]], r1_shst_plot[[14]], r1_shst_plot[[15]], r1_shst_plot[[16]],
             ncol=2,nrow=4)
grid.arrange(r1_shst_plot[[17]], r1_shst_plot[[18]], r1_shst_plot[[19]], r1_shst_plot[[20]],
             r1_shst_plot[[21]], r1_shst_plot[[22]], r1_shst_plot[[23]], r1_shst_plot[[24]],
             ncol=2,nrow=4)
grid.arrange(r1_shst_plot[[25]], r1_shst_plot[[26]], r1_shst_plot[[27]], r1_shst_plot[[28]],
             r1_shst_plot[[29]], r1_shst_plot[[30]], r1_shst_plot[[31]], r1_shst_plot[[32]],
             ncol=2,nrow=4)

region1_shiftst<-list()
for(i in seq(1:92)){
  for (j in seq(1:227)){
    if((colnames(region1))[i]==names(shiftst[j]))
      region1_shiftst[i]<-shiftst[j]
  }}
for(i in seq(1:92)){
  for (j in seq(1:227)){
    if((colnames(region1))[i]==names(shiftst[j]))
      names(region1_shiftst)[i]<-names(shiftst[j])
  }}
?map
state.map <- map("world", plot = FALSE, fill = FALSE)
panel.3dmap <- function(..., rot.mat, distance, xlim,
                        ylim, zlim, xlim.scaled, ylim.scaled, zlim.scaled) {
  scaled.val <- function(x, original, scaled) {
    scaled[1] + (x - original[1]) * diff(scaled)/diff(original)
  }
  m <- ltransform3dto3d(rbind(scaled.val(new_map$x,
                                         xlim, xlim.scaled), scaled.val(new_map$y, ylim,
                                                                        ylim.scaled), zlim.scaled[1]), rot.mat, distance)
  panel.lines(m[1, ], m[2, ], col = "grey40")
}
?scaled.val
pl <- cloud(reg_df$value ~ reg_df$lon + reg_df$lat, reg_df,
            panel.3d.cloud = function(...) {
              panel.3dmap(...)
              panel.3dscatter(...)
                  },
            col = "blue2",  type = "h", scales = list(draw = FALSE), zoom = 1.1,
            xlab = NULL, ylab = NULL, zlab = NULL,
            panel.aspect = 0.75, lwd = 2, screen = list(z = 30,x = -60), par.settings = list(axis.line = list(col = "transparent"),
box.3d = list(col = "transparent", alpha = 0)))
print(pl)



