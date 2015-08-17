library(lattice)
library(ggplot2)
library(gtable)
library(latticeExtra)
library(hexbin)
library(gridExtra)
library(corrplot)

######################################################
################################# shift listko_y60
listko<-listko_y60
shiftst<-shiftst_30_227
shiftst[15]
################################### shift 30
############################### Correlation Matrix
################### Correlation 227 Rivers, shift 30 years
dev.off()
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

resw_plot<-corrplot(resw,order="hclust",addrect=6,cl.ratio = 0.1, cl.align = "r",tl.cex=0.7)

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
res1 <- cor.mtest(reswt, 0.95)
res2 <- cor.mtest(reswt, 0.99)
corrplot(reswt, low = res1[[2]], upp = res1[[3]], order = "hclust", rect.col = "navy", 
         plotC = "rect", cl.pos = "n")
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
aag<-plotCov(resg[c(55:80),c(55:80)])

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
aap<-plotCov(resp[c(55:80),c(55:80)])

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

corrplot(reswt, order = "hclust", addrect = 3)



############################### Regions. corrplot for y60

comp<-dput(maa_y100[,1])

## Region1 6233460:6855402; 1:92

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
shiftforcor<-list()
for (i in seq(1:92)){
  for (j in seq(1:227))
    if(colnames(region1)[i]==comp[j]){
      shiftforcor[i]<-shiftst_30_227[j]
      names(shiftforcor)[i]<-names(shiftst_30_227)[j]
    }
}
shkw_r1<-matrix(NA,30,92)
for (i in seq(1,92,1)){
  for (j in seq(1,30,1)){
    shkw_r1[j,i]<-shiftforcor[[i]][j,1]
  }
}
colnames(shkw_r1)<-dput(names(shiftforcor))
rownames(shkw_r1)<-seq(1948,1977,1)
shkwt_r1<-t(shkw_r1)
reswt_r1<-cor(shkwt_r1,shkwt_r1)
resw_r1<-cor(shkw_r1,shkw_r1)
corrplot(reswt_r1)

#region2 /// 93:126 6855403:6357502
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

#region3 127:154   6854200:6607651
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

#region4

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


################################## single 
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




########################################## TRY and ERROR!!!



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
  reg1[c((i*30-29+(92*30*3)):(i*30+(92*30*3))),1]<-region1_shiftst[[i]][,3]
}
names(region1_shiftst)
re1_nam<-rep(rep(dput(noquote(names(region1_shiftst))),30),4)
region1_shiftst[[92]][,1]==reg1[c((92*30-29):(30*92)),1]
92*30
reg1_shiftst_df<-data.frame(rep(rep(c(1948:1977),92),4),c(rep("wei",(92*30)),rep("gum",(92*30)),rep("pe3",(92*30)),rep("ln3",(92*30))),reg1[,1],re1_nam,rep("Region 1",11040))
names(reg1_shiftst_df)[1]<-"years"
names(reg1_shiftst_df)[2]<-"func"
names(reg1_shiftst_df)[3]<-"value"
names(reg1_shiftst_df)[4]<-"GRDCNR"
names(reg1_shiftst_df)[5]<-"region"

### region 2
region2_shiftst<-list()
for(i in seq(1:23)){
  for (j in seq(1:227)){
    if((colnames(region2))[i]==names(shiftst[j]))
      region2_shiftst[i]<-shiftst[j]
  }}
for(i in seq(1:23)){
  for (j in seq(1:227)){
    if((colnames(region2))[i]==names(shiftst[j]))
      names(region2_shiftst)[i]<-names(shiftst[j])
  }}
reg2<-matrix(NA,(23*30*4),1)
for(i in seq(1:23)){
  reg2[c((i*30-29):(i*30)),1]<-region2_shiftst[[i]][,1]
  reg2[c((i*30-29+(23*30)):(i*30+(23*30))),1]<-region2_shiftst[[i]][,2]
  reg2[c((i*30-29+(23*30*2)):(i*30+(23*30*2))),1]<-region2_shiftst[[i]][,3]
  reg2[c((i*30-29+(23*30*3)):(i*30+(23*30*3))),1]<-region2_shiftst[[i]][,3]
}

re2_nam<-rep(rep(dput(noquote(names(region2_shiftst))),30),4)
reg2_shiftst_df<-data.frame(rep(rep(c(1948:1977),23),4),c(rep("wei",(23*30)),rep("gum",(23*30)),rep("pe3",(23*30)),rep("ln3",(23*30))),reg2[,1],re2_nam,rep("Region 2",2760))
names(reg2_shiftst_df)[1]<-"years"
names(reg2_shiftst_df)[2]<-"func"
names(reg2_shiftst_df)[3]<-"value"
names(reg2_shiftst_df)[4]<-"GRDCNR"
names(reg2_shiftst_df)[5]<-"region"

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
  reg3[c((i*30-29+(28*30*3)):(i*30+(28*30*3))),1]<-region3_shiftst[[i]][,3]
}

re3_nam<-rep(rep(dput(noquote(names(region3_shiftst))),30),4)
reg3_shiftst_df<-data.frame(rep(rep(c(1948:1977),28),4),c(rep("wei",(28*30)),rep("gum",(28*30)),rep("pe3",(28*30)),rep("ln3",(28*30))),reg3[,1],re3_nam,rep("Region 3",3360))
names(reg3_shiftst_df)[1]<-"years"
names(reg3_shiftst_df)[2]<-"func"
names(reg3_shiftst_df)[3]<-"value"
names(reg3_shiftst_df)[4]<-"GRDCNR"
names(reg3_shiftst_df)[5]<-"region"

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
  reg5[c((i*30-29+(29*30*3)):(i*30+(29*30*3))),1]<-region5_shiftst[[i]][,3]
}

re5_nam<-rep(rep(dput(noquote(names(region5_shiftst))),30),4)
reg5_shiftst_df<-data.frame(rep(rep(c(1948:1977),29),4),c(rep("wei",(29*30)),rep("gum",(29*30)),rep("pe3",(29*30)),rep("ln3",(29*30))),reg5[,1],re5_nam,rep("Region 5",3480))
names(reg5_shiftst_df)[1]<-"years"
names(reg5_shiftst_df)[2]<-"func"
names(reg5_shiftst_df)[3]<-"value"
names(reg5_shiftst_df)[4]<-"GRDCNR"
names(reg5_shiftst_df)[5]<-"region"

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
  reg6[c((i*30-29+(26*30*3)):(i*30+(26*30*3))),1]<-region6_shiftst[[i]][,3]
}

re6_nam<-rep(rep(dput(noquote(names(region6_shiftst))),30),4)
reg6_shiftst_df<-data.frame(rep(rep(c(1948:1977),26),4),c(rep("wei",(26*30)),rep("gum",(26*30)),rep("pe3",(26*30)),rep("ln3",(26*30))),reg6[,1],re6_nam,rep("region 6",3120))
names(reg6_shiftst_df)[1]<-"years"
names(reg6_shiftst_df)[2]<-"func"
names(reg6_shiftst_df)[3]<-"value"
names(reg6_shiftst_df)[4]<-"GRDCNR"
names(reg6_shiftst_df)[5]<-"region"

##### Combine all regions (except of nr 4)

reg_df<-rbind(reg1_shiftst_df,reg2_shiftst_df,reg3_shiftst_df,reg5_shiftst_df,reg6_shiftst_df)


myAwesomePlot <- xyplot(value ~ years|region,reg_df,
         scales = list(x = list(log = 10, equispaced.log = FALSE)),
         type = c("p", "smooth"), grid = TRUE, col.line = "darkorange", lwd = 4)
print(myAwesomePlot)

myOtherPlot <- stripplot(value ~ reorder(region, value),
                         subset(reg_df, subset = years %in% c(1948,1958,1968,1977)),
                         groups = years, auto.key = list(reverse.rows = TRUE),
                         jitter.data = TRUE, type = c("p", "a"), fun = median)

print(myAwesomePlot, pos = c(0, 0, 0.52, 1), more = TRUE)
print(myOtherPlot, pos = c(0.48, 0, 1, 1))


