install.packages("rworlxtra")
library(maps)
library(mapdata)
library(ggmap)
library(mapproj)
library(rworldmap)
library(rworlxtra)
library(maptools)
library(scales)
library(RColorBrewer)
library(graphics)
library(qplots)
library(grDevices)
require(plotrix)
library(fields)
library(eqs2lavaan)
library(corrplot)

# get coordinates for table 
listko<-listko_y60
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
#maa_y60<-maa
samps_y100<-maa_y100[,c(2:3)]
################################## CHoose the sample set
#listko<-listko_y60
#listko<-listko_y100
#shiftst<-shiftst_30
shiftst<-shiftst_30_227
#shift<-shift_30
#shift<-shift_40
#shift<-shift_50
#samps_y60<-maa[,c(2:3)]
#samps_y100<-maa[,c(2:3)]
#samps<-samps_y60
#samps<-samps_y60
################### Color brewer for heatmap - old heatmap for matrices
co<-colorRampPalette(c("darkgreen","green", "yellow", "orange","red", "violet"))
# heatmap (old)
for (i in seq_along(1:6)){
  color2D.matplot(shiftst[[i]],main=paste(names(shiftst)[i],sep=""),ylab="years",xlab="Functions",
                  extremes=co(30)[c(1:3)],show.legend=TRUE) 
}

for(i in seq_along(c(1:5))){
  color2D.matplot(shiftst[[i]],main=paste(names(shiftst)[i],sep=""),ylab="years",xlab="Functions",
                  extremes=co(30)[c(1:3)],show.legend=TRUE) 
  
}

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
?corrplot
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

comp<-dput(maa_y60[,1])

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

################### Correlation 45 Rivers, shift 50 years
dev.off()
shiftst<-shiftst_50
#Weibull
shkw<-matrix(NA,49,45)
for (i in seq(1,45,1)){
  for (j in seq(1,49,1)){
    shkw[j,i]<-shiftst[[i]][j,1]
    
  }
}
colnames(shkw)<-dput(names(shiftst))
rownames(shkw)<-seq(1908,1956,1)
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
shkg<-matrix(NA,49,45)
for (i in seq(1,45,1)){
  for (j in seq(1,49,1)){
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

##################### Regions. corrplot for y100

comp<-dput(maa_y60[,1])

## Region1 6337200:6731907; 1:92

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
for (i in seq(1:22)){
  for (j in seq(1:45))
    if(colnames(region1)[i]==comp[j]){
      shiftforcor[i]<-shiftst_50[j]
      names(shiftforcor)[i]<-names(shiftst_50)[j]
    }
}
shkw_r1<-matrix(NA,22,22)
for (i in seq(1,22,1)){
  for (j in seq(1,22,1)){
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
region2<-matrix(NA,23,23)
colnames(region2)<-dput(colnames(resw_plot)[23:45])
rownames(region2)<-dput(rownames(resw_plot)[23:45])
for (i in seq(1:22)){
  for(j in seq(1:22)){
    region2[i,j]<-resw_plot[(i+21),(j+21)]
    
  }
}
reg2_cor_y60<-matrix(NA,22,3)
for (i in seq(1:22)){
  for (j in seq(1:45))
    if(colnames(region2)[i]==comp[j]){
      reg2_cor_y60[i,]<-maa_y100[j,]
    }
}





################################## NO CORRELATION Heatmap
corrplot(shiftst_30[[1]],is.corr=FALSE,cl.ratio = 2,method = "ellipse", cl.lim = c(-3, 3),cl.align = "r")
corrplot.mixed(reswt_r1)

################################## MAPPING

###########Get map, high resolution necessary. xlim, ylim according to long and latit
newmap <- getMap(resolution = "high")
plot(newmap,xlim = c(-20,35),ylim = c(35,73),asp = 1)
points(reg1_cor_y60[,3],reg1_cor_y60[,2],col="green",pch=16)
points(reg2_cor_y60[,3],reg2_cor_y60[,2],col="red",pch=16)
points(reg3_cor_y60[,3],reg3_cor_y60[,2],col="red",pch=16)
points(reg5_cor_y60[,3],reg5_cor_y60[,2],col="red",pch=16)
points(reg6_cor_y60[,3],reg6_cor_y60[,2],col="green",pch=16)
reg2_cor_y60

### plot map with points, color needs work - with shift
### Get the coordinates for the points first

for(j in seq_along(c(10:13))){
  plot(newmap,xlim = c(2,30),ylim = c(41,66),asp = 1)
  title(main=paste("19",47+j,"-","19",77+j,sep=""))
  image.plot(legend.only=TRUE,col=co(30),zlim=range(2,-2))  
  for(i in seq_along(shift)){
    if(shiftst[[i]][j,1]>2){
      points(samps[i,2], samps[i,1], pch=2, col=co(300)[300], cex=1,lwd=4) }}
  for(i in seq_along(shift)){
    if(2>shiftst[[i]][j,1] && shiftst[[i]][j,1]>1.9){
      points(samps[i,2], samps[i,1], pch=19, col=co(300)[290], cex=1,lwd=4) }}
  for(i in seq_along(shift)){
    if(1.9>shiftst[[i]][j,1] && shiftst[[i]][j,1]>1.8){
      points(samps[i,2], samps[i,1], pch=19, col=co(300)[280], cex=1,lwd=4) }}
  for(i in seq_along(shift)){
    if(1.8>shiftst[[i]][j,1] && shiftst[[i]][j,1]>1.7){
      points(samps[i,2], samps[i,1], pch=19, col=co(300)[270], cex=1,lwd=4) }}
  for(i in seq_along(shift)){
    if(1.7>shiftst[[i]][j,1] && shiftst[[i]][j,1]>1.6){
      points(samps[i,2], samps[i,1], pch=19, col=co(300)[260], cex=1,lwd=4) }}
  for(i in seq_along(shift)){
    if(1.6>shiftst[[i]][j,1] && shiftst[[i]][j,1]>1.5){
      points(samps[i,2], samps[i,1], pch=19, col=co(300)[250], cex=1,lwd=4) }}
  for(i in seq_along(shift)){
    if(1.5>shiftst[[i]][j,1] && shiftst[[i]][j,1]>1.45){
      points(samps[i,2], samps[i,1], pch=19, col=co(300)[240], cex=1,lwd=4) }}
  for(i in seq_along(shift)){
    if(1.45>shiftst[[i]][j,1] && shiftst[[i]][j,1]>1.4){
      points(samps[i,2], samps[i,1], pch=19, col=co(300)[230], cex=1,lwd=4) }}
  for(i in seq_along(shift)){
    if(1.4>shiftst[[i]][j,1] && shiftst[[i]][j,1]>1.35){
      points(samps[i,2], samps[i,1], pch=19, col=co(300)[220], cex=1,lwd=4) }}
  for(i in seq_along(shift)){
    if(1.35>shiftst[[i]][j,1] && shiftst[[i]][j,1]>1.3){
      points(samps[i,2], samps[i,1], pch=19, col=co(300)[210], cex=1,lwd=4) }}
  for(i in seq_along(shift)){
    if(1.3>shiftst[[i]][j,1] && shiftst[[i]][j,1]>1.25){
      points(samps[i,2], samps[i,1], pch=19, col=co(300)[200], cex=1,lwd=4) }}
  for(i in seq_along(shift)){
    if(1.25>shiftst[[i]][j,1] && shiftst[[i]][j,1]>1.2){
      points(samps[i,2], samps[i,1], pch=19, col=co(300)[190], cex=1,lwd=4) }}
  for(i in seq_along(shift)){
    if(1.2>shiftst[[i]][j,1] && shiftst[[i]][j,1]>1.15){
      points(samps[i,2], samps[i,1], pch=19, col=co(300)[180], cex=1,lwd=4) }}
  for(i in seq_along(shift)){
    if(1.15>shiftst[[i]][j,1] && shiftst[[i]][j,1]>1.1){
      points(samps[i,2], samps[i,1], pch=19, col=co(300)[170], cex=1,lwd=4) }}
  for(i in seq_along(shift)){
    if(1.1>shiftst[[i]][j,1] && shiftst[[i]][j,1]>1){
      points(samps[i,2], samps[i,1], pch=19, col=co(300)[160], cex=1,lwd=4) }}
  for(i in seq_along(shift)){
    if(1>shiftst[[i]][j,1] && shiftst[[i]][j,1]>0.95){
      points(samps[i,2], samps[i,1], pch=19, col=co(300)[150], cex=1,lwd=4) }}
  for(i in seq_along(shift)){
    if(0.95>shiftst[[i]][j,1] && shiftst[[i]][j,1]>0.9){
      points(samps[i,2], samps[i,1], pch=19, col=co(300)[140], cex=1,lwd=4) }}
  for(i in seq_along(shift)){
    if(0.9>shiftst[[i]][j,1] && shiftst[[i]][j,1]>0.85){
      points(samps[i,2], samps[i,1], pch=19, col=co(300)[130], cex=1,lwd=4) }}
  for(i in seq_along(shift)){
    if(0.85>shiftst[[i]][j,1] && shiftst[[i]][j,1]>0.8){
      points(samps[i,2], samps[i,1], pch=19, col=co(300)[120], cex=1,lwd=4) }}
  for(i in seq_along(shift)){
    if(0.8>shiftst[[i]][j,1] && shiftst[[i]][j,1]>0.75){
      points(samps[i,2], samps[i,1], pch=19, col=co(300)[110], cex=1,lwd=4) }}
  for(i in seq_along(shift)){
    if(0.75>shiftst[[i]][j,1] && shiftst[[i]][j,1]>0.7){
      points(samps[i,2], samps[i,1], pch=19, col=co(300)[100], cex=1,lwd=4) }}
  for(i in seq_along(shift)){
    if(0.7>shiftst[[i]][j,1] && shiftst[[i]][j,1]>0.65){
      points(samps[i,2], samps[i,1], pch=19, col=co(300)[90], cex=1,lwd=4) }}
  for(i in seq_along(shift)){
    if(0.65>shiftst[[i]][j,1] && shiftst[[i]][j,1]>0.6){
      points(samps[i,2], samps[i,1], pch=19, col=co(300)[80], cex=1,lwd=4) }}
  for(i in seq_along(shift)){
    if(0.6>shiftst[[i]][j,1] && shiftst[[i]][j,1]>0.55){
      points(samps[i,2], samps[i,1], pch=19, col=co(300)[70], cex=1,lwd=4) }}
  for(i in seq_along(shift)){
    if(0.55>shiftst[[i]][j,1] && shiftst[[i]][j,1]>0.5){
      points(samps[i,2], samps[i,1], pch=19, col=co(300)[60], cex=1,lwd=4) }}
  for(i in seq_along(shift)){
    if(0.5>shiftst[[i]][j,1] && shiftst[[i]][j,1]>0.4){
      points(samps[i,2], samps[i,1], pch=19, col=co(300)[50], cex=1,lwd=4) }}
  for(i in seq_along(shift)){
    if(0.4>shiftst[[i]][j,1] && shiftst[[i]][j,1]>0.3){
      points(samps[i,2], samps[i,1], pch=19, col=co(300)[40], cex=1,lwd=4) }}
  for(i in seq_along(shift)){
    if(0.3>shiftst[[i]][j,1] && shiftst[[i]][j,1]>0.2){
      points(samps[i,2], samps[i,1], pch=19, col=co(300)[30], cex=1,lwd=4) }}
  for(i in seq_along(shift)){
    if(0.2>shiftst[[i]][j,1] && shiftst[[i]][j,1]>0.15){
      points(samps[i,2], samps[i,1], pch=19, col=co(300)[20], cex=1,lwd=4) }}
  for(i in seq_along(shift)){
    if(0.15>shiftst[[i]][j,1]){
      points(samps[i,2], samps[i,1], pch=3, col=co(300)[10], cex=1,lwd=4) }}
  
}


names(shiftst)[1]
dev.off()

