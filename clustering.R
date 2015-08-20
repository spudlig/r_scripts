
library("mclust")
library("vegan")
library("fpc")
library("pvclust")
library("apcluster")
################### how many cluster
### corrplot used previously 
?cor
corrplot(resw)
?corrplot
### elbow criterion (würde für 6 regionen gut passen, auch mit fünf)
wss <- (nrow(resw)-1)*sum(apply(resw,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(resw,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

### 
for (k in 2:227)
  resw[[k]] <- pam(resw, k) $ silinfo $ avg.width
k.best <- which.max(resw)
cat("silhouette-optimal number of clusters:", k.best, "\n")

### calinski criterion (schlägt 2 cluster vor - verständlich, da schons ehr früh getrennt wird)
fit <- cascadeKM(scale(resw, center = TRUE,  scale = TRUE), 1, 10, iter = 1000)
plot(fit, sortg = TRUE, grpmts.plot = TRUE)
calinski.best <- as.numeric(which.max(fit$results[2,]))
cat("Calinski criterion optimal number of clusters:", calinski.best, "\n")

### pamk (schlägt auch zwei cluster vor)
pamk.best <- pamk(resw)
cat("number of clusters estimated by optimum average silhouette width:", pamk.best$nc, "\n")
plot(pam(resw, pamk.best$nc))

### affinity propagation AP (schlägt 5 cluster vor)
d.apclus <- apcluster(negDistMat(r=5), resw)
cat("affinity propogation optimal number of clusters:", length(d.apclus@clusters), "\n")
# heatmap für AP
heatmap(d.apclus)
plot(d.apclus, resw)

########## BIC () 
d_clust <- Mclust(as.matrix(resw), G=1:20)
m.best <- dim(d_clust$z)[2]
cat("model-based optimal number of clusters:", m.best, "\n")
# 4 clusters
plot(d_clust)

### p-value for hirachrical clustering, bootstrap resampling
resw.pv <- pvclust(resw,method.hclust="complete",method.dist="cor",nboot = 10000)
plot(resw.pv)
seplot(resw.pv,identify=TRUE)
pvrect(resw.pv,alpha = 0.90)
resw.pv.par<-pvpick(resw.pv,alpha = 0.9)
resw.pv.par

# get coordinates for p-value regions - depending on the cluster correlation p-value
comp
for (i in seq(1:34)){
  for (j in seq(1:227))
    if(colnames(region2)[i]==comp[j]){
      reg2_cor_y60[i,]<-maa_y60[j,]
    }
}
# cluster 2
for (i in seq(1:34)){
  for (j in seq(1:227))
    if(colnames(region2)[i]==comp[j]){
      reg2_cor_y60[i,]<-maa_y60[j,]
    }
}
# cluster 3
for (i in seq(1:34)){
  for (j in seq(1:227))
    if(colnames(region2)[i]==comp[j]){
      reg2_cor_y60[i,]<-maa_y60[j,]
    }
}
# cluster 4
for (i in seq(1:34)){
  for (j in seq(1:227))
    if(colnames(region2)[i]==comp[j]){
      reg2_cor_y60[i,]<-maa_y60[j,]
    }
}
# cluster 5
for (i in seq(1:34)){
  for (j in seq(1:227))
    if(colnames(region2)[i]==comp[j]){
      reg2_cor_y60[i,]<-maa_y60[j,]
    }
}
# cluster 6
for (i in seq(1:34)){
  for (j in seq(1:227))
    if(colnames(region2)[i]==comp[j]){
      reg2_cor_y60[i,]<-maa_y60[j,]
    }
}
# cluster 7
for (i in seq(1:34)){
  for (j in seq(1:227))
    if(colnames(region2)[i]==comp[j]){
      reg2_cor_y60[i,]<-maa_y60[j,]
    }
}
### Gap Statistics
clusGap(resw, kmeans, 10, B = 100, verbose = interactive())

### 
nb <- NbClust(resw, diss="NULL", distance = "euclidean", 
              min.nc=2, max.nc=15, method = "kmeans", 
              index = "alllong", alphaBeale = 0.1)
hist(nb$Best.nc[1,], breaks = max(na.omit(nb$Best.nc[1,])))

############ difference
euc_shst_y100<-diss(shiftt,METHOD = "EUCL")
cid_shst_y100<-diss(shiftt,METHOD = "CID")
dtw_shst_y100<-diss(shiftt,METHOD = "DTWARP")
fre_shst_y100<-diss(shiftt,METHOD = "FRECHET")
hc_ward_eucl<-hclust(euc_shst_y100,method="ward.D2")
plot(hc_ward_eucl,hang=-0.01,cex=0.7)
cutree(hc_ward_eucl,6)

str(euc_shst_y100)
print(euc_shst_y100)
plot(dtw_shst_y100)
pvalues.clust(dtw_shst_y100,0.05)
loo1nn.cv(dtw_shst_y100,G = seq(1,25651,1))


### DTW
#weibull 
shiftt<-matrix(NA,227,30)
for (i in seq(1,227,1)){
  shiftt[i,]<-shiftst_30_227[[i]][,1]
}
rownames(shiftt)<-names(shiftst_30_227)
dtw_shst_y100<-diss(shiftt,METHOD = "DTWARP")
dtw_shst_y100_clust<-hclust(dtw_shst_y100)
dtw_shst_y100_cut6<-cutree(dtw_shst_y100_clust,k=6)
plot(hclust(dtw_shst_y100))
hc_a<-dtw_shst_y100_cut6[order(dtw_shst_y100_cut6)]
newmap <- getMap(resolution = "high")
plot(newmap,xlim = c(-20,35),ylim = c(35,73),asp = 1)
### cluster group 1
hc_a_1<-hc_a[c(1:95)]
hc_1<-matrix(NA,95,3)
for (i in seq(1:95)){
  for (j in seq(1:227))
    if(names(hc_a_1)[i]==comp[j]){
      hc_1[i,]<-maa_y60[j,]
    }
}
points(hc_1[,3],hc_1[,2],col="green",pch=16)
colnames(resw_r1)
#compare correlation region1 with dtw region hc_a_1
sim_crdtw_1<-matrix(NA,95,1)
for(i in seq(1:95)){
  for(j in seq(1:92)){
    if(names(hc_a_1)[i]==colnames(resw_r1)[j]){
      sim_crdtw_1[i,1]<-names(hc_a_1)[i]
    }
  }
}

### cluster group 2
hc_a_2<-hc_a[c(96:130)]
hc_2<-matrix(NA,34,3)
for (i in seq(1:34)){
  for (j in seq(1:227))
    if(names(hc_a_2)[i]==comp[j]){
      hc_2[i,]<-maa_y60[j,]
    }
}
points(hc_2[,3],hc_2[,2],col="red",pch=16)
#compare correlation region2 with dtw region hc_a_2
sim_crdtw_2<-matrix(NA,34,1)
for(i in seq(1:34)){
  for(j in seq(1:92)){
    if(names(hc_a_2)[i]==colnames(resw_r2)[j]){
      sim_crdtw_2[i,1]<-names(hc_a_2)[i]
    }
  }
}


### cluster group 3
hc_a_3<-hc_a[c(131:165)]
hc_3<-matrix(NA,34,3)
for (i in seq(1:34)){
  for (j in seq(1:227))
    if(names(hc_a_3)[i]==comp[j]){
      hc_3[i,]<-maa_y60[j,]
    }
}
points(hc_3[,3],hc_3[,2],col="orange",pch=16)

### cluster group 4
hc_a_4<-hc_a[c(166:188)]
hc_4<-matrix(NA,22,3)
for (i in seq(1:22)){
  for (j in seq(1:227))
    if(names(hc_a_4)[i]==comp[j]){
      hc_4[i,]<-maa_y60[j,]
    }
}
points(hc_4[,3],hc_4[,2],col="blue",pch=16)

### cluster group 5
hc_a_5<-hc_a[c(198:220)]
hc_5<-matrix(NA,22,3)
for (i in seq(1:22)){
  for (j in seq(1:227))
    if(names(hc_a_5)[i]==comp[j]){
      hc_5[i,]<-maa_y60[j,]
    }
}
points(hc_5[,3],hc_5[,2],col="violet",pch=16)

### cluster group 6
hc_a_6<-hc_a[c(221:227)]
hc_6<-matrix(NA,6,3)
for (i in seq(1:6)){
  for (j in seq(1:227))
    if(names(hc_a_6)[i]==comp[j]){
      hc_6[i,]<-maa_y60[j,]
    }
}
points(hc_6[,3],hc_6[,2],col="yellow",pch=16)

