install.packages("mapdata")
install.packages("ggmap")
install.packages("rworldmap")
install.packages("rworldxtra")
install.packages("RColorBrewer")
install.packages("graphics")
install.packages("gplots")
install.packages("grDevices")
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


### Col brewer for the legend
coll<-brewer.pal(11,"RdYlGn")
coll<-heat.co

co<-colorRampPalette(c("red","green"))
color.bar <- function(lut, min, max=-min, nticks=11, ticks=seq(min, max, len=nticks), title='') {
  scale = (length(lut)-1)/(max-min)
  
  dev.new(width=1.75, height=5)
  plot(c(2,70), c(min,max), type='n', bty='n', xaxt='n', xlab='', yaxt='n', ylab='', main=title)
  axis(2, ticks, las=1)
  for (i in 1:(length(lut)-1)) {
    y = (i-1)/scale + min
    rect(0,y,10,y+1/scale, col=lut[i], border=NA)
  }
}
color.bar(colorRampPalette(c("darkgreen", "yellow", "orange", "darkred"))(30), -2)
co<-colorRampPalette(c("darkgreen", "darkyellow", "darkorange", "darkred"))
plot(c(1:300),col=co(300),pch=19)

for (i in seq_along(1:6)){
  color2D.matplot(shiftst[[i]],main=paste(names(shiftst)[i],sep=""),ylab="years",xlab="Functions",
                  extremes=co(30)[c(1:30)],show.legend=TRUE) 
}
par(mfrow=c(1,5))
for(i in seq_along(c(1:5))){
  color2D.matplot(shiftst[[i]],main=paste(names(shiftst)[i],sep=""),ylab="years",xlab="Functions",
                  extremes=co(30)[c(1:30)],show.legend=TRUE) 
  
}


image.plot(legend.only=TRUE,col=co(30),zlim=range(2,-2))  
axis(1,at=1)
labels=seq(201,300,length.out=10)) 
color.legend(104,30,112,70,seq(-110,-30,length=11), 
             align="rb",rect.col=color.scale(1:30,1,c(0,1),0),gradient="y") 

image.plot(legend.only=TRUE,col=co(30),zlim=range(1,10))
dev.off()

### Get map, high resolution necessary. xlim, ylim according to long and latit
newmap <- getMap(resolution = "high")
ggplot(newmap,,xlim = c(2,30),ylim = c(41,66),asp = 1)
par(mfrow=c(2,2))

### plot map with points, color needs work - with shift
### Get the coordinates of the point at the results.R

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
}

names(shiftst)[1]
dev.off()

############################################################################################
############################ PLOT Serveral Maps - maybe useful #############################
############################################################################################
############################ Also if else functions at the end #############################
############################################################################################

pcontorta <- readShapeLines("../mapdata/NUTS_2013_SHP/data/NUTS_RG_01M_2013.shp")
plot(pcontorta, add=TRUE, xlim = c(-20, 59),ylim = c(35, 71), col=alpha("darkgreen", 0.6), border=FALSE)

plot(newmap,xlim = c(2,30),ylim = c(41,66),asp = 1)
points(samps[,2], samps[,1], pch=19, col="red", cex=1)
riverways <- readShapeLines("../mapdata/europe-waterways-shape/waterways.shp")
plot(riverways, add=TRUE, xlim = c(2,30),ylim = c(41,66), col=alpha("darkblue", 0.6), border=FALSE)

coord <- mapproject(as.numeric(samps[,2]), as.numeric(samps[,1]), proj="gilbert")
points(coord,pch=20,cex=0.8,col="red")
text(x=0.1,y=0.7,labels="Sampling Sites")
map.scale(y = 0.9)


map("worldHires","Germany", xlim=c(9), ylim=c(53), col="gray90", fill=TRUE)
newmap <- getMap(resolution = "high")
europe.limits <- geocode(c("CapeFligely,RudolfIsland,Franz Josef Land,Russia",
                           "Gavdos,Greece",
                           "Faja Grande,Azores",
                           "SevernyIsland,Novaya Zemlya,Russia")
plot(newmap,
  xlim = c(2,30),
  ylim = c(41,66),
  asp = 1
)

plot(newmap,
     xlim = range(europe.limits$lon),
     ylim = range(europe.limits$lat),
     asp = 1
)

par(mfrow=c(5,2))
for(j in seq_along(c(1:10))){
  plot(plot(newmap,xlim = c(2,30),ylim = c(41,66),asp = 1))  
  for( i in seq_along(shiftst)){
    if(shiftst[[i]][1,1]>2){
      points(samps[i,2], samps[i,1], pch=2, col=coll[1], cex=1,lwd=4)
    }
    } else {if(2>shiftst[[i]][1,1] && shiftst[[i]][1,1]>1.5){
      points(samps[i,2], samps[i,1], pch=19, col=coll[2], cex=1,lwd=4)
    }else {if(1.5>shiftst[[i]][1,1] && shiftst[[i]][1,1]>1.4){
      points(samps[i,2], samps[i,1], pch=19, col=coll[3], cex=1,lwd=4)
    }else {if(1.4>shiftst[[i]][1,1] && shiftst[[i]][1,1]>1.3){
      points(samps[i,2], samps[i,1], pch=19, col=coll[4], cex=1,lwd=4)
    }else {if(1.3>shiftst[[i]][1,1] && shiftst[[i]][1,1]>1.2){
      points(samps[i,2], samps[i,1], pch=19, col=coll[5], cex=1,lwd=4)
    }else {if(1.2>shiftst[[i]][1,1] && shiftst[[i]][1,1]>1.1){
      points(samps[i,2], samps[i,1], pch=19, col=coll[6], cex=1,lwd=4)
    }else {if(1.1>shiftst[[i]][1,1] && shiftst[[i]][1,1]>1){
      points(samps[i,2], samps[i,1], pch=19, col=coll[7], cex=1,lwd=4)
    }else {if(1>shiftst[[i]][1,1] && shiftst[[i]][1,1]>0.9){
      points(samps[i,2], samps[i,1], pch=19, col=coll[8], cex=1,lwd=4)
    }else {if(0.9>shiftst[[i]][1,1] && shiftst[[i]][1,1]>0.8){
      points(samps[i,2], samps[i,1], pch=19, col=coll[9], cex=1,lwd=4)
    }else {if(0.8>shiftst[[i]][1,1] && shiftst[[i]][1,1]>0.7){
      points(samps[i,2], samps[i,1], pch=19, col=coll[10], cex=1,lwd=4)
    }else {if(0.7>shiftst[[i]][1,1] && shiftst[[i]][1,1]>0.6){
      points(samps[i,2], samps[i,1], pch=19, col=coll[11], cex=1,lwd=4)
    }else {if(0.6>shiftst[[i]][1,1] ){
      points(samps[i,2], samps[i,1], pch=19, col=“darkgreen”, cex=1,lwd=4)
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
  }
}

for(i in seq_along(shift)){
  if(shiftst[[i]][1,1]>2){
    points(samps[i,2], samps[i,1], pch=2, col=coll[1], cex=1,lwd=4) }}

if(2>shiftst[[i]][1,1] && shiftst[[i]][1,1]>1.9){
  points(samps[i,2], samps[i,1], pch=19, col=coll[2], cex=1,lwd=4)
  if(1.9>shiftst[[i]][1,1] && shiftst[[i]][1,1]>1.8){
    points(samps[i,2], samps[i,1], pch=19, col=coll[3], cex=1,lwd=4)
  }}}

else {if(1.8>shiftst[[i]][1,1] && shiftst[[i]][1,1]>1.7){
  points(samps[i,2], samps[i,1], pch=19, col=coll[4], cex=1,lwd=4)
}else {if(1.7>shiftst[[i]][1,1] && shiftst[[i]][1,1]>1.6){
  points(samps[i,2], samps[i,1], pch=19, col=coll[5], cex=1,lwd=4)
}else {if(1.6>shiftst[[i]][1,1] && shiftst[[i]][1,1]>1.5){
  points(samps[i,2], samps[i,1], pch=19, col=coll[6], cex=1,lwd=4)
}else {if(1.4>shiftst[[i]][1,1] && shiftst[[i]][1,1]>1.4){
  points(samps[i,2], samps[i,1], pch=19, col=coll[4], cex=1,lwd=4)
}
}
}
}
}
for(i in seq_along(shift)){
  if(shiftst[[i]][1,1]>2){
    points(samps[i,2], samps[i,1], pch=2, col=coll[1], cex=1,lwd=4) }}
for(i in seq_along(shift)){
  if(2>shiftst[[i]][1,1] && shiftst[[i]][1,1]>1.5){
    points(samps[i,2], samps[i,1], pch=19, col=coll[2], cex=1,lwd=4) }}
for(i in seq_along(shift)){
  if(1.4>shiftst[[i]][1,1] && shiftst[[i]][1,1]>1.3){
    points(samps[i,2], samps[i,1], pch=19, col=coll[3], cex=1,lwd=4) }}
for(i in seq_along(shift)){
  if(1.3>shiftst[[i]][1,1] && shiftst[[i]][1,1]>1.2){
    points(samps[i,2], samps[i,1], pch=19, col=coll[4], cex=1,lwd=4) }}
for(i in seq_along(shift)){
  if(1.2>shiftst[[i]][1,1] && shiftst[[i]][1,1]>1.1){
    points(samps[i,2], samps[i,1], pch=19, col=coll[5], cex=1,lwd=4) }}
for(i in seq_along(shift)){
  if(1.1>shiftst[[i]][1,1] && shiftst[[i]][1,1]>1){
    points(samps[i,2], samps[i,1], pch=19, col=coll[6], cex=1,lwd=4) }}
for(i in seq_along(shift)){
  if(1>shiftst[[i]][1,1] && shiftst[[i]][1,1]>0.9){
    points(samps[i,2], samps[i,1], pch=19, col=coll[7], cex=1,lwd=4) }}
for(i in seq_along(shift)){
  if(0.9>shiftst[[i]][1,1] && shiftst[[i]][1,1]>0.8){
    points(samps[i,2], samps[i,1], pch=19, col=coll[8], cex=1,lwd=4) }}
for(i in seq_along(shift)){
  if(0.8>shiftst[[i]][1,1] && shiftst[[i]][1,1]>0.7){
    points(samps[i,2], samps[i,1], pch=19, col=coll[9], cex=1,lwd=4) }}
for(i in seq_along(shift)){
  if(0.7>shiftst[[i]][1,1] && shiftst[[i]][1,1]>0.6){
    points(samps[i,2], samps[i,1], pch=19, col=coll[10], cex=1,lwd=4) }}
for(i in seq_along(shift)){
  if(0.6>shiftst[[i]][1,1]){
    points(samps[i,2], samps[i,1], pch=3, col=coll[11], cex=1,lwd=4) }} 
