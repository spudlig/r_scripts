resw<-cor(shkw,shkw)

resw_plot<-corrplot(resw,order="hclust",addrect=6,cl.ratio = 0.1, cl.align = "r",tl.cex=0.7)

?corrplot
cor.mtest <- function(mat, conf.level = 0.95) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
