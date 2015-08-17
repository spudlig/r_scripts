install.packages("devtools")
library("devtools")
setwd("../")
create("packages")
boot_wei <- function(x,i) { 
  y0.wei<-quawei(f=0.99, para=pelwei(samlmu(x[i]),bound=0))
}
