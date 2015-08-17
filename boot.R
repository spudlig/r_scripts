install.packages("lmom")
install.packages("boot")
install.packages("FAdist")
install.packages("PearsonDS")
install.packages("TSclust")
library(lmom)
library(boot)
library(FAdist)
library(PearsonDS)

alpha=0.05
boot_wei <- function(x,i) { 
  y0.wei<-quawei(f=0.99, para=pelwei(samlmu(x[i]),bound=0))
}
boot_gum <- function(x,i) { 
  y0.gum<-quagum(f=0.99, para=pelgum(samlmu(x[i])))
}
boot_pe3 <- function(x,i) { 
  y0.pe3<-quape3(f=0.99, para=pelpe3(samlmu(x[i])))
}
boot_ln3 <- function(x,i) { 
  y0.ln3<-qualn3(f=0.99, para=pelln3(samlmu(x[i]),bound=0))
}
########## CI with shift 30 years for listko_y60 maxOL_list_y60. R=1000
CI_shift_60_wei<-matrix(NA,30,3)
CI_shift_60_gum<-matrix(NA,30,3)
CI_shift_60_pe3<-matrix(NA,30,3)
CI_shift_60_ln3<-matrix(NA,30,3)
CI_shift_y60_wei<-list()
CI_shift_y60_gum<-list()
CI_shift_y60_pe3<-list()
CI_shift_y60_ln3<-list()
maxOL_list<-maxOL_list_y60
for(j in seq(1,227,1)){
  if(j %in% seq(1,227,1)){
    cat("river", j, "\n") 
  }
for (k in seq(1:30)){
    seqq<-seq(k,29+k,1)
    boot_shift60_wei<-suppressWarnings(boot(maxOL_list[[j]][seqq],boot_wei, R=5000))
    boot_shift60_gum<-suppressWarnings(boot(maxOL_list[[j]][seqq],boot_gum, R=5000))
    boot_shift60_pe3<-suppressWarnings(boot(maxOL_list[[j]][seqq],boot_pe3, R=5000))
    boot_shift60_ln3<-suppressWarnings(boot(maxOL_list[[j]][seqq],boot_ln3, R=5000))
    if(k %in% seq(1,30,1)){
      cat("loop", k, "\n") 
    }
    for (l in seq(1:3)){
      CI_shift_60_wei[k,l]<-suppressWarnings(append((boot.ci(boot_shift60_wei, conf=(1-alpha))$bca[4:5]),boot_shift60_wei$t0[[1]],after=2)[[l]])
      CI_shift_60_gum[k,l]<-suppressWarnings(append((boot.ci(boot_shift60_gum, conf=(1-alpha))$bca[4:5]),boot_shift60_gum$t0[[1]],after=2)[[l]])
      CI_shift_60_pe3[k,l]<-suppressWarnings(append((boot.ci(boot_shift60_pe3, conf=(1-alpha))$bca[4:5]),boot_shift60_pe3$t0[[1]],after=2)[[l]])
      CI_shift_60_ln3[k,l]<-suppressWarnings(append((boot.ci(boot_shift60_ln3, conf=(1-alpha))$bca[4:5]),boot_shift60_ln3$t0[[1]],after=2)[[l]])
  }
}
CI_shift_y60_wei[j]<-list(CI_shift_60_wei)
CI_shift_y60_gum[j]<-list(CI_shift_60_gum)
CI_shift_y60_pe3[j]<-list(CI_shift_60_pe3)
CI_shift_y60_ln3[j]<-list(CI_shift_60_ln3)
}

########### CI with fakeyears for 2000 years R=5000
bootwei_fa_2000<-matrix(NA,2000,4)
bootgum_fa_2000<-matrix(NA,2000,4)
bootpe3_fa_2000<-matrix(NA,2000,4)
bootln3_fa_2000<-matrix(NA,2000,4)
for (i in seq(1:2000)){
  seqq<-seq(1,29+i,1)  
  bootwei_0 <- suppressWarnings(boot(fakeyears[,1][seqq], boot_wei, R=5000))  
  bootgum_0 <- suppressWarnings(boot(fakeyears[,2][seqq], boot_gum, R=5000))  
  bootpe3_0 <- suppressWarnings(boot(fakeyears[,3][seqq], boot_pe3, R=5000))  
  bootln3_0 <- suppressWarnings(boot(fakeyears[,4][seqq], boot_ln3, R=5000))  
  if(i %in% seq(1,5,1)){
    cat("loop", i, "\n") 
  }  
  for (j in seq(1:3)){
    bootwei[i,j]<-suppressWarnings(append((boot.ci(bootwei_0, conf=(1-alpha))$bca[4:5]),bootwei_0$t0[[1]],after=2)[[j]])
    bootgum[i,j]<-suppressWarnings(append((boot.ci(bootgum_0, conf=(1-alpha))$bca[4:5]),bootgum_0$t0[[1]],after=2)[[j]])
    bootpe3[i,j]<-suppressWarnings(append((boot.ci(bootpe3_0, conf=(1-alpha))$bca[4:5]),bootpe3_0$t0[[1]],after=2)[[j]])
    bootln3[i,j]<-suppressWarnings(append((boot.ci(bootln3_0, conf=(1-alpha))$bca[4:5]),bootln3_0$t0[[1]],after=2)[[j]])
  }
}

########## CI with fakeyears for 30/40/50/80/100/150/200/300/500 years with R=5000
## matrices
bootwei30<-matrix(NA,2000,3)
bootgum30<-matrix(NA,2000,3)
bootpe330<-matrix(NA,2000,3)
bootln330<-matrix(NA,2000,3)
bootwei40<-matrix(NA,2000,3)
bootgum40<-matrix(NA,2000,3)
bootpe340<-matrix(NA,2000,3)
bootln340<-matrix(NA,2000,3)
bootwei50<-matrix(NA,2000,3)
bootgum50<-matrix(NA,2000,3)
bootpe350<-matrix(NA,2000,3)
bootln350<-matrix(NA,2000,3)
bootwei80<-matrix(NA,2000,3)
bootgum80<-matrix(NA,2000,3)
bootpe380<-matrix(NA,2000,3)
bootln380<-matrix(NA,2000,3)
bootwei100<-matrix(NA,2000,3)
bootgum100<-matrix(NA,2000,3)
bootpe3100<-matrix(NA,2000,3)
bootln3100<-matrix(NA,2000,3)
bootwei150<-matrix(NA,2000,3)
bootgum150<-matrix(NA,2000,3)
bootpe3150<-matrix(NA,2000,3)
bootln3150<-matrix(NA,2000,3)
bootwei200<-matrix(NA,2000,3)
bootgum200<-matrix(NA,2000,3)
bootpe3200<-matrix(NA,2000,3)
bootln3200<-matrix(NA,2000,3)
bootwei300<-matrix(NA,2000,3)
bootgum300<-matrix(NA,2000,3)
bootpe3300<-matrix(NA,2000,3)
bootln3300<-matrix(NA,2000,3)
bootwei500<-matrix(NA,2000,3)
bootgum500<-matrix(NA,2000,3)
bootpe3500<-matrix(NA,2000,3)
bootln3500<-matrix(NA,2000,3)

## 30
for (i in seq(1:1971)){
  seqq<-seq(i,29+i,1)  
  bootwei_30 <- suppressWarnings(boot(fakeyears[,1][seqq], boot_wei, R=5000))  
  bootgum_30 <- suppressWarnings(boot(fakeyears[,2][seqq], boot_gum, R=5000))  
  bootpe3_30 <- suppressWarnings(boot(fakeyears[,3][seqq], boot_pe3, R=5000))  
  bootln3_30 <- suppressWarnings(boot(fakeyears[,4][seqq], boot_ln3, R=5000))  
  if(i %in% seq(1,1971,10)){
    cat("loop", i, "\n") 
  }  
  for (j in seq(1:3)){
    bootwei30[i,j]<-suppressWarnings(append((boot.ci(bootwei_30, conf=(1-alpha))$bca[4:5]),bootwei_30$t0[[1]],after=2)[[j]])
    bootgum30[i,j]<-suppressWarnings(append((boot.ci(bootgum_30, conf=(1-alpha))$bca[4:5]),bootgum_30$t0[[1]],after=2)[[j]])
    bootpe330[i,j]<-suppressWarnings(append((boot.ci(bootpe3_30, conf=(1-alpha))$bca[4:5]),bootpe3_30$t0[[1]],after=2)[[j]])
    bootln330[i,j]<-suppressWarnings(append((boot.ci(bootln3_30, conf=(1-alpha))$bca[4:5]),bootln3_30$t0[[1]],after=2)[[j]])
  }
}
## 40
for (i in seq(1:1961)){
  seqq<-seq(i,39+i,1)  
  bootwei_40 <- suppressWarnings(boot(fakeyears[,1][seqq], boot_wei, R=5000))  
  bootgum_40 <- suppressWarnings(boot(fakeyears[,2][seqq], boot_gum, R=5000))  
  bootpe3_40 <- suppressWarnings(boot(fakeyears[,3][seqq], boot_pe3, R=5000))  
  bootln3_40 <- suppressWarnings(boot(fakeyears[,4][seqq], boot_ln3, R=5000))  
  if(i %in% seq(1,1961,10)){
    cat("loop", i, "\n") 
  }  
  for (j in seq(1:3)){
    bootwei40[i,j]<-suppressWarnings(append((boot.ci(bootwei_40, conf=(1-alpha))$bca[4:5]),bootwei_40$t0[[1]],after=2)[[j]])
    bootgum40[i,j]<-suppressWarnings(append((boot.ci(bootgum_40, conf=(1-alpha))$bca[4:5]),bootgum_40$t0[[1]],after=2)[[j]])
    bootpe340[i,j]<-suppressWarnings(append((boot.ci(bootpe3_40, conf=(1-alpha))$bca[4:5]),bootpe3_40$t0[[1]],after=2)[[j]])
    bootln340[i,j]<-suppressWarnings(append((boot.ci(bootln3_40, conf=(1-alpha))$bca[4:5]),bootln3_40$t0[[1]],after=2)[[j]])
  }
}
## 50
for (i in seq(1:1951)){
  seqq<-seq(i,49+i,1)  
  bootwei_50 <- suppressWarnings(boot(fakeyears[,1][seqq], boot_wei, R=5000))  
  bootgum_50 <- suppressWarnings(boot(fakeyears[,2][seqq], boot_gum, R=5000))  
  bootpe3_50 <- suppressWarnings(boot(fakeyears[,3][seqq], boot_pe3, R=5000))  
  bootln3_50 <- suppressWarnings(boot(fakeyears[,4][seqq], boot_ln3, R=5000))  
  if(i %in% seq(1,1951,10)){
    cat("loop", i, "\n") 
  }  
  for (j in seq(1:3)){
    bootwei50[i,j]<-suppressWarnings(append((boot.ci(bootwei_50, conf=(1-alpha))$bca[4:5]),bootwei_50$t0[[1]],after=2)[[j]])
    bootgum50[i,j]<-suppressWarnings(append((boot.ci(bootgum_50, conf=(1-alpha))$bca[4:5]),bootgum_50$t0[[1]],after=2)[[j]])
    bootpe350[i,j]<-suppressWarnings(append((boot.ci(bootpe3_50, conf=(1-alpha))$bca[4:5]),bootpe3_50$t0[[1]],after=2)[[j]])
    bootln350[i,j]<-suppressWarnings(append((boot.ci(bootln3_50, conf=(1-alpha))$bca[4:5]),bootln3_50$t0[[1]],after=2)[[j]])
  }
}
## 80
for (i in seq(1:1921)){
  seqq<-seq(i,79+i,1)  
  bootwei_80 <- suppressWarnings(boot(fakeyears[,1][seqq], boot_wei, R=5000))  
  bootgum_80 <- suppressWarnings(boot(fakeyears[,2][seqq], boot_gum, R=5000))  
  bootpe3_80 <- suppressWarnings(boot(fakeyears[,3][seqq], boot_pe3, R=5000))  
  bootln3_80 <- suppressWarnings(boot(fakeyears[,4][seqq], boot_ln3, R=5000))  
  if(i %in% seq(1,1921,10)){
    cat("loop", i, "\n") 
  }  
  for (j in seq(1:3)){
    bootwei80[i,j]<-suppressWarnings(append((boot.ci(bootwei_80, conf=(1-alpha))$bca[4:5]),bootwei_80$t0[[1]],after=2)[[j]])
    bootgum80[i,j]<-suppressWarnings(append((boot.ci(bootgum_80, conf=(1-alpha))$bca[4:5]),bootgum_80$t0[[1]],after=2)[[j]])
    bootpe380[i,j]<-suppressWarnings(append((boot.ci(bootpe3_80, conf=(1-alpha))$bca[4:5]),bootpe3_80$t0[[1]],after=2)[[j]])
    bootln380[i,j]<-suppressWarnings(append((boot.ci(bootln3_80, conf=(1-alpha))$bca[4:5]),bootln3_80$t0[[1]],after=2)[[j]])
  }
}
## 100
for (i in seq(1:1901)){
  seqq<-seq(i,99+i,1)  
  bootwei_100 <- suppressWarnings(boot(fakeyears[,1][seqq], boot_wei, R=5000))  
  bootgum_100 <- suppressWarnings(boot(fakeyears[,2][seqq], boot_gum, R=5000))  
  bootpe3_100 <- suppressWarnings(boot(fakeyears[,3][seqq], boot_pe3, R=5000))  
  bootln3_100 <- suppressWarnings(boot(fakeyears[,4][seqq], boot_ln3, R=5000))  
  if(i %in% seq(1,1901,10)){
    cat("loop", i, "\n") 
  }  
  for (j in seq(1:3)){
    bootwei100[i,j]<-suppressWarnings(append((boot.ci(bootwei_100, conf=(1-alpha))$bca[4:5]),bootwei_100$t0[[1]],after=2)[[j]])
    bootgum100[i,j]<-suppressWarnings(append((boot.ci(bootgum_100, conf=(1-alpha))$bca[4:5]),bootgum_100$t0[[1]],after=2)[[j]])
    bootpe3100[i,j]<-suppressWarnings(append((boot.ci(bootpe3_100, conf=(1-alpha))$bca[4:5]),bootpe3_100$t0[[1]],after=2)[[j]])
    bootln3100[i,j]<-suppressWarnings(append((boot.ci(bootln3_100, conf=(1-alpha))$bca[4:5]),bootln3_100$t0[[1]],after=2)[[j]])
  }
}
## 150
for (i in seq(1:1851)){
  seqq<-seq(i,149+i,1)  
  bootwei_150 <- suppressWarnings(boot(fakeyears[,1][seqq], boot_wei, R=5000))  
  bootgum_150 <- suppressWarnings(boot(fakeyears[,2][seqq], boot_gum, R=5000))  
  bootpe3_150 <- suppressWarnings(boot(fakeyears[,3][seqq], boot_pe3, R=5000))  
  bootln3_150 <- suppressWarnings(boot(fakeyears[,4][seqq], boot_ln3, R=5000))  
  if(i %in% seq(1,1851,10)){
    cat("loop", i, "\n") 
  }  
  for (j in seq(1:3)){
    bootwei150[i,j]<-suppressWarnings(append((boot.ci(bootwei_150, conf=(1-alpha))$bca[4:5]),bootwei_150$t0[[1]],after=2)[[j]])
    bootgum150[i,j]<-suppressWarnings(append((boot.ci(bootgum_150, conf=(1-alpha))$bca[4:5]),bootgum_150$t0[[1]],after=2)[[j]])
    bootpe3150[i,j]<-suppressWarnings(append((boot.ci(bootpe3_150, conf=(1-alpha))$bca[4:5]),bootpe3_150$t0[[1]],after=2)[[j]])
    bootln3150[i,j]<-suppressWarnings(append((boot.ci(bootln3_150, conf=(1-alpha))$bca[4:5]),bootln3_150$t0[[1]],after=2)[[j]])
  }
}
## 200
for (i in seq(1:1801)){
  seqq<-seq(i,199+i,1)  
  bootwei_200 <- suppressWarnings(boot(fakeyears[,1][seqq], boot_wei, R=5000))  
  bootgum_200 <- suppressWarnings(boot(fakeyears[,2][seqq], boot_gum, R=5000))  
  bootpe3_200 <- suppressWarnings(boot(fakeyears[,3][seqq], boot_pe3, R=5000))  
  bootln3_200 <- suppressWarnings(boot(fakeyears[,4][seqq], boot_ln3, R=5000))  
  if(i %in% seq(1,1801,10)){
    cat("loop", i, "\n") 
  }  
  for (j in seq(1:3)){
    bootwei200[i,j]<-suppressWarnings(append((boot.ci(bootwei_200, conf=(1-alpha))$bca[4:5]),bootwei_200$t0[[1]],after=2)[[j]])
    bootgum200[i,j]<-suppressWarnings(append((boot.ci(bootgum_200, conf=(1-alpha))$bca[4:5]),bootgum_200$t0[[1]],after=2)[[j]])
    bootpe3200[i,j]<-suppressWarnings(append((boot.ci(bootpe3_200, conf=(1-alpha))$bca[4:5]),bootpe3_200$t0[[1]],after=2)[[j]])
    bootln3200[i,j]<-suppressWarnings(append((boot.ci(bootln3_200, conf=(1-alpha))$bca[4:5]),bootln3_200$t0[[1]],after=2)[[j]])
  }
}
## 300
for (i in seq(1:1701)){
  seqq<-seq(i,299+i,1)  
  bootwei_300 <- suppressWarnings(boot(fakeyears[,1][seqq], boot_wei, R=5000))  
  bootgum_300 <- suppressWarnings(boot(fakeyears[,2][seqq], boot_gum, R=5000))  
  bootpe3_300 <- suppressWarnings(boot(fakeyears[,3][seqq], boot_pe3, R=5000))  
  bootln3_300 <- suppressWarnings(boot(fakeyears[,4][seqq], boot_ln3, R=5000))  
  if(i %in% seq(1,1701,10)){
    cat("loop", i, "\n") 
  }  
  for (j in seq(1:3)){
    bootwei300[i,j]<-suppressWarnings(append((boot.ci(bootwei_300, conf=(1-alpha))$bca[4:5]),bootwei_300$t0[[1]],after=2)[[j]])
    bootgum300[i,j]<-suppressWarnings(append((boot.ci(bootgum_300, conf=(1-alpha))$bca[4:5]),bootgum_300$t0[[1]],after=2)[[j]])
    bootpe3300[i,j]<-suppressWarnings(append((boot.ci(bootpe3_300, conf=(1-alpha))$bca[4:5]),bootpe3_300$t0[[1]],after=2)[[j]])
    bootln3300[i,j]<-suppressWarnings(append((boot.ci(bootln3_300, conf=(1-alpha))$bca[4:5]),bootln3_300$t0[[1]],after=2)[[j]])
  }
}
## 500
for (i in seq(1:1501)){
  seqq<-seq(i,499+i,1)  
  bootwei_500 <- suppressWarnings(boot(fakeyears[,1][seqq], boot_wei, R=5000))  
  bootgum_500 <- suppressWarnings(boot(fakeyears[,2][seqq], boot_gum, R=5000))  
  bootpe3_500 <- suppressWarnings(boot(fakeyears[,3][seqq], boot_pe3, R=5000))  
  bootln3_500 <- suppressWarnings(boot(fakeyears[,4][seqq], boot_ln3, R=5000))  
  if(i %in% seq(1,1501,10)){
    cat("loop", i, "\n") 
  }  
  for (j in seq(1:3)){
    bootwei500[i,j]<-suppressWarnings(append((boot.ci(bootwei_500, conf=(1-alpha))$bca[4:5]),bootwei_500$t0[[1]],after=2)[[j]])
    bootgum500[i,j]<-suppressWarnings(append((boot.ci(bootgum_500, conf=(1-alpha))$bca[4:5]),bootgum_500$t0[[1]],after=2)[[j]])
    bootpe3500[i,j]<-suppressWarnings(append((boot.ci(bootpe3_500, conf=(1-alpha))$bca[4:5]),bootpe3_500$t0[[1]],after=2)[[j]])
    bootln3500[i,j]<-suppressWarnings(append((boot.ci(bootln3_500, conf=(1-alpha))$bca[4:5]),bootln3_500$t0[[1]],after=2)[[j]])
  }
}

########## CI with listko_y100 for shift of 30,40,50 years
maxOL_list<-maxOL_listY100
CI_shift_100_wei_30<-matrix(NA,70,3)
CI_shift_100_gum_30<-matrix(NA,70,3)
CI_shift_100_pe3_30<-matrix(NA,70,3)
CI_shift_100_ln3_30<-matrix(NA,70,3)
CI_shift_y100_wei_30<-list()
CI_shift_y100_gum_30<-list()
CI_shift_y100_pe3_30<-list()
CI_shift_y100_ln3_30<-list()
CI_shift_100_wei_40<-matrix(NA,60,3)
CI_shift_100_gum_40<-matrix(NA,60,3)
CI_shift_100_pe3_40<-matrix(NA,60,3)
CI_shift_100_ln3_40<-matrix(NA,60,3)
CI_shift_y100_wei_40<-list()
CI_shift_y100_gum_40<-list()
CI_shift_y100_pe3_40<-list()
CI_shift_y100_ln3_40<-list()
CI_shift_100_wei_50<-matrix(NA,50,3)
CI_shift_100_gum_50<-matrix(NA,50,3)
CI_shift_100_pe3_50<-matrix(NA,50,3)
CI_shift_100_ln3_50<-matrix(NA,50,3)
CI_shift_y100_wei_50<-list()
CI_shift_y100_gum_50<-list()
CI_shift_y100_pe3_50<-list()
CI_shift_y100_ln3_50<-list()
## for maxOL_Y100 with shift 30 (seqq needs to go from 1:70, as its 100years)
for(j in seq(1,45,1)){
  if(j %in% seq(1,45,1)){
    cat("river", j, "\n") 
  }
  for (k in seq(1:70)){
    seqq<-seq(k,29+k,1)
    boot_shift100_wei_30<-suppressWarnings(boot(maxOL_list[[j]][seqq],boot_wei, R=5000))
    boot_shift100_gum_30<-suppressWarnings(boot(maxOL_list[[j]][seqq],boot_gum, R=5000))
    boot_shift100_pe3_30<-suppressWarnings(boot(maxOL_list[[j]][seqq],boot_pe3, R=5000))
    boot_shift100_ln3_30<-suppressWarnings(boot(maxOL_list[[j]][seqq],boot_ln3, R=5000))
    if(k %in% seq(1,70,1)){
      cat("loop", k, "\n") 
    }
    for (l in seq(1:3)){
      CI_shift_100_wei_30[k,l]<-suppressWarnings(append((boot.ci(boot_shift100_wei_30, conf=(1-alpha))$bca[4:5]),boot_shift100_wei_30$t0[[1]],after=2)[[l]])
      CI_shift_100_gum_30[k,l]<-suppressWarnings(append((boot.ci(boot_shift100_gum_30, conf=(1-alpha))$bca[4:5]),boot_shift100_gum_30$t0[[1]],after=2)[[l]])
      CI_shift_100_pe3_30[k,l]<-suppressWarnings(append((boot.ci(boot_shift100_pe3_30, conf=(1-alpha))$bca[4:5]),boot_shift100_pe3_30$t0[[1]],after=2)[[l]])
      CI_shift_100_ln3_30[k,l]<-suppressWarnings(append((boot.ci(boot_shift100_ln3_30, conf=(1-alpha))$bca[4:5]),boot_shift100_ln3_30$t0[[1]],after=2)[[l]])
    }
  }
  CI_shift_y100_wei_30[j]<-list(CI_shift_100_wei_30)
  CI_shift_y100_gum_30[j]<-list(CI_shift_100_gum_30)
  CI_shift_y100_pe3_30[j]<-list(CI_shift_100_pe3_30)
  CI_shift_y100_ln3_30[j]<-list(CI_shift_100_ln3_30)
}

## for maxOL_Y100 with shift 40
for(j in seq(1,45,1)){
  if(j %in% seq(1,45,1)){
    cat("river", j, "\n") 
  }
  for (k in seq(1:60)){
    seqq<-seq(k,39+k,1)
    boot_shift100_wei_40<-suppressWarnings(boot(maxOL_list[[j]][seqq],boot_wei, R=5000))
    boot_shift100_gum_40<-suppressWarnings(boot(maxOL_list[[j]][seqq],boot_gum, R=5000))
    boot_shift100_pe3_40<-suppressWarnings(boot(maxOL_list[[j]][seqq],boot_pe3, R=5000))
    boot_shift100_ln3_40<-suppressWarnings(boot(maxOL_list[[j]][seqq],boot_ln3, R=5000))
    if(k %in% seq(1,60,1)){
      cat("loop", k, "\n") 
    }
    for (l in seq(1:3)){
      CI_shift_100_wei_40[k,l]<-suppressWarnings(append((boot.ci(boot_shift100_wei_40, conf=(1-alpha))$bca[4:5]),boot_shift100_wei_40$t0[[1]],after=2)[[l]])
      CI_shift_100_gum_40[k,l]<-suppressWarnings(append((boot.ci(boot_shift100_gum_40, conf=(1-alpha))$bca[4:5]),boot_shift100_gum_40$t0[[1]],after=2)[[l]])
      CI_shift_100_pe3_40[k,l]<-suppressWarnings(append((boot.ci(boot_shift100_pe3_40, conf=(1-alpha))$bca[4:5]),boot_shift100_pe3_40$t0[[1]],after=2)[[l]])
      CI_shift_100_ln3_40[k,l]<-suppressWarnings(append((boot.ci(boot_shift100_ln3_40, conf=(1-alpha))$bca[4:5]),boot_shift100_ln3_40$t0[[1]],after=2)[[l]])
    }
  }
  CI_shift_y100_wei_40[j]<-list(CI_shift_100_wei_40)
  CI_shift_y100_gum_40[j]<-list(CI_shift_100_gum_40)
  CI_shift_y100_pe3_40[j]<-list(CI_shift_100_pe3_40)
  CI_shift_y100_ln3_40[j]<-list(CI_shift_100_ln3_40)
}

## for maxOL_Y100 with shift 50
for(j in seq(1,45,1)){
  if(j %in% seq(1,45,1)){
    cat("river", j, "\n") 
  }
  for (k in seq(1:50)){
    seqq<-seq(k,49+k,1)
    boot_shift100_wei_50<-suppressWarnings(boot(maxOL_list[[j]][seqq],boot_wei, R=5000))
    boot_shift100_gum_50<-suppressWarnings(boot(maxOL_list[[j]][seqq],boot_gum, R=5000))
    boot_shift100_pe3_50<-suppressWarnings(boot(maxOL_list[[j]][seqq],boot_pe3, R=5000))
    boot_shift100_ln3_50<-suppressWarnings(boot(maxOL_list[[j]][seqq],boot_ln3, R=5000))
    if(k %in% seq(1,50,1)){
      cat("loop", k, "\n") 
    }
    for (l in seq(1:3)){
      CI_shift_100_wei_50[k,l]<-suppressWarnings(append((boot.ci(boot_shift100_wei_50, conf=(1-alpha))$bca[4:5]),boot_shift100_wei_50$t0[[1]],after=2)[[l]])
      CI_shift_100_gum_50[k,l]<-suppressWarnings(append((boot.ci(boot_shift100_gum_50, conf=(1-alpha))$bca[4:5]),boot_shift100_gum_50$t0[[1]],after=2)[[l]])
      CI_shift_100_pe3_50[k,l]<-suppressWarnings(append((boot.ci(boot_shift100_pe3_50, conf=(1-alpha))$bca[4:5]),boot_shift100_pe3_50$t0[[1]],after=2)[[l]])
      CI_shift_100_ln3_50[k,l]<-suppressWarnings(append((boot.ci(boot_shift100_ln3_50, conf=(1-alpha))$bca[4:5]),boot_shift100_ln3_50$t0[[1]],after=2)[[l]])
    }
  }
  CI_shift_y100_wei_50[j]<-list(CI_shift_100_wei_50)
  CI_shift_y100_gum_50[j]<-list(CI_shift_100_gum_50)
  CI_shift_y100_pe3_50[j]<-list(CI_shift_100_pe3_50)
  CI_shift_y100_ln3_50[j]<-list(CI_shift_100_ln3_50)
}
################## Plots

### shift_30_45[[2]] und shift_30_227[[12]] are with NR. 6140700 the same

### plot listko_y60, shift 30 years
p=seq(1,30,1)
par(mfrow=c(2,2))
plot(CI_shift_y60_wei[[15]][,3],type="l",ylim=c(6000,13000),xlab = "Years",ylab="Discharge[m^3/s]",main="WEI")
lines(CI_shift_y60_wei[[15]][,1],col="black")
lines(CI_shift_y60_wei[[15]][,2],col="black")
polygon(c(p,rev(p)),c(CI_shift_y60_wei[[15]][,1][c(1:30)],rev(CI_shift_y60_wei[[15]][,2][c(1:30)])), col = "grey50")
lines(CI_shift_y60_wei[[15]][,3],col="black")
lines(CI_shift_y60_gum[[15]][,3],col="red")
lines(CI_shift_y60_pe3[[15]][,3],col="green")
lines(CI_shift_y60_ln3[[15]][,3],col="blue")

plot(CI_shift_y60_gum[[2]][,3],type="l",ylim=c(300,900),xlab = "Years",ylab="Discharge[m^3/s]",main="GUM")
lines(CI_shift_y60_gum[[2]][,1],col="black")
lines(CI_shift_y60_gum[[2]][,2],col="black")
polygon(c(p,rev(p)),c(CI_shift_y60_gum[[1]][,1],rev(CI_shift_y60_gum[[1]][,2])), col = "grey50")
lines(CI_shift_y60_gum[[2]][,3],col="black")
lines(CI_shift_y60_gum[[2]][,3],col="red")
lines(CI_shift_y60_pe3[[2]][,3],col="green")
lines(CI_shift_y60_ln3[[2]][,3],col="blue")

plot(CI_shift_y60_pe3[[1]][,3],type="l",ylim=c(400,850),xlab = "Years",ylab="Discharge[m^3/s]",main="PE3")
lines(CI_shift_y60_pe3[[1]][,1],col="gray")
lines(CI_shift_y60_pe3[[1]][,2],col="gray")
polygon(c(p,rev(p)),c(CI_shift_y60_pe3[[1]][,1],rev(CI_shift_y60_pe3[[1]][,2])), col = "grey50")
lines(CI_shift_y60_pe3[[1]][,3],col="black")
lines(CI_shift_y60_gum[[1]][,3],col="red")
lines(CI_shift_y60_pe3[[1]][,3],col="green")
lines(CI_shift_y60_ln3[[1]][,3],col="blue")

plot(CI_shift_y60_ln3[[1]][,3],type="l",,ylim=c(300,900),xlab = "Years",ylab="Discharge[m^3/s]",main="LN3")
lines(CI_shift_y60_ln3[[1]][,1],col="gray")
lines(CI_shift_y60_ln3[[1]][,2],col="gray")
polygon(c(p,rev(p)),c(CI_shift_y60_ln3[[1]][,1],rev(CI_shift_y60_ln3[[1]][,2])), col = "grey50")
lines(CI_shift_y60_ln3[[1]][,3],col="black")
lines(CI_shift_y60_gum[[1]][,3],col="red")
lines(CI_shift_y60_pe3[[1]][,3],col="green")
lines(CI_shift_y60_ln3[[1]][,3],col="blue")

names(shift_30_227)
####################################### plot CI for listko_y100 shift 30 years

p=seq(1,70,1)
par(mfrow=c(2,2))
plot(CI_shift_y100_wei_30[[3]][,3],type="l",ylim=c(6000,13000),xlab = "Years",ylab="Discharge[m^3/s]",main="WEI")
lines(CI_shift_y100_wei_30[[3]][,1],col="black")
lines(CI_shift_y100_wei_30[[3]][,2],col="black")
polygon(c(p,rev(p)),c(CI_shift_y100_wei_30[[3]][,1],rev(CI_shift_y100_wei_30[[3]][,2])), col = "grey50")
lines(CI_shift_y100_wei_30[[3]][,3],col="black")
lines(CI_shift_y100_wei_30[[3]][,3],col="black")
lines(CI_shift_y100_gum_30[[3]][,3],col="red")
lines(CI_shift_y100_pe3_30[[3]][,3],col="green")
lines(CI_shift_y100_ln3_30[[3]][,3],col="blue")

plot(CI_shift_y100_gum_30[[3]][,3],type="l",ylim=c(6000,15000),xlab = "Years",ylab="Discharge[m^3/s]",main="GUM")
lines(CI_shift_y100_gum_30[[3]][,1],col="black")
lines(CI_shift_y100_gum_30[[3]][,2],col="black")
polygon(c(p,rev(p)),c(CI_shift_y100_gum_30[[3]][,1],rev(CI_shift_y100_gum_30[[3]][,2])), col = "grey50")
lines(CI_shift_y100_gum_30[[3]][,3],col="black")
lines(CI_shift_y100_wei_30[[3]][,3],col="black")
lines(CI_shift_y100_gum_30[[3]][,3],col="red")
lines(CI_shift_y100_pe3_30[[3]][,3],col="green")
lines(CI_shift_y100_ln3_30[[3]][,3],col="blue")

plot(CI_shift_y100_pe3_30[[3]][,3],type="l",ylim=c(6000,15000),xlab = "Years",ylab="Discharge[m^3/s]",main="PE3")
lines(CI_shift_y100_pe3_30[[3]][,1],col="gray")
lines(CI_shift_y100_pe3_30[[3]][,2],col="gray")
polygon(c(p,rev(p)),c(CI_shift_y100_pe3_30[[3]][,1],rev(CI_shift_y100_pe3_30[[3]][,2])), col = "grey50")
lines(CI_shift_y100_pe3_30[[3]][,3],col="black")
lines(CI_shift_y100_wei_30[[3]][,3],col="black")
lines(CI_shift_y100_gum_30[[3]][,3],col="red")
lines(CI_shift_y100_pe3_30[[3]][,3],col="green")
lines(CI_shift_y100_ln3_30[[3]][,3],col="blue")

plot(CI_shift_y100_ln3_30[[3]][,3],type="l",,ylim=c(6000,15000),xlab = "Years",ylab="Discharge[m^3/s]",main="LN3")
lines(CI_shift_y100_ln3_30[[3]][,1],col="gray")
lines(CI_shift_y100_ln3_30[[3]][,2],col="gray")
polygon(c(p,rev(p)),c(CI_shift_y100_ln3_30[[3]][,1],rev(CI_shift_y100_ln3_30[[3]][,2])), col = "grey50")
lines(CI_shift_y100_ln3_30[[3]][,3],col="black")
lines(CI_shift_y100_wei_30[[3]][,3],col="black")
lines(CI_shift_y100_gum_30[[3]][,3],col="red")
lines(CI_shift_y100_pe3_30[[3]][,3],col="green")
lines(CI_shift_y100_ln3_30[[3]][,3],col="blue")


####################################### plot CI for listko_y100 shift 40 years
p=seq(1,60,1)
par(mfrow=c(2,2))
plot(CI_shift_y100_wei_40[[3]][,3],type="l",ylim=c(6000,12000),xlab = "Years",ylab="Discharge[m^3/s]",main="WEI")
lines(CI_shift_y100_wei_40[[3]][,1],col="black")
lines(CI_shift_y100_wei_40[[3]][,2],col="black")
polygon(c(p,rev(p)),c(CI_shift_y100_wei_40[[3]][,1],rev(CI_shift_y100_wei_40[[3]][,2])), col = "grey50")
lines(CI_shift_y100_wei_40[[3]][,3],col="black")
lines(CI_shift_y100_wei_40[[3]][,3],col="black")
lines(CI_shift_y100_gum_40[[3]][,3],col="red")
lines(CI_shift_y100_gum_40[[3]][,1],col="red",lty=2)
lines(CI_shift_y100_gum_40[[3]][,2],col="red",lty=2)
lines(CI_shift_y100_pe3_40[[3]][,3],col="green")
lines(CI_shift_y100_pe3_40[[3]][,1],col="green",lty=2)
lines(CI_shift_y100_pe3_40[[3]][,2],col="green",lty=2)
lines(CI_shift_y100_ln3_40[[3]][,3],col="blue")
lines(CI_shift_y100_ln3_40[[3]][,1],col="blue",lty=2)
lines(CI_shift_y100_ln3_40[[3]][,2],col="blue",lty=2)

plot(CI_shift_y100_gum_40[[3]][,3],type="l",ylim=c(6000,15000),xlab = "Years",ylab="Discharge[m^3/s]",main="GUM")
lines(CI_shift_y100_gum_40[[3]][,1],col="black")
lines(CI_shift_y100_gum_40[[3]][,2],col="black")
polygon(c(p,rev(p)),c(CI_shift_y100_gum_40[[3]][,1],rev(CI_shift_y100_gum_40[[3]][,2])), col = "grey50")
lines(CI_shift_y100_wei_40[[3]][,3],col="black")
lines(CI_shift_y100_wei_40[[3]][,3],col="black")
lines(CI_shift_y100_gum_40[[3]][,3],col="red")
lines(CI_shift_y100_gum_40[[3]][,1],col="red",lty=2)
lines(CI_shift_y100_gum_40[[3]][,2],col="red",lty=2)
lines(CI_shift_y100_pe3_40[[3]][,3],col="green")
lines(CI_shift_y100_pe3_40[[3]][,1],col="green",lty=2)
lines(CI_shift_y100_pe3_40[[3]][,2],col="green",lty=2)
lines(CI_shift_y100_ln3_40[[3]][,3],col="blue")
lines(CI_shift_y100_ln3_40[[3]][,1],col="blue",lty=2)
lines(CI_shift_y100_ln3_40[[3]][,2],col="blue",lty=2)

plot(CI_shift_y100_pe3_40[[3]][,3],type="l",ylim=c(6000,15000),xlab = "Years",ylab="Discharge[m^3/s]",main="PE3")
lines(CI_shift_y100_pe3_40[[3]][,1],col="gray")
lines(CI_shift_y100_pe3_40[[3]][,2],col="gray")
polygon(c(p,rev(p)),c(CI_shift_y100_pe3_40[[3]][,1],rev(CI_shift_y100_pe3_40[[3]][,2])), col = "grey50")
lines(CI_shift_y100_wei_40[[3]][,3],col="black")
lines(CI_shift_y100_wei_40[[3]][,3],col="black")
lines(CI_shift_y100_gum_40[[3]][,3],col="red")
lines(CI_shift_y100_gum_40[[3]][,1],col="red",lty=2)
lines(CI_shift_y100_gum_40[[3]][,2],col="red",lty=2)
lines(CI_shift_y100_pe3_40[[3]][,3],col="green")
lines(CI_shift_y100_pe3_40[[3]][,1],col="green",lty=2)
lines(CI_shift_y100_pe3_40[[3]][,2],col="green",lty=2)
lines(CI_shift_y100_ln3_40[[3]][,3],col="blue")
lines(CI_shift_y100_ln3_40[[3]][,1],col="blue",lty=2)
lines(CI_shift_y100_ln3_40[[3]][,2],col="blue",lty=2)

plot(CI_shift_y100_ln3_40[[3]][,3],type="l",,ylim=c(6000,15000),xlab = "Years",ylab="Discharge[m^3/s]",main="LN3")
lines(CI_shift_y100_ln3_40[[3]][,1],col="gray")
lines(CI_shift_y100_ln3_40[[3]][,2],col="gray")
polygon(c(p,rev(p)),c(CI_shift_y100_ln3_40[[3]][,1],rev(CI_shift_y100_ln3_40[[3]][,2])), col = "grey50")
lines(CI_shift_y100_wei_40[[3]][,3],col="black")
lines(CI_shift_y100_wei_40[[3]][,3],col="black")
lines(CI_shift_y100_gum_40[[3]][,3],col="red")
lines(CI_shift_y100_gum_40[[3]][,1],col="red",lty=2)
lines(CI_shift_y100_gum_40[[3]][,2],col="red",lty=2)
lines(CI_shift_y100_pe3_40[[3]][,3],col="green")
lines(CI_shift_y100_pe3_40[[3]][,1],col="green",lty=2)
lines(CI_shift_y100_pe3_40[[3]][,2],col="green",lty=2)
lines(CI_shift_y100_ln3_40[[3]][,3],col="blue")
lines(CI_shift_y100_ln3_40[[3]][,1],col="blue",lty=2)
lines(CI_shift_y100_ln3_40[[3]][,2],col="blue",lty=2)


lines(CI_shift_y100_ln3_40[[3]][,3],col="black")
lines(CI_shift_y100_wei_40[[3]][,3],col="black")
lines(CI_shift_y100_gum_40[[3]][,3],col="red")
lines(CI_shift_y100_pe3_40[[3]][,3],col="green")
lines(CI_shift_y100_ln3_40[[3]][,3],col="blue")

####################################### plot CI for listko_y100 shift 50 years
p=seq(1,50,1)
par(mfrow=c(2,2))
plot(CI_shift_y100_wei_50[[3]][,3],type="l",ylim=c(6000,13000),xlab = "Years",ylab="Discharge[m^3/s]",main="WEI")
lines(CI_shift_y100_wei_50[[3]][,1],col="black",lty=2)
lines(CI_shift_y100_wei_50[[3]][,2],col="black",lty=2)
polygon(c(p,rev(p)),c(CI_shift_y100_wei_50[[3]][,1],rev(CI_shift_y100_wei_50[[3]][,2])), col = "grey50")
lines(CI_shift_y100_wei_50[[3]][,3],col="black")
lines(CI_shift_y100_wei_50[[3]][,3],col="black")
lines(CI_shift_y100_gum_50[[3]][,3],col="red")
lines(CI_shift_y100_gum_50[[3]][,1],col="red",lty=2)
lines(CI_shift_y100_gum_50[[3]][,2],col="red",lty=2)
lines(CI_shift_y100_pe3_50[[3]][,3],col="green")
lines(CI_shift_y100_pe3_50[[3]][,1],col="green",lty=2)
lines(CI_shift_y100_pe3_50[[3]][,2],col="green",lty=2)
lines(CI_shift_y100_ln3_50[[3]][,3],col="blue")
lines(CI_shift_y100_ln3_50[[3]][,1],col="blue",lty=2)
lines(CI_shift_y100_ln3_50[[3]][,2],col="blue",lty=2)

plot(CI_shift_y100_gum_50[[3]][,3],type="l",ylim=c(6000,15000),xlab = "Years",ylab="Discharge[m^3/s]",main="GUM")
lines(CI_shift_y100_gum_50[[3]][,1],col="black",lty=2)
lines(CI_shift_y100_gum_50[[3]][,2],col="black",lty=2)
polygon(c(p,rev(p)),c(CI_shift_y100_gum_50[[3]][,1],rev(CI_shift_y100_gum_50[[3]][,2])), col = "grey50")
lines(CI_shift_y100_wei_50[[3]][,3],col="black")
lines(CI_shift_y100_wei_50[[3]][,3],col="black")
lines(CI_shift_y100_gum_50[[3]][,3],col="red")
lines(CI_shift_y100_gum_50[[3]][,1],col="red",lty=2)
lines(CI_shift_y100_gum_50[[3]][,2],col="red",lty=2)
lines(CI_shift_y100_pe3_50[[3]][,3],col="green")
lines(CI_shift_y100_pe3_50[[3]][,1],col="green",lty=2)
lines(CI_shift_y100_pe3_50[[3]][,2],col="green",lty=2)
lines(CI_shift_y100_ln3_50[[3]][,3],col="blue")
lines(CI_shift_y100_ln3_50[[3]][,1],col="blue",lty=2)
lines(CI_shift_y100_ln3_50[[3]][,2],col="blue",lty=2)

plot(CI_shift_y100_pe3_50[[3]][,3],type="l",ylim=c(6000,15000),xlab = "Years",ylab="Discharge[m^3/s]",main="PE3")
lines(CI_shift_y100_pe3_50[3][,1],col="gray")
lines(CI_shift_y100_pe3_50[[3]][,2],col="gray")
polygon(c(p,rev(p)),c(CI_shift_y100_pe3_50[[3]][,1],rev(CI_shift_y100_pe3_50[[3]][,2])), col = "grey50")
lines(CI_shift_y100_wei_50[[3]][,3],col="black")
lines(CI_shift_y100_wei_50[[3]][,3],col="black")
lines(CI_shift_y100_gum_50[[3]][,3],col="red")
lines(CI_shift_y100_gum_50[[3]][,1],col="red",lty=2)
lines(CI_shift_y100_gum_50[[3]][,2],col="red",lty=2)
lines(CI_shift_y100_pe3_50[[3]][,3],col="green")
lines(CI_shift_y100_pe3_50[[3]][,1],col="green",lty=2)
lines(CI_shift_y100_pe3_50[[3]][,2],col="green",lty=2)
lines(CI_shift_y100_ln3_50[[3]][,3],col="blue")
lines(CI_shift_y100_ln3_50[[3]][,1],col="blue",lty=2)
lines(CI_shift_y100_ln3_50[[3]][,2],col="blue",lty=2)

plot(CI_shift_y100_ln3_50[[3]][,3],type="l",ylim=c(6000,15000),xlab = "Years",ylab="Discharge[m^3/s]",main="LN3")
lines(CI_shift_y100_ln3_50[[3]][,1],col="gray")
lines(CI_shift_y100_ln3_50[[3]][,2],col="gray")
polygon(c(p,rev(p)),c(CI_shift_y100_ln3_50[[3]][,1],rev(CI_shift_y100_ln3_50[[3]][,2])), col = "grey50")
lines(CI_shift_y100_wei_50[[3]][,3],col="black")
lines(CI_shift_y100_wei_50[[3]][,3],col="black")
lines(CI_shift_y100_gum_50[[3]][,3],col="red")
lines(CI_shift_y100_gum_50[[3]][,1],col="red",lty=2)
lines(CI_shift_y100_gum_50[[3]][,2],col="red",lty=2)
lines(CI_shift_y100_pe3_50[[3]][,3],col="green")
lines(CI_shift_y100_pe3_50[[3]][,1],col="green",lty=2)
lines(CI_shift_y100_pe3_50[[3]][,2],col="green",lty=2)
lines(CI_shift_y100_ln3_50[[3]][,3],col="blue")
lines(CI_shift_y100_ln3_50[[3]][,1],col="blue",lty=2)
lines(CI_shift_y100_ln3_50[[3]][,2],col="blue",lty=2)


lines(CI_shift_y100_ln3_50[[3]][,3],col="black")
lines(CI_shift_y100_wei_50[[3]][,3],col="black")
lines(CI_shift_y100_gum_50[[3]][,3],col="red")
lines(CI_shift_y100_pe3_50[[3]][,3],col="green")
lines(CI_shift_y100_ln3_50[[3]][,3],col="blue")


############## PLOT compare 30/40/50
dev.off()
p=seq(1,70,1)
plot(CI_shift_y100_wei_30[[3]][,3],type="l",ylim=c(6000,13000),xlab = "Years",ylab="Discharge[m^3/s]",main="WEI")
lines(CI_shift_y100_wei_30[[3]][,1],col="black")
lines(CI_shift_y100_wei_30[[3]][,2],col="black")
polygon(c(p,rev(p)),c(CI_shift_y100_wei_30[[3]][,1],rev(CI_shift_y100_wei_30[[3]][,2])), col = "grey50")
lines(CI_shift_y100_wei_30[[3]][,3],col="black")
lines(CI_shift_y100_wei_40[[3]][,3],col="red")
lines(CI_shift_y100_wei_50[[3]][,3],col="green")

p=seq(1,70,1)
plot(CI_shift_y100_gum_30[[3]][,3],type="l",ylim=c(6000,13000),xlab = "Years",ylab="Discharge[m^3/s]",main="GUM")
lines(CI_shift_y100_gum_30[[3]][,1],col="black")
lines(CI_shift_y100_gum_30[[3]][,2],col="black")
polygon(c(p,rev(p)),c(CI_shift_y100_gum_30[[3]][,1],rev(CI_shift_y100_gum_30[[3]][,2])), col = "grey50")
lines(CI_shift_y100_gum_30[[3]][,3],col="black")
lines(CI_shift_y100_gum_40[[3]][,3],col="red")
lines(CI_shift_y100_gum_50[[3]][,3],col="green")

plot(CI_shift_y100_gum_30[[1]][,3],ylim=c(min(CI_shift_y100_gum_30[[1]]),max(CI_shift_y100_gum_30[[1]]+1000)),type="l",col="red")
lines(CI_shift_y100_wei_30[[1]][,3],)
lines(CI_shift_y100_ln3_30[[1]][,3],col="blue")
lines(CI_shift_y100_pe3_30[[1]][,3],col="green")
####################################### plot CI for FAKEYEARS y2000
p=seq(1,2000,1)
par(mfrow=c(2,2))
#CI_fakeyears2000_wei<-bootwei[c(1:2000),]
#CI_fakeyears2000_gum<-bootgum[c(1:2000),]
#CI_fakeyears2000_pe3<-bootpe3[c(1:2000),]
#CI_fakeyears2000_ln3<-bootln3[c(1:2000),]
#save(CI_fakeyears2000_wei,file="~/Studium/Watermanagement/Masterarbeit/WorkSpace/selected/CI_fakeyears2000_wei.RData")

plot(CI_fakeyears2000_wei[,3],type="l",ylim=c(7500,11000),xlab = "Years",ylab="Discharge[m^3/s]",main="WEI")
lines(CI_fakeyears2000_wei[,1],col="black")
lines(CI_fakeyears2000_wei[,2],col="black")
polygon(c(p,rev(p)),c(CI_fakeyears2000_wei[,1],rev(CI_fakeyears2000_wei[,2])), col = "grey50")
lines(CI_fakeyears2000_wei[,3],col="black")
#lines(CI_fakeyears2000_wei[,3],col="black")
#lines(CI_fakeyears2000_gum[,3],col="red")
#lines(CI_fakeyears2000_pe3[,3],col="green")
#lines(CI_fakeyears2000_ln3[,3],col="blue")

plot(CI_fakeyears2000_gum[,3],type="l",ylim=c(7500,13000),xlab = "Years",ylab="Discharge[m^3/s]",main="GUM")
lines(CI_fakeyears2000_gum[,1],col="black")
lines(CI_fakeyears2000_gum[,2],col="black")
polygon(c(p,rev(p)),c(CI_fakeyears2000_gum[,1],rev(CI_fakeyears2000_gum[,2])), col = "grey50")
lines(CI_fakeyears2000_gum[,3],col="black")
#lines(CI_fakeyears2000_wei[,3],col="black")
#lines(CI_fakeyears2000_gum[,3],col="red")
#lines(CI_fakeyears2000_pe3[,3],col="green")
#lines(CI_fakeyears2000_ln3[,3],col="blue")

plot(CI_fakeyears2000_pe3[,3],type="l",ylim=c(9000,21000),xlab = "Years",ylab="Discharge[m^3/s]",main="PE3")
lines(CI_fakeyears2000_pe3[,1],col="gray")
lines(CI_fakeyears2000_pe3[,2],col="gray")
polygon(c(p,rev(p)),c(CI_fakeyears2000_pe3[,1],rev(CI_fakeyears2000_pe3[,2])), col = "grey50")
lines(CI_fakeyears2000_pe3[,3],col="black")
#lines(CI_fakeyears2000_wei[,3],col="black")
#lines(CI_fakeyears2000_gum[,3],col="red")
#lines(CI_fakeyears2000_pe3[,3],col="green")
#lines(CI_fakeyears2000_ln3[,3],col="blue")

plot(CI_fakeyears2000_ln3[,3],type="l",,ylim=c(7000,14000),xlab = "Years",ylab="Discharge[m^3/s]",main="LN3")
lines(CI_fakeyears2000_ln3[,1],col="gray")
lines(CI_fakeyears2000_ln3[,2],col="gray")
polygon(c(p,rev(p)),c(CI_fakeyears2000_ln3[,1],rev(CI_fakeyears2000_ln3[,2])), col = "grey50")
lines(CI_fakeyears2000_ln3[,3],col="black")
#lines(CI_fakeyears2000_wei[,3],col="black")
#lines(CI_fakeyears2000_gum[,3],col="red")
#lines(CI_fakeyears2000_pe3[,3],col="green")
#lines(CI_fakeyears2000_ln3[,3],col="blue")
dev.off()

### plot all fakeyears in one
plot(CI_fakeyears2000_pe3[,3],type="l",ylim=c(7000,21000),xlab = "Years",ylab="Discharge[m^3/s]",main="All Functions")
lines(CI_fakeyears2000_pe3[,1],col="gray")
lines(CI_fakeyears2000_pe3[,2],col="gray")
polygon(c(p,rev(p)),c(CI_fakeyears2000_pe3[,1],rev(CI_fakeyears2000_pe3[,2])), col = "grey50")
lines(CI_fakeyears2000_pe3[,3],col="green")
lines(CI_fakeyears2000_ln3[,1],col="gray")
lines(CI_fakeyears2000_ln3[,2],col="gray")
polygon(c(p,rev(p)),c(CI_fakeyears2000_ln3[,1],rev(CI_fakeyears2000_ln3[,2])), col = "grey50")
lines(CI_fakeyears2000_ln3[,3],col="blue")
lines(CI_fakeyears2000_gum[,1],col="black")
lines(CI_fakeyears2000_gum[,2],col="black")
polygon(c(p,rev(p)),c(CI_fakeyears2000_gum[,1],rev(CI_fakeyears2000_gum[,2])), col = "grey50")
lines(CI_fakeyears2000_gum[,3],col="red")
lines(CI_fakeyears2000_wei[,1],col="black")
lines(CI_fakeyears2000_wei[,2],col="black")
polygon(c(p,rev(p)),c(CI_fakeyears2000_wei[,1],rev(CI_fakeyears2000_wei[,2])), col = "grey50")
lines(CI_fakeyears2000_wei[,3],col="black")
lines(CI_fakeyears2000_ln3[,3],col="blue")

####################################### plot CI for FAKEYEARS y50 up to 200 by 50 years
### for wei
par(mfrow=c(2,3))
p=seq(1,30,1)
plot(CI_fakeyears2000_wei[p,3],type="l",ylim=c(7500,15000),xlab = "Years",ylab="Discharge[m^3/s]",main="WEI 30 years")
lines(CI_fakeyears2000_wei[p,1],col="black")
lines(CI_fakeyears2000_wei[p,2],col="black")
polygon(c(p,rev(p)),c(CI_fakeyears2000_wei[p,1],rev(CI_fakeyears2000_wei[p,2])), col = "grey50")
lines(CI_fakeyears2000_wei[p,3],col="black")
lines(CI_fakeyears2000_wei[p,3],col="black")
lines(CI_fakeyears2000_gum[p,3],col="red")
lines(CI_fakeyears2000_pe3[p,3],col="green")
lines(CI_fakeyears2000_ln3[p,3],col="blue")
lines(rep(CI_fakeyears2000_wei[2000,3],NROW(p)),col="orange")

p=seq(1,40,1)
plot(CI_fakeyears2000_wei[p,3],type="l",ylim=c(7500,15000),xlab = "Years",ylab="Discharge[m^3/s]",main="WEI 40 years")
lines(CI_fakeyears2000_wei[p,1],col="black")
lines(CI_fakeyears2000_wei[p,2],col="black")
polygon(c(p,rev(p)),c(CI_fakeyears2000_wei[p,1],rev(CI_fakeyears2000_wei[p,2])), col = "grey50")
lines(CI_fakeyears2000_wei[p,3],col="black")
lines(CI_fakeyears2000_wei[p,3],col="black")
lines(CI_fakeyears2000_gum[p,3],col="red")
lines(CI_fakeyears2000_pe3[p,3],col="green")
lines(CI_fakeyears2000_ln3[p,3],col="blue")
lines(rep(CI_fakeyears2000_wei[2000,3],NROW(p)),col="orange")

p=seq(1,50,1)
plot(CI_fakeyears2000_wei[p,3],type="l",ylim=c(7500,15000),xlab = "Years",ylab="Discharge[m^3/s]",main="WEI 50 years")
lines(CI_fakeyears2000_wei[p,1],col="black")
lines(CI_fakeyears2000_wei[p,2],col="black")
polygon(c(p,rev(p)),c(CI_fakeyears2000_wei[p,1],rev(CI_fakeyears2000_wei[p,2])), col = "grey50")
lines(CI_fakeyears2000_wei[p,3],col="black")
lines(CI_fakeyears2000_wei[p,3],col="black")
lines(CI_fakeyears2000_gum[p,3],col="red")
lines(CI_fakeyears2000_pe3[p,3],col="green")
lines(CI_fakeyears2000_ln3[p,3],col="blue")
lines(rep(CI_fakeyears2000_wei[2000,3],NROW(p)),col="orange")

p=seq(1,100,1)
plot(CI_fakeyears2000_wei[p,3],type="l",ylim=c(7500,15000),xlab = "Years",ylab="Discharge[m^3/s]",main="WEI 100 years")
lines(CI_fakeyears2000_wei[p,1],col="black")
lines(CI_fakeyears2000_wei[p,2],col="black")
polygon(c(p,rev(p)),c(CI_fakeyears2000_wei[p,1],rev(CI_fakeyears2000_wei[p,2])), col = "grey50")
lines(CI_fakeyears2000_wei[p,3],col="black")
lines(CI_fakeyears2000_wei[p,3],col="black")
lines(CI_fakeyears2000_gum[p,3],col="red")
lines(CI_fakeyears2000_pe3[p,3],col="green")
lines(CI_fakeyears2000_ln3[p,3],col="blue")
lines(rep(CI_fakeyears2000_wei[2000,3],NROW(p)),col="orange")

p=seq(1,150,1)
plot(CI_fakeyears2000_wei[p,3],type="l",ylim=c(7500,15000),xlab = "Years",ylab="Discharge[m^3/s]",main="WEI 150 years")
lines(CI_fakeyears2000_wei[p,1],col="black")
lines(CI_fakeyears2000_wei[p,2],col="black")
polygon(c(p,rev(p)),c(CI_fakeyears2000_wei[p,1],rev(CI_fakeyears2000_wei[p,2])), col = "grey50")
lines(CI_fakeyears2000_wei[p,3],col="black")
lines(CI_fakeyears2000_wei[p,3],col="black")
lines(CI_fakeyears2000_gum[p,3],col="red")
lines(CI_fakeyears2000_pe3[p,3],col="green")
lines(CI_fakeyears2000_ln3[p,3],col="blue")
lines(rep(CI_fakeyears2000_wei[2000,3],NROW(p)),col="orange")

p=seq(1,200,1)
plot(CI_fakeyears2000_wei[p,3],type="l",ylim=c(7500,15000),xlab = "Years",ylab="Discharge[m^3/s]",main="WEI 200 years")
lines(CI_fakeyears2000_wei[p,1],col="black")
lines(CI_fakeyears2000_wei[p,2],col="black")
polygon(c(p,rev(p)),c(CI_fakeyears2000_wei[p,1],rev(CI_fakeyears2000_wei[p,2])), col = "grey50")
lines(CI_fakeyears2000_wei[p,3],col="black")
lines(CI_fakeyears2000_wei[p,3],col="black")
lines(CI_fakeyears2000_gum[p,3],col="red")
lines(CI_fakeyears2000_pe3[p,3],col="green")
lines(CI_fakeyears2000_ln3[p,3],col="blue")
lines(rep(CI_fakeyears2000_wei[2000,3],NROW(p)),col="orange")
dev.off()
### plot compare 1:2000 accumulate with shift over fakeyears 30yrs 50 yrs 80yrs 100yrs 150yrs 200yrs 300yrs
par(mfrow=c(3,3))
p=seq(1,2000,1)
plot(CI_fakeyears2000_wei[p,3],type="l",ylim=c(7500,10500),xlab = "Years",ylab="",main="WEI 30 years",print=F,yaxt="n")
axis(4,ylab="Discharge[m^3/s]")
lines(CI_fakeyears2000_wei[p,2],col="black")
lines(CI_fakeyears2000_wei[p,1],col="black")
lines(CI_fakeyears2000_wei[p,2],col="black")
polygon(c(p,rev(p)),c(CI_fakeyears2000_wei[p,1],rev(CI_fakeyears2000_wei[p,2])), col = "#7B92A8")
lines(CI_fakeyears2000_wei[p,3],col="black")
p=seq(1,1969,1)
lines(shift_fake_wei30[p,3],col="black")
bp_fake_30<-asTheEconomist(bwplot(~df_shift_fake$wei|ord))[1]
pushViewport(viewport(layout=grid.layout(1, 2, respect=TRUE)))
pushViewport(viewport(layout.pos.col=1), viewport(angle=90))
print(bp_fake_30,c(0.03,0.75,0.94,1),more=T,newpage=F)

p=seq(1,2000,1)
plot(CI_fakeyears2000_wei[p,3],type="l",ylim=c(7500,11000),xlab = "Years",ylab="",main="WEI 40 years",more=TRUE,yaxt="n",
     grid=T)
axis(4,ylab="Discharge[m^3/s]")
lines(CI_fakeyears2000_wei[p,1],col="black")
lines(CI_fakeyears2000_wei[p,2],col="black")
polygon(c(p,rev(p)),c(CI_fakeyears2000_wei[p,1],rev(CI_fakeyears2000_wei[p,2])), col = "#7B92A8")
lines(CI_fakeyears2000_wei[p,3],col="black")
p=seq(1,1959,1)
lines(shift_fake_wei40[p,3],col="black")
bp_fake_40<-asTheEconomist(bwplot(~df_shift_fake$wei|ord))[2]
pushViewport(viewport(layout=grid.layout(1, 2, respect=TRUE)))
pushViewport(viewport(layout.pos.col=1), viewport(angle=90))
print(bp_fake_40,c(0.1,0.75,0.8,1),more=T,newpage=F)


p=seq(1,2000,1)
plot(CI_fakeyears2000_wei[p,3],type="l",ylim=c(7500,11000),xlab = "Years",ylab="Discharge[m^3/s]",main="WEI 50 years")
lines(CI_fakeyears2000_wei[p,1],col="black")
lines(CI_fakeyears2000_wei[p,2],col="black")
polygon(c(p,rev(p)),c(CI_fakeyears2000_wei[p,1],rev(CI_fakeyears2000_wei[p,2])), col = "grey50")
lines(CI_fakeyears2000_wei[p,3],col="black")
p=seq(1,1949,1)
lines(shift_fake_wei50[p,3],col="black")

p=seq(1,2000,1)
plot(CI_fakeyears2000_wei[p,3],type="l",ylim=c(7500,11000),xlab = "Years",ylab="Discharge[m^3/s]",main="WEI 80 years")
lines(CI_fakeyears2000_wei[p,1],col="black")
lines(CI_fakeyears2000_wei[p,2],col="black")
polygon(c(p,rev(p)),c(CI_fakeyears2000_wei[p,1],rev(CI_fakeyears2000_wei[p,2])), col = "grey50")
lines(CI_fakeyears2000_wei[p,3],col="black")
p=seq(1,1919,1)
lines(shift_fake_wei80[p,3],col="black")

p=seq(1,2000,1)
plot(CI_fakeyears2000_wei[p,3],type="l",ylim=c(7500,11000),xlab = "Years",ylab="Discharge[m^3/s]",main="WEI 100 years")
lines(CI_fakeyears2000_wei[p,1],col="black")
lines(CI_fakeyears2000_wei[p,2],col="black")
polygon(c(p,rev(p)),c(CI_fakeyears2000_wei[p,1],rev(CI_fakeyears2000_wei[p,2])), col = "grey50")
lines(CI_fakeyears2000_wei[p,3],col="black")
p=seq(1,1899,1)
lines(shift_fake_wei100[p,3],col="black")

p=seq(1,2000,1)
plot(CI_fakeyears2000_wei[p,3],type="l",ylim=c(7500,11000),xlab = "Years",ylab="Discharge[m^3/s]",main="WEI 150 years")
lines(CI_fakeyears2000_wei[p,1],col="black")
lines(CI_fakeyears2000_wei[p,2],col="black")
polygon(c(p,rev(p)),c(CI_fakeyears2000_wei[p,1],rev(CI_fakeyears2000_wei[p,2])), col = "grey50")
lines(CI_fakeyears2000_wei[p,3],col="black")
p=seq(1,1849,1)
lines(shift_fake_wei150[p,3],col="black")

p=seq(1,2000,1)
plot(CI_fakeyears2000_wei[p,3],type="l",ylim=c(7500,11000),xlab = "Years",ylab="Discharge[m^3/s]",main="WEI 200 years")
lines(CI_fakeyears2000_wei[p,1],col="black")
lines(CI_fakeyears2000_wei[p,2],col="black")
polygon(c(p,rev(p)),c(CI_fakeyears2000_wei[p,1],rev(CI_fakeyears2000_wei[p,2])), col = "grey50")
lines(CI_fakeyears2000_wei[p,3],col="black")
p=seq(1,1799,1)
lines(shift_fake_wei200[p,3],col="black")

p=seq(1,2000,1)
plot(CI_fakeyears2000_wei[p,3],type="l",ylim=c(7500,11000),xlab = "Years",ylab="Discharge[m^3/s]",main="WEI 300 years")
lines(CI_fakeyears2000_wei[p,1],col="black")
lines(CI_fakeyears2000_wei[p,2],col="black")
polygon(c(p,rev(p)),c(CI_fakeyears2000_wei[p,1],rev(CI_fakeyears2000_wei[p,2])), col = "grey50")
lines(CI_fakeyears2000_wei[p,3],col="black")
p=seq(1,1699,1)
lines(shift_fake_wei300[p,3],col="black")

p=seq(1,2000,1)
plot(CI_fakeyears2000_wei[p,3],type="l",ylim=c(7500,11000),xlab = "Years",ylab="Discharge[m^3/s]",main="WEI 500 years")
lines(CI_fakeyears2000_wei[p,1],col="black")
lines(CI_fakeyears2000_wei[p,2],col="black")
polygon(c(p,rev(p)),c(CI_fakeyears2000_wei[p,1],rev(CI_fakeyears2000_wei[p,2])), col = "grey50")
lines(CI_fakeyears2000_wei[p,3],col="black")
p=seq(1,1499,1)
lines(shift_fake_wei500[p,3],col="black")


###################### Density Plot for different shift (30/40/50/80/100/150/200/300/500)
asTheEconomist(densityplot(~shift_fake_wei500[seq(1,1499,1),3]+shift_fake_wei300[seq(1,1699,1),3]+shift_fake_wei200[seq(1,1799,1),3]+
              shift_fake_wei150[seq(1,1849,1),3]+shift_fake_wei100[seq(1,1899,1),3]+shift_fake_wei80[seq(1,1919,1),3]+
              shift_fake_wei50[seq(1,1949,1),3]+shift_fake_wei40[seq(1,1959,1),3]+shift_fake_wei30[seq(1,1969,1),3],auto.key=TRUE,
              col=c("#3E647D", "#82C0E9", "#2D6D66", "#BFA19C","#D7D29E", "darkgreen", 
                    "#90353B", "#9C8847", "orange" ),
              key=list(lines=list(lwd=4,col=c("#3E647D", "#82C0E9", "#2D6D66", "#BFA19C","#D7D29E", "darkgreen", 
                                              "#90353B", "#9C8847", "orange" )),
                       space="right",text=list(c("Weibull 500 Years","Weibull 300 Years",
                                                 "Weibull 200 Years","Weibull 150 Years","Weibull 100 Years",
                                                 "Weibull 80 Years","Weibull 50 Years","Weibull 40 Years",
                                                 "Weibull 30 Years"),cex=1.3))),
              xlab=list("Discharge m^3/s"))

################### BOXPLOT for different shift (30/40/50/80/100/150/200/300/500)
df_shift_fake<-data.frame(rbind(shift_fake_wei500,shift_fake_wei300,shift_fake_wei200,shift_fake_wei150,shift_fake_wei100,
                     shift_fake_wei80,shift_fake_wei50,shift_fake_wei40,shift_fake_wei30),c(rep("500 years",1499),
                rep("300 years",1699),rep("200 years",1799),rep("150 years",1849),rep("100 years",1899),rep("80 years",1919),
                rep("50 years",1949),rep("40 years",1959),rep("30 years",1969)))
names(df_shift_fake)<-c("wei","gum","pe3","ln3","years")
ord<-as.factor(c(rep("500 years",1499),
  rep("300 years",1699),rep("200 years",1799),rep("150 years",1849),
  rep("100 years",1899),rep("80 years",1919),rep("50 years",1949),
  rep("40 years",1959),rep("30 years",1969)))
ord<-factor(ord,levels=c("30 years","40 years","50 years","80 years","100 years","150 years","200 years","300 years","500 years"))
#ord<-factor(ord,levels=c("30","40","50","80","100","150","200","300","500"))

pushViewport(viewport(layout=grid.layout(1, 2, respect=TRUE)))
pushViewport(viewport(layout.pos.col=1), viewport(angle=90))
print(asTheEconomist(bwplot(~df_shift_fake$wei|ord)),more=T,newpage=F)
asTheEconomist(bwplot(~df_shift_fake$wei|ord))
asTheEconomist(bwplot(~df_shift_fake$gum|ord))
asTheEconomist(bwplot(~df_shift_fake$pe3|ord))
asTheEconomist(bwplot(~df_shift_fake$ln3|ord))

#### bwplot for fakeyears under - no significant difference wei
bp_fake_30<-asTheEconomist(bwplot(~df_shift_fake$wei|ord))[1]
bp_fake_40<-asTheEconomist(bwplot(~df_shift_fake$wei|ord))[2]
bp_fake_50<-asTheEconomist(bwplot(~df_shift_fake$wei|ord))[3]
bp_fake_80<-asTheEconomist(bwplot(~df_shift_fake$wei|ord))[4]
bp_fake_100<-asTheEconomist(bwplot(~df_shift_fake$wei|ord))[5]
bp_fake_150<-asTheEconomist(bwplot(~df_shift_fake$wei|ord))[6]
bp_fake_200<-asTheEconomist(bwplot(~df_shift_fake$wei|ord))[7]
bp_fake_300<-asTheEconomist(bwplot(~df_shift_fake$wei|ord))[8]
bp_fake_500<-asTheEconomist(bwplot(~df_shift_fake$wei|ord))[9]
## ln3
bp_fake_30<-asTheEconomist(bwplot(~df_shift_fake$ln3|ord))[1]
bp_fake_40<-asTheEconomist(bwplot(~df_shift_fake$ln3|ord))[2]
bp_fake_50<-asTheEconomist(bwplot(~df_shift_fake$ln3|ord))[3]
bp_fake_80<-asTheEconomist(bwplot(~df_shift_fake$ln3|ord))[4]
bp_fake_100<-asTheEconomist(bwplot(~df_shift_fake$ln3|ord))[5]
bp_fake_150<-asTheEconomist(bwplot(~df_shift_fake$ln3|ord))[6]
bp_fake_200<-asTheEconomist(bwplot(~df_shift_fake$ln3|ord))[7]
bp_fake_300<-asTheEconomist(bwplot(~df_shift_fake$ln3|ord))[8]
bp_fake_500<-asTheEconomist(bwplot(~df_shift_fake$ln3|ord))[9]
## pe3
bp_fake_30<-asTheEconomist(bwplot(~df_shift_fake$pe3|ord))[1]
bp_fake_40<-asTheEconomist(bwplot(~df_shift_fake$pe3|ord))[2]
bp_fake_50<-asTheEconomist(bwplot(~df_shift_fake$pe3|ord))[3]
bp_fake_80<-asTheEconomist(bwplot(~df_shift_fake$pe3|ord))[4]
bp_fake_100<-asTheEconomist(bwplot(~df_shift_fake$pe3|ord))[5]
bp_fake_150<-asTheEconomist(bwplot(~df_shift_fake$pe3|ord))[6]
bp_fake_200<-asTheEconomist(bwplot(~df_shift_fake$pe3|ord))[7]
bp_fake_300<-asTheEconomist(bwplot(~df_shift_fake$pe3|ord))[8]
bp_fake_500<-asTheEconomist(bwplot(~df_shift_fake$pe3|ord))[9]
## gum
bp_fake_30<-asTheEconomist(bwplot(~df_shift_fake$gum|ord))[1]
bp_fake_40<-asTheEconomist(bwplot(~df_shift_fake$gum|ord))[2]
bp_fake_50<-asTheEconomist(bwplot(~df_shift_fake$gum|ord))[3]
bp_fake_80<-asTheEconomist(bwplot(~df_shift_fake$gum|ord))[4]
bp_fake_100<-asTheEconomist(bwplot(~df_shift_fake$gum|ord))[5]
bp_fake_150<-asTheEconomist(bwplot(~df_shift_fake$gum|ord))[6]
bp_fake_200<-asTheEconomist(bwplot(~df_shift_fake$gum|ord))[7]
bp_fake_300<-asTheEconomist(bwplot(~df_shift_fake$gum|ord))[8]
bp_fake_500<-asTheEconomist(bwplot(~df_shift_fake$gum|ord))[9]
dev.off()
# depending on which function one wants to plot, change above
print(bp_fake_30,c(0.0,0.8,1,1),more=T,newpage=F)
print(bp_fake_50,c(0.0,0.6,1,0.8),more=T,newpage=F)
print(bp_fake_100,c(0.0,0.4,1,0.6),more=T,newpage=F)
print(bp_fake_300,c(0.0,0.2,1,0.4),more=T,newpage=F)
print(bp_fake_500,c(0.0,0.0,1,0.2))
dev.off()
############## get f-value  ANOVA!
plotmeans(df_shift_fake$wei~df_shift_fake$years,digits=2, ccol="red", mean.labels=T, main="Plot of discharge means by different window shift")
boxplot(df_shift_fake$wei~df_shift_fake$years, main="Plot of discharge means by different window shift", xlab="Window Shift", ylab="Discharge", col=rainbow(9))
btwn<-aov(df_shift_fake$wei~df_shift_fake$years)
summary(btwn)
######################################## bwplot for y100 
# df with one river (Bratislava)
df_shift_y100_CI_3<-data.frame(rbind(CI_shift_y100_wei_30[[3]],CI_shift_y100_wei_40[[3]],
                                     CI_shift_y100_wei_50[[3]],
                                       CI_shift_y100_gum_30[[3]],CI_shift_y100_gum_40[[3]],
                                     CI_shift_y100_gum_50[[3]],CI_shift_y100_pe3_30[[3]],
                                     CI_shift_y100_pe3_40[[3]],CI_shift_y100_pe3_50[[3]],
                                     CI_shift_y100_ln3_30[[3]],CI_shift_y100_ln3_40[[3]],
                                     CI_shift_y100_ln3_50[[3]]),
                             rep(c(rep("30 years",70),rep("40 years",60),rep("50 years",50)),4),
                             c(rep("wei",180),rep("gum",180),rep("pe3",180),rep("ln3",180)))
#names(df_shift_y100_CI_3)<-c("down_wei","up_wei","mean_wei","down_gum","up_gum","mean_gum",
#                           "down_pe3","up_pe3","mean_pe3",
#                           "down_ln3","up_ln3","mean_ln3","years")
names(df_shift_y100_CI_3)<-c("dw_value","up_value","value","years","func")
getGroups(df_shift_y100_CI_3|df_shift_y100_CI_3$func,data=df_shift_y100_CI_3)

bp_30<-asTheEconomist(bwplot(~df_shift_y100_CI_3$value[181:360]|df_shift_y100_CI_3$years,))[1]
bp_40<-asTheEconomist(bwplot(~df_shift_y100_CI_3$value[181:360]|df_shift_y100_CI_3$years))[2]
bp_50<-asTheEconomist(bwplot(~df_shift_y100_CI_3$value[181:360]|df_shift_y100_CI_3$years))[3]
print(bp_30,c(0.0,0.66,1,1),more=T,newpage=F)
print(bp_40,c(0.0,0.33,1,0.66),more=T,newpage=F)
print(bp_50,c(0.0,0,1,0.33),more=T,newpage=F)
df_shift_y100_CI_3$value
bp_30<-asTheEconomist(bwplot(~df_shift_y100_CI_3$value[c(1:180)]|df_shift_y100_CI_3$years))[1]
bp_40<-asTheEconomist(bwplot(~df_shift_y100_CI_3$value[c(1:180)]|df_shift_y100_CI_3$years))[2]
bp_50<-asTheEconomist(bwplot(~df_shift_y100_CI_3$value[c(1:180)]|df_shift_y100_CI_3$years))[3]
print(bp_30,c(0.0,0.66,1,1),more=T,newpage=F)
print(bp_40,c(0.0,0.33,1,0.66),more=T,newpage=F)
print(bp_50,c(0.0,0,1,0.33),more=T,newpage=F)
bp_30<-asTheEconomist(bwplot(~df_shift_y100_CI_3$value[c(361:540)]|df_shift_y100_CI_3$years))[1]
bp_40<-asTheEconomist(bwplot(~df_shift_y100_CI_3$value[c(361:540)]|df_shift_y100_CI_3$years))[2]
bp_50<-asTheEconomist(bwplot(~df_shift_y100_CI_3$value[c(361:540)]|df_shift_y100_CI_3$years))[3]
print(bp_30,c(0.0,0.66,1,1),more=T,newpage=F)
print(bp_40,c(0.0,0.33,1,0.66),more=T,newpage=F)
print(bp_50,c(0.0,0,1,0.33),more=T,newpage=F)
bp_30<-asTheEconomist(bwplot(~df_shift_y100_CI_3$value[c(541:720)]|df_shift_y100_CI_3$years))[1]
bp_40<-asTheEconomist(bwplot(~df_shift_y100_CI_3$value[c(541:720)]|df_shift_y100_CI_3$years))[2]
bp_50<-asTheEconomist(bwplot(~df_shift_y100_CI_3$value[c(541:720)]|df_shift_y100_CI_3$years))[3]
print(bp_30,c(0.0,0.66,1,1),more=T,newpage=F)
print(bp_40,c(0.0,0.33,1,0.66),more=T,newpage=F)
print(bp_50,c(0.0,0,1,0.33),more=T,newpage=F)
dev.off()


#################### Colors
show_col(economist_pal()(6))
show_col(economist_pal(stata=TRUE)(16))
