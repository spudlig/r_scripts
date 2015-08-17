install.packages("magrittr")
install.packages("dplyr")
install.packages("ggplot2")
library(fBasics)
library(dplyr)
library(magrittr)
library(ggplot2)

iris %>% 
  filter(Species %>% substr(1,1) %>% equals("v")) %>%
  group_by(Sepal.Length, Species) %>%
  summarize(total = sum(Species)) %>%
  qplot(Sepal.Length, total, color=Species, data= ., geom="line") %>%
  add(ggtitle('HA!')) %>%
  print
iris %>% 
  filter(Species %>% substr(1,2) %>% equals("ve")) %>%
  group_by(Petal.Length, Sepal.Length) %>%
  print
 iris %$% 
  1

a<-matrix(c(1:100),10,10,byrow=T)
a[1,c(3:4)]<-NA
mean(a[1,])
a
