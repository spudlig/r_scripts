install.packages("ismev")
install.packages("lmom")
install.packages("lmomRFA")
library(ismev)
library(lmom)
library(lmomRFA)
ll<-formList

### Gumbel
almom<-samlmu(a$data)
blmom<-samlmu(b$data)
blmom
alm<-pelwei(almom,bound=NULL)
blm<-pelgum(b$data)
apar<-cdfwei(a$data,para=(alm))
bpar<-cdfgum(b$data,par=(blm))
evplot(apar,pch=2,type="l",rp.axis=T)
evplot(bpar,pch=2,type="l")

summary((bpar>0.99999998))


# Sample L-moments
cdfAll<-list(NULL)
cdfFun<-function(df){
  lmom1<-samlmu(as.numeric(df[[4]]))
  tglm<-pelgum(lmom1)
  tplm<-pelpe3(lmom1)
  twlm<-pelwei(lmom1)
  tllm<-pelln3(lmom1)
  cg<-data.frame(cdfgum(as.numeric(df[[4]]),para=(tglm)))
  cp<-data.frame(cdfpe3(as.numeric(df[[4]]),para=(tplm)))
  cw<-data.frame(cdfwei(as.numeric(df[[4]]),para=(twlm)))
  cl<-data.frame(cdfln3(as.numeric(df[[4]]),para=(tllm)))
  return(list(cg,cp,cw,cl))
}
cdfAll<-(lapply(llform[c(1:9)],cdfFun))
cdfa<-as.data.frame(cdfAll)
summary(cdfAll$a6335060[[1]])

evplot(cdfAll$a6335060[[1]])

