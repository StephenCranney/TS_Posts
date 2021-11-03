library(foreign)
gss <- read.dta("/Users/stephencranney/Desktop/GSS_stata/gss7221_r1.dta")
library(Hmisc)
library(pollster)
getwd()

#Subset LDS

gss$lds<-ifelse(gss$other=="mormon", 1, 0)
gss$lds<-ifelse(is.na(gss$lds), 0, gss$lds)

PercLDS_2021<-crosstab(df = gss, x =year  , y =lds , weight = wtssps)
write.csv(PercLDS_2021,"PercLDS_2021.csv")

PercLDS<-crosstab(df = gss, x =year  , y =lds , weight = wtssall)
write.csv(PercLDS,"PercLDS.csv")

