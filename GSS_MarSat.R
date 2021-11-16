library(foreign)
gss <- read.dta("/Users/stephencranney/Desktop/gss7221_r1a.dta")
library(Hmisc)
library(pollster)
library(weights)

table(gss$hapmar, gss$year)

#only take last 15 years. 
#2003 or 2009 same 

gss <- subset(gss, year > 2009)

#HapMar
gss$hapmar_n<-ifelse(gss$hapmar=="very happy", 3, 
                     ifelse(gss$hapmar=="pretty happy", 2,
                            ifelse(gss$hapmar=="not too happy", 1, NA)))

#Subset LDS
gss$lds<-ifelse(gss$other==64, 1, 0)
gss$lds<-ifelse(is.na(gss$lds), 0, gss$lds)

#subset lds only
gss_lds<-subset(gss, lds==1)
gss_notlds<-subset(gss, lds==0)

#Summary stats
mean(gss_notlds$hapmar_n, na.rm=TRUE)
mean(gss_lds$hapmar_n, na.rm=TRUE)

wtd.mean(gss_notlds$hapmar_n, gss_notlds$wtssall, na.rm=TRUE)
wtd.mean(gss_lds$hapmar_n, gss_lds$wtssall, na.rm=TRUE)

wtd.t.test(gss$lds,gss$hapmar_n, weight = gss$wtssall)
t.test(gss$lds,gss$hapmar_n)

#Taking things all together, how would you describe your marriage? 
#Would you say that your marriage is very happy, pretty happy, or not too happy?

table(gss$lds, gss$hapmar)
114+44+1






