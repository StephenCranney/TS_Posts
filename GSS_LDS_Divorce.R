library(foreign)
gss <- read.dta("/Users/stephencranney/Desktop/GSS7218_R3.DTA")
library(Hmisc)
library(pollster)

#only take last 15 years. 
gss <- subset(gss, year > 2003)

#Subset LDS
gss$lds<-ifelse(gss$other=="mormon", 1, 0)
gss$lds<-ifelse(is.na(gss$lds), 0, gss$lds)

#Generate divorced indicator 
gss$DIVORCE<-ifelse(gss$divorce=="yes" | gss$marital=="divorced" | gss$marital=="separated", 1, 0)

#Remove the never marrieds 
gss<-subset(gss, marital!='never married')
gss_lds<-subset(gss, other=='mormon')


#Summary stats
aggregate(gss$DIVORCE, list(gss$lds), FUN=mean)
wtd.mean(gss$DIVORCE, gss$wtssall)
wtd.mean(gss_lds$DIVORCE, gss_lds$wtssall)
crosstab(df = gss, x = lds  , y = DIVORCE , weight = wtssall)

#Basic t-test and regressions
t.test(gss$lds,gss$DIVORCE)
summary(glm(DIVORCE~ lds+ age, data=gss, weights=wtssall))
summary(glm(DIVORCE~ lds+ age + year, data=gss, weights=wtssall))
summary(glm(DIVORCE~ lds, data=gss, weights=wtssall))

