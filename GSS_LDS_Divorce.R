library(foreign)
gss <- read.dta("/Users/stephencranney/Desktop/GSS7218_R3.DTA")

#only take last 15 years. 
gss <- subset(gss, year > 2003)

#Subset LDS
gss$lds<-ifelse(gss$other=="mormon", 1, 0)

#Generate divorced indicator 
gss$DIVORCE<-ifelse(gss$divorce=="yes" | gss$marital=="divorced" | gss$marital=="separated", 1, 0)

#Remove the never marrieds 
gss<-subset(gss, marital!='never married')

#Summary stats
aggregate(gss$DIVORCE, list(gss$lds), FUN=mean)

#Basic t-test and regressions
t.test(gss$lds,gss$DIVORCE)
summary(glm(DIVORCE~ lds+ age, data=gss))
summary(glm(DIVORCE~ lds+ age + year, data=gss))
summary(glm(DIVORCE~ lds, data=gss))

