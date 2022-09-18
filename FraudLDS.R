library(haven)

census <- read_dta('Desktop/U.S. Religion Census Religious Congregations and Membership Study, 2010 (County File).DTA')
load(file='Desktop/ICPSR_33523/DS0001/33523-0001-Data.rda')

#Merge
#census, FIPS, STCODE, CNTYCODE

table(census$fips)
table(census$stcode)
table(census$cntycode)

da33523.0001$stcode<-da33523.0001$FIPS_ST
da33523.0001$cntycode<-da33523.0001$FIPS_CTY

table(da33523.0001$FIPS_ST)
table(da33523.0001$FIPS_CTY)

censuscrime <- merge(da33523.0001,census,by=c("stcode","cntycode"))

#Create a fraud per thousand measure
table(censuscrime$CPOPARST)

myvars <- c("CPOPARST", "stcode", "cntycode", "ldsrate", "FRAUD")
censuscrime <- censuscrime[myvars]

#Hand checked numbers for several counties against Wikipedia, numbers are consistent. 

#Create FRAUD per thousand measure
censuscrime$fraudperk<-(censuscrime$FRAUD/censuscrime$CPOPARST)*1000

#Negative
cor.test(censuscrime$fraudperk, censuscrime$ldsrate)

model1<-lm(censuscrime$fraudperk~ censuscrime$ldsrate)
summary(model1)

plot(censuscrime$fraudperk, censuscrime$ldsrate, main="Church of Jesus Christ of Latter-day Saints and Fraud, County-Level", 
     xlab="Fraud per K", ylab="Latter-day Saints per k", pch=19, cex=.5)








