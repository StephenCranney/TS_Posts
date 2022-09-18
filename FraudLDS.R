library(haven)

census <- read_dta('Desktop/U.S. Religion Census Religious Congregations and Membership Study, 2010 (County File).DTA')
load(file='Desktop/ICPSR_37059/DS0001/37059-0001-Data.rda')

#Merge
#census, FIPS, STCODE, CNTYCODE

table(census$fips)
table(census$stcode)
table(census$cntycode)

da37059.0001$stcode<-da37059.0001$FIPS_ST
da37059.0001$cntycode<-da37059.0001$FIPS_CTY

table(da37059.0001$FIPS_ST)
table(da37059.0001$FIPS_CTY)

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








