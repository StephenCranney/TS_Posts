# NOTE: To load data, you must download both the extract's data and the DDI
# and also set the working directory to the folder with these files (or change the path below).

#1850-1930

if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")

ddi <- read_ipums_ddi("/Users/stephencranney/Desktop/usa_00021.xml")
data <- read_ipums_micro(ddi)

table(data$LIT)
data <- data[data$LIT != 9, ]
data <- data[data$LIT != 0, ]

attributes(data$SEX)

data$SEX2 <- factor(data$SEX, levels = c(1, 2), labels = c("Male", "Female"))
table(data$SEX, data$SEX2)

data$STATEFIP2 <- ifelse(data$STATEFIP == 48, "Texas",
                        ifelse(data$STATEFIP == 49, "Utah",
                               ifelse(data$STATEFIP == 50, "Vermont", data$STATEFIP)))

table(data$STATEFIP, data$STATEFIP2)


#1850-1860: Persons age 20+.
#1870-1910: Persons age 10+. Not available in the 1880 100% database.
#1910 Puerto Rico: Persons age 5+.
#1920-1940: Persons age 10+.

data$illiterate<-ifelse(data$LIT==1, 1, 0)

mean_by_group <- aggregate(data$illiterate ~ data$YEAR + data$STATEFIP2 + data$SEX2, data, mean)

#Checked
XDF<-subset(data, YEAR==1850 & STATEFIP==48 & SEX==1)
mean(XDF$illiterate)
table(XDF$YEAR)

library(ggplot2)

# Assuming mean_by_group contains the result of your aggregate function
ggplot(mean_by_group, aes(x = `data$YEAR`, y = `data$illiterate`, color = interaction(`data$SEX2`, `data$STATEFIP2`))) +
  geom_line() +
  labs(x = "Year", y = "Illiterate Proportion", color = "Sex & State") +
  theme_minimal() +
  ggtitle("Trends in Illiterate Proportion by Sex, State, and Year")

mean_by_group$`data$YEAR`

#Check graph against data. Checked.
#Would have taken my machine all day to crunch the numbers. 
#All of the censuses



