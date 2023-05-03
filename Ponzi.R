library("readxl")
library(openxlsx)
library(maps)
library(ggplot2)
library(datasets)
library(dplyr) # For arrange() function
#install.packages("viridis")
library(viridis)

df <- read_excel("/Users/stephencranney/Desktop/Ponzi.xlsx")

U<-subset(df, State=="UT")
table(U$Year2)

df2<-as.data.frame(table(df$State))

#Export, clean up just states, , add State populations, double check, re-import.
write.xlsx(df2, "/Users/stephencranney/Desktop/Ponzi_State.xlsx")
write_excel

#2021 populations five-year ACS estimates. DP05ACS DEMOGRAPHIC AND HOUSING ESTIMATES

df3 <- read_excel("/Users/stephencranney/Desktop/Ponzi_State_Clean.xlsx")

df3$PeoplePerPonzi<-df3$TotalPop/df3$Ponzi

df3$State2 <- state.name[match(df3$State, state.abb)]
df3$State2 <- tolower(df3$State2)

us_map <- map_data("state")

#https://r-graphics.org/recipe-miscgraph-choropleth
P_map <- merge(us_map, df3, by.x = "region", by.y = "State2")
P_map <- arrange(P_map, group, order)

#Set Arkansas to missing to show variation
P_map$PeoplePerPonzi[which(P_map$region == "arkansas")] <- NA
P_map$PeoplePerPonzi[which(P_map$region == "new mexico")] <- NA
P_map$PeoplePerPonzi[which(P_map$region == "oklahoma")] <- NA
P_map$PeoplePerPonzi[which(P_map$region == "kansas")] <- NA


ggplot(P_map, aes(x = long, y = lat, group = group, fill = PeoplePerPonzi)) +
  geom_polygon(colour = "black") +
  coord_map("polyconic") +
  scale_fill_viridis(name = "People Per Ponzi Schemes") +
  ggtitle("People Per Ponzi Scheme") +
  labs(caption = "Ponzi data from PonziTracker.com; Population derived from 2021 ACS 5-Year Estimates") +
  theme(plot.caption = element_text(size = 8))