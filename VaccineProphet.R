library(foreign)
library(dplyr)
library(ggplot2)
library(lubridate)
library(dplyr)

#Data source
#https://data.cdc.gov/Vaccinations/COVID-19-Vaccinations-in-the-United-States-Jurisdi/unsk-b7fc

df<-read.csv("/Users/stephencranney/Desktop/COVID-19_Vaccinations_in_the_United_States_Jurisdiction 10.19.25 PM.csv")

#Subset just for non-LDS-by-Utah cases
df<-subset(df, (Location=="UT") | (Location=="CO") | (Location=="KS") | (Location=="MT") | (Location=="NM"))

#Two dates of interest: 

#8/12/2021: First Presidency announces support for vaccines explicitly. August 12th. 
#https://newsroom.churchofjesuschrist.org/article/first-presidency-message-covid-19-august-2021

#January 19, 2021. President Nelson vaccinated. 
#https://www.facebook.com/russell.m.nelson/posts/3650727088328034

# Convert the "Date" column to Date format
df$Date <- as.Date(df$Date, format = "%m/%d/%Y")

#Checked
#dfK <- df %>%
#  select(Date, Date2)

# Create the new variable based on the condition
#Checked
df <- df %>%
  mutate(after_FPAnnounce = ifelse(Date >= as.Date("2021-08-12"), 1, 0))

# Create the new variable based on the condition
#Checked
df <- df %>%
  mutate(after_NelsonVaccination = ifelse(Date >= as.Date("2021-01-19"), 1, 0))

df <- df %>% select(Date, Location, Administered, after_NelsonVaccination, after_FPAnnounce)

df <- df %>%
  mutate(SpecialHighlight = case_when(
    Location == "UT" & Date >= "2021-08-12" ~ "UT after First Presidency Announcement",
    Location == "UT" & Date >= "2021-01-19" ~ "UT after President Nelson Vaccinated",
    TRUE ~ as.character(Location)
  ))


# Open a JPEG device
jpeg(filename = "/Users/stephencranney/Desktop/my_plot.jpeg", width = 800, height = 600, quality = 3000)

# Create the plot
p <- ggplot(df, aes(x = Date, y = Administered)) +
  geom_line(aes(group = Location, color = SpecialHighlight), size = 1) +
  scale_color_manual(name = "Church Announcements and Utah",
                     values = c("UT after First Presidency Announcement" = "red",
                                "UT after President Nelson Vaccinated" = "orange",
                                "UT" = "black", "Other" = "grey")) +
  scale_y_continuous(labels = scales::comma) +  # Add this line to avoid scientific notation + 
  labs(title = "Administered Over Time by Location",
       x = "Date",
       y = "Vaccines Administered") +
  theme(plot.title = element_text(hjust = 0.5))  # Center the title

print(p)

# Close the JPEG device
dev.off()

#Checked the last one in graph, rank ordered: 
#Colorado
#Utah
#Kansas
#NM
#Montana

2349395
for Kansas for 06/06/2021
2349395

2021-06-06
NM
1903485
1903485

#As a DID reminder: https://www.princeton.edu/~otorres/DID101R.pdf

df$Treated<-ifelse(df$Location=="UT", 1, 0)
#Checked
table(df$Location, df$Treated)

df$did_after_FPAnnounce = df$after_FPAnnounce * df$Treated
df$did_after_NelsonVaccination = df$after_NelsonVaccination * df$Treated

#Checked to make sure Administered is the right var, check sums with others. 

didreg = lm(df$Administered ~ df$Treated + df$after_FPAnnounce + df$did_after_FPAnnounce)
summary(didreg)

#Only 50%, the signal is lost, but it says the signal is not strong enough. 

#Try with different variation, show p-values. 



#If we sort the ending spot the top one is CO, next is KS, next is NM, next is MO.

#DID plus aggregate, in theory it should work but its traditinoally done using rates. 




# Define target locations
target_locations <- c("KS", "CO", "MT", "NM")

# Iterate over each target location
for (location in target_locations) {
  cat("\nRunning analysis for location:", location, "\n")
  
  # Filter for only UT and the current target location
  df_filtered <- df %>% 
    filter(Location %in% c("UT", location))
  
  ######Looking at 1 month before and after President Nelson vaccination
  cat("1 month before and after President Nelson vaccination:\n")
  
  target_date <- as.Date("2021-01-19")
  one_month <- 30  # 30 days 
  
  df_filtered2 <- df_filtered %>% 
    filter(between(Date, target_date - days(one_month), target_date + days(one_month)))
  
  df_filtered2$did_after_NelsonVaccination = df_filtered2$after_NelsonVaccination * df_filtered2$Treated
  didreg = lm(df_filtered2$Administered ~ df_filtered2$Treated + df_filtered2$after_NelsonVaccination + df_filtered2$did_after_NelsonVaccination)
  print(summary(didreg))
  
  ######Looking at 1 month before and after FP announcement
  cat("1 month before and after FP announcement:\n")
  
  target_date <- as.Date("2021-08-12")
  one_month <- 30  # 30 days 
  
  df_filtered2 <- df_filtered %>% 
    filter(between(Date, target_date - days(one_month), target_date + days(one_month)))
  
  df_filtered2$did_after_FPAnnounce = df_filtered2$after_FPAnnounce * df_filtered2$Treated
  didreg = lm(df_filtered2$Administered ~ df_filtered2$Treated + df_filtered2$after_FPAnnounce + df_filtered2$did_after_FPAnnounce)
  print(summary(didreg))
  
  ######Looking at 2 months before and after FP announcement
  cat("2 months before and after FP announcement:\n")
  
  target_date <- as.Date("2021-08-12")
  one_month <- 60  # 60 days 
  
  df_filtered2 <- df_filtered %>% 
    filter(between(Date, target_date - days(one_month), target_date + days(one_month)))
  
  df_filtered2$did_after_FPAnnounce = df_filtered2$after_FPAnnounce * df_filtered2$Treated
  didreg = lm(df_filtered2$Administered ~ df_filtered2$Treated + df_filtered2$after_FPAnnounce + df_filtered2$did_after_FPAnnounce)
  print(summary(didreg))
}


#2020-12-14 is the start date, 
#2021-01-19 is Nelson vaccination. One month before and after. 

#Different time slices. as well, right before and after for years. 
#Figure out that weird spike
#2022-11-02
#UT
#8347450, it's real, data quality issue. Hand checked. Sum total. 

#Dilluted the LDS effect, not high enoguh power to get that. 
#Visually a discernible bump. Not huge enough to not necessarily be noise. 

