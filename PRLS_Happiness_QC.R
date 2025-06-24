# 1. SETUP: LOAD LIBRARIES
library(haven)
library(dplyr)
library(Hmisc)
library(survey)
library(pollster)
library(ggplot2)
library(scales)


# 2. DATA LOADING AND PREPARATION
# Load the data from .sav file
file_path <- "~/Desktop/2023-24 RLS Public Use File Feb 19.sav"
prls <- read_sav(file_path)

# Replace 99 (coded for missing/skipped) with NA in key variables
prls <- prls %>%
  mutate(across(c(CURREL, FRMREL),
                ~ ifelse(. == 900000, NA, .)))


prls <- prls %>%
  mutate(across(c(HAPPY, SATIS_A, SATIS_B, REGION, EDUCREC, INC_SDT1,
                  FERTREC, MARITAL, GENDER, BIRTHDECADE, GOD, PRAY, ATTNDPERRLS),
                ~ ifelse(. == 99, NA, .)))

# 3. VARIABLE CREATION
# Create binary outcome variables (1 for highest satisfaction, 0 otherwise)
prls$veryhappy <- ifelse(prls$HAPPY == 1, 1, 0)
prls$excellenthealth <- ifelse(prls$SATIS_A == 1, 1, 0)
prls$excellentfamilylife <- ifelse(prls$SATIS_B == 1, 1, 0)

# Create binary variables for religious status
prls$LDS <- ifelse(prls$CURREL == 20000, 1, 0)
prls$convertLDS <- ifelse(prls$CURREL == 20000 & prls$FRMREL != 20000, 1, 0)
prls$lifelongLDS <- ifelse(prls$CURREL == 20000 & prls$FRMREL == 20000, 1, 0)
prls$exLDS <- ifelse(prls$CURREL != 20000 & prls$FRMREL == 20000, 1, 0)

# Create a single, combined 'MormonStatus' factor and a labeled 'GENDER' factor
prls <- prls %>%
  mutate(
    MormonStatus = case_when(
      lifelongLDS == 1 ~ "Lifelong LDS",
      convertLDS == 1 ~ "Convert to LDS",
      exLDS == 1 ~ "Ex-LDS",
      CURREL != 20000 & FRMREL != 20000 ~ "Never LDS",
      TRUE ~ "Other" # Catch any other cases
    ),
    # Convert GENDER to a factor with descriptive labels
    # 1=Male, 2=Female
    GENDER = factor(GENDER, levels = c(1, 2), labels = c("Male", "Female"))
  )

X<-subset(prls, MormonStatus=="Other")
table(X$FRMREL, X$CURREL, useNA = "ifany")

prls <- subset(prls, MormonStatus != "Other")

table(prls$MormonStatus, prls$lifelongLDS)
table(prls$MormonStatus, prls$convertLDS)
table(prls$MormonStatus, prls$exLDS)
table(prls$MormonStatus)


#QCed up to this point


# --- The following sections will now create, display, and save each plot ---

table(prls$MormonStatus)

#Checked each one: 
X<-subset(prls, MormonStatus=="Convert to LDS" & GENDER=="Female")
wtd.mean(X$excellenthealth, weights=X$WEIGHT, na.rm=TRUE)
wtd.mean(X$veryhappy, weights=X$WEIGHT, na.rm=TRUE)
wtd.mean(X$excellentfamilylife, weights=X$WEIGHT, na.rm=TRUE)

# 4. PLOT 1: PERCENT 'VERY HAPPY'
# Calculate summary statistics
happy_summary <- prls %>%
  filter(!is.na(MormonStatus) & !is.na(GENDER) & !is.na(veryhappy) & !is.na(WEIGHT)) %>%
  group_by(MormonStatus, GENDER) %>%
  summarise(
    PercentVeryHappy = weighted.mean(veryhappy, w = WEIGHT, na.rm = TRUE)
  ) %>%
  ungroup()

# Create the plot and assign it to a variable
happy_plot <- ggplot(happy_summary, aes(x = MormonStatus, y = PercentVeryHappy, fill = GENDER)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "% Reporting 'Very Happy' by Gender and Religious Background (Weighted)",
    subtitle = "Pew Religious Landscape Survey, 2023-2024",
    x = "Religious Background",
    y = "Percent 'Very Happy'",
    fill = "Gender"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the plot in R session
print(happy_plot)

# Save the plot 
ggsave(
  filename = "~/Desktop/very_happy_by_religion.png",
  plot = happy_plot,
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)


# 5. PLOT 2: PERCENT 'EXTREMELY HEALTHY'
# Calculate summary statistics
healthy_summary <- prls %>%
  filter(!is.na(MormonStatus) & !is.na(GENDER) & !is.na(excellenthealth) & !is.na(WEIGHT)) %>%
  group_by(MormonStatus, GENDER) %>%
  summarise(
    PercentExcellentHealth = weighted.mean(excellenthealth, w = WEIGHT, na.rm = TRUE)
  ) %>%
  ungroup()

# Create the plot and assign it to a variable
healthy_plot <- ggplot(healthy_summary, aes(x = MormonStatus, y = PercentExcellentHealth, fill = GENDER)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "% Reporting 'Excellent Health' by Gender and Religious Background (Weighted)",
    subtitle = "Pew Religious Landscape Survey, 2023-2024",
    x = "Religious Background",
    y = "Percent 'Excellent Health'",
    fill = "Gender"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the plot in R session
print(healthy_plot)

# Save the plot to Desktop
ggsave(
  filename = "~/Desktop/excellent_health_by_religion.png",
  plot = healthy_plot,
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)


# 6. PLOT 3: PERCENT 'EXCELLENT FAMILY LIFE'
# Calculate summary statistics
family_summary <- prls %>%
  filter(!is.na(MormonStatus) & !is.na(GENDER) & !is.na(excellentfamilylife) & !is.na(WEIGHT)) %>%
  group_by(MormonStatus, GENDER) %>%
  summarise(
    PercentExcellentFamily = weighted.mean(excellentfamilylife, w = WEIGHT, na.rm = TRUE)
  ) %>%
  ungroup()

# Create the plot and assign it to a variable
family_plot <- ggplot(family_summary, aes(x = MormonStatus, y = PercentExcellentFamily, fill = GENDER)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "% Reporting 'Excellent Family Life' by Gender and Religious Background (Weighted)",
    subtitle = "Pew Religious Landscape Survey, 2023-2024",
    x = "Religious Background",
    y = "Percent 'Excellent Family Life'",
    fill = "Gender"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the plot in R session
print(family_plot)

# Save the plot to Desktop
ggsave(
  filename = "~/Desktop/excellent_family_life_by_religion.png",
  plot = family_plot,
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)


#Regression

attributes(prls$SATIS_A)

prls$MormonStatus <- as.factor(prls$MormonStatus)
prls$MormonStatus <- relevel(prls$MormonStatus, ref = "Lifelong LDS")

model1<-lm(prls$HAPPY~prls$MormonStatus, weights=prls$WEIGHT)
model2<-lm(prls$SATIS_A~prls$MormonStatus, weights=prls$WEIGHT) #Health
model3<-lm(prls$SATIS_B~prls$MormonStatus, weights=prls$WEIGHT) #Family Life

summary(model1)
summary(model2)
summary(model3)


