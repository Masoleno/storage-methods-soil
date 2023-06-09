
# Load libraries

#library(psych)
#library(MASS)
library(tidyverse)
library(gridExtra)
library(rstatix)
library(car)
library(MANOVA.RM)

# Read in csv file created at the end of the data-wrangling script
tidyData <- read.csv("chemistry-data-tidy.csv")

#Look at the data
head(tidyData)
str(tidyData)

# Rename columns so they're easier to write.
tidyData<- tidyData %>% 
  rename(
    "NH4 (mg/kg)" = NH4..mg.kg.,
    "TON (mg/kg)" = TON..mg.kg.,
    "K (mg/kg)" = K..mg.kg.,
    "NO2 (mg/kg)" = NO2..mg.kg.,
    "NO3 (mg/kg)" = NO3..mg.kg.,
    "Conductivity (mV)" = Conductivity..mV.,
    "Moisture (%)" = Moisture....
  )

head(tidyData)

# Convert to factors so that ANOVA and Tukey's work
tidyData$Weeks <- as.factor(tidyData$Weeks)

tidyData$Treatment <- as.factor(tidyData$Treatment)

str(tidyData)


# Extract pH data 
pH_data <- tidyData %>%
  select(Sample.ID, pH, Treatment, Weeks)


head(pH_data,20)
str(pH_data)


# Fitting the anova model
model.aov <- aov(pH ~ Treatment*Weeks + Error(Sample.ID/(Treatment*Weeks)), data = pH_data)
summary(model.aov)

#Can also define error like this, doesn't make much difference to the results
model.aov <- aov(pH ~ Treatment*Weeks + Error(Sample.ID/(Treatment+Weeks)), data = pH_data)
summary(model.aov)  
