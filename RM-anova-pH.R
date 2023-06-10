#Set numbers to show in standard format instead of scientific notation. 
#(Reactivate scientific notation with options(scipen=0))
options(scipen = 999)

# Load libraries ----

#library(psych)
#library(MASS)
library(tidyverse)
library(gridExtra)
library(rstatix)
library(car)
library(MANOVA.RM)

# Read in csv file created at the end of the data-wrangling script ----
tidyData <- read.csv("chemistry-data-tidy.csv")

#Look at the data
head(tidyData)
str(tidyData)

# Rename columns so they're easier to write. ----
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

# Convert to factors ----
tidyData$Weeks <- as.factor(tidyData$Weeks)

tidyData$Treatment <- as.factor(tidyData$Treatment)

str(tidyData)

# RM ANOVA for pH ----
## Extract pH data ----
pH_data <- tidyData %>%
  select(Sample.ID, pH, Treatment, Weeks)


head(pH_data,20)
str(pH_data)

## Testing for normality ----
### Plotting ----
ggplot(data = pH_data, aes(sample=pH)) +
  geom_qq() + 
  geom_qq_line(color="red") + 
  xlab("Theoretical") +
  ylab("Sample") +
  facet_grid(Treatment ~ Weeks)

ggplot(data = pH_data, aes(pH)) +
  geom_histogram() +
  facet_grid(Treatment ~ Weeks)
### Shapiro-Wilks ----
normality<-pH_data %>%
  group_by(Treatment,Weeks) %>%
  shapiro_test(pH)
data.frame(normality)

## Fitting anova model with rstatix package ----
pH_mod <- anova_test(data = pH_data, dv = pH, wid = Sample.ID, within = c(Treatment, Weeks))
get_anova_table(pH_mod) #the get_anova_table function (rstatix) automatically applies "Greenhouse-Geisser sphericity 
#correction" on any factors that violate this assumption

### Post-hoc tests ----
pH.one.way <- pH_data %>%
  group_by(Treatment) %>%
  anova_test(dv = pH, wid = Sample.ID, within = Weeks) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
pH.one.way

pH.one.way2 <- pH_data %>%
  group_by(Weeks) %>%
  anova_test(dv = pH, wid = Sample.ID, within = Treatment) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
pH.one.way2

### Pairwise comparisons between treatment groups ----
pwc <- pH_data %>%
  group_by(Weeks) %>%
  pairwise_t_test(
    pH ~ Treatment, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc

### Pairwise comparisons between time points ----
pwc2 <- pH_data %>%
  group_by(Treatment) %>%
  pairwise_t_test(
    pH ~ Weeks, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc2


## Fitting the anova model using aov ----
model.aov <- aov(pH ~ Treatment*Weeks + Error(Sample.ID/(Treatment*Weeks)), data = pH_data)
summary(model.aov)

#Can also define error like this, doesn't make much difference to the results
model.aov <- aov(pH ~ Treatment*Weeks + Error(Sample.ID/(Treatment+Weeks)), data = pH_data)
summary(model.aov)  
