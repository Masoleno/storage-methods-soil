# Script used for the data analysis for the paper 'Soil Sample Storage Conditions Affect Measurements of pH, and Nutrients' 
# by Maya Sollen-Norrlin and Naomi Rintoul-Hynes. 

# Set numbers to show in standard format instead of scientific notation. 
# (Reactivate scientific notation with options(scipen = 0))
options(scipen = 999)

# Load libraries ----
library(tidyverse)
library(gridExtra)
library(lme4) 
library(ggpubr)
library(rstatix) #used for normality testing using shapiro_test()
library(car)
library(emmeans)

# Read in and prepare data (csv file from the 'data-wrangling' script with dry-corrected nutrient concentrations) ----
tidyData <- read.csv("chemistry-data-tidy-dryCorrected.csv")

# Look at the data
head(tidyData)
str(tidyData)

# Rename columns so they're easier to refer to ----
tidyData<- tidyData %>% 
  rename(
    "NH4" = NH4..mg.kg.,
    "K" = K..mg.kg.,
    "NO2" = NO2..mg.kg.,
    "NO3" = NO3..mg.kg.,
    "Moisture" = Moisture....
  )

head(tidyData)

# Convert Treatment and Weeks to factors ----
tidyData$Weeks <- as.factor(tidyData$Weeks)

tidyData$Treatment <- as.factor(tidyData$Treatment)

str(tidyData)

# Make sure any negative values are set to 0
tidyData <- tidyData%>% 
  mutate(across(c(Moisture, NO3:NH4), ~ ifelse(.x<0, 0, .x)))

str(tidyData)
tail(tidyData, 20)

# Calculate summary data for reporting in manuscript ----
# Remove NA's so that means aren't influenced by missing values
no_NA_data <- tidyData %>%
  drop_na()

data_long <- no_NA_data %>%
  pivot_longer(cols = Moisture:K,
               names_to = "Compound",
               values_to = "Concentration (mg/kg)")

summary.data <- data_long %>%
  group_by(Treatment, Weeks, Compound) %>%
  summarise(
    meanConc = mean(`Concentration (mg/kg)`),
    sdConc = sd(`Concentration (mg/kg)`),
    medConc = median(`Concentration (mg/kg)`)
  )

# Save as csv so it can be easily refered to
write_csv(summary.data, "data-summaries.csv", col_names = TRUE, na = "")

# Testing for normality ----
### Plotting ----

# Plotting histograms and qqplots
#pH
gghistogram(tidyData, "pH", ggtheme = theme_bw(), bins = 10) +
  facet_grid(Weeks~Treatment, labeller = "label_both") +
  labs(title = "pH")

ggqqplot(tidyData, "pH", ggtheme = theme_bw()) +
  facet_grid(Weeks~Treatment, labeller = "label_both") +
  labs(title = "pH")

# NO3 
gghistogram(tidyData, "NO3", ggtheme = theme_bw(), bins = 10) +
  facet_grid(Weeks~Treatment, labeller = "label_both") +
  labs(title = "NO3")

ggqqplot(tidyData, "NO3", ggtheme = theme_bw()) +
  facet_grid(Weeks~Treatment, labeller = "label_both") +
  labs(title = "NO3")
# NO2
gghistogram(tidyData, "NO2", ggtheme = theme_bw(), bins = 10) +
  facet_grid(Weeks~Treatment, labeller = "label_both") +
  labs(title = "NO2")

ggqqplot(tidyData, "NO2", ggtheme = theme_bw()) +
  facet_grid(Weeks~Treatment, labeller = "label_both") +
  labs(title = "NO2")
# NH4
gghistogram(tidyData, "NH4", ggtheme = theme_bw(), bins = 10) +
  facet_grid(Weeks~Treatment, labeller = "label_both") +
  labs(title = "NH4")

ggqqplot(tidyData, "NH4", ggtheme = theme_bw()) +
facet_grid(Weeks~Treatment, labeller = "label_both") +
  labs(title = "NH4")
# K
gghistogram(tidyData, "K", ggtheme = theme_bw(), bins = 10) +
  facet_grid(Weeks~Treatment, labeller = "label_both") +
  labs(title = "K")

ggqqplot(tidyData, "K", ggtheme = theme_bw()) +
  facet_grid(Weeks~Treatment, labeller = "label_both") +
  labs(title = "K")

# Testing all variables for normality, grouped by Treatment and Weeks. 
# This uses shapiro_test() from the rstatix package.
### Shapiro-Wilks ----
normality <-tidyData%>%
  group_by(Treatment,Weeks) %>%
  shapiro_test(pH, NO3, NO2, NH4, K)
data.frame(normality)
normality %>%
  group_by(Treatment, Weeks, variable)
head(normality)

normality$p <- round(normality$p, 4)

# Removing extreme outliers (as defined by the 'identify_outlier' function in rstatix (values above Q3 + 3xIQR or below Q1 - 3xIQR )) ----
# Code adapted from https://www.geeksforgeeks.org/how-to-remove-outliers-from-multiple-columns-in-r-dataframe/
## Checking for outliers ----
pH_outlier <- tidyData %>%
  group_by(Treatment, Weeks) %>%
  identify_outliers(pH)
data.frame(pH_outlier)

NO3_outlier <- tidyData %>%
  group_by(Treatment, Weeks) %>%
  identify_outliers(NO3)
data.frame(NO3_outlier)

NO2_outlier <- tidyData %>%
  group_by(Treatment, Weeks) %>%
  identify_outliers(NO2)
data.frame(NO2_outlier)

NH4_outlier <- tidyData %>%
  group_by(Treatment, Weeks) %>%
  identify_outliers(NH4)
data.frame(NH4_outlier)

K_outlier <- tidyData %>%
  group_by(Treatment, Weeks) %>%
  identify_outliers(K)
data.frame(K_outlier)

## Removing outliers and re-testing normality ----
# Too many data points are removed from the week 24 groups for this to be a valid option, especially since some data still
# violates the normality assumption
detect_outlier <- function(x) {
  # calculate first quantile
  quantile1 <- quantile(x, probs = .25, na.rm = TRUE)
  
  # calculate third quantile
  quantile3 <- quantile(x, probs = 0.75, na.rm = TRUE)
  
  # calculate interquartile range
  iqr <- IQR(x, na.rm = TRUE)
  
  # return TRUE or FALSE
  x > quantile3 + (iqr * 3) | x < quantile1 - (iqr * 3)
}

remove_outlier <- function(dataframe, columns = names(dataframe)) {
  
  # for loop to traverse in columns vector
  for (col in columns) {
    
    # remove observation if it satisfies outlier function
    dataframe <- dataframe[!detect_outlier(dataframe[[col]]), ]
  }
  
  # return dataframe
  print("Remove outliers")
  print(dataframe)
}

# Test data with outliers removed for normality again 
no_outlier_data <- remove_outlier(tidyData, c("pH", "NO3", "NO2", "NH4", "K"))

no_outlier_data <- drop_na(no_outlier_data)

normality2 <-no_outlier_data%>%
  group_by(Treatment,Weeks) %>%
  shapiro_test(pH, NO3, NO2, NH4, K)
data.frame(normality2)
normality2 %>%
  group_by(Treatment, Weeks, variable)
head(normality2)

normality2$p <- round(normality2$p, 4)





# Transforming data and re-testing normality ----
### Log10 transformation
tidyData <- tidyData %>%
  mutate(pH_log10 = log10(pH + 1))

tidyData <- tidyData %>%
  mutate(NO3_log10 = log10(NO3 + 1))

tidyData <- tidyData %>%
  mutate(NO2_log10 = log10(NO2 + 1))

tidyData <- tidyData %>%
  mutate(NH4_log10= log10(NH4 + 1))

tidyData <- tidyData %>%
  mutate(K_log10 = log10(K + 1))

str(tidyData)

### Square root transformation ----
tidyData <- tidyData %>%
    group_by(Treatment, Weeks) %>%
    mutate(pH_sqrt = sqrt(pH))

tidyData <- tidyData %>%
  group_by(Treatment, Weeks) %>%
  mutate(NO3_sqrt = sqrt(NO3))

tidyData <- tidyData %>%
  group_by(Treatment, Weeks) %>%
  mutate(NO2_sqrt = sqrt(NO2))

tidyData <- tidyData %>%
  group_by(Treatment, Weeks) %>%
  mutate(NH4_sqrt= sqrt(NH4))

tidyData <- tidyData %>%
  group_by(Treatment, Weeks) %>%
  mutate(K_sqrt = sqrt(K))


### Testing log10 transformed data for normality ----
normality2 <-tidyData%>%
  group_by(Treatment,Weeks) %>%
  shapiro_test(pH_log10, NO3_log10, NO2_log10, NH4_log10, K_log10)
data.frame(normality2)
normality2 %>%
  group_by(Treatment, Weeks, variable)
head(normality2)
normality2$p <- round(normality2$p, 4)

ggqqplot(tidyData, "pH_log10", ggtheme = theme_bw()) +
  facet_grid(Weeks~Treatment, labeller = "label_both") +
  labs(title = "pH_log10")


ggqqplot(tidyData, "NO3_log10", ggtheme = theme_bw()) +
  facet_grid(Weeks~Treatment, labeller = "label_both") +
  labs(title = "NO3_log10")

gghistogram(tidyData, "NO3_log10", ggtheme = theme_bw()) +
  facet_grid(Weeks~Treatment, labeller = "label_both") +
  labs(title = "NO3_log10")

ggqqplot(tidyData, "NO2_log10", ggtheme = theme_bw()) +
  facet_grid(Weeks~Treatment, labeller = "label_both") +
  labs(title = "NO2_log10")

ggqqplot(tidyData, "NH4_log10", ggtheme = theme_bw()) +
  facet_grid(Weeks~Treatment, labeller = "label_both") +
  labs(title = "NH4_log10")

gghistogram(tidyData, "NH4_log10", ggtheme = theme_bw()) +
  facet_grid(Weeks~Treatment, labeller = "label_both") +
  labs(title = "NH4_log10")

ggqqplot(tidyData, "K_log10", ggtheme = theme_bw()) +
  facet_grid(Weeks~Treatment, labeller = "label_both") +
  labs(title = "K_log10")



### Testing sqrt transformed data for normality ----
normality3 <-tidyData%>%
  group_by(Treatment,Weeks) %>%
  shapiro_test(pH_sqrt, NO3_sqrt, NO2_sqrt, NH4_sqrt, K_sqrt)
data.frame(normality3)
normality3 %>%
  group_by(Treatment, Weeks, variable)
head(normality3)

normality3$p <- round(normality3$p, 4)

ggqqplot(tidyData, "pH_sqrt", ggtheme = theme_bw()) +
  facet_grid(Weeks~Treatment, labeller = "label_both") +
  labs(title = "pH_sqrt")


ggqqplot(tidyData, "NO3_sqrt", ggtheme = theme_bw()) +
  facet_grid(Weeks~Treatment, labeller = "label_both") +
  labs(title = "NO3_sqrt")

ggqqplot(tidyData, "NO2_sqrt", ggtheme = theme_bw()) +
  facet_grid(Weeks~Treatment, labeller = "label_both") +
  labs(title = "NO2_sqrt")

ggqqplot(tidyData, "NH4_sqrt", ggtheme = theme_bw()) +
  facet_grid(Weeks~Treatment, labeller = "label_both") +
  labs(title = "NH4_sqrt")

ggqqplot(tidyData, "K_sqrt", ggtheme = theme_bw()) +
  facet_grid(Weeks~Treatment, labeller = "label_both") +
  labs(title = "K_sqrt")

# Since transformed data is still not normally distributed, analyses have been carried out on non-transformed data below


# Fitting models ----
## pH ----
# Run model using lme
pH.lme <- lme4::lmer(pH ~ Treatment*Weeks + (1|Sample.ID), data = tidyData)
ph.aov <- Anova(pH.lme, type = 3)

ph.aov

shapiro.test(residuals(pH.lme))
pH.residuals.plot <- ggqqplot(resid(pH.lme)) + labs(title = "pH residuals")
pH.residuals.plot

### Post-hoc tests ----
pH.emm <- emmeans(pH.lme, ~ Weeks|Treatment)

pH.pwc <- emmeans(pH.lme, pairwise ~ Weeks|Treatment)
pH.pwc

## NO3 ----
# Run model using lme
NO3.lme <- lme4::lmer(NO3 ~ Treatment*Weeks + (Treatment|Sample.ID) + (Weeks|Sample.ID), data = tidyData)

no3.aov <- Anova(NO3.lme, type = 3)

no3.aov

shapiro.test(residuals(NO3.lme))shapiro.test(residuals(NO3.Weekslme))
NO3.residuals.plot <- ggqqplot(resid(NO3.lme)) + labs(title = "NO3 residuals")
NO3.residuals.plot

### Post-hoc tests ----
NO3.emm <- emmeans(NO3.lme, ~ Weeks|Treatment)
NO3.pwc <- emmeans(NO3.emm, pairwise ~ Weeks|Treatment)
NO3.pwc

## NO2 ----
# Run model using lme
NO2.lme <- lme4::lmer(NO2 ~ Treatment*Weeks + (1|Sample.ID), data = tidyData)
no2.aov <- Anova(NO2.lme, type = 3)

no2.aov

shapiro.test(residuals(NO2.lme))
NO2.residuals.plot <- ggqqplot(resid(NO2.lme)) + labs(title = "NO2 residuals")
NO2.residuals.plot

### Post-hoc tests ----
NO2.emm <- emmeans(NO2.lme, ~ Weeks|Treatment)
NO2.pwc <- emmeans(NO2.emm, pairwise ~ Weeks|Treatment)

NO2.pwc

## NH4----
# Removing rows where NH4 results are unusable (mistake with the dilution so concentration was outside calibration)
NH4_data <- tidyData %>%
  filter(!row_number() %in% c(56, 57, 66))

# Run model using lme
NH4.lme <- lme4::lmer(NH4 ~ Treatment*Weeks + (1|Sample.ID), data = NH4_data)
nh4.aov <- Anova(NH4.lme, type = 3)

nh4.aov

shapiro.test(residuals(NH4.lme))
NH4.residuals.plot <- ggqqplot(resid(NH4.lme)) + labs(title = "NH4 residuals")
NH4.residuals.plot

### Post-hoc tests ----
NH4.emm <- emmeans(NH4.lme, ~ Weeks|Treatment)
NH4.pwc <- emmeans(NH4.emm, pairwise ~ Weeks|Treatment)
NH4.pwc

## K ----
# Run model using lme
K.lme <- lme4::lmer(K ~ Treatment*Weeks + Treatment*Weeks + (1|Sample.ID), data = tidyData)
k.aov <- Anova(K.lme, type = 3)

k.aov

shapiro.test(residuals(K.lme))
K.residuals.plot <- ggqqplot(resid(K.lme)) + labs(title = "K residuals")
K.residuals.plot

### Post-hoc tests ----
K.emm <- emmeans(K.lme, ~ Weeks|Treatment)
K.pwc <- emmeans(K.emm, pairwise ~ Weeks|Treatment)
K.pwc


# Printing all results together to console for easier reading ----
## Aov outputs ----
ph.aov$test <- rep("pH")
no3.aov$test <- rep("NO3")
no2.aov$test <- rep("NO2")
nh4.aov$test <- rep("NH4")
k.aov$test <- rep("K")

combined.aov <- rbind(ph.aov, no3.aov, no2.aov, nh4.aov, k.aov)
combined.aov$`Pr(>Chisq)` <- round(combined.aov$`Pr(>Chisq)`, 3)

## pwc outputs ----
ph.pwc.out <- data.frame(test = rep("pH", 9), summary(pH.pwc)$contrasts)
no3.pwc.out <- data.frame(test = rep("NO3", 9), summary(NO3.pwc)$contrasts)
no2.pwc.out <- data.frame(test = rep("NO2", 9), summary(NO2.pwc)$contrasts)
nh4.pwc.out <- data.frame(test = rep("NH4", 9), summary(NH4.pwc)$contrasts)
k.pwc.out <- data.frame(test = rep("K", 9), summary(K.pwc)$contrasts)

combined.pwc <- rbind(ph.pwc.out, no3.pwc.out, no2.pwc.out, nh4.pwc.out, k.pwc.out)
combined.pwc$p.value <- round(combined.pwc$p.value, 3)

## Saving results to csv ----
write_csv(combined.aov, "aov-output.csv")
write.csv(combined.pwc, "pwc-output.csv")