# Set numbers to show in standard format instead of scientific notation. 
# (Reactivate scientific notation with options(scipen = 0))
options(scipen = 999)

# Load libraries ----
library(tidyverse)
library(gridExtra)
library(afex)
library(ggpubr)
library(lme4)
library(car)


# Read in csv file created at the end of the data-wrangling script ----
tidyData <- read.csv("chemistry-data-tidy-dryCorrected.csv")

# Look at the data
head(tidyData)
str(tidyData)

# Rename columns so they're easier to write. ----
tidyData<- tidyData %>% 
  rename(
    "NH4" = NH4..mg.kg.,
    "K" = K..mg.kg.,
    "NO2" = NO2..mg.kg.,
    "NO3" = NO3..mg.kg.,
    "Moisture" = Moisture....
  )

head(tidyData)

# Convert to factors ----
tidyData$Weeks <- as.factor(tidyData$Weeks)

tidyData$Treatment <- as.factor(tidyData$Treatment)

str(tidyData)

tidyData <- tidyData%>% 
  mutate(across(c(Moisture, NO3:NH4), ~ ifelse(.x<0, 0, .x)))

str(tidyData)
tail(tidyData, 20)

# Show data summary to extract mean moisture content (%) for reporting in manuscript
tidyData %>%
  dplyr::filter(Treatment == "Fridge", Weeks == "0") %>%
  summary()

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

# Too many data points are removed from the week 24 groups for this to be a valid option, especially since some data still
# violates the normality assumption


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

### Natural log transformation
tidyData <- tidyData %>%
  group_by(Treatment, Weeks) %>%
  mutate(pH_log = log(pH + 1))

tidyData <- tidyData %>%
  group_by(Treatment, Weeks) %>%
  mutate(NO3_log = log(NO3 + 1))

tidyData <- tidyData %>%
  group_by(Treatment, Weeks) %>%
  mutate(NO2_log = log(NO2 + 1))

tidyData <- tidyData %>%
  group_by(Treatment, Weeks) %>%
  mutate(NH4_log = log(NH4 + 1))

tidyData <- tidyData %>%
  group_by(Treatment, Weeks) %>%
  mutate(K_log = log(K + 1))


### Testing log10 transformed data for normality ----
normality3 <-tidyData%>%
  group_by(Treatment,Weeks) %>%
  shapiro_test(pH_log10, NO3_log10, NO2_log10, NH4_log10, K_log10)
data.frame(normality3)
normality3 %>%
  group_by(Treatment, Weeks, variable)
head(normality3)

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
normality4 <-tidyData%>%
  group_by(Treatment,Weeks) %>%
  shapiro_test(pH_sqrt, NO3_sqrt, NO2_sqrt, NH4_sqrt, K_sqrt)
data.frame(normality4)
normality4 %>%
  group_by(Treatment, Weeks, variable)
head(normality4)

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

### Testing natural log transformed data for normality
normality5 <-tidyData%>%
  group_by(Treatment,Weeks) %>%
  shapiro_test(pH_log, NO3_log, NO2_log, NH4_log, K_log)
data.frame(normality5)
normality5 %>%
  group_by(Treatment, Weeks, variable)
head(normality5)

ggqqplot(tidyData, "pH_log", ggtheme = theme_bw()) +
  facet_grid(Weeks~Treatment, labeller = "label_both") +
  labs(title = "pH_log")


ggqqplot(tidyData, "NO3_log", ggtheme = theme_bw()) +
  facet_grid(Weeks~Treatment, labeller = "label_both") +
  labs(title = "NO3_log")

ggqqplot(tidyData, "NO2_log", ggtheme = theme_bw()) +
  facet_grid(Weeks~Treatment, labeller = "label_both") +
  labs(title = "NO2_log")

ggqqplot(tidyData, "NH4_log", ggtheme = theme_bw()) +
  facet_grid(Weeks~Treatment, labeller = "label_both") +
  labs(title = "NH4_log")

ggqqplot(tidyData, "K_log", ggtheme = theme_bw()) +
  facet_grid(Weeks~Treatment, labeller = "label_both") +
  labs(title = "K_log")





# Fitting models ----
## pH ----
# Check for rows with NA's in pH column
tidyData[!complete.cases(tidyData$pH),]

# Run model
# using afex
pH.test <- afex::aov_car(pH ~ Treatment*Weeks + Error(Sample.ID/(Treatment*Weeks)), data = tidyData)
pH.test
summary(pH.test)

resids <- residuals(pH.test, append = TRUE)

shapiro.test(resids$.residuals)

#using lme




## NO3 ----
# Check for  rows with NA's in NO3 column
tidyData[!complete.cases(tidyData$NO3),]

# Remove NA's
NO3_data <- tidyData %>%
  drop_na(NO3)
head(NO3_data)

# Run model
NO3.test <- afex::aov_car(NO3 ~ Treatment*Weeks + Error(Sample.ID/(Treatment*Weeks)), data = tidyData)
NO3.test
summary(NO3.test)

NO3.resids <- residuals(NO3.test, append = TRUE)

shapiro.test(NO3.resids$.residuals)





## NO2 ----
# Check for  rows with NA's in NO2 column
tidyData[!complete.cases(tidyData$NO2),]

# Remove NA's
NO2_data <- tidyData %>%
  drop_na(NO2)
head(NO2_data)

# Run model
# Using afex
NO2.test <- afex::aov_car(NO2 ~ Treatment*Weeks + Error(Sample.ID/(Treatment*Weeks)), data = tidyData)
NO2.test
summary(NO2.test)

NO2.resids <- residuals(NO2.test, append = TRUE)

shapiro.test(NO2.resids$.residuals)

# Using lme



## NH4----
# Check for  rows with NA's in NH4 column
tidyData[!complete.cases(tidyData$NH4),]

# Removing rows where NH4 results are unusable (mistake with the dilution so concentration was outside calibration)
NH4_data <- tidyData %>%
  filter(!row_number() %in% c(56, 57, 66))

# Run model
# Using afex
NH4.test <- afex::aov_car(NH4 ~ Treatment*Weeks + Error(Sample.ID/(Treatment*Weeks)), data = tidyData)
NH4.test
summary(NH4.test)

NH4.resids <- residuals(NH4.test, append = TRUE)

shapiro.test(NH4.resids$.residuals)

# Using lme



## K ----
# Check for  rows with NA's in K column
tidyData[!complete.cases(tidyData$K),]

# Remove NA's
K_data <- tidyData %>%
  drop_na(K)
head(K_data)
tail(K_data)

# Run model
# Using afex
K.test <- afex::aov_car(K ~ Treatment*Weeks + Error(Sample.ID/(Treatment + Weeks + Treatment:Weeks)), data = K_data)
K.test
summary(K.test)

K.resids <- residuals(K.test, append = TRUE)

shapiro.test(K.resids$.residuals)

# Using lme



# Post-hoc tests ----
#### pH ----
##### Effect of time (Weeks) at each Treatment type----
pH.one.way <- tidyData %>%
  group_by(Treatment) %>%
  anova_test(dv = pH, wid = Sample.ID, within = Weeks) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")

pH.one.way

##### Pairwise comparisons between time points ----
pH.pwc <- tidyData %>%
  group_by(Treatment) %>%
  pairwise_t_test(
    pH ~ Weeks, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pH.pwc


#### Conductivity ----
##### Effect of time (Weeks) at each Treatment type----
con.one.way <- tidyData %>%
  group_by(Treatment) %>%
  anova_test(dv = Conductivity, wid = Sample.ID, within = Weeks) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")

con.one.way

##### Pairwise comparisons between time points ----
con.pwc <- tidyData %>%
  group_by(Treatment) %>%
  pairwise_t_test(
    Conductivity ~ Weeks, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
con.pwc


#### NO3----
##### Effect of time (Weeks) at each Treatment type----
NO3.one.way <- NO3_data %>%
  group_by(Treatment) %>%
  anova_test(dv = NO3, wid = Sample.ID, within = Weeks) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")

NO3.one.way

##### Pairwise comparisons between time points ----
NO3.pwc <- tidyData %>%
  group_by(Treatment) %>%
  pairwise_t_test(
    NO3 ~ Weeks, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
NO3.pwc


#### NO2 ----
##### Effect of time (Weeks) at each Treatment type----
NO2.one.way <- NO2_data %>%
  group_by(Treatment) %>%
  anova_test(dv = NO2, wid = Sample.ID, within = Weeks) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")

NO2.one.way

##### Pairwise comparisons between time points ----
NO2.pwc <- tidyData %>%
  group_by(Treatment) %>%
  pairwise_t_test(
    NO2 ~ Weeks, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
NO2.pwc


#### NH4 ----
##### Effect of time (Weeks) at each Treatment type----
NH4.one.way <- NH4_data %>%
  group_by(Treatment) %>%
  anova_test(dv = NH4, wid = Sample.ID, within = Weeks) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")

NH4.one.way

##### Pairwise comparisons between time points ----
NH4.pwc <- tidyData %>%
  group_by(Treatment) %>%
  pairwise_t_test(
    NH4 ~ Weeks, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
NH4.pwc


#### TON ----
##### Effect of time (Weeks) at each Treatment type----
TON.one.way <- tidyData %>%
  group_by(Treatment) %>%
  anova_test(dv = TON, wid = Sample.ID, within = Weeks) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")

TON.one.way

##### Pairwise comparisons between time points ----
TON.pwc <- tidyData %>%
  group_by(Treatment) %>%
  pairwise_t_test(
    TON ~ Weeks, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
TON.pwc


#### K ----
##### Effect of time (Weeks) at each Treatment type----
K.one.way <- K_data %>%
  group_by(Treatment) %>%
  anova_test(dv = K, wid = Sample.ID, within = Weeks) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")

K.one.way

##### Pairwise comparisons between time points ---- Also not working with the reduced data set
K.pwc <- tidyData %>%
  group_by(Treatment) %>%
  pairwise_t_test(
    K ~ Weeks, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
K.pwc



# Printing all results together to console for easier reading ----
get_anova_table(pH_mod)
pH.one.way
pH.pwc
get_anova_table(cond_mod)
con.one.way
con.pwc
get_anova_table(NO3_mod)
NO3.one.way
NO3.pwc
get_anova_table(NO2_mod)
NO2.one.way
NO2.pwc
get_anova_table(NH4_mod)
NH4.one.way
NH4.pwc
get_anova_table(TON_mod)
TON.one.way
TON.pwc
get_anova_table(K_mod)
K.one.way
K.pwc

# Saving results to csv ----
pH.aov <- data.frame(test = rep("pH", 3), get_anova_table(pH_mod))
cond.aov <- data.frame(test = rep("Conductivity", 3), get_anova_table(cond_mod))
NO3.aov <- data.frame(test = rep("NO3", 3),get_anova_table(NO3_mod))
NO2.aov <-data.frame(test = rep("NO2", 3), get_anova_table(NO2_mod))
NH4.aov <- data.frame(test = rep("NH4", 3), get_anova_table(NH4_mod))
TON.aov <- data.frame(test = rep("TON", 3), get_anova_table(TON_mod))
K.aov <- data.frame(test = rep("K", 3), get_anova_table(K_mod))

aov_results <- rbind(pH.aov, cond.aov, NO3.aov, NO2.aov, NH4.aov, TON.aov, K.aov)
write_csv(aov_results, "aov-results.csv")

data.frame(pH.pwc)
data.frame(con.pwc)
data.frame(NO3.pwc)
data.frame(NO2.pwc)
data.frame(NH4.pwc)
data.frame(TON.pwc)
data.frame(K.pwc)

pwc_results <- rbind(pH.pwc, con.pwc, NO3.pwc, NO2.pwc, NH4.pwc, TON.pwc, K.pwc)
write_csv(pwc_results, "pwc-results.csv")

ph.owc <- data.frame(test = rep("pH", nrow(pH.one.way)), pH.one.way)
con.owc <- data.frame(test = rep("Conductivity", nrow(con.one.way)), con.one.way)
NO3.owc <- data.frame(test = rep("NO3", nrow(NO3.one.way)), NO3.one.way)
NO2.owc <- data.frame(test = rep("NO2", nrow(NO2.one.way)), NO2.one.way)
NH4.owc <- data.frame(test = rep("NH4", nrow(NH4.one.way)), NH4.one.way)
TON.owc <- data.frame(test = rep("TON", nrow(TON.one.way)), TON.one.way)
K.owc <- data.frame(test = rep("K", nrow(K.one.way)), K.one.way)

owc_results <- rbind(ph.owc, con.owc, NO3.owc, NO2.owc,NH4.owc, TON.owc, K.owc)
write_csv(owc_results, "owc-results.csv")





tidyDataLong <- tidyData %>%
  pivot_longer(cols = c(Moisture:K, pH_log10:K_log10),
               names_to = "Variable",
               values_to = "Repsonse")

head(tidyDataLong, 20)
