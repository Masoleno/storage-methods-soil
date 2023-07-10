#Set numbers to show in standard format instead of scientific notation. 
#(Reactivate scientific notation with options(scipen=0))
options(scipen = 999)

# Load libraries ----

#library(psych)
#library(MASS)
library(tidyverse)
library(gridExtra)
#library(car)
library(rstatix)


# Read in csv file created at the end of the data-wrangling script ----
tidyData <- read.csv("chemistry-data-tidy.csv")

#Look at the data
head(tidyData)
str(tidyData)

# Rename columns so they're easier to write. ----
tidyData<- tidyData %>% 
  rename(
    "NH4" = NH4..mg.kg.,
    "TON" = TON..mg.kg.,
    "K" = K..mg.kg.,
    "NO2" = NO2..mg.kg.,
    "NO3" = NO3..mg.kg.,
    "Conductivity" = Conductivity..mV.,
    "Moisture" = Moisture....
  )

head(tidyData)

# Convert to factors ----
tidyData$Weeks <- as.factor(tidyData$Weeks)

tidyData$Treatment <- as.factor(tidyData$Treatment)

str(tidyData)


# Testing for normality ----
### Plotting ----
#plotting histograms and qqplots in a for loop
for (i in 3:9) {
  plot1 <- ggplot(data = tidyData, aes(sample = tidyData[,i])) +
    geom_qq() +
    geom_qq_line(color="red") + 
    labs(title = colnames(tidyData[i])) +
    facet_grid(Treatment ~ Weeks)
  print(plot1)
  
  plot2 <- ggplot(data = tidyData, aes(tidyData[, i])) +
    geom_histogram(binwidth = 10) + # some histograms need different binwidths so will have to tweak this or plot outside of loop
    labs(title = colnames(tidyData[i])) +
    facet_grid(Treatment ~ Weeks)
  print(plot2)
}

### Shapiro-Wilks ----

## This is neat but haven't figured out how to group it by treatment and weeks first.
# lshap <- lapply(tidyData[3:9], shapiro.test)
# 
# lshap[[1]]
# 
# shap_res <- sapply(lshap, `[` , c("statistic", "p.value"))
# t(shap_res)

#Testing all variables for normality, grouped by Treatment and Weeks. This uses shapiro_test() from the rstatix
#package.
normality<-tidyData %>%
  group_by(Treatment,Weeks) %>%
  shapiro_test(pH, Conductivity, NO3, NO2, NH4, TON, K)
data.frame(normality)
normality %>%
  group_by(Treatment, Weeks, variable)
head(normality)


# Fitting anova model with rstatix package ----
## pH ----
# Check for rows with NA's in pH column
tidyData[!complete.cases(tidyData$pH),]

# Run model
pH_mod <- anova_test(data = tidyData, dv = pH, wid = Sample.ID, within = c(Treatment, Weeks))
get_anova_table(pH_mod) #the get_anova_table function (rstatix) automatically applies "Greenhouse-Geisser 
# sphericity correction" on any factors that violate this assumption

## Conductivity ----
# Check for rows with NA's in Conductivity column
tidyData[!complete.cases(tidyData$Conductivity),]

# Run model
cond_mod <- anova_test(data = tidyData, dv = Conductivity, wid = Sample.ID, within = c(Treatment, Weeks))
get_anova_table(cond_mod) 

## NO3 ----
# Check for  rows with NA's in NO3 column
tidyData[!complete.cases(tidyData$NO3),]

# Remove NA's
NO3_data <- tidyData %>%
  drop_na(NO3)
head(NO3_data)

# Run model
NO3_mod <- anova_test(data = NO3_data, dv = NO3, wid = Sample.ID, within = c(Treatment, Weeks))
get_anova_table(NO3_mod)

## NO2 ----
# Check for  rows with NA's in NO2 column
tidyData[!complete.cases(tidyData$NO2),]

# Remove NA's
NO2_data <- tidyData %>%
  drop_na(NO2)
head(NO2_data)

# Run model
NO2_mod <- anova_test(data = NO2_data, dv = NO2, wid = Sample.ID, within = c(Treatment, Weeks))
get_anova_table(NO2_mod)

## NH4----
# Check for  rows with NA's in NO2 column
tidyData[!complete.cases(tidyData$NH4),]

# Run model
NH4_mod <- anova_test(data = tidyData, dv = NH4, wid = Sample.ID, within = c(Treatment, Weeks))
get_anova_table(NH4_mod)

## TON ----
# Check for  rows with NA's in NO2 column
tidyData[!complete.cases(tidyData$TON),]

# Run model
TON_mod <- anova_test(data = tidyData, dv = TON, wid = Sample.ID, within = c(Treatment, Weeks))
get_anova_table(TON_mod)

## K ----
# Check for  rows with NA's in NO2 column
tidyData[!complete.cases(tidyData$K),]

# Remove NA's
K_data <- tidyData %>%
  drop_na(K)
head(K_data)

# Run model
K_mod <- anova_test(data = K_data, dv = K, wid = Sample.ID, within = c(Treatment, Weeks))
get_anova_table(K_mod)



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
NH4.one.way <- tidyData %>%
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

##### Pairwise comparisons between time points ----
K.pwc <- tidyData %>%
  group_by(Treatment) %>%
  pairwise_t_test(
    K ~ Weeks, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
K.pwc



## Printing all results together to console for easier reading ----
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

data.frame(pH.pwc)
data.frame(con.pwc)
data.frame(NO3.pwc)
data.frame(NO2.pwc)
data.frame(NH4.pwc)
data.frame(TON.pwc)
data.frame(K.pwc)
pwc_results <- rbind(pH.pwc, con.pwc, NO3.pwc, NO2.pwc, NH4.pwc, TON.pwc, K.pwc)

write_csv(pwc_results, "pwc-results.csv")

# This works but haven't figured out how to get the tested variable as a column 
data.frame(pH.one.way)
data.frame(con.one.way)
data.frame(NO3.one.way)
data.frame(NO2.one.way)
data.frame(NH4.one.way)
data.frame(TON.one.way)
data.frame(K.one.way)
owc_results <- rbind(pH.one.way, con.one.way, NO3.one.way, NO2.one.way, NH4.one.way, TON.one.way, K.one.way)

# This doesn't work as anova results are in a list
# data.frame(pH_mod)
# data.frame(cond_mod)
# data.frame(NO3_mod)
# data.frame(NO2_mod)
# data.frame(NH4_mod)
# data.frame(TON_mod)
# data.frame(K_mod)
# aov_results <- rbind(pH_mod, cond_mod, NO3_mod, NO2_mod, NH4_mod, TON_mod, K_mod)

## Fitting the anova model using aov ----
# model.aov <- aov(pH ~ Treatment*Weeks + Error(Sample.ID/(Treatment*Weeks)), data = pH_data)
# summary(model.aov)

#Can also define error like this, doesn't make much difference to the results
# model.aov <- aov(pH ~ Treatment*Weeks + Error(Sample.ID/(Treatment+Weeks)), data = pH_data)
# summary(model.aov)  




## Looping - this is currently throwing an error - cba to fix ----

# results_df <- data.frame(Column = character(),
#                           F_value = numeric(),
#                           P_value = numeric(),
#                           p_significance_level = character(),
#                           stringsAsFactors = FALSE)


# for (col in 3:9) {
# column <- names(tidyData[i])
#   avz <- anova_test(tidyData, dv = tidyData[ , col], wid = Sample.ID, within = c(Treatment, Weeks))
#   result <- get_anova_table(avz)
#   f_value <- result[[1]][1, 4]
#   p_value <- result[[1]][1, 5]
#   p_significant <- result[[1]][1, 6]
# 
#   row <- data.frame(Column = column, F_value = f_value, P_value = p_value,
#                   p_significance_level= p_significant, stringsAsFactors = FALSE)
#   }
#   
#   cat("Repeated Measures Two-Way ANOVA for", col, ":\n")
#   print(avz)
#   cat("\n")
# }
###########################################