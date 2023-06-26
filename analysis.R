#Set numbers to show in standard format instead of scientific notation. 
#(Reactivate scientific notation with options(scipen=0))
options(scipen = 999)

# Load libraries ----

#library(psych)
#library(MASS)
library(tidyverse)
library(gridExtra)
library(car)
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

# RM ANOVA for pH ----
# Extract pH data ----
#pH_data <- tidyData %>%
#  select(Sample.ID, pH, Treatment, Weeks)

# 
# head(pH_data,20)
# str(pH_data)

## Testing for normality ----
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
    geom_histogram(binwidth = 10) +
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


## Fitting anova model with rstatix package ----
### pH ----
pH_mod <- anova_test(data = tidyData, dv = pH, wid = Sample.ID, within = c(Treatment, Weeks))
get_anova_table(pH_mod) #the get_anova_table function (rstatix) automatically applies "Greenhouse-Geisser 
# sphericity correction" on any factors that violate this assumption

### Conductivity ----
cond_mod <- anova_test(data = tidyData, dv = Conductivity, wid = Sample.ID, within = c(Treatment, Weeks))
get_anova_table(cond_mod) 

### NO3 ----
NO3_mod <- anova_test(data = tidyData, dv = NO3, wid = Sample.ID, within = c(Treatment, Weeks))
get_anova_table(NO3_mod)

### NO2 ----
NO2_mod <- anova_test(data = tidyData, dv = NO2, wid = Sample.ID, within = c(Treatment, Weeks))
get_anova_table(NO2_mod)

### NH4----
NH4_mod <- anova_test(data = tidyData, dv = NH4, wid = Sample.ID, within = c(Treatment, Weeks))
get_anova_table(NH4_mod)

### TON ----
TON_mod <- anova_test(data = tidyData, dv = TON, wid = Sample.ID, within = c(Treatment, Weeks))
get_anova_table(TON_mod)

### K ----
K_mod <- anova_test(data = tidyData, dv = K, wid = Sample.ID, within = c(Treatment, Weeks))
get_anova_table(K_mod)
# Printing all results together to console for easier reading
get_anova_table(pH_mod)
get_anova_table(cond_mod)
get_anova_table(NO3_mod)
get_anova_table(NO2_mod)
get_anova_table(NH4_mod)
get_anova_table(TON_mod)
get_anova_table(K_mod)


### Post-hoc tests ----

pH.one.way <- tidyData %>%
  group_by(Weeks) %>%
  anova_test(dv = pH, wid = Sample.ID, within = Treatment) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
pH.one.way


pH.one.way2 <- tidyData %>%
  group_by(Treatment) %>%
  anova_test(dv = pH, wid = Sample.ID, within = Weeks) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")

pH.one.way2

### Pairwise comparisons between treatment groups ----
pwc <- tidyData %>%
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


## This is currently throwing an error - cba to fix
# columns <- names(tidyData[, 3:9])
# for (col in columns) {
#   avz <- anova_test(tidyData, dv = tidyData[ , col], wid = Sample.ID, within = c(Treatment, Weeks))
#   get_anova_table(avz)
#   
#   cat("Repeated Measures Two-Way ANOVA for", col, ":\n")
#   print(avz)
#   cat("\n")
# }
# 
# tidyData %>%
#   group_by(Treatment, Weeks) %>%
#   nest() %>%
#   mutate(ano_obj = map( ~anova_test(data = tidyData, dv = .x, wid = Sample.ID, within = c(Treatment, Weeks)))
###########################################