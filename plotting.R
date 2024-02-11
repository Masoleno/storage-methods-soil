# Script used for making the plots for the paper 'Soil Sample Storage Conditions Affect Measurements of pH, and Nutrients' 
# by Maya Sollen-Norrlin and Naomi Rintoul-Hynes. 

# Load required libraries ----
library(tidyverse)
library(ggpubr)
library(superb)

# Read in and prepare data (csv file from the 'data-wrangling' script with dry-corrected nutrient concentrations) ----
tidyData <- read.csv("chemistry-data-tidy-dryCorrected.csv")

str(tidyData)

## Convert weeks to factor so it can be used to group the plots ----
tidyData$Weeks <- as.factor(tidyData$Weeks)
str(tidyData)

## Rename columns to make them easier to refer to ----
tidyData<- tidyData %>% 
  rename(
    "NH4" = NH4..mg.kg.,
    "K" = K..mg.kg.,
    "NO2" = NO2..mg.kg.,
    "NO3" = NO3..mg.kg.,
    "Moisture" = Moisture....
  )

head(tidyData)

## Replacing negative values in the moisture and nitrate columns with 0 ----
tidyData <- tidyData%>% 
  mutate(across(c(Moisture, NO3:NH4), ~ ifelse(.x<0, 0, .x)))

tail(tidyData, 20)

# Make plots ---- 

## pH (figure 1) ----
pH.plot <- ggplot(tidyData, aes(x = Weeks, y = pH)) +
  stat_boxplot(aes(Weeks, pH), geom = "errorbar") +
  geom_boxplot(aes(Weeks, pH), outlier.shape = NA, coef = 0) +
  stat_summary(fun = mean, geom = "point", shape = 5, size = 5) +
  ylab("pH") +
  facet_wrap(.~Treatment) +
  theme(text = element_text(family = "Arial", size =20), axis.text = element_text(family = "Arial", size = 18), 
        panel.background = element_rect(fill = NA, colour ='black')) +
  scale_x_discrete(labels = c("Fresh control", "7", "24"))

pH.plot

ph.plot <- pH.plot + 
  showSignificance(c(2,3), 6.6, -0.07, "**", panel = list(Treatment = "Dry"), 
                   textParams = list(size = 7, family= "Arial"), segmentParams = list(linewidth =0.8)) +
  showSignificance(c(1,3), 6.7, -0.09, "**", panel = list(Treatment = "Dry"), 
                   textParams = list(size = 7, family= "Arial"), segmentParams = list(linewidth =0.8)) +
  showSignificance(c(1,2), 6.64, -0.07, "***", panel = list(Treatment = "Fridge"), 
                   textParams = list(size = 7, family= "Arial"), segmentParams = list(linewidth =0.8)) +
  showSignificance(c(2.2,3), 6.64, -0.07, "***", panel = list(Treatment = "Fridge"), 
                   textParams = list(size = 7, family= "Arial"), segmentParams = list(linewidth =0.8)) +
  showSignificance(c(1,3), 6.74, -0.07, "***", panel = list(Treatment = "Fridge"), 
                   textParams = list(size = 7, family= "Arial"), segmentParams = list(linewidth =0.8)) +
  showSignificance(c(2,3), 6.6, -0.07, "**", panel = list(Treatment = "Frozen"), 
                   textParams = list(size = 7, family= "Arial"), segmentParams = list(linewidth =0.8))

ph.plot

## NO3 (figure 2) ----
no3_plot <- ggplot(tidyData, aes(x = Weeks, y = NO3)) +
  stat_boxplot(aes(Weeks, NO3), geom = "errorbar") +
  geom_boxplot(aes(Weeks, NO3), outlier.shape = NA, coef = 0) +
  stat_summary(fun = mean, geom = "point", shape = 5, size = 5) +
  ylab(bquote(NO[3]-N (mg/kg))) +
  facet_wrap(.~Treatment) +
  theme(text = element_text(family = "Arial", size = 22), axis.text = element_text(family = "Arial", size = 18), 
        panel.background = element_rect(fill = NA, colour ='black')) +
  scale_x_discrete(labels = c("Fresh control", "7", "24")) +
  scale_y_continuous(limits = c(0,60), n.breaks = 8)

no3_plot

no3.plot <- no3_plot + 
  showSignificance(c(1.05,2.2), 43, - 3, "***", panel = list(Treatment = "Dry"), 
                   textParams = list(size = 7, family= "Arial"), segmentParams = list(linewidth =0.8)) +
  showSignificance(c(1,3), 49, -5, "***", panel = list(Treatment = "Dry"), 
                   textParams = list(size = 7, family= "Arial"), segmentParams = list(linewidth =0.8)) +
  showSignificance(c(1.1,2), 52, -5, "**", panel = list(Treatment = "Fridge"), 
                   textParams = list(size = 7, family= "Arial"), segmentParams = list(linewidth =0.8)) +
  showSignificance(c(2.2,3), 49, -3, "***", panel = list(Treatment = "Fridge"), 
                   textParams = list(size = 7, family= "Arial"), segmentParams = list(linewidth =0.8)) +
  showSignificance(c(1,3), 58, -5, "***", panel = list(Treatment = "Fridge"), 
                   textParams = list(size = 7, family= "Arial"), segmentParams = list(linewidth =0.8)) +
  showSignificance(c(2,3), 45, -3, "***", panel = list(Treatment = "Frozen"), 
                   textParams = list(size = 7, family= "Arial"), segmentParams = list(linewidth =0.8)) +
  showSignificance(c(1,3), 51, -3, "***", panel = list(Treatment = "Frozen"), 
                   textParams = list(size = 7, family= "Arial"), segmentParams = list(linewidth =0.8))

no3.plot

## Nitrogen line graph (figure 3) ----
# Subset only nitrogen data from main data set
N_data <- tidyData %>%
  select(Sample.ID, NO3, NO2, NH4, Treatment, Weeks)

head(N_data)

# Check for missing values and remove
N_data[!complete.cases(N_data), ]

N_data <- N_data %>%
  drop_na()

N_data[!complete.cases(N_data), ]

# Make data into long format
N_data_long <- N_data %>%
  pivot_longer(cols = NO3:NH4,
               names_to = "Nitrogen compound",
               values_to = "Concentration (mg/kg)")

head(N_data_long, 20)

# Calculate summary statistics (mean, standard deviation, and standard error)
summaryN <- N_data_long %>%
  group_by(Treatment, Weeks, `Nitrogen compound`) %>%
  summarise(
    meanConc = mean(`Concentration (mg/kg)`),
    sdConc = sd(`Concentration (mg/kg)`),
    seConc = sdConc/sqrt(length(Sample.ID))
  )

summaryN

# Double check that there are no NAs
summaryN[!complete.cases(summaryN), ]

# Make the plot 
N_plot <- ggplot(summaryN, aes(x = factor(Weeks), y = meanConc, color = factor(`Nitrogen compound`), group = `Nitrogen compound`)) +
  geom_line() +
  geom_point(size = 2.5) +
  geom_errorbar(aes(ymin = meanConc - seConc, ymax = meanConc + seConc), width = 0.1)+
  facet_wrap(~Treatment) +
  theme(text = element_text(family = "Arial", size = 22), panel.grid.major = element_blank(),
        axis.text = element_text(family = "Arial", size = 18),
        panel.grid.minor = element_blank(), legend.title = element_text(size = 15), 
        legend.position = "bottom") +
  scale_x_discrete(labels = c("Fresh control", "7", "24")) +
  labs(x = "Weeks", y = "Mean concentration (mg/kg)", color = "Nitrogen form:") 

# Specify colours that are colour-blindness friendly and set legend labels
N_plot <- N_plot + scale_color_manual(values = c("#E69F00", "#009E73",  "#56B4E9"), labels = c(expression(NH[4]-N), expression(NO[2]-N), expression(NO[3]-N)))

N_plot

## NO2 (figure 4) ----
no2_plot <- ggplot(tidyData, aes(x = Weeks, y = NO2)) +
  stat_boxplot(aes(Weeks, NO2), geom = "errorbar") +
  geom_boxplot(aes(Weeks, NO2), outlier.shape = NA, coef = 0) +
  stat_summary(fun = mean, geom = "point", shape = 5, size = 5) +
  ylab(bquote(NO[2]-N (mg/kg))) +
  facet_wrap(.~Treatment) +
  theme(text = element_text(family = "Arial", size = 22), axis.text = element_text(family = "Arial", size = 18),
        panel.background = element_rect(fill = NA, colour ='black')) +
  scale_x_discrete(labels = c("Fresh control", "7", "24")) +
  scale_y_continuous(limits = c(0,2.6), n.breaks = 8)

no2_plot

no2.plot <- no2_plot + 
  showSignificance(c(0.9,2), 1.3, - 0.05, "***", panel = list(Treatment = "Dry"), 
                   textParams = list(size = 7, family= "Arial"), segmentParams = list(linewidth =0.8)) +
  showSignificance(c(1,2), 1.7, -0.05, "*", panel = list(Treatment = "Frozen"), 
                   textParams = list(size = 7, family= "Arial"), segmentParams = list(linewidth =0.8)) +
  showSignificance(c(1,3), 2.5, -0.05, "***", panel = list(Treatment = "Frozen"), 
                   textParams = list(size = 7, family= "Arial"), segmentParams = list(linewidth =0.8))

no2.plot

## NH4 (figure 5) ----
nh4_plot <- ggplot(tidyData, aes(x = Weeks, y = NH4)) +
  stat_boxplot(aes(Weeks, NH4), geom = "errorbar") +
  geom_boxplot(aes(Weeks, NH4), outlier.shape = NA, coef = 0) +
  stat_summary(fun = mean, geom = "point", shape = 5, size = 5) +
  ylab(bquote(NH[4]-N (mg/kg))) +
  facet_wrap(.~Treatment) +
  theme(text = element_text(family = "Arial", size = 22), axis.text = element_text(family = "Arial", size = 18), 
        panel.background = element_rect(fill = NA, colour ='black')) +
  scale_x_discrete(labels = c("Fresh control", "7", "24")) +
  scale_y_continuous(limits = c(0,60), n.breaks = 8)

nh4_plot

nh4.plot <- nh4_plot + 
  showSignificance(c(1,2), 52, - 3, "***", panel = list(Treatment = "Dry"), 
                   textParams = list(size = 7, family= "Arial"), segmentParams = list(linewidth =0.8)) +
  showSignificance(c(0.9,3), 56, - 3, "***", panel = list(Treatment = "Dry"), 
                   textParams = list(size = 7, family= "Arial"), segmentParams = list(linewidth =0.8)) +
  showSignificance(c(1,3), 16, -3, "*", panel = list(Treatment = "Frozen"), 
                   textParams = list(size = 7, family= "Arial"), segmentParams = list(linewidth =0.8)) 

nh4.plot

## K (figure 6) ----
k_plot <- ggplot(tidyData, aes(x = Weeks, y = K)) +
  stat_boxplot(aes(Weeks, K), geom = "errorbar") +
  geom_boxplot(aes(Weeks, K), outlier.shape = NA, coef = 0) +
  stat_summary(fun = mean, geom = "point", shape = 5, size = 5) +
  ylab(bquote(K (mg/kg))) +
  facet_wrap(.~Treatment) +
  theme(text = element_text(family = "Arial", size = 22), axis.text = element_text(family = "Arial", size = 18),
        panel.background = element_rect(fill = NA, colour ='black')) +
  scale_x_discrete(labels = c("Fresh control", "7", "24")) +
  scale_y_continuous(limits = c(0,125), n.breaks = 8) 

k_plot

k.plot <- k_plot + 
  showSignificance(c(1,1.9), 102, - 3, "**", panel = list(Treatment = "Dry"), 
                   textParams = list(size = 7, family= "Arial"), segmentParams = list(linewidth =0.8)) +
  showSignificance(c(2,3), 104, - 3, "***", panel = list(Treatment = "Dry"), 
                   textParams = list(size = 7, family= "Arial"), segmentParams = list(linewidth =0.8)) +
  showSignificance(c(1,3), 115, - 3, "**", panel = list(Treatment = "Dry"), 
                   textParams = list(size = 7, family= "Arial"), segmentParams = list(linewidth =0.8)) +
  showSignificance(c(1,2), 111, -3, "***", panel = list(Treatment = "Fridge"), 
                   textParams = list(size = 7, family= "Arial"), segmentParams = list(linewidth =0.8)) +
  showSignificance(c(0.9,3), 122, -3, "**", panel = list(Treatment = "Fridge"), 
                   textParams = list(size = 7, family= "Arial"), segmentParams = list(linewidth =0.8)) +
  showSignificance(c(2,3), 100, -3, "*", panel = list(Treatment = "Frozen"), 
                   textParams = list(size = 7, family= "Arial"), segmentParams = list(linewidth =0.8)) +
  showSignificance(c(1,3), 110, -3, "*", panel = list(Treatment = "Frozen"), 
                   textParams = list(size = 7, family= "Arial"), segmentParams = list(linewidth =0.8)) 

k.plot