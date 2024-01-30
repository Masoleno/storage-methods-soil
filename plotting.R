
## Load libraries
library(tidyverse)
library(ggpubr)
library(gridExtra)
library(superb)

## Read in data
tidyDataDry <- read.csv("chemistry-data-tidy-dryCorrected.csv")

str(tidyDataDry)

## Convert weeks to factor so it can be used to group the plots
tidyDataDry$Weeks <- as.factor(tidyDataDry$Weeks)
str(tidyDataDry)

## Rename columns to make things easier 
tidyDataDry<- tidyDataDry %>% 
  rename(
    "NH4" = NH4..mg.kg.,
    "K" = K..mg.kg.,
    "NO2" = NO2..mg.kg.,
    "NO3" = NO3..mg.kg.,
    "Moisture" = Moisture....
  )

head(tidyDataDry)

## Replacing negative values in the moisture and nitrate columns with 0. 
tidyDataDry <- tidyDataDry%>% 
  mutate(across(c(Moisture, NO3:NH4), ~ ifelse(.x<0, 0, .x)))


tail(tidyDataDry, 20)

## Make frequency historgrams
for (i in 3:7) {
  plot1 <- ggplot(tidyDataDry, aes(tidyDataDry[,i])) +
    geom_histogram() +
    labs(x = colnames(tidyDataDry[i]))
  print(plot1)
  
}


## Boxplots
# plot.list <- list()
# for(i in 4:7){
#   plot <- ggplot(tidyDataDry, aes(x = Weeks, y = tidyDataDry[,i])) +
#     stat_boxplot(aes(Weeks, tidyDataDry[,i]), geom = "errorbar") +
#     geom_boxplot(aes(Weeks, tidyDataDry[,i]), outlier.shape = NA, coef = 0) +
#     ylab(colnames(tidyDataDry[i])) +
#     facet_wrap(.~Treatment) +
#     theme(text = element_text(family = "Arial", size = 16), panel.background = element_rect(fill = NA, colour = 'black')) +
#     scale_x_discrete(labels = c("Fresh control", "7", "24")) +
#     scale_y_continuous(limits = c(min(tidyDataDry[,i]), max(tidyDataDry[,i])))
#   plot.list[[i]] <- plot
#   #ggsave(file = paste0("boxplot-by-treatment-", colnames(tidyData[i]), ".jpeg"), plot = plot, height = 7, width = 9, units = "in")
# }

# no3.plot <- plot.list[[4]]
# no2.plot <- plot.list[[5]]
# nh4.plot <- plot.list[[6]]
# k.plot <- plot.list[[7]]

### pH
pH_plot <- ggplot(tidyDataDry, aes(x = Weeks, y = pH)) +
  stat_boxplot(aes(Weeks, pH), geom = "errorbar") +
  geom_boxplot(aes(Weeks, pH), outlier.shape = NA, coef = 0) +
  ylab("pH") +
  facet_wrap(.~Treatment) +
  theme(text = element_text(family = "Arial", size = 16), 
        panel.background = element_rect(fill = NA, colour ='black')) +
  scale_x_discrete(labels = c("Fresh control", "7", "24")) +
  scale_y_continuous(limits = c(5.3, 6.9))

ph.plot <- pH_plot + 
  showSignificance(c(2,3), 6.6, -0.07, "***", panel = list(Treatment = "Dry")) +
  showSignificance(c(1,3), 6.7, -0.09, "**", panel = list(Treatment = "Dry")) +
  showSignificance(c(1,2), 6.67, -0.07, "***", panel = list(Treatment = "Fridge")) +
  showSignificance(c(2.2,3), 6.64, -0.07, "***", panel = list(Treatment = "Fridge")) +
  showSignificance(c(1,3), 6.8, -0.07, "***", panel = list(Treatment = "Fridge")) +
  showSignificance(c(2,3), 6.6, -0.07, "***", panel = list(Treatment = "Frozen"))

ph.plot

ggsave(file = "pH-boxplot.jpeg", pH.plot, height = 7, width = 9, units = "in")

## NO3
no3_plot <- ggplot(tidyDataDry, aes(x = Weeks, y = NO3)) +
  stat_boxplot(aes(Weeks, NO3), geom = "errorbar") +
  geom_boxplot(aes(Weeks, NO3), outlier.shape = NA, coef = 0) +
  ylab(bquote(NO[3]-N (mg/kg))) +
  facet_wrap(.~Treatment) +
  theme(text = element_text(family = "Arial", size = 16), 
        panel.background = element_rect(fill = NA, colour ='black')) +
  scale_x_discrete(labels = c("Fresh control", "7", "24")) +
  scale_y_continuous(limits = c(0,60), n.breaks = 8)

no3_plot

no3.plot <- no3_plot + 
  showSignificance(c(1.05,2.2), 43, - 3, "***", panel = list(Treatment = "Dry")) +
  showSignificance(c(1,3), 45, -5, "**", panel = list(Treatment = "Dry")) +
  showSignificance(c(1.1,2), 52, -5, "**", panel = list(Treatment = "Fridge")) +
  showSignificance(c(2.2,3), 49, -3, "***", panel = list(Treatment = "Fridge")) +
  showSignificance(c(1,3), 55, -5, "***", panel = list(Treatment = "Fridge")) +
  showSignificance(c(2,3), 45, -3, "***", panel = list(Treatment = "Frozen")) +
  showSignificance(c(1,3), 49, -3, "***", panel = list(Treatment = "Frozen"))

no3.plot

#ggsave(file = "NO3-boxplot.jpeg", no3.plot, height = 7, width = 9, units = "in")

## NO2
no2_plot <- ggplot(tidyDataDry, aes(x = Weeks, y = NO2)) +
  stat_boxplot(aes(Weeks, NO2), geom = "errorbar") +
  geom_boxplot(aes(Weeks, NO2), outlier.shape = NA, coef = 0) +
  ylab(bquote(NO[2]-N (mg/kg))) +
  facet_wrap(.~Treatment) +
  theme(text = element_text(family = "Arial", size = 16), 
        panel.background = element_rect(fill = NA, colour ='black')) +
  scale_x_discrete(labels = c("Fresh control", "7", "24")) +
  scale_y_continuous(limits = c(0,3), n.breaks = 8)

no2_plot

no2.plot <- no2_plot + 
  showSignificance(c(0.9,2), 1.3, - 0.05, "***", panel = list(Treatment = "Dry")) +
  showSignificance(c(1,2), 1.7, -0.05, "*", panel = list(Treatment = "Frozen")) +
  showSignificance(c(1,3), 3, -0.1, "***", panel = list(Treatment = "Frozen"))

no2.plot

## NH4
nh4_plot <- ggplot(tidyDataDry, aes(x = Weeks, y = NH4)) +
  stat_boxplot(aes(Weeks, NH4), geom = "errorbar") +
  geom_boxplot(aes(Weeks, NH4), outlier.shape = NA, coef = 0) +
  ylab(bquote(NH[4]-N (mg/kg))) +
  facet_wrap(.~Treatment) +
  theme(text = element_text(family = "Arial", size = 16), 
        panel.background = element_rect(fill = NA, colour ='black')) +
  scale_x_discrete(labels = c("Fresh control", "7", "24")) +
  scale_y_continuous(limits = c(0,60), n.breaks = 8)

nh4_plot

nh4.plot <- nh4_plot + 
  showSignificance(c(1,2), 52, - 3, "***", panel = list(Treatment = "Dry")) +
  showSignificance(c(0.9,3), 54, - 3, "***", panel = list(Treatment = "Dry")) +
  showSignificance(c(1,3), 16, -3, "*", panel = list(Treatment = "Frozen")) 

nh4.plot


## K
k_plot <- ggplot(tidyDataDry, aes(x = Weeks, y = K)) +
  stat_boxplot(aes(Weeks, K), geom = "errorbar") +
  geom_boxplot(aes(Weeks, K), outlier.shape = NA, coef = 0) +
  ylab(bquote(K (mg/kg))) +
  facet_wrap(.~Treatment) +
  theme(text = element_text(family = "Arial", size = 16), 
        panel.background = element_rect(fill = NA, colour ='black')) +
  scale_x_discrete(labels = c("Fresh control", "7", "24")) +
  scale_y_continuous(limits = c(0,max(tidyDataDry$K)), n.breaks = 8)

k_plot

k.plot <- k_plot + 
  showSignificance(c(1,1.9), 102, - 3, "***", panel = list(Treatment = "Dry")) +
  showSignificance(c(2,3), 104, - 3, "***", panel = list(Treatment = "Dry")) +
  showSignificance(c(1,3), 108, - 3, "***", panel = list(Treatment = "Dry")) +
  showSignificance(c(1,2), 111, -3, "***", panel = list(Treatment = "Fridge")) +
  showSignificance(c(0.9,3), 115, -3, "***", panel = list(Treatment = "Fridge")) +
  showSignificance(c(2,3), 100, -3, "*", panel = list(Treatment = "Frozen")) +
  showSignificance(c(1,3), 104, -3, "*", panel = list(Treatment = "Frozen")) 

k.plot

## Plotting Nitrogen line graph (fig. 7)
N_data <- dryData %>%
  select(Sample.ID, `NO3 (mg/kg)`, `NO2 (mg/kg)`, `NH4 (mg/kg)`, Treatment, Weeks)

head(N_data)


N_data[!complete.cases(N_data), ]


N_data <- N_data %>%
  drop_na()

N_data[!complete.cases(N_data), ]


## Make data into long format for plotting on line graph
N_data_long <- N_data %>%
  pivot_longer(cols = `NO3 (mg/kg)`:`NH4 (mg/kg)`,
               names_to = "Nitrogen compound",
               values_to = "Concentration (mg/kg)")

head(N_data_long, 20)

## Calculate summary statistics
summaryN <- N_data_long %>%
  group_by(Treatment, Weeks, `Nitrogen compound`) %>%
  summarise(
    meanConc = mean(`Concentration (mg/kg)`),
    sdConc = sd(`Concentration (mg/kg)`)
  )

summaryN

summaryN[!complete.cases(summaryN), ]

## Make the plot (fig. 7)
N_plot <- ggplot(summaryN, aes(x = factor(Weeks), y = meanConc, color = factor(`Nitrogen compound`), group = `Nitrogen compound`)) +
  geom_line() +
  geom_point() +
  facet_wrap(~Treatment) +
  theme(text = element_text(family = "Arial", size = 16), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), legend.title = element_text(size = 12), 
        legend.position = "bottom") +
  labs(x = "Time (weeks)", y = "Mean concentration (mg/kg)", shape = "Nitrogen form") +
  scale_color_discrete(labels = c("NH4", "NO2", "NO3"))



N_plot + scale_color_manual(values = c("#E69F00", "#009E73",  "#56B4E9"))

ggsave(filename = "Nitrogen-linegraph.jpeg", plot = N_plot,  width = 9, unit = "in")





