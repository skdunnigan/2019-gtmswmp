library(tidyverse)
library(ggplot2)
library(ggpubr)

se<- function(x) sd(x)/sqrt(length(x))

setwd("Z:/Research/SWMP/BIOMONITORING/EMERGENT VEGETATION/Emerg Veg Data")
SET <- read_csv("Data Subsets/2013-19SET.csv")

SET$Site <- factor(SET$Site)
SET$Station <- factor(SET$Station)

SET.1 <- SET %>%
  group_by(Season, Site) %>%
  summarize(Ht = mean(HeightAdj),
            se = se(HeightAdj))

SET.1$Season <- factor(SET.1$Season, levels = c('Spring 2013', 'Fall 2013', 'Spring 2014', 'Fall 2014', 'Spring 2015', 'Fall 2015', 'Spring 2016', 'Fall 2016', 'Spring 2017', 'Fall 2017', 'Spring 2018', 'Fall 2018', 'Spring 2019', 'Fall 2019'))

ggplot(SET.1, aes(Season, Ht, group = Site, colour = Site)) + 
  geom_line(size = 1) +
  geom_point() +
  ylim(150, 300) +
  geom_errorbar(SET.1, mapping = aes(x = Season, ymin = Ht-se, ymax = Ht+se, width = 0.3)) +
  geom_smooth(method = "lm", fill = NA, linetype = 2, size=1) +
  stat_regline_equation(
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~`, `~")),
    label.y.npc = 1) +
    #scale_color_manual() +
  theme_classic()

# --------------------------------just 22
se<- function(x) sd(x)/sqrt(length(x))

setwd("Z:/Research/SWMP/BIOMONITORING/EMERGENT VEGETATION/Emerg Veg Data")
SET <- read_csv("Z:/Research/SWMP/BIOMONITORING/EMERGENT VEGETATION/Emerg Veg Data/Data Subsets/2013-19SET.csv")

SET$Site <- factor(SET$Site)
SET$Station <- factor(SET$Station)

SET.1 <- SET %>%
  group_by(Season, Site, Station) %>%
  summarize(Ht = mean(HeightAdj),
            se = se(HeightAdj))

SET.1$Season <- factor(SET.1$Season, levels = c('Spring 2013', 'Fall 2013', 'Spring 2014', 'Fall 2014', 'Spring 2015', 'Fall 2015', 'Spring 2016', 'Fall 2016', 'Spring 2017', 'Fall 2017', 'Spring 2018', 'Fall 2018', 'Spring 2019', 'Fall 2019'))

SET.1 %>%
  filter(Site == '22') %>%
  ggplot(aes(Season, Ht, group = Station, colour = Station)) + 
  geom_line(size = 1) +
  geom_point() +
  ylim(150, 300) +
  # geom_errorbar(mapping = aes(x = Season, ymin = Ht-se, ymax = Ht+se, width = 0.3)) +
  geom_smooth(method = "lm", fill = NA, linetype = 2, size=1) +
  stat_regline_equation(
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~`, `~")),
    label.y.npc = 1) +
  #scale_color_manual() +
  theme_classic()
