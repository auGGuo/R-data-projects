# Data Visualization Part 1
# August Guo
# Oct 5 2020

# Working directory
setwd('~/Desktop/coding_club_r/CC-5-Datavis-master/')

# Libraries
library(tidyr)
library(dplyr)
library(ggplot2)
library(readr)
library(gridExtra)

# Import data 
LPI <- read.csv('LPIdata_CC.csv')

# Reshape data frame into long form
LPI.long <- gather(LPI, key = year, value = abundance, X1970:X2014)

# Change year and abundance into number
str(LPI.long)
LPI.long$year <- parse_number(LPI.long$year)
LPI.long$abundance <- as.numeric(LPI.long$abundance)
str(LPI.long)

# Pick Griffon vulture / Eurasian griffon as a subset for further analysis
vulture <- filter(LPI.long, Common.Name=='Griffon vulture / Eurasian griffon')

# There are a lot of NAs in this dataframe, so we will get rid of the empty rows using na.omit()
vulture <- na.omit(vulture)

# Histograms to visualise data distribution ----
vulture.hist <- ggplot(vulture, aes(x=abundance)) + 
  geom_histogram()
vulture.hist

# Make the histogram look better 
vulture.hist <- ggplot(vulture, aes(x=abundance)) + 
  geom_histogram(binwidth=250, col='orange4', fill='orange3') +  #Changing the binwidth and colours
  geom_vline(aes(xintercept=mean(abundance)), # Adding a line for mean abundance
             colour='red', linetype='dashed', size=1) + 
  theme_bw() + ## Changing the theme to get rid of the grey background
  xlab('\nGriffon vulture abundance') + # \n adds a blank line between axis and text
  ylab('Count\n') +
  theme(panel.grid = element_blank()) # Removing the grey grid lines

vulture.hist

# Scatter plot to examine population change over time ----
# Visualize how the Griffon vulture populations have changed between 1970 and 2017 in Croatia and in Italy.
vultureItCr <- filter(vulture, Country.list %in% c('Italy', 'Croatia'))

(vultureItCr.scat <- ggplot(vultureItCr) +
  geom_point(aes(x=year, y=abundance, colour = Country.list))
  )

# Add a smooth line: 
(vultureItCr.scat <- ggplot(vultureItCr, aes(x=year, y=abundance, colour=Country.list)) +
  geom_point(size=2) +
  geom_smooth(method = 'lm', aes(fill=Country.list))
  )


  




