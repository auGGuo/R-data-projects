# Basic Data Manipulation 

## Packages ----
library(tidyr)
library(dplyr)

## set working directory ----
setwd('~/Desktop/coding_club_r/CC-3-DataManip-master')

## Load data set ----
elongation <- read.csv('EmpetrumElongation.csv', header = TRUE)

## Check import and preview of the data ---
head(elongation) # first 6 observations
str(elongation)  # types of variables 

elongation$Indiv 
length(unique(elongation$Indiv)) 

# find out the ID for the sixth observation
elongation[6,]$Indiv

# access the values for Individual number 603
elongation[elongation$Indiv == 603, ]

## Subsetting with one condition ----
# returns only the data for zones 2-3
elongation[elongation$Zone < 4, ]

# Returns only the data for zones 2-3-4
elongation[elongation$Zone <= 4, ]

# Use ! to return the same data as last statement 
elongation[!elongation$Zone >= 5, ]

## Subsetting with two conditions ----
# Returns only data for zones 2 and 7
elongation[elongation$Zone == 2 | elongation$Zone == 7, ]

# Returns data for shrubs in zone 2 whose ID numbers are between 300 and 400
elongation[elongation$Zone == 2 & elongation$Indiv %in% c(300:400), ]

## CHANGING VARIABLE NAMES AND VALUES IN A DATA FRAME ----
elong2 <- elongation
names(elong2)

#Changing Indiv to ID
names(elong2)[2] <- 'ID'

# fix a mistake in the data, and the value 5.1 for individual 
# 373 in year 2008 should really be 5.7
elong2[elong2$ID == 373, 'X2008'] <- 5.7

## CREATING A FACTOR ----
str(elong2)

# Turn Zone into factor 
elong2$Zone <- as.factor(elong2$Zone)
class(elong2$Zone) 

## CHANGING A FACTOR'S LEVELS
# Shows the different factor levels
levels(elong2$Zone) 

# Overwrite the original levels with new names
levels(elong2$Zone) <- c('a', 'b', 'c', 'd', 'e', 'f')

## Data manipulation ----
# Turn the data frame into long formate with each row and column corresponding 
# to observation and a varibale
elongation_long <- gather(elongation, Year, Length, 
                          c(X2007, X2008, X2009, X2010, X2011, X2012))

# Alternatively, we can specify the col numbers 
elongation_long <- gather(elongation, Year, Length, c(3:8))

# spread() is the inverse function, allowing table to go from long 
# to wide format
elongation_wide <- spread(elongation_long, Year, Length)

# now visualize with long format data
boxplot(Length~Year, data = elongation_long, xlab = 'Year', 
        ylab = 'Elongation(cm)', 
        main = "Annual growth of Empetrum hermaphroditum")

## Data manipulation with dplyr ----
## Rename function (new name = old name) ----
elongation_long <- rename(elongation_long, zone = Zone, indiv = Indiv, 
                          year = Year, length = Length)

## filter function for subsetting rows ----
# keep observations from zones 2 and 3 only, and from years 2009 to 2011
elong_subset <- filter(elongation_long, zone %in% c(2,3), 
                       year %in% c('X2009', 'X2010', 'X2011'))

# For comparison, the base R equivalent would be
elongation_long[elongation_long$zone %in% c(2,3) 
                & elongation_long$year %in% c('X2009', 'X2010', 'X2011'), ]

## SELECT COLUMNS ----
# ditch the zone column
elong_no.zone <- select(elongation_long, indiv, year, length)
# alternatively using minus 
elong_no.zone <- select(elongation_long, -zone)

# For comparison, the base R equivalent would be (not assigned to an object here):
elongation_long[ , -1]  # removes first column

# select() allows rename and reorder columns on the fly
elong_no.zone <- select(elongation_long, Year = year, Shrub.ID = indiv, 
                       Growth = length)

## MUTATE to create new columns ----
elong_total <- mutate(elongation, total.growth = X2007 + X2008 + X2009 + 
                        X2010 + X2011 + X2012)

## GROUP_BY: creates an internal grouping structure ----
elong_grouped <- group_by(elongation_long, Indiv) 

## SUMMARISE data with a range of summary statistics ----
summary1 <- summarise(elongation_long, total.growth = sum(Length))
summary2 <- summarise(elong_grouped, total.growth = sum(Length))

summary3 <- summarise(elong_grouped, total.growth = sum(Length),
                                     mean.growth = mean(Length),
                                    sd.growth = sd(Length))

## JOIN for merging two data sets into one ----

# Load the treatments associated with each individual
treatments <- read.csv('EmpetrumTreatments.csv', header=TRUE, sep = ';')
head(treatments)

elongation_long <- rename(elongation_long, 'zone' = 'Zone', 'indiv' = 'Indiv', 
                          'year' = 'Year', 'length' = 'Length')

# Join the two data frames by indiv and zone. The column names are spelled differently, 
# so we need to tell the function which columns represent a match. 
# We have two columns that contain the same information in both datasets: 
# zone -Zone and indiv-Indiv.

experiment1 <- left_join(elongation_long, treatments, 
                         by = c('indiv' = 'Indiv', 'zone' = 'Zone'))

# The equivalent base R function is merge() as shown below
experiment2 <- merge(elongation_long, treatments, by.x = c('indiv', 'zone'),
                     by.y = c('Indiv', 'Zone'))


# let’s check if they affect growth by drawing another box plot
boxplot(length ~ Treatment, data = experiment1, main = 'Data from Experiment1')
boxplot(length ~ Treatment, data = experiment2, main = 'Data from Experiment2')
