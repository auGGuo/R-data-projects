#  Exploratory Data Analysis of Health Insurance Dataset 

# Preamble: I have a dataset of customers with known health insurance status and 
# some identified properties, such as age, employment status, income etc.

# Goal: identify the customer properties I should market inexpensive health 
# insurance packages to.

# Load data 
url <- 'https://raw.githubusercontent.com/WinVector/zmPDSwR/master/Custdata/custdata.tsv'

custdata <- read.table(url, header=T, sep='\t')

# Load packages
library(ggplot2)
library(scales)
library(dplyr)

# Data exploration 
summary(custdata)

# Understand variable 'income'
summary(custdata$income)
# High range of income is hard to model. We will need to do log transformation.
# Also need to handle negative income values.

# Visualize 'income' distribution
ggplot(data=custdata) +
  geom_density(aes(x=income)) +
  scale_x_continuous(labels=dollar)
# Spot a small subpopulation at the income range of ~$400,000

# With a big range, hard to see the distribution around ~$100,000. 
# Do log transformation.
ggplot(data=custdata) + 
   geom_density(aes(x=income)) +
   scale_x_log10(breaks=c(100, 1000, 10000, 100000), labels=dollar) +
   annotation_logticks(sides='bt')
# Income peaks at 40,000. Most customers have income of $20,000-100,000.
# Use log scale when % change or change in order of magnitude is more important
# than change in absolute value. Log transformation also helps to visualize 
# highly skewed data. 

# Understand variable 'age' 
summary(custdata$age)
outAge <- boxplot(custdata$age, plot=F)$out
ratioAge <- sd(custdata$age, na.rm=T)/mean(custdata$age, na.rm = T)
# 0 could mean 'unknown'; identify outliers and store in outAge. 
# If ratioAge is too small, it suggests age is not a good predictor. 

# Visualize 'age' distribution
ggplot(data=custdata) +
  geom_histogram(aes(x=age), binwidth=5, fill='green')

# Understand marital status 
ggplot(data=custdata) + 
  geom_bar(aes(x=marital.stat))
# Bar chart is a histogram for discrete data. It shows the frequency distribution
# for each catagorical variable. If we believe marital status will be a good 
# indicator for having insurance or not, we would want to have enough customers 
# with different marital status to help us discover the relationship.

# Understand the state of residence distribution 
ggplot(data=custdata) +
  geom_bar(aes((x=state.of.res))) +
  coord_flip()
# Reorder the bar chart for better visualization 
statesums <- table(custdata$state.of.res)

statedf <- as.data.frame(statesums) %>% 
  rename(state.of.res=Var1, count=Freq)

statedf <- transform(statedf, state.of.res=reorder(state.of.res, count))

ggplot(data=statedf) + 
  geom_bar(aes(x=reorder(state.of.res, -count), y=count)) +
  coord_flip()





