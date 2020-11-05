# Data Exploration 

# Preamble: I have a dataset of customers with known health insurance status and 
# some identified properties, such as age, employment status, income etc.

# Goal: identify the customer properties I should market inexpensive health 
# insurance packages to.

# Load data 
url <- 'https://raw.githubusercontent.com/WinVector/zmPDSwR/master/Custdata/custdata.tsv'

custdata <- read.table(url, header=T, sep='\t')

# Data exploration 
summary(custdata)

# Understand var 'income'
summary(custdata$income)
# High range of income suggest log transformation is required.


