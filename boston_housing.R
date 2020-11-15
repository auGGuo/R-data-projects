# title: Boston housing data exploration 
# Sep 24 2020

housing.df <- read.csv('WestRoxbury.csv')

dim(housing.df)

head(housing.df)

View(housing.df)

# show the first 10 rows of the first column only
housing.df[1:10, 1]

# show the first 10 rows of each of the columns
housing.df[1:10,]

# show the fifth row of the first 10 columns
housing.df[5, 1:10]

# show the whole first column
housing.df[, 1]

# show the whole 'TOTAL.VALUE' column
housing.df$TOTAL.VALUE

# show the first 10 rows of the 'TOTAL.VALUE' column
housing.df$TOTAL.VALUE[1:10]

# find the length of the 'TOTAL.VALUE' column
length(housing.df$TOTAL.VALUE)

# find the mean of the 'TOTAL.VALUE' column
mean(housing.df$TOTAL.VALUE)

# find summary statistics for each column
summary(housing.df)

# random sample of 5 observations 
s <- sample(row.names(housing.df), 10)
s
housing.df[s, ]

# oversample houses with over 10 rooms
s <- sample(row.names(housing.df), 5, prob=ifelse(housing.df$ROOMS>10, 0.9, 0.1))
housing.df[s,]

# print a list of variables to the screen.
names(housing.df)

# print the list in a useful column format
t(t(names(housing.df)))

# change the first column's name
colnames(housing.df)[1] <- 'TOTAL_VALUE'

# check the class of the 'REMODEL' column
class(housing.df$REMODEL)

# change 'REMODEL' to factor
housing.df$REMODEL <- factor(housing.df$REMODEL, levels = c('None', 'Old', 
                                                              'Recent'), ordered=TRUE)

# check the level of 'REMODEL'
levels(housing.df$REMODEL)

# check BEDROOMS and TOTAL_VALUE
class(housing.df$BEDROOMS)
class(housing.df$TOTAL_VALUE)

# creating dummy variables
xtotal <- model.matrix(~ 0 + BEDROOMS + REMODEL, data = housing.df)