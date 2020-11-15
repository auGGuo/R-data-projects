# Machine Learning on California Housing

# working directory 
setwd('~/Desktop/r_projects/practice')

# packages
library(ggplot2)
library(randomForest)

# data processing 
url = 'https://raw.githubusercontent.com/ageron/handson-ml/master/datasets/housing/housing.csv'
housing = read.csv(url)

head(housing)
str(housing)

# counting NA in all columns 
colSums(is.na(housing))
filter(housing, is.na(total_bedrooms))

# remove rows with total_bedrooms is NA as this is an important var
housing_clean = filter(housing, !is.na(total_bedrooms))

# data partition
dataset_size = floor(nrow(housing_clean)/2)
indexes = sample(1:nrow(housing_clean),  size=dataset_size, replace=FALSE)
train = housing_clean[indexes, ]
test = housing[-indexes, ]

# modeling
rf.model = randomForest(median_house_value ~ ., data=train, 
                        importance=T, proximities=T)

rf.model

# plot the error rate 
plot(rf.model)

# find the number of tree wt the lowest error rate 
which.min(rf.model$mse)

# RMSE of the optimal random forest
sqrt(rf.model$mse[which.min(rf.model$mse)])





