# Diamond Analysis 

setwd('~/Desktop/r_projects/practice')

# Load data
data('diamonds')

head(diamonds)
summary(diamonds)

# Packages
library(ggplot2)
library(scales)
library(hexbin)

# Check for NA 
colSums(is.na(diamonds))

# Check for price distribution 
ggplot(data=diamonds, aes(x=price)) +
  geom_density() +
  scale_x_log10(breaks=c(100, 1000, 10000), labels=dollar) +
  annotation_logticks(sides='bt')
# Two peaks: ~$1,000 and ~$5,000  

# Check for color distribution 
ggplot(data=diamonds, aes(x=color)) +
  geom_bar()
# Each color has sufficient quantity of diamonds. Color can a good predictor.

# Carat and price interation 
ggplot(data=diamonds, aes(x=carat, y=price)) + 
  geom_hex(binwidth=c(1, 1000)) +
  scale_y_continuous(labels=dollar) + 
  geom_smooth(color='white', se=F)

# Depth and price
ggplot(data=diamonds, aes(x=depth, y=price)) + 
  geom_hex(binwidth=c(5, 1000)) +
  scale_y_continuous(labels=dollar) + 
  geom_smooth(color='white', se=F)
# The fit line suggest depth might not be a good predictor for price 
cor(diamonds$price, diamonds$depth)

# Check x and price interation
ggplot(data=diamonds, aes(x=x, y=price)) + 
  geom_hex(binwidth=c(1, 1000)) +
  scale_y_continuous(labels=dollar) + 
  geom_smooth(color='white', se=F)
# With x greater than 3, price increase with x. 
# x smaller than 3 seem like outliers. 
xOutliers <- boxplot(diamonds$x, plot=FALSE)$out

# Check the obseravations which have x=0
diamonds[diamonds$x==0,]

# Observations with x=0 also have 0 value in y or/and z. Need to be removed 
diamonds_clean <- diamonds[diamonds$x!=0,]
diamonds_clean[diamonds_clean$x==0,]

# Check y and price interaction
ggplot(data=diamonds_clean, aes(x=y, y=price)) + 
  geom_hex(binwidth=c(5, 1000)) +
  scale_y_continuous(labels=dollar) + 
  geom_smooth(color='green', se=F) 
# y greater than 25 seem like outliers.
# Check y outliers 
yOutliers <- boxplot(diamonds_clean$y, plot=FALSE)$out

# Remove obervations with y greater 20
diamonds_clean <- diamonds_clean[diamonds_clean$y<20, ]

# Check z and price interaction
ggplot(data=diamonds_clean, aes(x=z, y=price)) + 
  geom_hex(binwidth=c(5, 1000)) +
  scale_y_continuous(labels=dollar) + 
  geom_smooth(color='green', se=F) 
# z greater than 20 seem like outliers.

# Remove observations with z greater 20
diamonds_clean <- diamonds_clean[diamonds_clean$z<20, ]

# Check table and price interaction
ggplot(data=diamonds_clean, aes(x=table, y=price)) + 
  geom_hex(binwidth=c(5, 1000)) +
  scale_y_continuous(labels=dollar) + 
  geom_smooth(color='green', se=F)
# Table greater than 90 seem like outliers. 

# Remove observations with z greater 90
diamonds_clean <- diamonds_clean[diamonds_clean$table<90, ]

# cut and price 
ggplot(diamonds_clean) + 
  geom_histogram(aes(x=price)) +
  facet_wrap(~cut, scales='free_y')

# Take subset data for investigation 
indexes <- sample(1:nrow(diamonds), 
                  size=floor(nrow(diamonds) * 0.05), 
                  replace=F)
data <- diamonds[indexes, ]

indexes2 <- sample(1:nrow(diamonds_clean), 
                   size=floor(nrow(diamonds_clean)*0.05),
                   replace=F)

data2 <- diamonds_clean[indexes2,]



# Data Partition
library(caret)
train_index <- createDataPartition(y=data$price, 
                                   p=0.7,
                                   list=F,
                                   times=1)

train <- data[train_index,]
test <- data[-train_index,]

# Function to calculate r squared 
rsq <-function(y, f) { 1 - sum((y-f)^2)/sum((y-mean(y))^2) }
# Function to calculate rmse
rmse <- function(y, f){ sqrt(mean((y-f)^2)) }

# Linear regression 
fitControl <- trainControl(method='cv', number=10, savePredictions = T)
lg_model <- train(price~., 
                  data=train,
                  method='lm',
                  trControl=fitControl)

prediction_lg <- predict(lg_model, newdata=test)
comparison <- data.frame(predict_lg=prediction_lg, true=test$price)

# Inspect prediction quality
library(ggplot2)
ggplot(data=comparison, aes(x=predict_lg, y=true)) +
  geom_point(alpha=0.2, color='blue') +
  geom_smooth(aes(x=predict_lg, y=true)) +
  geom_line(aes(x=predict_lg, y=predict_lg))

rsq_lg <- rsq(test$price, prediction_lg) # r2=0.93
rmse_lg <- rmse(test$price, prediction_lg) # rmse=1014.9

# Random forest 
rf_model <- train(price~.,
                  data=train, 
                  method='rf',
                  trControl=fitControl)

# Feature importance 
impt_rf <- varImp(rf_model)

ggplot(data=impt_rf, aes(x=impt_rf[,1])) +
  geom_boxplot() +
  ggtitle('Feature Importance for Random Forest')

# Prediction
prediction_rf <- predict(rf_model, newdata=test)
comparison$predict_rf <- prediction_rf

# Inspect performance 
ggplot(data=comparison, aes(x=predict_rf, y=true)) +
  geom_point(alpha=0.2, color='blue') +
  geom_smooth() +
  geom_line(aes(x=predict_rf, y=predict_rf))

rsq_rf <- rsq(test$price, prediction_rf) # r2=0.969
rmse <- rmse(test$price, prediction_rf) # rmse=674.5

# Support vector machine 
svm_model <- train(price~.,
                   data=train,
                   method='svmLinear',
                   trControl=fitControl)

prediction_svm <- predict(svm_model, newdata=test)
comparison$predict_svm <- prediction_svm

# Inpsect performance 
ggplot(data=comparison, aes(x=predict_svm, y=true)) +
  geom_point(alpha=0.2, color='blue') + 
  geom_smooth(lwd=3, linetype=2) +
  geom_line(aes(x=predict_svm, y=predict_svm))

rsq_svm <- rsq(test$price, prediction_svm) # r2=0.92
rmse_svm <- rmse(test$price, prediction_svm) # 1051


