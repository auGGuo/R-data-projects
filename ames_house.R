# Ames Housing Price Prediction

# Load packages
library(rsample)
library(AmesHousing)
library(randomForest)

# Import dataset
ames <- make_ames()

# Data exploration
summary(ames)
str(ames)

dataset_size = nrow(ames)
set.seed(123)
indexes = sample(1:nrow(ames), size=floor(dataset_size/2), replace=FALSE)

train = ames[indexes, ]
test = ames[-indexes, ]

nrow(train) + nrow(test) == nrow(ames)

set.seed(666)
rf.model = randomForest(
  formula = Sale_Price ~ ., 
  data = train)

rf.model

plot(rf.model)

which.min(rf.model$mse)

min.mse = rf.model$mse[which.min(rf.model$mse)]
min.rmse = sqrt(min.mse)
min.rmse

# create a rf model including test_set
x_test = dplyr::select(test, -Sale_Price)
y_test = test$Sale_Price

rf.model.comp = randomForest(
  formula = Sale_Price ~ ., 
  data = train,
  xtest = x_test, 
  ytest = y_test
)

oob.rmse = sqrt(rf.model.comp$mse)
test.rmse = sqrt(rf.model.comp$test$mse)

# compare error rate in graph 

tibble(
  'Out of Bag Error' = oob.rmse,
  'Test Error' = test.rmse,
  'ntrees' = 1:rf.model.comp$ntree
) %>% 
  gather('Metric', 'RMSE', -ntrees) %>% 
  ggplot(aes(x=ntrees, y=RMSE, col=Metric)) +
    geom_line() +
    xlab('Number of trees') +
    scale_y_continuous(labels=scales::dollar)





