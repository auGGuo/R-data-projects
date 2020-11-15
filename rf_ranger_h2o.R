
# working directory 
setwd('~/Desktop/r_projects/practice')

library(dplyr)
library(ggplot2)

library(ranger)
library(h2o)

library(rsample)
library(AmesHousing)

ames = make_ames()

# split data set 
set.seed(123)
split <- initial_split(ames, prop = 0.7, strata = 'Sale_Price')
ames_train <- training(split)
ames_test <- testing(split)


# number of features
n_features <- length(setdiff(names(ames), 'Sale_Price'))

# train a default random forest model
ames_rf1 <- ranger(
  Sale_Price ~., 
  data = ames_train, 
  mtry = floor(n_features / 3),
  respect.unordered.factors = 'order', 
  seed = 123
)

# get OOB RMSE
(default_rmse <- sqrt(ames_rf1$prediction.error))

# create hyperparameter grid
hyper_grid <- expand.grid(
  mtry = floor(n_features * c(.05, .15, .25, .333, .4)),
  min.node.size = c(1, 3, 5, 10),
  replace = c(TRUE, FALSE),
  sample.fraction = c(.5, .63, .8),
  rmse = NA
) 

# do a full Cartesian search with ranger

for (i in 1:nrow(hyper_grid)) {
  # fit model for ith hyperparameter combination
  fit <- ranger(
    Sale_Price ~.,
    data = ames_train,
    num.trees = n_features * 10,
    mtry = hyper_grid$mtry[i],
    min.node.size =hyper_grid$min.node.size[i],
    replace = hyper_grid$replace[i],
    sample.fraction = hyper_grid$sample.fraction[i],
    verbose = FALSE,
    seed = 123, 
    respect.unordered.factors = 'order'
  )
  
  # export OOB error 
  hyper_grid$rmse[i] <- sqrt(fit$prediction.error)
}

# assess top 10 models
hyper_grid %>% 
  arrange(rmse) %>% 
  mutate(perc_gain = (default_rmse - rmse) / default_rmse * 100) %>% 
  head(10)

# fit a random forest model with h2o
h2o.no_progress()
h2o.init(max_mem_size = '5g')

# convert training data to h2o object
train_h2o <- as.h2o(ames_train)

# set the response column to Sale_Price
response <-'Sale_Price'

# set the predictor names
predictors <- setdiff(names(ames_train), response)

h2o_rf1 <- h2o.randomForest(
  x = predictors,
  y = response,
  training_frame = train_h2o,
  ntrees = n_features * 10,
  seed = 123
)

h2o_rf1

# save the model on the disk 
saveRDS(h2o_rf1, 'h2o_rf1_ames.dat')

# load the model 
h2o_rf2 = readRDS('h2o_rf1_ames.dat')
h2o_rf2

# hyperparameter grid
hyper_grid <- list(
  mtries = floor(n_features * c(.05, .15, .25, .333, .4)),
  min_rows = c(1, 3, 5, 10), # min.node.size
  max_depth = c(10, 20, 30),
  sample_rate = c(.55, .632, .7, .8)
)

# random grid search strategy
search_criteria <- list(
  strategy = 'RandomDiscrete',
  stopping_metric = 'mse',
  stopping_tolerance = 0.001, # stop if improvement is < 0.1%
  stopping_rounds = 10, # over the last 10 models
  max_runtime_secs = 60*5 # or stop search after 5 min.
)

# perform grid search 
random_grid <- h2o.grid(
  algorithm = 'randomForest',
  grid_id = 'rf_random_grid',
  x = predictors, 
  y = response, 
  training_frame = train_h2o,
  hyper_params = hyper_grid,
  ntrees = n_features * 10, 
  seed = 123, 
  stopping_metric = 'RMSE',
  stopping_rounds = 10,  # stop if last 10 trees added
  stopping_tolerance = 0.005, # don't improve RMSE by 0.5%
  search_criteria = search_criteria
)

# collect the results and sort by our model performance metric of choice

random_grid_perf <- h2o.getGrid(
  grid_id = 'rf_random_grid',
  sort_by = 'mse',
  decreasing = FALSE
) 
 
random_grid_perf

# feature interpretation 
# use the best hyperparameters identified from the grid search 
rf_impurity <- ranger(
  formula = Sale_Price ~., 
  data = ames_train,
  num.trees = 2000,
  mtry = 26, 
  min.node.size = 3,
  sample.fraction = .8,
  replace = FALSE,
  importance = 'impurity',
  respect.unordered.factors = 'order',
  verbose = T,
  seed = 123
)

rf_permutation <-ranger(
  formula = Sale_Price ~., 
  data = ames_train, 
  x = features,
  y = response, 
  sample.fraction = 0.8,
  min.node.size = 3,
  num.trees = 2000,
  mtry = 26,
  replace = FALSE,
  importance = 'permutation',
  respect.unordered.factors = 'order',
  verbose = FALSE,
  seed = 123
)

p1 <- vip::vip(rf_impurity, num_features = 25, bar = T)
p2 <- vip::vip(rf_permutation, num_features = 25, bar = T)

gridExtra::grid.arrange(p1, p2, nrow = 1)
