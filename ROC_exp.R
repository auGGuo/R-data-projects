# ROC and AOC Illustration
# Oct20 2020

library(pROC)
library(randomForest)

set.seed(420)

num.samples = 100

# generate 100 random number from a normal distribution  
weight = sort(rnorm(n=num.samples, mean=172, sd=29))  

# classify if a person is obese or not obese

# runif generate n uniformly distributed 1~100 and compare to the rank
# therefore ifelse randomly decide if a person is obese or not
obese = ifelse(test=(runif(n=num.samples, min=1, max=100) < rank(weight)),
                     yes=1, no=0)
obese

plot(x=weight, y=obese)

# use glm to fit logistic regression curve to the data
glm.fit = glm(obese ~ weight, family=binomial)

lines(weight, glm.fit$fitted.values) 
# glm$fitted.values contains est prob that each sample is obese 

roc(obese, glm.fit$fitted.values, plot=TRUE)

# if we want to make the ROC graph looks nicer we specify 
# pty(plottype) as s(square)
par(pty='s')
roc(obese, glm.fit$fitted.values, plot=TRUE)

# we can fix the x axis to be 1-specificity
par(pty='s')
roc(obese, glm.fit$fitted.values, plot=TRUE, legacy.axes=TRUE)

# we can make relabel the x and y axes
roc(obese, glm.fit$fitted.values, plot=TRUE, legacy.axes=TRUE, 
    percent=TRUE, 
    xlab='False Positive Percentage',
    ylab='True Positive Percentage', 
    col='#377eb8',
    lwd=3)

# take a look of the thresholds and corresponding tpp and fpp
roc.info = roc(obese, glm.fit$fitted.values, legacy.axes=TRUE)

roc.df = data.frame(
  tpp=roc.info$sensitivities * 100,
  fpp=(1 - roc.info$specificities) * 100, 
  thresholds= roc.info$thresholds
)

head(roc.df) 
tail(roc.df)

roc.df[roc.df$tpp>60 & roc.df$tpp<80, ]

# build a random forest model 
rf.model = randomForest(factor(obese) ~ weight)

# draw ROC curve for logistic regression 
# glm.fit$fitted.values is the prob the person is obese
roc(obese, glm.fit$fitted.values, plot=TRUE, col='red', print.auc=TRUE)

# draw ROC curve for random forest 
roc(obese, rf.model$votes[, 1], plot=TRUE, col='blue', print.auc=TRUE, 
    add=TRUE, print.auc.y=0.4)


