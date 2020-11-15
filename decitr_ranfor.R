# Decsison Tree and Random Forest

setwd('~/Desktop/r_projects/prac_ds_r')
url <- 'https://raw.githubusercontent.com/WinVector/zmPDSwR/master/Spambase/spamD.tsv'

spamD <- read.table(url, header=T, sep='\t')
head(spamD)

spamTrain <- subset(spamD, spamD$rgroup>=10)
spamTest <- subset(spamD, spamD$rgroup<10)

spamVars <- setdiff(colnames(spamD), list('rgroup','spam'))
spamFormula <- as.formula(paste('spam=="spam"', 
                                paste(spamVars, collapse='+'), 
                                sep='~'))

#A function to calculate log likelihood (for calculating deviance).
loglikelihood <- function(y, py) {
  pysmooth <- ifelse(py==0, 1e-12,
                     ifelse(py==1, 1-1e-12, py))
  sum(y * log(pysmooth) + (1-y)*log(1 - pysmooth))
}

#A function to calculate and return various measures on the model: normalized 
#deviance, prediction accuracy, and f1, which is the product of precision and recall.
accuracyMeasures <- function(pred, truth, name="model") {
  dev.norm <- -2*loglikelihood(as.numeric(truth), pred)/length(pred)
  # Convert the class probability estimator into a classifier by
  # labeling documents that score greater than 0.5 as spam.
  ctable <- table(truth=truth,
                  pred=(pred>0.5))
  accuracy <- sum(diag(ctable))/sum(ctable)
  precision <- ctable[2,2]/sum(ctable[,2])
  recall <- ctable[2,2]/sum(ctable[2,])
  f1 <- precision*recall
  data.frame(model=name, accuracy=accuracy, f1=f1, dev.norm)
}

library(rpart)
treemodel <- rpart(spamFormula, spamTrain)

# Evaluate the decision tree model against the training and test sets.
accuracyMeasures(predict(treemodel, newdata=spamTrain),
                 spamTrain$spam=="spam",
                 name="tree, training")

accuracyMeasures(predict(treemodel, newdata=spamTest),
                 spamTest$spam=="spam",
                 name="tree, test")

# the accuracy and F1 scores both degrade on the test set, and the
# deviance increases (we want the deviance to be small)

#Now try bagging the decision trees.
ntrain <- dim(spamTrain)[1]
# Use bootstrap samples the same size as the training set
n <- ntrain 
ntree <- 100

#Build the bootstrap samples by sampling the row indices of spamTrain
#with replacement. Each column of the matrix samples represents the
#row indices into spamTrain that comprise the bootstrap sample.
samples <- sapply(1:ntree,
                  FUN = function(iter)
                  {sample(1:ntrain, size=n, replace=T)})


# Train the individual decision trees and return them in a list.
treelist <-lapply(1:ntree,
                  FUN=function(iter)
                  {samp <- samples[,iter];
                  rpart(spamFormula, spamTrain[samp,])})

# predict.bag assumes the underlying classifier returns decision
# probabilities, not decisions.
predict.bag <- function(treelist, newdata) {
  preds <- sapply(1:length(treelist),
                  FUN=function(iter) {
                    predict(treelist[[iter]], newdata=newdata)})
  predsums <- rowSums(preds)
  predsums/length(treelist)
}

#Evaluate the bagged decision trees against the training and test sets.
accuracyMeasures(predict.bag(treelist, newdata=spamTrain),
                 spamTrain$spam=="spam",
                 name="bagging, training")

accuracyMeasures(predict.bag(treelist, newdata=spamTest),
                 spamTest$spam=="spam",
                 name="bagging, test") 

#bagging improves accuracy and F1, and reduces deviance over both the
#training and test sets when compared to the single decision tree

library(randomForest)

set.seed(5123512)
spamTrain$spam <- as.factor(spamTrain$spam)

spamFormula <- as.formula(paste('spam', 
                                paste(spamVars, collapse='+'), 
                                sep='~'))

fmodel <- randomForest(spamFormula,
  data=spamTrain,
  #Use 100 trees to be compatible with the bagging example.
  ntree=100,
  #Specify each node of a tree must have a min of 7 elements, to be compatible 
  #with the default minimum node size that rpart() uses on this training set.                     
  nodesize=7,
  importance=T)

accuracyMeasures(predict(fmodel,
                         newdata=spamTrain[,spamVars],type='prob')[,'spam'],
                 spamTrain$spam=="spam",name="random forest, train")


accuracyMeasures(predict(fmodel,
                         newdata=spamTest[,spamVars],type='prob')[,'spam'],
                 spamTest$spam=="spam",name="random forest, test")


# Assess variable importance
varImp <- importance(fmodel)
varImp[1:10,]

#The importance() function returns a matrix of importance
#measures (larger values = more important).

varImpPlot(fmodel, type=1)

#Knowing which variables are most important (or at least, which variables contribute
#the most to the structure of the underlying decision trees) can help you with variable
#reduction. This is useful not only for building smaller, faster trees, but for
#choosing variables to be used by another modeling algorithm, if that’s desired.

#Fitting with fewer variables
selVars <- names(sort(varImp[,1], decreasing=T))[1:25]

# Build a random forest model using only the 25 most important variables.
fsel <- randomForest(
  x = spamTrain[, selVars],
  y = spamTrain$spam,
  ntree = 100,
  nodesize = 7,
  importance = T
)

accuracyMeasures(predict(fsel,
                 newdata=spamTrain[,selVars],type='prob')[,'spam'],
                 spamTrain$spam=="spam",name="RF small, train")

accuracyMeasures(predict(fsel,
                         newdata=spamTest[,selVars],type='prob')[,'spam'],
                 spamTest$spam=="spam",name="RF small, test")
# The smaller model performs just as well as the random forest model built 
# using all 57 variables









