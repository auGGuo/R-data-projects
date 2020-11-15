# Logistic Rregression

setwd('~/Desktop/r_projects/prac_ds_r')

load('NatalRiskData.rData')

train <- sdata[sdata$ORIGRANDGROUP<=5,]
test <- sdata[sdata$ORIGRANDGROUP>5,]

complications <- c("ULD_MECO","ULD_PRECIP","ULD_BREECH")
riskfactors <- c("URF_DIAB", "URF_CHYPER", "URF_PHYPER",
                 "URF_ECLAM")
y <- "atRisk"
x <- c("PWGT",
       "UPREVIS",
       "CIG_REC",
       "GESTREC3",
       "DPLURAL",
       complications,
       riskfactors)

fmla <- paste(y, paste(x, collapse='+'), sep=' ~ ')
fmla

model <- glm(fmla, data=train, family=binomial(link='logit'))

train$pred <- predict(model, newdata=train, type='response')
test$pred <- predict(model, newdata=test, type='response')
# type="response" tells the predict() function to return the predicted probabilities y. 
# If you don’t specify, then by default predict() will return the output of the link function, logit(y). 

library(ggplot2)

ggplot(data=train, aes(x=pred, col=atRisk, linetype=atRisk)) +
  geom_density()

# exploring modelling trade off
library(ROCR)
library(grid)

# create ROCR prediction object.
predObj <- prediction(train$pred, train$atRisk)

# create ROCR object to calculate precision as a function of threshold.
precObj <- performance(predObj, measure='prec')

# calculate recall as a function of threshold.
recObj <- performance(predObj, measure='rec')

# extract precsion 
precision <- (precObj @ y.values)[[1]]
# extract threshold
prec.x <- (precObj @ x.values)[[1]]
# extract recall 
recall <- (recObj @y.values)[[1]]

rocFrame <- data.frame(threshold=prec.x,
                       precision=precision,
                       recall=recall)

# Function to plot multiple plots on one page (stacked).
nplot <- function(plist) {
  n <- length(plist)
  grid.newpage()
  pushViewport(viewport(layout=grid.layout(n,1)))
  vplayout=function(x,y) {viewport(layout.pos.row=x, layout.pos.col=y)}
  for(i in 1:n) {
    print(plist[[i]], vp=vplayout(i,1))
  }
}

# Calculate rate of at-risk births in the training set.
pnull <- mean(as.numeric(train$atRisk))

# Plot enrichment rate as a function of threshold.
p1 <- ggplot(data=rocFrame, aes(x=threshold)) +
  geom_line(aes(y=precision/pnull)) +
  coord_cartesian(xlim =c(0, 0.05), ylim=c(0, 10))

# Plot recall as a function of threshold.
p2 <- ggplot(data=rocFrame, aes(x=threshold)) +
  geom_line(aes(y=recall)) +
  coord_cartesian(xlim =c(0, 0.05))

nplot(list(p1, p2))

# A threshold of 0.02 might be a good trade-off. The resulting classifier 
# will identify a set of potential at-risk situations that finds about half 
# of all the true at-risk situations, with a true positive rate 2.5 times 
# higher than the overall population 

# Evaluating our chosen model

# create confusion matrix
ctab.test <- table(pred=test$pred>0.02, atRisk=test$atRisk)
ctab.test

precision <- ctab.test[2,2]/sum(ctab.test[2,])
precision

recall <- ctab.test[2,2]/sum(ctab.test[,2])
recall

enrich <- precision/mean(as.numeric(test$atRisk))
enrich

# The resulting classifier is low-precision, but identifies a set of potential 
# at-risk cases that contains 55.5% of the true positive cases in the test set, 
# at a rate 2.66 times higher than the overall average.

# Finding relations and extracting advice from logistic model
coefficients(model)

# for premature baby(GESTREC3<37 weeks), coef is 1.545, so the odds of premature 
# baby is at risk is e^1.545=4.68 higher than normal baby(GESTREC3>37 wks)
# say, the prob of a normal baby having a rare disease is 1% (odds=0.01/0.99=0.0101)
# then the odds for premature baby to have the disease is 4.68*0.0101=0.047
# so the prob is 0.047/(1+0.047)=4.5%

# the coef for UPREVIS (number of prenatal medical visits) is -0.063
# every prenatal visit lower the odds of risk exp(-0.063)=0.94
# so three visits for will lower the odds of risk to 0.94*0.94*0.94=0.83
# for that mother with premature baby if she make 3 visits: the odds become
# 0.047*0.83=.039, the prob is 0.039/(1+0.039)=3.75%

# coefficients should be trusted when p values <0.05
summary(model)
