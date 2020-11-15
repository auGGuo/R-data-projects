# Generalized Addictive Model (GAM)

setwd('~/Desktop/r_projects/prac_ds_r')

set.seed(602957)

# create artificial dataset 

x <- rnorm(1000)
noise <- rnorm(1000, sd=1.5)

y <- 3*sin(2*x) + cos(0.75*x) - 1.5*(x^2 ) + noise

select <- runif(1000)

#hist(x)
#hist(y)
#hist(select)

frame <- data.frame(x=x, y=y)

train <- frame[select>0.1, ]
test <- frame[select<=0.1, ]

#hist(train$x)
#hist(train$y)

# Try linear regression
lin.model <- lm(y ~ x, data=train)
summary(lin.model)
# r2 is 0.04 suggesting poor fit

# calculate the root mean squared error (rmse)
resid.lin <- train$y - predict(lin.mdoel)
sqrt(mean(resid.lin^2))

library(ggplot2)
ggplot(data=train) +
  geom_point(aes(x=x, y=y), alpha=.2) +
  geom_line(aes(x=x, y=predict(lin.mdoel)))

# try GAM 
library(mgcv)

# Then gam() will search for the spline s() that best describes the relationship 
# between x and y. Only terms surrounded by s() get the GAM/spline treatment.
glin.model <- gam(y ~ s(x), data=train)

#The converged parameter tells us if the algorithm converged.
# We should only trust the output if this is TRUE.
glin.model$converged

summary(glin.model)
# r square
cor(train$y, predict(glin.model))^2

#“R-sq (adj)” is the adjusted R-squared. “Deviance explained” is
#the raw R-squared (0.834).

#calculate the root mean squared error (rmse)
resid.glin <- train$y - predict(glin.model)
sqrt(mean(resid.glin^2))

ggplot(data=train) +
  geom_point(aes(x=x, y=y), alpha=0.2) +
  geom_line(aes(x=x, y=predict(glin.model)), color='blue')

# Comparing linear regression and GAM performance
actual <- test$y
pred.lin <- predict(lin.model, newdata = test)
pred.glin <- predict(glin.model, newdata = test)

resid.lin <- actual - pred.lin
resid.glin <- actual - pred.glin

sqrt(mean(resid.lin^2))
sqrt(mean(resid.glin^2))

# Compare the R-squared of the linear model and the GAM on test data.
cor(actual, pred.lin)^2
cor(actual, pred.glin)^2

#The GAM performed similarly on both sets (RMSE of 1.40 on test versus 1.45 on training;
#R-squared of 0.78 on test versus 0.83 on training). So there’s likely no overfit

#Extracting the nonlinear relationships
plot(glin.model)

#We can extract the data points that were used to make this graph by using the
#predict() function with the argument type="terms". 

sx <- predict(glin.model, type="terms")

summary(sx)

xframe <- cbind(train, sx=sx[,1])

ggplot(xframe, aes(x=x)) + geom_point(aes(y=y), alpha=0.4) +
  geom_line(aes(y=sx))


# Predict a newborn baby’s weight (DBWT). As input, consider mother’s weight (PWGT), 
# mother’s pregnancy weight gain (WTGAIN), mother’s age (MAGER), and the number of 
# prenatal medical visits (UPREVIS)

library(mgcv)
library(ggplot2)

load("NatalBirthData.rData")
train <- subset(sdata, sdata$ORIGRANDGROUP<=5)
test <- sdata[sdata$ORIGRANDGROUP>5,]

# build linear reg model 
form.lin <- as.formula('DBWT ~ PWGT + WTGAIN + MAGER + UPREVIS')
lin.model <- lm(form.lin, data=train)

summary(lin.model)

# build GAM model 
form.glin <- as.formula('DBWT ~ s(PWGT) + s(WTGAIN) + s(MAGER) + s(UPREVIS)')
glin.model <- gam(form.glin, data=train)

glin.model$converged

summary(glin.model)
#The GAM has improved the fit, and all four variables seem to have a nonlinear 
#relationship with birth weight, as evidenced by edfs all greater than 1.

#Check for overfit with hold-out data.
pred.lin <- predict(lin.model, newdata=test)
pred.glin <- predict(glin.model, newdata=test)

cor(pred.lin, test$DBWT)^2
cor(pred.glin, test$DBWT)^2
#The performance of the linear model and the GAM were similar on the test set, as they
#were on the training set, so there’s no substantial overfit.

#Using GAM for logistic regression
#predict the birth of underweight babies (defined as DBWT < 2000)

#linear model 
form <- as.formula('DBWT<2000 ~ PWGT + WTGAIN + MAGER + UPREVIS')
logmod <- glm(form, data=train, family=binomial(link='logit'))

summary(logmod)

#GAM 
form2 <- as.formula('DBWT<2000 ~ s(PWGT) + s(WTGAIN) + s(MAGER) + s(UPREVIS)')
glogmodel <-gam(form2, data=train, family=binomial(link='logit'))

glogmodel$converged

summary(glogmodel)
