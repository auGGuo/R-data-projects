# Linear Rregression

setwd('~/Desktop/r_projects/prac_ds_r')

load('psub.RData')

head(psub)

dtrain <- subset(psub, ORIGRANDGROUP >= 500)
dtest <- subset(psub, ORIGRANDGROUP < 500)

model <- lm(log(PINCP, base=10) ~ AGEP + SEX + COW + SCHL, data=dtrain)

dtest$predLogPINCP <- predict(model, newdata=dtest)
dtrain$predLogPINCP <- predict(model, newdata=dtrain)

# inspect prediction quality 
ggplot(data=dtest, mapping=aes(x=predLogPINCP, y=log(PINCP, base=10))) +
  geom_point(alpha=.2, col='black') +
  geom_smooth(aes(x=predLogPINCP, y=log(PINCP, base=10)), col='black') +
  geom_line(aes(x=log(PINCP, base=10), y=log(PINCP, base=10)), 
            col='blue', linetype=2) +
  scale_x_continuous(limits=c(4, 5.5)) +
  scale_y_continuous(limits=c(3.5, 5.5))

# If the predictions are very good, then the plot will be dots
# arranged near the line y=x, which is the line of perfect prediction
# however the graph shows the dots scatter widely along the blue line 
# suggesting low quality fit 


# calculate r squared 
rsq <-function(y, f) { 1 - sum((y-f)^2)/sum((y-mean(y))^2) }
rsq(log(dtrain$PINCP, base=10), predict(model, newdata = dtrain))
rsq(log(dtest$PINCP, base=10), predict(model, newdata = dtest))
# We want to see R-squares higher than this (say, 0.7–1.0).
# A significantly lower R-squared on test data compared to train data suggests 
# an overfit model that looks good in training and won’t work in production.

# calculate rmse
rmse <- function(y, f){ sqrt(mean((y-f)^2)) }
rmse(log(dtrain$PINCP, base=10), predict(model, newdata=dtrain))
rmse(log(dtest$PINCP, base=10), predict(model, newdata=dtest))

# how does education(SCHL) affect income(PINCP)
coefficients(model)

# coefficient interpreation ----
# The level (no high school diploma) not shown is called the reference level;
# SCHLBachelor's degree we find the coefficient 0.39, which is read as
# “The model gives a 0.39 bonus to log income for having a bachelor’s degree 
# This means that the income ratio between someone with a bachelor’s degree 
# and the equivalent person (same sex, age, and class of work) without a high 
# school degree is about 10^0.39, or 2.45 times higher.

# The modeled relation between the bachelor’s degree holder’s expected income 
# and high schoolgraduate’s (all other variables being equal) is 10^(0.39-0.10), 
# or about 1.8 times greater.


# check coefficients are reliable ----
summary(model)


