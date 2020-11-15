#KerneL Method 

# An artificial kernel example
u <- c(1,2)
v <- c(3,4)

#Define a function of two vector variables (both two dimensional) as the sum of
#various products of terms.
k <- function(u,v) {
  u[1]*v[1] + u[2]*v[2] +
    u[1]*u[1]*v[1]*v[1] + u[2]*u[2]*v[2]*v[2] +
    u[1]*u[2]*v[1]*v[2]
}

# Define a function of a single vector variable that returns a vector
# containing the original entries plus all products of entries.  
phi <- function(x) {
    x <- as.numeric(x)
    c(x,x*x,combn(x,2,FUN=prod))
}  

print(k(u,v))
print(phi(u))
print(phi(v))
print(as.numeric(phi(u) %*% phi(v)))

#Applying stepwise linear regression to PUMS data
load('psub.RData')

dtrain <- subset(psub, psub$ORIGRANDGROUP>=500)
dtest <- subset(psub, psub$ORIGRANDGROUP<500)

#Ask that the linear regression model we’re building be stepwise improved, which 
#is a powerful automated procedure for removing variables that don’t seem to have 
#significant impacts (can improve generalization performance).
m1 <- step(
  lm(log(PINCP, base=10) ~ AGEP + SEX + COW + SCHL,
     data=dtrain),
  direction='both'
  )

#Define the RMSE function.
rmse <- function(y, f){ sqrt( mean( (y-f)^2 ) ) }

print(rmse(log(dtest$PINCP, base=10),
           predict(m1, newdata=dtest)
           )
      )

#The quality of this prediction was middling (the RMSE isn’t that small), but the model
#exposed some of the important relationships. In a real project, you’d do your utmost
#to find new explanatory variables. But you’d also be interested to see if any combination
#of variables you were already using would help with prediction. We’ll work
#through finding some of these combinations using an explicit phi().


#Explicit kernel transforms are a formal way to unify ideas like reshaping 
#variables and adding interaction terms.

#Now apply explicit kernel transform
#Define our primal kernel function: map a vector to a copy of itself
#plus all square terms and crossmultiplied terms.
phi <- function(x) {
  x <- as.numeric(x)
  c(x,x*x,combn(x,2,FUN=prod))
}

# to be continued when the brain can process this math intense section again!!!















































