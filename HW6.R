# CRIM 4012
# Assignment #6
# Saara Ghani; 15486636

setwd("/Users/saaraghani/Desktop/Spring24/CRIM4012")

#### Question 1 ----
X <- rbind(c(1,1),
           c(1,2),
           c(1,3))

y <- rbind(c(4),
           c(5),
           c(6))

# Q1a.
t(X) %*% X

# Q1b.
solve(t(X) %*% X)

# Q1c.
solve(t(X) %*% X) %*% t(X)

# Q1d.
solve(t(X) %*% X) %*% t(X) %*% y

# Q1e.
lm(y~-1+X) 
# the -1 indicates that we should not include the intercept in the linear 
# regression model


#### Question 2 ----
set.seed(20240224)
x <- 1:10
y <- -1 + 2*x + rnorm(10)

# Q2a.
X <- cbind(1, x)
X

# Q2b.
bHat <- solve(t(X) %*% X) %*% t(X) %*% y
bHat

# Q2c. 
lm(y~x)

# Q2d.
x0 <- c(1,2)
predX0 <- bHat %*% x0
predX0

# Q2e.
plot(x,y)
points(x = x0[2], y = predX0[2], col = "purple2")


# Q2f.
w <- X %*% solve(t(X) %*% X) %*% rbind(c(1),c(2))
w
# The observations with higher values have greater weight in making a 
# prediction at x0 = 2/


#### Question 3 ----
X <- cbind(1, c(66,69,76,72), c(1.6764,1.7526,1.9304,1.8288))

# Q3a.
solve(t(X) %*% X)

# Error message: 
# Error in solve.default(t(X) %*% X) : 
# system is computationally singular: reciprocal condition number = 7.58483e-20

# Reason for above error message:
# The matrix is computational singular, which means that the matrix can not be 
# inverted because it has a determinant of zero (or close to zero). This happens 
# when columns in the matrix are linearly dependant (which occurs because 
# columns 2 and 3 are perfectly correlated).

# 3b.
lambda <- 0.00001
lambdaI <- diag(c(0,lambda,lambda))
solve(lambdaI + (t(X) %*% X))
solve(diag(c(0,1,1)) + (t(X) %*% X)) # I get a matrix!

# 3c. 
y <- c(186,263,246,287)
betaHat <- solve(t(X) %*% (X) + lambdaI) %*% t(X) %*% y

plot(X[,2],y)
lines(x = X[,2], y = X %*% betaHat, col = "purple")

# Do the predicted values seem reasonable?
# The predicted values lie in the middle of the actual values, and appear to 
# form a line of best fit. They are far off from the actual values but do seem 
# to represent a reasonable average prediction as they lie an even distance 
# from each point, with two points above and two points below the line.

# 3d.
lambda <- 1000
lambdaI <- diag(c(0,lambda,lambda))
betaHat <- solve(t(X) %*% (X) + lambdaI) %*% t(X) %*% y

plot(X[,2],y)
lines(x = X[,2], y = X %*% betaHat, col = "purple")

# This change in lambda has made betaHat much more extreme. The predicted value 
# also no longer appear to be a line of best fit; now they form a almost 
# horizontal line that does a very poor job of predicting the actual values in 
# comparison to part c.



##### Question 4 ----
x <- c(0,0,0,0,0,1,1,1,1,1)
y <- c(0,1,0,0,0,0,1,1,1,0)

X <- cbind(1,x)
betaHat <- c(0,0)

# 4a.
for (i in 1:100) {
  p <- as.numeric(1/(1+exp(-X %*% betaHat)))
  W <- diag((p*(1-p)))
  G <- t(X) %*% (y-p)
  H <- -t(X) %*% W %*% X
  betaHat <- betaHat - solve(H) %*% G
  print(G)
}

print(betaHat)

# 4b.
glm(y~x, family=binomial) |> coef()

# 4c.
sqrt(diag(-solve(H)))
glm(y~x, family=binomial) |> summary()
# These values are equal to the error values in the summary