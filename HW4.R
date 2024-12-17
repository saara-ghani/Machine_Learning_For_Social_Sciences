# CRIM 4012 HW 4
# Saara Ghani

setwd("/Users/saaraghani/Desktop/Spring24/CRIM4012/HW")

# 5.
x <- c(0,0,0,0,0,1,1,1,1,1)
y <- c(0,1,0,0,0,0,1,1,1,0)

b0 <- 0
b1 <- 0

p <- (exp(b0 + (b1 * x))) / (1 + exp(b0 + (b1 * x)))
dJ0 <- -sum(y-p)
dJ1 <- -sum(x*(y-p))

# Question: What do the signs of dJ0 and dJ1 tell you about how you need to 
# adjust this initial guess for b0 and b1 to minimize the negative Bernoulli 
# log likelihood?

# Answer: the sign for dJ0 is positive, so we should reduce the b0 guess.
# The sign for DJ1 is negative, so we should increase the b1 guess.


# 6. 
b0 <- 0
b1 <- 0
lambda <- 0.01

for (i in 1:5000) {
  p <- (exp(b0 + (b1 * x))) / (1 + exp(b0 + (b1 * x)))
  dJ0 <- -sum(y-p)
  dJ1 <- -sum(x*(y-p))
  
  b0 <- b0 - lambda*dJ0
  b1 <- b1 - lambda*dJ1

  print(c(b0,b1))
}

# 7. 
# P(Y=1|x)
x <- c(0, 1)
1 / (1 + exp(-1*(b0+(b1*x))))
# Answer: 
# P(Y = 1 | x = 0) = 0.2
# P(Y = 1 | x = 1) = 0.6
