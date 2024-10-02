x <- 1:7
px <- c(.01, .03, .13, .25, .39, .17, .02)
ex <- sum(x*px); ex

x <- 1:6
px <- c(.3, .25, .15, .05, .1, .15)
ex <- sum(x*px); ex # E[X]
ex2 <- sum(x^2*px); ex2 # E[X^2]
varx <- ex2 - ex^2; varx

sum(x^2*px) - sum(x * px)^2

var_fn <- function(x, px){
  ex <- sum(x*px) # E[X]
  ex2 <- sum(x^2*px) # E[X^2]
  varx <- ex2 - ex^2
  
  return(varx)
}
sqrt(var_fn(x, px)) # sd

x <- c(0, 2, 4, 8, 16)
px <- c(0.3,  0.25, 0.15, 0.05, 0.1, 0.15)
test <- var_fn(x, px); test

ex <- sum(x*px); ex # E[X]
ex2 <- sum(x^2 * px); ex2 # E[X^2]

ex2 - ex^2 # variance

# --------

# Pr(X=3) for binomial where n=3, p=0.75
choose(6,3) * .75^3 * .25^3
# or
# dbinom(k, n, p) is the same as Pr(X=k) for b(x; n, p).
?dbinom
dbinom(3, 6, 0.75)



# Pr(X <= 4)
1 - (choose(6,5) * .75^5 * .25^1 + choose(6,6) * .75^6 * .25^0)
# or...
# pbinom(k, n, p) is the same as Pr(X <= k) with n and p. 
pbinom(5, 6, .75) - pbinom(1, 6, .75)
sum(dbinom(2:5, 6, .75))

pbinom(4, 6, .75)
1 - sum(dbinom(5:6, 6, 0.75))

# n=10, M=5, N=25
# Pr(X=2)

choose(5,2)*choose(20,8)/choose(25,10)

# or

?dhyper
# dhyper(x, M, N-M, n)
dhyper(2, 5, 25-5, 10)

x <- 2; M <- 5; N <- 25; n <- 10
dhyper(x, M, N-M, n)

# Pr(X <= 2)
phyper(2, M, N-M, n)

# Pr(2 <= X <= 5) = Pr(X <= 5) - Pr(X <= 1)
phyper(5, M, N-M, n) - phyper(1, M, N-M, n)

# Pr(2 < X < 5) = Pr(3 <= X <= 4) 
phyper(4, M, N-M, n) - phyper(2, M, N-M, n)

# Pr(X >= 4) = 1 - Pr(X < 4) = 1 - Pr(X <= 3)
1 - phyper(3, M, N-M, n)





# poisson stuff

# Pr(X=0)
exp(-2) #e^(-2)

# or
?dpois
dpois(0, 2)

# Pr(Y > 1) = 1-Pr(Y <= 1)
1-ppois(1, 2)

# Pr(X=0)*Pr(Y=0)
dpois(0, 2)*dpois(0,5)

