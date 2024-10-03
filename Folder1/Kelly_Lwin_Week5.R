# Function to calculate expected value, variance, and standard deviation
calculate_statistics <- function(x, px) {
  # Validate that probabilities sum to 1
  if (sum(px) != 1) {
    stop("Probabilities must sum to 1.")
  }
  
  # Calculate the expected value E[X]
  ex <- sum(x * px)
  cat("Expected Value E[X]:", ex, "\n") # Print expected value
  
  # Calculate the expected value of the square E[X^2]
  ex2 <- sum(x^2 * px)
  cat("Expected Value of X^2 E[X^2]:", ex2, "\n") # Print expected value of the square
  
  # Calculate the variance Var[X]
  varx <- ex2 - ex^2
  cat("Variance Var[X]:", varx, "\n") # Print variance
  
  # Calculate the standard deviation sd[X]
  sd <- sqrt(varx)
  cat("Standard Deviation sd[X]:", sd, "\n") # Print standard deviation
}
# Example usage:
# First scenario
x1 <- 1:7
px1 <- c(.01, .03, .13, .25, .39, .17, .02)
cat("\nScenario 1:\n")
calculate_statistics(x1, px1)
# 
# # Second scenario
x2 <- 1:6
px2 <- c(.3, .25, .15, .05, .1, .15)
cat("\nScenario 2:\n")
calculate_statistics(x2, px2)

# Function to calculate expected value, variance, and binomial probability
calculate_statistics_and_probability <- function(x, px, k, n, p) {
  # Validate that probabilities sum to 1
  if (sum(px) != 1) {
    stop("Probabilities must sum to 1.")
  }
  
  # Calculate the expected value E[X]
  ex <- sum(x * px)
  cat("Expected Value E[X]:", ex, "\n") # Print expected value
  
  # Calculate the expected value of the square E[X^2]
  ex2 <- sum(x^2 * px)
  cat("Expected Value of X^2 E[X^2]:", ex2, "\n") # Print expected value of the square
  
  # Calculate the variance Var[X]
  varx <- ex2 - ex^2
  cat("Variance Var[X]:", varx, "\n") # Print variance
  
  # Calculate and print the probability for a binomial distribution
  # Pr(X = k) where n is the number of trials and p is the success probability
  binomial_probability <- choose(n, k) * p^k * (1 - p)^(n - k)
  cat("Probability Pr(X =", k, "):", binomial_probability, "\n") # Print binomial probability
  
  # Alternatively, calculate and print the same probability using dbinom function
  dbinom_probability <- dbinom(k, n, p)
  cat("Probability using dbinom Pr(X =", k, "):", dbinom_probability, "\n") # Print dbinom probability
}

# Example usage:
x <- c(0, 2, 4, 8, 16)
px <- c(0.3,  0.25, 0.15, 0.05, 0.1, 0.15)
# Set parameters for binomial distribution
k <- 3    # Number of successes
n <- 6    # Number of trials
p <- 0.75 # Probability of success
# Call the function with the defined parameters
calculate_statistics_and_probability(x, px, k, n, p)

#-------------------------------------------------------------------------------
# SKIP THIS SECTION : DO NOT RUN THE CODE
#-------------------------------------------------------------------------------
#
#-------------------------------------------------------------------------------
# Define values and probabilities
x <- c(0, 2, 4, 8, 16)          # Possible outcomes
px <- c(0.3, 0.25, 0.15, 0.05, 0.1, 0.15)  # Corresponding probabilities

# Calculate variance using custom function
test <- var_fn(x, px); test

# Calculate expected value E[X]
ex <- sum(x * px); ex  # E[X]

# Calculate E[X^2]
ex2 <- sum(x^2 * px); ex2  # E[X^2]

# Calculate variance
ex2 - ex^2  # Variance

# --------

# Calculate P(X=3) for binomial distribution (n=6, p=0.75)
choose(6, 3) * 0.75^3 * 0.25^3  # Manual calculation

# Alternative using dbinom function
# dbinom(k, n, p) gives P(X=k)
dbinom(3, 6, 0.75)  # Calculate P(X=3)

# Calculate P(X <= 4) using manual binomial probabilities
1 - (choose(6, 5) * 0.75^5 * 0.25^1 + choose(6, 6) * 0.75^6 * 0.25^0)

# Alternative using pbinom for P(X <= 5) - P(X <= 1)
pbinom(5, 6, 0.75) - pbinom(1, 6, 0.75)

# Sum probabilities for X from 2 to 5
sum(dbinom(2:5, 6, 0.75))

# Calculate P(X <= 4) directly
pbinom(4, 6, 0.75)

# Calculate P(X >= 5) = 1 - P(X <= 4)
1 - sum(dbinom(5:6, 6, 0.75))

# Hypergeometric parameters: n=10, M=5, N=25
# Calculate P(X = 2)
choose(5, 2) * choose(20, 8) / choose(25, 10)

# Alternative using dhyper for P(X = 2)
dhyper(2, 5, 25 - 5, 10)  # N - M for failures

# Assign values for convenience
x <- 2; M <- 5; N <- 25; n <- 10
dhyper(x, M, N - M, n)  # Calculate P(X = 2)

# Calculate P(X <= 2)
phyper(2, M, N - M, n)

# Calculate P(2 <= X <= 5) = P(X <= 5) - P(X <= 1)
phyper(5, M, N - M, n) - phyper(1, M, N - M, n)

# Calculate P(3 <= X <= 4) using hypher
phyper(4, M, N - M, n) - phyper(2, M, N - M, n)

# Calculate P(X >= 4) = 1 - P(X <= 3)
1 - phyper(3, M, N - M, n)
#-------------------------------------------------------------------------------
# 
#-------------------------------------------------------------------------------
# Function to calculate and print Poisson probabilities
poisson_probabilities <- function(lambda_X, k_X, lambda_Y, k_Y) {
  # Calculate P(X=k_X) where X has mean lambda_X
  P_X_k <- dpois(k_X, lambda_X)  # Probability of X being equal to k_X
  
  # Calculate P(Y=k_Y) where Y has mean lambda_Y
  P_Y_k <- dpois(k_Y, lambda_Y)  # Probability of Y being equal to k_Y
  
  # Calculate P(Y > k_Y)
  P_Y_greater_than_k <- 1 - ppois(k_Y, lambda_Y)  # Complement probability
  
  # Calculate joint probability P(X=k_X) * P(Y=k_Y)
  joint_probability <- P_X_k * P_Y_k
  
  # Print the results
  cat("P(X =", k_X, ") =", P_X_k, "\n")  # Print P(X=k_X)
  cat("P(Y =", k_Y, ") =", P_Y_k, "\n")  # Print P(Y=k_Y)
  cat("P(Y >", k_Y, ") =", P_Y_greater_than_k, "\n")  # Print P(Y>k_Y)
  cat("P(X =", k_X, ") * P(Y =", k_Y, ") =", joint_probability, "\n")  # Print joint probability
}

# Example usage:
# Let's say we want to calculate for X with λ=2 and k=0,
# and Y with λ=5 and k=0.
poisson_probabilities(lambda_X = 2, k_X = 0, lambda_Y = 5, k_Y = 0)