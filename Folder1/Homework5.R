# HOMEWORK 5

# Part (a) and (b)
binomial_probability <- function(x, n, p) {
  result <- choose(n, x) * (p^x) * ((1 - p)^(n - x))
  cat("Binomial Probability:", result)
}

binomial_probability(5, 8, 0.25)
binomial_probability(6, 8, 0.65)

n <- 7; p <- 0.65
choose(n,k) * p^k * (1-p)^k

# Part (c)
n_c <- 7
p_c <- 0.65
k_c <- 3:5
prob_3_to_5 <- sum(dbinom(k_c, n_c, p_c))
cat("P(3 ≤ X ≤ 5):", prob_3_to_5, "\n")

# Part (d)
n_d <- 9
p_d <- 0.15
prob_1_or_more <- 1 - dbinom(0, n_d, p_d)
cat("P(1 ≤ X):", prob_1_or_more, "\n")

#-------------------------------------------------------------------------------
# Parameters
n <- 20          # Total number of accidents
p <- 0.7         # Probability of a single vehicle accident

# (a) Probability that at most 8 involve a single vehicle
p_at_most_8 <- pbinom(8, n, p)
print(round(p_at_most_8, 3))  # Rounded to three decimal places

# (b) Probability that exactly 8 involve a single vehicle
p_exactly_8 <- dbinom(8, n, p)
print(round(p_exactly_8, 3))  # Rounded to three decimal places

# (c) Probability that exactly 10 involve multiple vehicles
# To find this, we need to find the probability of 10 failures (i.e., multiple vehicles)
p_exactly_10_multiple <- dbinom(10, n, 0.3)  # Using q (0.3)
print(round(p_exactly_10_multiple, 3))  # Rounded to three decimal places

# (d) Probability that between 5 and 8, inclusive, involve a single vehicle
p_between_5_and_8 <- pbinom(8, n, p) - pbinom(4, n, p)
print(round(p_between_5_and_8, 3))  # Rounded to three decimal places

# (e) Probability that at least 5 involve a single vehicle
p_at_least_5 <- 1 - pbinom(4, n, p)
print(round(p_at_least_5, 3))  # Rounded to three decimal places

# (f) Probability that exactly 8 involve a single vehicle and the other 12 involve multiple vehicles
p_exactly_8_and_12_multiple <- dbinom(8, n, p)  # Same as (b)
print(round(p_exactly_8_and_12_multiple, 3))  # Rounded to three decimal places
#-------------------------------------------------------------------------------
# Parameters of the Hypergeometric Distribution:
# N: Total number of individuals in the population
# M: Total number of successes in the population
# n: Number of draws

# Function to compute hypergeometric probabilities and statistics
compute_hypergeometric <- function(N, M, n, X) {
  # Probability of exactly X first-time test takers
  P_X <- dhyper(X, M, N - M, n)  
  cat("P(X =", X, "):", P_X, "\n")  
  
  # Cumulative probability of at most X
  P_X_leq_X <- phyper(X, M, N - M, n)  
  cat("P(X <=", X, "):", P_X_leq_X, "\n")  
  
  # Probability of at least X
  P_X_geq_X <- 1 - phyper(X - 1, M, N - M, n)  
  cat("P(X >=", X, "):", P_X_geq_X, "\n")  
  
  # Mean of hypergeometric distribution
  mean_X <- n * (M / N)  
  cat("Mean of X:", mean_X, "individuals\n")  
  
  # Standard deviation of hypergeometric distribution
  std_dev_X <- sqrt(n * (M / N) * ((N - M) / N) * ((N - n) / (N - 1)))  
  cat("Standard Deviation of X:", std_dev_X, "individuals\n")  
}

# Example usage
compute_hypergeometric(N = 18, M = 7, n = 6, X = 2)  # Change X as needed

value <- 60-15
compute_hypergeometric(N = 60, M = 35, n = value, X = 10) 
#-------------------------------------------------------------------------------
# Parameters
n <- 100                # total customers

# Part (a): Debit Card Payments
p_debit <- 0.3         # probability of using a debit card
mean_debit <- n * p_debit
variance_debit <- n * p_debit * (1 - p_debit)

# Part (b): Not Paying with Cash
p_no_cash <- 0.8      # probability of not using cash
mean_no_cash <- n * p_no_cash
variance_no_cash <- n * p_no_cash * (1 - p_no_cash)

# Results
mean_debit
variance_debit
mean_no_cash
variance_no_cash
#-------------------------------------------------------------------------------
# Function to calculate binomial distribution statistics
binomial_stats <- function(n, p, k) {
  # Expected value (mean)
  ex <- n * p
  # Variance
  var <- n * p * (1 - p)
  # Standard deviation
  sd <- sqrt(var)
  
  # Print Expected value, Variance, and Standard deviation
  cat("Expected Value (Mean):", ex, "\n")
  cat("Variance:", var, "\n")
  cat("Standard Deviation:", sd, "\n")
  
  # Probability of getting exactly k successes
  prob_exact <- dbinom(k, n, p)
  
  # Print probabilities for each k
  cat("\nProbabilities for each k in", k, ":\n")
  for (i in k) {
    cat("P(X =", i, "):", prob_exact[i - min(k) + 1], "\n") # Adjust index for k vector
  }
  
  # Cumulative probability for k
  cumulative_prob <- pbinom(k, n, p)
  cat("\nCumulative probabilities for k in", k, ":\n")
  for (i in k) {
    cat("P(X <=", i, "):", cumulative_prob[i - min(k) + 1], "\n") # Adjust index for k vector
  }
}

# Example usage
n <- 20              # number of trials
p <- 0.25            # probability of success
k <- 10:15          # range of k to evaluate
binomial_stats(n, p, k)
#-------------------------------------------------------------------------------
# Parameters
n <- 20      # number of trials
p <- 0.25    # probability of a fax message

# (a) Expected number of calls
E_X <- n * p
cat("Expected number of calls (E(X)) =", E_X, "\n")  # E(X) = 5

# (b) Standard deviation
var_X <- n * p * (1 - p)    # Variance
sd_X <- sqrt(var_X)          # Standard deviation
cat("Standard deviation (σ(X)) =", sd_X, "\n")  # σ(X) ≈ 1.936

# (c) Calculate threshold for exceeding expected by 2 SD
threshold <- E_X + 2 * sd_X
cat("Threshold for exceeding expected by 2 SD =", threshold, "\n")

# Probability that the number of calls exceeds the threshold
P_X_gt_threshold <- 1 - pbinom(threshold, n, p)
cat("Probability that X exceeds the expected number by more than 2 standard deviations =", P_X_gt_threshold, "\n")  # ≈ 0.041
#-------------------------------------------------------------------------------
#---
# Function to calculate binomial probability
binomial_probability <- function(n, x, p) {
  return(choose(n, x) * (p^x) * ((1 - p)^(n - x)))
}

# (a) b(5; 8, 0.25)
binomial_probability(8, 5, 0.25)

# (b) b(6; 8, 0.65)
binomial_probability(8, 6, 0.65)

# (c) P(3 ≤ X ≤ 5) when n = 7 and p = 0.65
n_c <- 7
p_c <- 0.65
prob_c <- sum(sapply(3:5, function(x) {
  binomial_probability(n_c, x, p_c)
}))
cat("P(3 ≤ X ≤ 5 | n =", n_c, ", p =", p_c, ") =", round(prob_c, 3), "\n")

# (d) P(1 ≤ X) when n = 9 and p = 0.15
n_d <- 9
p_d <- 0.15
prob_d <- sum(sapply(1:n_d, function(x) {
  binomial_probability(n_d, x, p_d)
}))
cat("P(1 ≤ X | n =", n_d, ", p =", p_d, ") =", round(prob_d, 3), "\n")
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Parameters
n1 <- 25  # number of students in the first section
n2 <- 35  # number of students in the second section
N <- n1 + n2  # total number of students
k <- 15  # total number of graded projects

# Probability of at least 10 projects from the same section
prob_at_least_10 <- 0

# Calculate probabilities for 10 to 15 from the first section
for (i in 10:15) {
  if (i <= n1) {
    prob_at_least_10 <- prob_at_least_10 + choose(n1, i) * choose(n2, k - i) / choose(N, k)
  }
}

# Calculate probabilities for 10 to 15 from the second section
for (i in 10:15) {
  if (i <= n2) {
    prob_at_least_10 <- prob_at_least_10 + choose(n2, i) * choose(n1, k - i) / choose(N, k)
  }
}

# Output the result rounded to four decimal places
prob_at_least_10_rounded <- round(prob_at_least_10, 4)
prob_at_least_10_rounded
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Parameters
n1 <- 25  # number of students in the first section
n2 <- 35  # number of students in the second section
N <- n1 + n2  # total number of students
k <- 15  # total number of graded projects

# Expected value of projects from the second section in the first 15
E_X <- k * (n2 / N)

# Variance of projects from the second section in the first 15
Var_X <- k * (n2 / N) * (n1 / N) * ((N - k) / (N - 1))

# Mean of the number of remaining projects from the second section
mean_remaining <- n2 - E_X

# Standard deviation of the number of remaining projects from the second section
std_dev_remaining <- sqrt(Var_X)

# Round results
mean_remaining_rounded <- round(mean_remaining)
std_dev_remaining_rounded <- round(std_dev_remaining, 3)

# Output the results
mean_remaining_rounded
std_dev_remaining_rounded
#-------------------------------------------------------------------------------
# Homework 5 Q8
#-------------------------------------------------------------------------------
# Parameters
N <- 18  # Total number of individuals
K <- 7   # Number of first-time test takers
n <- 6   # Number of individuals selected

# Calculate the mean
mean_X <- n * (K / N)

# Calculate the standard deviation
std_dev_X <- sqrt(n * (K / N) * ((N - K) / N) * ((N - n) / (N - 1)))

# Round the results to three decimal places
mean_X_rounded <- round(mean_X, 3)
std_dev_X_rounded <- round(std_dev_X, 3)

# Output the results
cat("Mean:", mean_X_rounded, "\n")
cat("Standard Deviation:", std_dev_X_rounded, "\n")
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Define parameters
n <- 15
p_no_citations <- 0.60
p_at_least_one_citation <- 0.40

# (a) Probability that at least 10 have no citations
prob_at_least_10_no_citations <- sum(dbinom(10:n, size = n, prob = p_no_citations))

# (b) Probability that fewer than half have at least one citation
prob_fewer_than_half_at_least_one <- sum(dbinom(0:7, size = n, prob = p_at_least_one_citation))

# (c) Probability that the number with at least one citation is between 5 and 10, inclusive
prob_between_5_and_10_at_least_one <- sum(dbinom(5:10, size = n, prob = p_at_least_one_citation))

# Round results to three decimal places
prob_at_least_10_no_citations <- round(prob_at_least_10_no_citations, 3)
prob_fewer_than_half_at_least_one <- round(prob_fewer_than_half_at_least_one, 3)
prob_between_5_and_10_at_least_one <- round(prob_between_5_and_10_at_least_one, 3)

# Print results
cat("Probability that at least 10 have no citations:", prob_at_least_10_no_citations, "\n")
cat("Probability that fewer than half have at least one citation:", prob_fewer_than_half_at_least_one, "\n")
cat("Probability that number with at least one citation is between 5 and 10:", prob_between_5_and_10_at_least_one, "\n")
#-------------------------------------------------------------------------------
# Homework 5 Q7
#-------------------------------------------------------------------------------
# Parameters
n <- 100          # Number of customers

# (a) Debit card
p_debit <- 0.3   # Probability of using a debit card
mean_debit <- n * p_debit
variance_debit <- n * p_debit * (1 - p_debit)

cat("Mean number of customers using a debit card:", mean_debit, "\n")
cat("Variance of the number of customers using a debit card:", variance_debit, "\n")

# (b) Not using cash
p_not_cash <- 0.8  # Probability of not using cash
mean_not_cash <- n * p_not_cash
variance_not_cash <- n * p_not_cash * (1 - p_not_cash)

cat("Mean number of customers not using cash:", mean_not_cash, "\n")
cat("Variance of the number of customers not using cash:", variance_not_cash, "\n")
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
