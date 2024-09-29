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
