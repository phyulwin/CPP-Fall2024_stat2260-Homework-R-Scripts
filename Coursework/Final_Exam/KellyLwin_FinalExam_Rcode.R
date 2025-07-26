#-------------------------------------------------------------------------------
# KELLY LWIN - 12/10/2024
#-------------------------------------------------------------------------------
validate_probabilities <- function(probabilities) {
  if (sum(probabilities) != 1) stop("Probabilities must sum to 1.")
}

compute_and_display_statistics <- function(x, px) {
  validate_probabilities(px)
  ex <- sum(x * px)
  ex2 <- sum(x^2 * px)
  varx <- ex2 - ex^2
  sd <- sqrt(varx)
  cat(
    "Expected Value E[X]:", ex, "\n",
    "Expected Value of X^2 E[X^2]:", ex2, "\n",
    "Variance Var[X]:", varx, "\n",
    "Standard Deviation sd[X]:", sd, "\n"
  )
  list(Expected_Value = ex, Expected_Value_Squared = ex2, Variance = varx, Standard_Deviation = sd)
}

binomial_distribution <- function(n, p, k = NULL) {
  if (!is.null(k)) return(list(PMF = dbinom(k, size = n, prob = p)))
  list(Expected_Value = n * p, Variance = n * p * (1 - p))
}

relative_frequency <- function(data) {
  table(data) / length(data)
}

create_histogram <- function(data) {
  hist(data, breaks = seq(min(data), max(data), length.out = data_breaks), right = FALSE,
       main = "Histogram of Data", xlab = "Data Values", ylab = "Frequency")
}

generate_boxplot <- function(bp_data) {
  cat("Median:", median(bp_data), "\nSummary:\n", summary(bp_data), "\n")
  bp <- boxplot(bp_data, horizontal = TRUE)
  cat("\nFive-Number Summary:\n", bp$stats, "\n")
  if (length(bp$out) > 0) cat("\nOutliers:\n", bp$out, "\n") else cat("\nNo outliers found.\n")
}

find_binomial_prob <- function(x, px, k, n, p) {
  stats <- compute_and_display_statistics(x, px)
  binomial_prob <- dbinom(k, n, p)
  cat("Probability Pr(X =", k, "):", binomial_prob, "\n")
  stats
}

test2_q1 <- function() {
  # Define the PDF function that handles vectorized input
  f <- function(x) {
    ifelse(x > 0 & x <= 5, (2 / 25) * x, 0)
  }
  
  # Define the CDF function
  F <- function(x) {
    ifelse(x <= 0, 0, ifelse(x > 0 & x <= 5, (x * x) / 25, 1))
  }
  
  # Probability from 3 cm to 5 cm
  from_3cm_to_5cm <- F(5) - F(3)
  from_3cm_to_5cm
  
  # 75th percentile for length of bee
  percentile75 <- uniroot(function(x) F(x) - 0.75, c(0, 5))$root
  percentile75
  
  # Expected length of bee
  expected_length <- integrate(f, 0, 5)$value
  expected_length
}

test2_q2 <- function() {
  sigma_x_bar = 3.31 / sqrt(196)
  sigma_x_bar
  
  t = qt(0.975, 195)
  t
  
  x_bar <- 51.64     
  s <- 3.31        
  n <- 196            
  confidence_level <- 0.95
  
  t_score <- qt((1 + confidence_level) / 2, df = n - 1)
  
  
  margin_of_error <- t_score * (s / sqrt(n))
  margin_of_error
  
  lower_bound <- x_bar - margin_of_error
  upper_bound <- x_bar + margin_of_error
  
  lower_bound
  upper_bound
}

test2_q3 <- function() {
  n <- 356
  x <- 278
  
  p_bar <- x / n
  z <- qnorm(0.90)
  ME <- z * sqrt((p_bar * (1 - p_bar)) / n)
  
  p_bar
  ME
  
  upper_bound <- p_bar + ME
  upper_bound
  
  confidence_level <- 0.90
  z <- qnorm(1 - (1 - confidence_level) / 2)
  p_hat <- 0.5
  ME <- 0.03
  
  # sample size
  n_needed <- (z^2 * p_hat * (1 - p_hat)) / (ME^2)
  n_needed <- ceiling(n_needed)
  n_needed
}

test2_q4 <- function() {
  mu <- 24
  sigma <- 5
  
  lower_bound_a <- mu - 1.1 * sigma
  upper_bound_a <- mu + 1.1 * sigma
  probability_a <- pnorm(upper_bound_a, mean = mu, sd = sigma) - pnorm(lower_bound_a, mean = mu, sd = sigma)
  
  lower_bound_b <- mu + 1 * sigma
  upper_bound_b <- mu + 2 * sigma
  probability_b <- pnorm(upper_bound_b, mean = mu, sd = sigma) - pnorm(lower_bound_b, mean = mu, sd = sigma)
  
  probability_a
  probability_b
  
  n <- 16
  SE <- sigma / sqrt(n)
  lower_bound <- 22.5
  upper_bound <- 23.25
  
  probability <- pnorm(upper_bound, mean = mu, sd = SE) - pnorm(lower_bound, mean = mu, sd = SE)
  probability
  
  n <- 16
  SE <- sigma / sqrt(n)
  cutoff_time <- qnorm(0.05, mean = mu, sd = SE)
  cutoff_time
}
#-------------------------------------------------------------------------------
# TEST WORK STARTS HERE
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# QUESTION 1
#-------------------------------------------------------------------------------
k <- 4 / (2^4)

F <- function(x) {
  if (x < 0) {
    return(0)
  } else if (x <= 2) {
    return((k * (x^4)) / 4)
  } else {
    return(1)
  }
}

f <- function(x) {
  if (x < 0 || x > 2) {
    return(0)
  } else {
    return(k * x^3)
  }
}

prob_75sec <- F(1.25)

percentile_25 <- 0
for (x in seq(0, 2, by = 0.01)) { 
  if (F(x) >= 0.25) {
    percentile_25 <- x
    break
  }
}

x_values <- seq(0, 2, by = 0.01)
pdf_values <- sapply(x_values, f)
expected_time <- sum(x_values * pdf_values * 0.01)

# results
k
prob_75sec
percentile_25
expected_time
#-------------------------------------------------------------------------------
# QUESTION 2
#-------------------------------------------------------------------------------
remaining_liquid <- c(0.53, 0.63, 0.41, 0.50, 0.35)
n <- length(remaining_liquid)

sample_mean <- mean(remaining_liquid)
sample_sd <- sd(remaining_liquid)

mu_0 <- 0.6

t_stat <- (sample_mean - mu_0) / (sample_sd / sqrt(n))
p_value <- pt(t_statistic, df = n - 1)

t_stat
p_value
#-------------------------------------------------------------------------------
# QUESTION 3
#-------------------------------------------------------------------------------
n <- 275
x <- 100

p_hat <- x / n

z_score <- qnorm(0.95) 
ME <- z_score * sqrt((p_hat * (1 - p_hat)) / n)

CI_upper <- p_hat + ME

desired_ME <- 0.09
n_needed <- (z_score^2 * p_hat * (1 - p_hat)) / (desired_ME^2)
n_needed <- ceiling(n_needed)

# results
p_hat
ME
CI_upper
n_needed
#-------------------------------------------------------------------------------
# QUESTION 4
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# QUESTION 5
#-------------------------------------------------------------------------------
x <- c(16, 18, 20, 24)
p_x <- c(0.15, 0.20, 0.35, NA)

p_x[4] <- 1 - sum(p_x, na.rm = TRUE)

expected_value <- sum(x * p_x)
variance <- sum((x^2 * p_x)) - (expected_value^2)

# f(X) = 65X - 650
f_x <- function(X) {
  65 * X - 650
}

expected_price <- sum(f_x(x) * p_x)
price_variance <- sum((f_x(x)^2) * p_x) - (expected_price^2)

# g(X) = X - 0.009X^2
g_x <- function(X) {
  X - 0.009 * (X^2)
}

expected_actual_capacity <- sum(g_x(x) * p_x)

# results
p_x[4]
expected_value
variance
expected_price
price_variance
expected_actual_capacity
#-------------------------------------------------------------------------------
# QUESTION 6
#-------------------------------------------------------------------------------
# true/false
lambda <- 0.0133
median_distance <- qexp(0.5, rate = lambda)

claim <- 57.1425

# for part e
total <- 42
white <- 14
picking <- 10
count_in_hand <- 5

probability <- dhyper(count_in_hand, white, total - white, picking)
probability
