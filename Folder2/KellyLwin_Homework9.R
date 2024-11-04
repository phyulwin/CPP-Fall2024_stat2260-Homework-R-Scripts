#-------------------------------------------------------------------------------
# Problem 1
#-------------------------------------------------------------------------------
# Given values
mu <- 70       # Mean (GPa)
sigma <- 1.6   # Standard deviation (GPa)

# Sample sizes
n1 <- 64
n2 <- 256

# (a) Calculations for n = 64
E_X_n1 <- mu
sigma_X_n1 <- sigma / sqrt(n1)

# (b) Calculations for n = 256
E_X_n2 <- mu
sigma_X_n2 <- sigma / sqrt(n2)

# Display results
cat("For n = 64:\n")
cat("E(X) =", E_X_n1, "GPa\n")
cat("σ_X =", sigma_X_n1, "GPa\n\n")

cat("For n = 256:\n")
cat("E(X) =", E_X_n2, "GPa\n")
cat("σ_X =", sigma_X_n2, "GPa\n")
#-------------------------------------------------------------------------------
# Problem 2
#-------------------------------------------------------------------------------
# Given data
mu <- 70            # Mean Young's modulus in GPa
sigma <- 2          # Standard deviation in GPa

# Part (a)
n_a <- 9            # Sample size for part (a)
sigma_x_a <- sigma / sqrt(n_a)    # Standard deviation of sample mean for n = 9
lower_bound <- 69
upper_bound <- 71

# P(69 <= X <= 71)
p_a <- pnorm(upper_bound, mean = mu, sd = sigma_x_a) - pnorm(lower_bound, mean = mu, sd = sigma_x_a)
p_a <- round(p_a, 4)  # Round to 4 decimal places

# Part (b)
n_b <- 25            # Sample size for part (b)
sigma_x_b <- sigma / sqrt(n_b)    # Standard deviation of sample mean for n = 25
threshold <- 71

# P(X > 71)
p_b <- 1 - pnorm(threshold, mean = mu, sd = sigma_x_b)
p_b <- round(p_b, 4)  # Round to 4 decimal places

# Output results
cat("Part (a): P(69 <= X <= 71) =", p_a, "\n")
cat("Part (b): P(X > 71) =", p_b, "\n")
#-------------------------------------------------------------------------------
# Problem 3
#-------------------------------------------------------------------------------
# Given values
mean_hardness <- 50
std_dev <- 1.6

# (a) Sample size of 14
n1 <- 14
# Calculate the standard error for sample size of 14
std_error1 <- std_dev / sqrt(n1)
# Calculate the probability that the sample mean hardness is at least 51
probability_a <- 1 - pnorm(51, mean = mean_hardness, sd = std_error1)
# Round the result to four decimal places
probability_a <- round(probability_a, 4)

# (b) Sample size of 44
n2 <- 44
# Calculate the standard error for sample size of 44
std_error2 <- std_dev / sqrt(n2)
# Calculate the probability that the sample mean hardness is at least 51
probability_b <- 1 - pnorm(51, mean = mean_hardness, sd = std_error2)
# Round the result to four decimal places
probability_b <- round(probability_b, 4)

# Print the results
cat("Probability for sample size of 14 pins (part a):", probability_a, "\n")
cat("Probability for sample size of 44 pins (part b):", probability_b, "\n")
#-------------------------------------------------------------------------------
# Problem 4
#-------------------------------------------------------------------------------
# Given parameters
mean_density <- 2.7  # Mean sediment density
sd_density <- 0.74    # Standard deviation of sediment density
sample_size <- 25     # Sample size

# (a) Probability that the sample average is at most 3.00
# Standard error of the mean
sem <- sd_density / sqrt(sample_size)

# Calculate probabilities using the normal distribution
p_at_most_3 <- pnorm(3.00, mean = mean_density, sd = sem)

# Probability that the sample average is between 2.7 and 3.00
p_between_2.7_and_3 <- pnorm(3.00, mean = mean_density, sd = sem) - pnorm(2.7, mean = mean_density, sd = sem)

# Output results
cat("Probability that the sample average sediment density is at most 3.00:", round(p_at_most_3, 4), "\n")
cat("Probability that the sample average sediment density is between 2.7 and 3.00:", round(p_between_2.7_and_3, 4), "\n")

# (b) Required sample size for the first probability to be at least 0.99
# To find n, we need to solve pnorm(3.00, mean, sd/sqrt(n)) >= 0.99
required_prob <- 0.99

# Calculate the z-score corresponding to the desired probability
z_score <- qnorm(required_prob)

# Rearranging the formula: n = (sd / (mean_target - mean))^2
n_required <- ceiling((sd_density / (3.00 - mean_density))^2 * (z_score^2))

# Output the required sample size
cat("Required sample size to ensure probability is at least 0.99:", n_required, "specimens\n")
#-------------------------------------------------------------------------------
# Problem 5
#-------------------------------------------------------------------------------
# Given values
mean_strength <- 10100    # Population mean
sd_strength <- 499        # Population standard deviation
n <- 40                   # Sample size

# Standard error of the mean
se_mean <- sd_strength / sqrt(n)

# Calculate probability that sample mean is between 10000 and 10300
lower_bound <- 10000
upper_bound <- 10300

# Probability for the lower and upper bounds
p_lower <- pnorm(lower_bound, mean = mean_strength, sd = se_mean)
p_upper <- pnorm(upper_bound, mean = mean_strength, sd = se_mean)

# Probability that sample mean is between 10000 and 10300
probability <- p_upper - p_lower
round(probability, 4)
#-------------------------------------------------------------------------------
# Problem 6
#-------------------------------------------------------------------------------
# Define the confidence intervals
interval1 <- c(114.6, 115.4)
interval2 <- c(114.4, 115.6)

# Calculate the sample mean for each interval
mean1 <- mean(interval1)
mean2 <- mean(interval2)

# Calculate the overall mean (average of the two sample means)
sample_mean <- (mean1 + mean2) / 2
sample_mean
#-------------------------------------------------------------------------------
# Problem 8
#-------------------------------------------------------------------------------
# Given values
x_bar <- 52.7  # sample mean
sigma <- 2.8   # population standard deviation

# Function to calculate confidence interval
confidence_interval <- function(x_bar, sigma, n, confidence_level) {
  z <- qnorm(1 - (1 - confidence_level) / 2)  # z-score for two-tailed test
  margin_error <- z * (sigma / sqrt(n))
  lower <- x_bar - margin_error
  upper <- x_bar + margin_error
  return(c(lower, upper))
}

# (a) 95% CI for n = 25
n_a <- 25
ci_95_a <- confidence_interval(x_bar, sigma, n_a, 0.95)
cat("(a) 95% CI for n = 25:", round(ci_95_a, 2), "watts\n")

# (b) 95% CI for n = 100
n_b <- 100
ci_95_b <- confidence_interval(x_bar, sigma, n_b, 0.95)
cat("(b) 95% CI for n = 100:", round(ci_95_b, 2), "watts\n")

# (c) 99% CI for n = 100
ci_99_c <- confidence_interval(x_bar, sigma, n_b, 0.99)
cat("(c) 99% CI for n = 100:", round(ci_99_c, 2), "watts\n")

# (d) 82% CI for n = 100
ci_82_d <- confidence_interval(x_bar, sigma, n_b, 0.82)
cat("(d) 82% CI for n = 100:", round(ci_82_d, 2), "watts\n")

# (e) Required sample size for a 99% CI with width 1.0
desired_width <- 1.0
z_99 <- qnorm(1 - (1 - 0.99) / 2)
n_required <- ceiling((z_99 * sigma / (desired_width / 2))^2)
cat("(e) Required sample size for 99% CI with width 1.0:", n_required, "\n")
#-------------------------------------------------------------------------------
# Problem 9
#-------------------------------------------------------------------------------
# Given data
x <- 1192.0  # Sample mean
s <- 506.6   # Sample standard deviation
n <- 44      # Sample size
confidence_level <- 0.99

# Calculate the critical z-value for 99% confidence
z <- qnorm((1 + confidence_level) / 2)

# Calculate the margin of error
margin_of_error <- z * (s / sqrt(n))

# Calculate confidence interval
lower_bound <- x - margin_of_error
upper_bound <- x + margin_of_error

# Print results
cat("99% Confidence Interval: (", round(lower_bound, 1), ",", round(upper_bound, 1), ")\n")
#-------------------------------------------------------------------------------
# Problem 10
#-------------------------------------------------------------------------------
# Given data
sigma <- 0.73  # true standard deviation of porosity

# (a) 95% CI for true average porosity of a seam
n1 <- 22           # sample size
xbar1 <- 4.85      # sample mean porosity
alpha1 <- 0.05     # significance level for 95% CI

# Calculate 95% confidence interval
z1 <- qnorm(1 - alpha1 / 2)
margin_of_error1 <- z1 * (sigma / sqrt(n1))
lower_bound1 <- xbar1 - margin_of_error1
upper_bound1 <- xbar1 + margin_of_error1
cat("95% CI for part (a):", round(lower_bound1, 2), ",", round(upper_bound1, 2), "\n")


# (b) 98% CI for true average porosity of another seam
n2 <- 10           # sample size
xbar2 <- 4.56      # sample mean porosity
alpha2 <- 0.02     # significance level for 98% CI

# Calculate 98% confidence interval
z2 <- qnorm(1 - alpha2 / 2)
margin_of_error2 <- z2 * (sigma / sqrt(n2))
lower_bound2 <- xbar2 - margin_of_error2
upper_bound2 <- xbar2 + margin_of_error2
cat("98% CI for part (b):", round(lower_bound2, 2), ",", round(upper_bound2, 2), "\n")


# (c) Required sample size for 95% confidence interval width of 0.44
width_c <- 0.44
z_c <- qnorm(1 - alpha1 / 2)
n_c <- ceiling((z_c * sigma / (width_c / 2))^2)
cat("Sample size for part (c):", n_c, "specimens\n")


# (d) Required sample size for 99% confidence interval width of 0.25
width_d <- 0.25
alpha_d <- 0.01   # significance level for 99% CI
z_d <- qnorm(1 - alpha_d / 2)
n_d <- ceiling((z_d * sigma / (width_d / 2))^2)
cat("Sample size for part (d):", n_d, "specimens\n")
#-------------------------------------------------------------------------------
# Problem 11
#-------------------------------------------------------------------------------
# Given values
sigma <- 100
n <- 81
sample_mean <- 8423

# Part (a) - 90% Confidence Interval
z_90 <- qnorm(0.95)  # z-score for 90% confidence level (two-tailed)
margin_error_90 <- z_90 * (sigma / sqrt(n))
ci_90_lower <- sample_mean - margin_error_90
ci_90_upper <- sample_mean + margin_error_90
ci_90 <- c(round(ci_90_lower, 1), round(ci_90_upper, 1))
ci_90

# Part (b) - z-score for 92% Confidence Interval
z_92 <- qnorm(0.96)
z_92
#-------------------------------------------------------------------------------