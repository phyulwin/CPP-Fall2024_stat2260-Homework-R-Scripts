#Problem 1----------------------------------------------------------------------
# Define the probabilities and degrees of freedom for each case
# (a) t0.10, 10
t_a <- qt(0.90, df = 10)

# (b) t0.05, 10
t_b <- qt(0.95, df = 10)

# (c) t0.05, 23
t_c <- qt(0.95, df = 23)

# (d) t0.05, 60
t_d <- qt(0.95, df = 60)

# (e) t0.005, 60
t_e <- qt(0.995, df = 60)

# Print the results rounded to three decimal places
cat("t0.10, 10:", round(t_a, 3), "\n")
cat("t0.05, 10:", round(t_b, 3), "\n")
cat("t0.05, 23:", round(t_c, 3), "\n")
cat("t0.05, 60:", round(t_d, 3), "\n")
cat("t0.005, 60:", round(t_e, 3), "\n")
#Problem 2----------------------------------------------------------------------
# (a) Central area = 0.95, df = 10
t_a <- qt(0.975, df = 10)  # 0.975 because we want the middle 95%, so each tail has 2.5%
cat("Central area = 0.95, df = 10: ±", round(t_a, 3), "\n")

# (b) Central area = 0.95, df = 20
t_b <- qt(0.975, df = 20)
cat("Central area = 0.95, df = 20: ±", round(t_b, 3), "\n")

# (c) Central area = 0.99, df = 20
t_c <- qt(0.995, df = 20)  # 0.995 because we want the middle 99%, so each tail has 0.5%
cat("Central area = 0.99, df = 20: ±", round(t_c, 3), "\n")

# (d) Central area = 0.99, df = 60
t_d <- qt(0.995, df = 60)
cat("Central area = 0.99, df = 60: ±", round(t_d, 3), "\n")

# (e) Upper-tail area = 0.01, df = 25
t_e <- qt(0.99, df = 25)  # 0.99 because we need the 1% upper tail
cat("Upper-tail area = 0.01, df = 25:", round(t_e, 3), "\n")

# (f) Lower-tail area = 0.025, df = 5
t_f <- qt(0.025, df = 5)  # 0.025 because we need the 2.5% lower tail
cat("Lower-tail area = 0.025, df = 5:", round(t_f, 3), "\n")
#Problem 3----------------------------------------------------------------------
# Observations
data <- c(417, 420, 422, 422, 425, 427, 432, 434, 436, 439, 445, 447, 449, 453, 456, 461, 465)

# Sample size
n <- length(data)

# Sample mean
mean_value <- mean(data)

# Sample standard deviation
sd_value <- sd(data)

# Degrees of freedom
df <- n - 1

# t-value for a 95% confidence interval
t_value <- qt(0.975, df)

# Margin of error
margin_of_error <- t_value * (sd_value / sqrt(n))

# Confidence interval
lower_bound <- mean_value - margin_of_error
upper_bound <- mean_value + margin_of_error

# Print the results rounded to two decimal places
cat("95% Confidence Interval:", round(lower_bound, 2), "to", round(upper_bound, 2), "\n")
#Problem 4----------------------------------------------------------------------
# Given data
p_hat <- 0.55
n <- 2341
confidence_level <- 0.99

# Calculate the z-score for a 99% confidence level
z <- qnorm(1 - (1 - confidence_level) / 2)

# Calculate the margin of error
margin_of_error <- z * sqrt((p_hat * (1 - p_hat)) / n)

# Calculate the confidence interval
lower_bound <- p_hat - margin_of_error
upper_bound <- p_hat + margin_of_error

# Print the results rounded to three decimal places
cat("99% Confidence Interval: (", round(lower_bound, 3), ",", round(upper_bound, 3), ")\n")

# Parameters
Z <- 2.576       # Z-score for a 99% confidence interval
E <- 0.02        # Desired margin of error (half of the width of 0.04)
p <- 0.5         # Conservative estimate for p to maximize sample size

# Sample size calculation
n <- (Z^2 * p * (1 - p)) / (E^2)

# Round up to the nearest integer
n_required <- ceiling(n)
n_required
#Problem 5----------------------------------------------------------------------
# Part (a)
n <- 2008            # Sample size
p_hat <- 0.29        # Sample proportion
z <- qnorm(0.995)    # Z-score for 99% confidence

# Calculate margin of error
ME <- z * sqrt((p_hat * (1 - p_hat)) / n)

# Confidence interval
lower <- p_hat - ME
upper <- p_hat + ME

cat("99% Confidence Interval for the proportion:", round(lower, 3), "to", round(upper, 3), "\n")

# Part (b)
# Margin of error (width/2) for 99% CI with p = 0.5
ME_target <- 0.025
p_max <- 0.5

# Required sample size for given margin of error
n_required <- ceiling((z^2 * p_max * (1 - p_max)) / ME_target^2)

cat("Required sample size for width of 0.05:", n_required)
#Problem 6----------------------------------------------------------------------
# Given data
n <- 55                  # sample size
sample_mean <- 8.11      # sample mean
sample_sd <- 1.44        # sample standard deviation
confidence_level <- 0.95 # confidence level

# Calculate standard error
standard_error <- sample_sd / sqrt(n)

# Calculate the Z-score for a 95% confidence level
z_score <- qnorm((1 + confidence_level) / 2)

# Calculate the confidence interval
lower_bound <- sample_mean - z_score * standard_error
upper_bound <- sample_mean + z_score * standard_error

# Print the results, rounded to 3 decimal places
cat("95% Confidence Interval: (", round(lower_bound, 3), ", ", round(upper_bound, 3), ")\n")
#Problem 7----------------------------------------------------------------------
# Define the endpoints of each confidence interval
interval1 <- c(111.4, 112.6)
interval2 <- c(111.1, 112.9)

# Calculate the midpoint of each interval
mean1 <- mean(interval1)
mean2 <- mean(interval2)

# Calculate the sample mean resonance frequency (average of the two midpoints)
sample_mean <- mean(c(mean1, mean2))

# Print the result
sample_mean
#Problem 8----------------------------------------------------------------------
# Given values
sigma <- 2.9   # Standard deviation
x_bar <- 54.6  # Sample mean

# (a) 95% CI for μ when n = 25
n1 <- 25
z_95 <- qnorm(0.975)  # z-value for 95% confidence
margin_error_95_a <- z_95 * (sigma / sqrt(n1))
lower_95_a <- x_bar - margin_error_95_a
upper_95_a <- x_bar + margin_error_95_a
cat("95% CI for μ when n = 25:", round(lower_95_a, 2), ",", round(upper_95_a, 2), "watts\n")

# (b) 95% CI for μ when n = 100
n2 <- 100
margin_error_95_b <- z_95 * (sigma / sqrt(n2))
lower_95_b <- x_bar - margin_error_95_b
upper_95_b <- x_bar + margin_error_95_b
cat("95% CI for μ when n = 100:", round(lower_95_b, 2), ",", round(upper_95_b, 2), "watts\n")

# (c) 99% CI for μ when n = 100
z_99 <- qnorm(0.995)  # z-value for 99% confidence
margin_error_99 <- z_99 * (sigma / sqrt(n2))
lower_99 <- x_bar - margin_error_99
upper_99 <- x_bar + margin_error_99
cat("99% CI for μ when n = 100:", round(lower_99, 2), ",", round(upper_99, 2), "watts\n")

# (d) 82% CI for μ when n = 100
z_82 <- qnorm(0.91)  # z-value for 82% confidence
margin_error_82 <- z_82 * (sigma / sqrt(n2))
lower_82 <- x_bar - margin_error_82
upper_82 <- x_bar + margin_error_82
cat("82% CI for μ when n = 100:", round(lower_82, 2), ",", round(upper_82, 2), "watts\n")

# (e) Required sample size for 99% CI width of 1.0
desired_width <- 1.0
n_required <- ceiling((2 * z_99 * sigma / desired_width)^2)
cat("Required sample size for 99% CI with width 1.0:", n_required, "\n")
#Problem 9----------------------------------------------------------------------
# Part (a)
sigma <- 0.78
x_bar_a <- 4.85
n_a <- 21
z_95 <- 1.96

margin_error_a <- z_95 * sigma / sqrt(n_a)
CI_a <- c(x_bar_a - margin_error_a, x_bar_a + margin_error_a)
round(CI_a, 2)

# Part (b)
x_bar_b <- 4.56
n_b <- 13
z_98 <- 2.33

margin_error_b <- z_98 * sigma / sqrt(n_b)
CI_b <- c(x_bar_b - margin_error_b, x_bar_b + margin_error_b)
round(CI_b, 2)

# Part (c)
width_c <- 0.5
n_c <- ceiling((2 * z_95 * sigma / width_c)^2)
n_c

# Part (d)
margin_error_d <- 0.18
z_99 <- 2.576

n_d <- ceiling((z_99 * sigma / margin_error_d)^2)
n_d
#Problem 10---------------------------------------------------------------------
# Given data
sigma <- 100        # standard deviation
n <- 64             # sample size
x_bar <- 8489       # sample mean
confidence_level_90 <- 0.90
confidence_level_92 <- 0.92

# Part (a): 90% Confidence Interval for the Mean
z_90 <- qnorm(1 - (1 - confidence_level_90) / 2)  # z-score for 90% confidence level
margin_of_error_90 <- z_90 * (sigma / sqrt(n))
lower_bound_90 <- x_bar - margin_of_error_90
upper_bound_90 <- x_bar + margin_of_error_90

cat("90% Confidence Interval:", round(lower_bound_90, 1), "to", round(upper_bound_90, 1), "lb\n")

# Part (b): 92% Confidence Interval for the Mean
z_92 <- qnorm(1 - (1 - confidence_level_92) / 2)  # z-score for 92% confidence level

cat("The value of z for a 92% confidence level should be changed to:", round(z_92, 2), "\n")
#Problem 11---------------------------------------------------------------------
# Given values
x_bar <- 1192.0       # Sample mean
s <- 506.6            # Sample standard deviation
n <- 44               # Sample size (number of observations)
confidence_level <- 0.99

# Calculate the t-score for a 99% confidence interval
t_score <- qt((1 + confidence_level) / 2, df = n - 1)

# Calculate margin of error
margin_of_error <- t_score * (s / sqrt(n))

# Calculate the confidence interval
lower_bound <- x_bar - margin_of_error
upper_bound <- x_bar + margin_of_error

# Display the confidence interval rounded to one decimal place
cat("99% Confidence Interval: [", round(lower_bound, 1), ", ", round(upper_bound, 1), "]\n")