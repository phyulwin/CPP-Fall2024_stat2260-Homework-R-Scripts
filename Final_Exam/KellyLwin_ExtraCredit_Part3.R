#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+Problem 26
# Given data
x_bar <- 60.51  # sample mean
s <- 3.79       # sample standard deviation
n <- 144        # sample size

# Confidence level
confidence_level <- 0.85

# Calculate the standard error
SE <- s / sqrt(n)

# Find the critical t-value for 85% confidence level and 143 degrees of freedom (n-1)
alpha <- 1 - confidence_level
t_critical <- qt(1 - alpha/2, df = n - 1)

# Calculate the margin of error
margin_of_error <- t_critical * SE

# Construct the confidence interval
CI_lower <- x_bar - margin_of_error
CI_upper <- x_bar + margin_of_error

# Display the results
CI_lower
CI_upper

# Given data for sample size calculation
sigma <- 8   # assumed population standard deviation
margin_of_error_required <- 1  # margin of error
confidence_level <- 0.85

# Calculate the critical z-value for 85% confidence level (normal distribution)
alpha <- 1 - confidence_level
z_critical <- qnorm(1 - alpha/2)

# Calculate the required sample size
n_required <- (z_critical * sigma / margin_of_error_required)^2

# Round up to the nearest integer for sample size
n_required <- ceiling(n_required)

# Display the required sample size
n_required
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Given parameters
mean_speed <- 46.6    # Mean speed in miles per minute
sd_speed <- 1.75      # Standard deviation in miles per minute

# (a) Probability that the maximum speed is at most 50 miles per minute
prob_a <- pnorm(50, mean = mean_speed, sd = sd_speed)
cat("Probability (at most 50 mph):", round(prob_a, 4), "\n")

# (b) Probability that the maximum speed is at least 47 miles per minute
prob_b <- 1 - pnorm(47, mean = mean_speed, sd = sd_speed)
cat("Probability (at least 47 mph):", round(prob_b, 4), "\n")

# (c) Probability that the speed differs from the mean by at most 1.5 standard deviations
lower_bound <- mean_speed - 1.5 * sd_speed
upper_bound <- mean_speed + 1.5 * sd_speed
prob_c <- pnorm(upper_bound, mean = mean_speed, sd = sd_speed) - pnorm(lower_bound, mean = mean_speed, sd = sd_speed)
cat("Probability (within 1.5 SD of mean):", round(prob_c, 4), "\n")
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++