#-------------------------------------------------------------------------------
# Problem 1
#-------------------------------------------------------------------------------
# (a) P(0 ≤ Z ≤ 2.29)
p_a <- pnorm(2.29) - pnorm(0)

# (b) P(0 ≤ Z ≤ 2)
p_b <- pnorm(2) - pnorm(0)

# (c) P(−2.10 ≤ Z ≤ 0)
p_c <- pnorm(0) - pnorm(-2.10)

# (d) P(−2.10 ≤ Z ≤ 2.10)
p_d <- pnorm(2.10) - pnorm(-2.10)

# (e) P(Z ≤ 1.68)
p_e <- pnorm(1.68)

# (f) P(−1.25 ≤ Z)
p_f <- 1 - pnorm(-1.25)

# (g) P(−1.10 ≤ Z ≤ 2.00)
p_g <- pnorm(2) - pnorm(-1.10)

# (h) P(1.68 ≤ Z ≤ 2.50)
p_h <- pnorm(2.50) - pnorm(1.68)

# (i) P(1.10 ≤ Z)
p_i <- 1 - pnorm(1.10)

# (j) P(|Z| ≤ 2.50)
p_j <- pnorm(2.50) - pnorm(-2.50)

# Print results
list(
  P_0_to_2_29 = p_a,
  P_0_to_2 = p_b,
  P_neg2_10_to_0 = p_c,
  P_neg2_10_to_2_10 = p_d,
  P_Z_leq_1_68 = p_e,
  P_neg1_25_to_inf = p_f,
  P_neg1_10_to_2 = p_g,
  P_1_68_to_2_50 = p_h,
  P_1_10_to_inf = p_i,
  P_abs_Z_leq_2_50 = p_j
)
#-------------------------------------------------------------------------------
# Problem 2
#-------------------------------------------------------------------------------
# (a) Φ(c) = 0.9834
c_a <- qnorm(0.9834)

# (b) P(0 ≤ Z ≤ c) = 0.3051
c_b <- qnorm(0.5 + 0.3051)

# (c) P(c ≤ Z) = 0.1335
c_c <- qnorm(1 - 0.1335)

# (d) P(−c ≤ Z ≤ c) = 0.6372
c_d <- qnorm(0.5 + 0.6372 / 2)

# (e) P(c ≤ |Z|) = 0.0128
c_e <- qnorm(1 - 0.0128 / 2)

# Print results rounded to 2 decimal places
cat("c for (a):", round(c_a, 2), "\n")
cat("c for (b):", round(c_b, 2), "\n")
cat("c for (c):", round(c_c, 2), "\n")
cat("c for (d):", round(c_d, 2), "\n")
cat("c for (e):", round(c_e, 2), "\n")
#-------------------------------------------------------------------------------
# Problem 3
#-------------------------------------------------------------------------------
# Calculate percentiles for the standard normal distribution
percentiles <- c(
  "91st" = qnorm(0.91),
  "9th" = qnorm(0.09),
  "75th" = qnorm(0.75),
  "25th" = qnorm(0.25),
  "10th" = qnorm(0.10)
)

# Round answers to two decimal places
percentiles <- round(percentiles, 2)
percentiles
#-------------------------------------------------------------------------------
# Problem 4
#-------------------------------------------------------------------------------
# Given alpha values
alpha_a <- 0.0057
alpha_b <- 0.18
alpha_c <- 0.647

# Calculate z_alpha
z_alpha_a <- qnorm(1 - alpha_a)
z_alpha_b <- qnorm(1 - alpha_b)
z_alpha_c <- qnorm(1 - alpha_c)

# Print results rounded to two decimal places
z_alpha_a <- round(z_alpha_a, 2)
z_alpha_b <- round(z_alpha_b, 2)
z_alpha_c <- round(z_alpha_c, 2)

z_alpha_a
z_alpha_b
z_alpha_c
#-------------------------------------------------------------------------------
# Problem 5
#-------------------------------------------------------------------------------
# Given parameters
mean_X <- 17.0       # Mean value in kips
sd_X <- 1.50         # Standard deviation in kips

# (a) P(X ≤ 17)
prob_a <- pnorm(17, mean = mean_X, sd = sd_X)
cat("P(X ≤ 17):", round(prob_a, 4), "\n")

# (b) P(X ≤ 18.5)
prob_b <- pnorm(18.5, mean = mean_X, sd = sd_X)
cat("P(X ≤ 18.5):", round(prob_b, 4), "\n")

# (c) P(X ≥ 9.5)
prob_c <- 1 - pnorm(9.5, mean = mean_X, sd = sd_X)
cat("P(X ≥ 9.5):", round(prob_c, 4), "\n")

# (d) P(15 ≤ X ≤ 20)
prob_d <- pnorm(20, mean = mean_X, sd = sd_X) - pnorm(15, mean = mean_X, sd = sd_X)
cat("P(15 ≤ X ≤ 20):", round(prob_d, 4), "\n")

# (e) P(|X − 17| ≤ 2)
prob_e <- pnorm(19, mean = mean_X, sd = sd_X) - pnorm(15, mean = mean_X, sd = sd_X)
cat("P(|X − 17| ≤ 2):", round(prob_e, 4), "\n")
#-------------------------------------------------------------------------------
# Problem 6
#-------------------------------------------------------------------------------
# Given parameters
mean_speed <- 46.8 # mean value in km/h
sd_speed <- 1.75   # standard deviation in km/h

# (a) Probability that maximum speed is at most 49 km/h
p_a <- pnorm(49, mean = mean_speed, sd = sd_speed)
cat("Probability that maximum speed is at most 49 km/h:", round(p_a, 4), "\n")

# (b) Probability that maximum speed is at least 47 km/h
p_b <- 1 - pnorm(47, mean = mean_speed, sd = sd_speed)
cat("Probability that maximum speed is at least 47 km/h:", round(p_b, 4), "\n")

# (c) Probability that maximum speed differs from the mean value by at most 1.5 standard deviations
lower_limit <- mean_speed - 1.5 * sd_speed
upper_limit <- mean_speed + 1.5 * sd_speed
p_c <- pnorm(upper_limit, mean = mean_speed, sd = sd_speed) - pnorm(lower_limit, mean = mean_speed, sd = sd_speed)
cat("Probability that maximum speed differs from the mean by at most 1.5 standard deviations:", round(p_c, 4), "\n")
#-------------------------------------------------------------------------------
# Problem 7
#-------------------------------------------------------------------------------
# Parameters
mu <- 0.60  # Mean concentration
sigma <- 0.09  # Standard deviation

# (a) Probability that the concentration exceeds 0.80
prob_exceeds_0_80 <- 1 - pnorm(0.80, mean = mu, sd = sigma)
cat("Probability that concentration exceeds 0.80:", round(prob_exceeds_0_80, 4), "\n")

# (b) Probability that the concentration is at most 0.50
prob_at_most_0_50 <- pnorm(0.50, mean = mu, sd = sigma)
cat("Probability that concentration is at most 0.50:", round(prob_at_most_0_50, 4), "\n")

# (c) Characterizing the largest 5% of all concentration values
largest_5_percent_threshold <- qnorm(0.95, mean = mu, sd = sigma)
cat("The largest 5% of all concentration values are above:", round(largest_5_percent_threshold, 4), "mg/cm3\n")
#-------------------------------------------------------------------------------
# Problem 8
#-------------------------------------------------------------------------------
# Given parameters
mean_length <- 28  # mean value in mm
sd_length <- 7.4   # standard deviation in mm

# (a) Probability calculations
prob_at_most_20 <- pnorm(20, mean = mean_length, sd = sd_length)
prob_less_than_20 <- pnorm(20, mean = mean_length, sd = sd_length, lower.tail = TRUE)

# (b) 75th percentile
percentile_75 <- qnorm(0.75, mean = mean_length, sd = sd_length)

# (c) 15th percentile
percentile_15 <- qnorm(0.15, mean = mean_length, sd = sd_length)

# (d) Values separating the middle 80% (smallest 10% and largest 10%)
lower_10_percent <- qnorm(0.10, mean = mean_length, sd = sd_length)
upper_10_percent <- qnorm(0.90, mean = mean_length, sd = sd_length)

# Output results
cat("At most 20 mm:", round(prob_at_most_20, 4), "\n")
cat("Less than 20 mm:", round(prob_less_than_20, 4), "\n")
cat("75th percentile:", round(percentile_75, 4), "mm\n")
cat("15th percentile:", round(percentile_15, 4), "mm\n")
cat("Smallest 10%:", round(lower_10_percent, 4), "mm\n")
cat("Largest 10%:", round(upper_10_percent, 4), "mm\n")
#-------------------------------------------------------------------------------
# Problem 9
#-------------------------------------------------------------------------------
# Define the mean and standard deviation
mean_value <- 0  # You can replace with the actual mean value if known
sd_value <- 1    # You can replace with the actual standard deviation value if known

# (a) Probability within 1.8 SDs of the mean
prob_a <- pnorm(mean_value + 1.8 * sd_value) - pnorm(mean_value - 1.8 * sd_value)
cat("Probability within 1.8 SDs of the mean:", round(prob_a, 4), "\n")

# (b) Probability farther than 2.4 SDs from the mean
prob_b <- 1 - (pnorm(mean_value + 2.4 * sd_value) - pnorm(mean_value - 2.4 * sd_value))
cat("Probability farther than 2.4 SDs from the mean:", round(prob_b, 4), "\n")

# (c) Probability between 1 and 2 SDs from the mean
prob_c <- (pnorm(mean_value + 2 * sd_value) - pnorm(mean_value + 1 * sd_value)) +
  (pnorm(mean_value - 1 * sd_value) - pnorm(mean_value - 2 * sd_value))
cat("Probability between 1 and 2 SDs from the mean:", round(prob_c, 4), "\n")
#-------------------------------------------------------------------------------
# Problem 10
#-------------------------------------------------------------------------------
# Set the rate parameter
lambda <- 1

# (a) Expected time between two successive arrivals
expected_time <- 1 / lambda
print(paste("Expected time:", round(expected_time, 3)))

# (b) Standard deviation of the time between successive arrivals
std_dev <- 1 / lambda
print(paste("Standard deviation:", round(std_dev, 3)))

# (c) P(X ≤ 4)
p_less_than_4 <- pexp(4, rate = lambda)
print(paste("P(X ≤ 4):", round(p_less_than_4, 3)))

# (d) P(1 ≤ X ≤ 3)
p_between_1_and_3 <- pexp(3, rate = lambda) - pexp(1, rate = lambda)
print(paste("P(1 ≤ X ≤ 3):", round(p_between_1_and_3, 3)))
#-------------------------------------------------------------------------------
# Problem 11
#-------------------------------------------------------------------------------
# Parameters
lambda <- 0.01312

# (a) Probabilities
# Probability that the distance is at most 100 m
p_at_most_100 <- pexp(100, rate = lambda)

# Probability that the distance is at most 200 m
p_at_most_200 <- pexp(200, rate = lambda)

# Probability that the distance is between 100 and 200 m
p_between_100_and_200 <- p_at_most_200 - p_at_most_100

# Print results
cat("At most 100 m:", round(p_at_most_100, 4), "\n")
cat("At most 200 m:", round(p_at_most_200, 4), "\n")
cat("Between 100 and 200 m:", round(p_between_100_and_200, 4), "\n")

# (b) Mean and standard deviation of the exponential distribution
mean_distance <- 1 / lambda
sd_distance <- 1 / lambda

# Probability that distance exceeds the mean by more than 2 standard deviations
p_exceeds_mean_plus_2sd <- 1 - pexp(mean_distance + 2 * sd_distance, rate = lambda)

# Print result
cat("Probability exceeds mean by more than 2 standard deviations:", round(p_exceeds_mean_plus_2sd, 4), "\n")

# (c) Median distance
median_distance <- qexp(0.5, rate = lambda)

# Print result
cat("Median distance:", round(median_distance, 2), "m\n")
#-------------------------------------------------------------------------------
# Problem 12
#-------------------------------------------------------------------------------
# Parameters
lambda <- 1 / 2.435  # Rate parameter (lambda = 1 / mean)

# (a)
# Probability that the duration of a particular rainfall event is at least 2 hours
prob_at_least_2 <- 1 - pexp(2, rate = lambda)

# Probability that the duration is at most 3 hours
prob_at_most_3 <- pexp(3, rate = lambda)

# Probability that the duration is between 2 and 3 hours
prob_between_2_and_3 <- prob_at_most_3 - (1 - pexp(2, rate = lambda))

# (b)
# Mean and standard deviation of the exponential distribution
mean_value <- 2.435
std_dev <- 2.435  # Standard deviation is the same as the mean for exponential distribution

# Probability that rainfall duration exceeds the mean value by more than 3 standard deviations
prob_exceeds_mean_by_3sd <- 1 - pexp(mean_value + 3 * std_dev, rate = lambda)

# Probability that it is less than the mean value by more than one standard deviation
prob_less_than_mean_by_1sd <- pexp(mean_value - std_dev, rate = lambda)

# Print results
cat("Probability at least 2 hours:", round(prob_at_least_2, 4), "\n")
cat("Probability at most 3 hours:", round(prob_at_most_3, 4), "\n")
cat("Probability between 2 and 3 hours:", round(prob_between_2_and_3, 4), "\n")
cat("Probability exceeds mean by more than 3 standard deviations:", round(prob_exceeds_mean_by_3sd, 4), "\n")
cat("Probability less than mean by more than 1 standard deviation:", round(prob_less_than_mean_by_1sd, 4), "\n")
#-------------------------------------------------------------------------------
# Problem 13
#-------------------------------------------------------------------------------
# Given parameters for the gamma distribution
mean_lifetime <- 28
sd_lifetime <- 14

# Calculate shape (k) and scale (theta) parameters
theta <- sd_lifetime^2 / mean_lifetime
k <- mean_lifetime / theta

# (a) Probability that a transistor will last between 14 and 28 weeks
prob_a <- pgamma(28, shape = k, scale = theta) - pgamma(14, shape = k, scale = theta)
round(prob_a, 3)

# (b) Probability that a transistor will last at most 28 weeks
prob_b <- pgamma(28, shape = k, scale = theta)
round(prob_b, 3)

# (c) 99th percentile of the lifetime distribution
percentile_c <- qgamma(0.99, shape = k, scale = theta)
round(percentile_c)

# (d) Value of t such that only 0.5% of all transistors would still be operating
t_value_d <- qgamma(0.995, shape = k, scale = theta)
round(t_value_d)