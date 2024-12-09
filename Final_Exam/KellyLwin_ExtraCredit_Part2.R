# Given data
total_bulbs <- c(40, 60, 75)
counts <- c(5, 3, 6) # 5 of 40-W, 3 of 60-W, 6 of 75-W
total <- sum(counts)

# (a) Probability that exactly two of the selected bulbs are rated 75-W
prob_a <- choose(6, 2) * choose(8, 1) / choose(14, 3)
round(prob_a, 4)

# (b) Probability that all three of the selected bulbs have the same rating
prob_b <- (choose(5, 3) + choose(3, 3) + choose(6, 3)) / choose(14, 3)
round(prob_b, 4)

# (c) Probability that one bulb of each type is selected
prob_c <- (choose(5, 1) * choose(3, 1) * choose(6, 1)) / choose(14, 3)
round(prob_c, 4)

# (d) Probability that it is necessary to examine at least six bulbs to find a 75-W bulb
prob_d <- (8/14) * (7/13) * (6/12) * (5/11) * (4/10)
round(prob_d, 4)
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# Parameters
A <- 0.20
B <- 4.25

# (a) PDF of X for uniform distribution
f_x <- function(x) {
  if (x >= A && x <= B) {
    return(1 / (B - A))
  } else {
    return(0)
  }
}

# (b) Probability that diameter exceeds 2 mm
prob_exceeds_2 <- function() {
  return((B - 2) / (B - A))
}

# (c) Probability that diameter is within 2 mm of the mean
mean_diameter <- (A + B) / 2
prob_within_2_mm <- function() {
  return((min(B, mean_diameter + 2) - max(A, mean_diameter - 2)) / (B - A))
}

# (d) Probability P(a < X < a + 3) for a satisfying 0.20 < a < a + 3 < 4.25
prob_within_a_to_a_plus_3 <- function(a) {
  return(3 / (B - A))
}

# Results
f_x_result <- round(f_x(3), 3)  # Example x = 3 for PDF
prob_exceeds_2_result <- round(prob_exceeds_2(), 3)
prob_within_2_mm_result <- round(prob_within_2_mm(), 3)
prob_within_a_to_a_plus_3_result <- round(prob_within_a_to_a_plus_3(1), 3)

f_x_result
prob_exceeds_2_result
prob_within_2_mm_result
prob_within_a_to_a_plus_3_result
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# Data
x <- c(20, 40, 60, 80)
y <- c(0.29, 1.35, 1.76, 2.12)

# Fit linear model
model <- lm(y ~ x)

# Get the R-squared value
r_squared <- summary(model)$r.squared

# Print the result
round(r_squared, 3)
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# Given data
x <- c(5, 12, 14, 16, 23, 30, 40, 46, 55, 67, 72, 81, 96, 112, 127)
y <- c(4, 10, 13, 15, 15, 25, 27, 48, 38, 46, 53, 70, 82, 99, 101)

# (b) Calculate point estimates of the slope and intercept of the population regression line
model <- lm(y ~ x)
slope <- coef(model)[2]
intercept <- coef(model)[1]
slope
intercept

# (c) Calculate a point estimate of the true average runoff volume when rainfall volume is 49
predicted_y <- predict(model, newdata = data.frame(x = 49))
predicted_y

# (d) Calculate a point estimate of the standard deviation 𝜎
std_dev <- summary(model)$sigma
std_dev

# (e) Calculate the proportion of the observed variation in runoff volume attributable to the regression
r_squared <- summary(model)$r.squared
r_squared
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# Given values
mean_YM <- 70   # mean Young's modulus in GPa
sd_YM <- 1.6    # standard deviation of Young's modulus in GPa

# Sample sizes
n_16 <- 16
n_256 <- 256

# (a) For n = 16
E_X_16 <- mean_YM
sd_X_16 <- sd_YM / sqrt(n_16)

# (b) For n = 256
E_X_256 <- mean_YM
sd_X_256 <- sd_YM / sqrt(n_256)

# Display results
cat("For n = 16:\n")
cat("E(X) =", E_X_16, "GPa\n")
cat("sd(X) =", sd_X_16, "GPa\n")

cat("\nFor n = 256:\n")
cat("E(X) =", E_X_256, "GPa\n")
cat("sd(X) =", sd_X_256, "GPa\n")
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# Given values
mean_value <- 80
std_dev <- 2

# a) P(79 ≤ X ≤ 81) when n = 16
n1 <- 16
std_err1 <- std_dev / sqrt(n1)
prob_a <- pnorm(81, mean = mean_value, sd = std_err1) - pnorm(79, mean = mean_value, sd = std_err1)
round(prob_a, 4)

# b) Probability that the sample mean exceeds 81 when n = 36
n2 <- 36
std_err2 <- std_dev / sqrt(n2)
prob_b <- 1 - pnorm(81, mean = mean_value, sd = std_err2)
round(prob_b, 4)
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# Percentile calculations for standard normal distribution
qnorm(c(0.61, 0.39, 0.74, 0.26, 0.13))
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# Required library
library(stats)

# Function to calculate z_alpha
z_alpha <- function(alpha) {
  return(qnorm(1 - alpha))
}

# a) alpha = 0.0063
alpha_a <- 0.0063
z_a <- z_alpha(alpha_a)
cat("z_alpha for alpha =", alpha_a, "is", round(z_a, 2), "\n")

# b) alpha = 0.08
alpha_b <- 0.08
z_b <- z_alpha(alpha_b)
cat("z_alpha for alpha =", alpha_b, "is", round(z_b, 2), "\n")

# c) alpha = 0.667
alpha_c <- 0.667
z_c <- z_alpha(alpha_c)
cat("z_alpha for alpha =", alpha_c, "is", round(z_c, 2), "\n")
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# Data
urban_data <- c(6.0, 5.0, 11.0, 33.0, 4.0, 5.0, 89.0, 18.0, 35.0, 17.0, 23.0)
farm_data <- c(4.0, 14.0, 11.0, 9.0, 9.0, 8.0, 4.0, 20.0, 5.0, 8.9, 21.0, 9.2, 3.0, 2.0, 0.3)

# Given sums
urban_xi <- 246.0
farm_xi <- 128.4
urban_xi2 <- 11600
farm_xi2 <- 1617.94

# Urban sample standard deviation
n_urban <- length(urban_data)
urban_s <- sqrt((urban_xi2 - (urban_xi^2 / n_urban)) / (n_urban - 1))

# Farm sample standard deviation
n_farm <- length(farm_data)
farm_s <- sqrt((farm_xi2 - (farm_xi^2 / n_farm)) / (n_farm - 1))

# Results
urban_s
farm_s
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# Given parameters
mean <- 19.0  # Mean of X
sd <- 1.5     # Standard deviation of X

# (a) P(X ≤ 19)
p_a <- pnorm(19, mean = mean, sd = sd)
cat("P(X ≤ 19):", round(p_a, 4), "\n")

# (b) P(X ≤ 20.5)
p_b <- pnorm(20.5, mean = mean, sd = sd)
cat("P(X ≤ 20.5):", round(p_b, 4), "\n")

# (c) P(X ≥ 11.5)
p_c <- 1 - pnorm(11.5, mean = mean, sd = sd)
cat("P(X ≥ 11.5):", round(p_c, 4), "\n")

# (d) P(17 ≤ X ≤ 22)
p_d <- pnorm(22, mean = mean, sd = sd) - pnorm(17, mean = mean, sd = sd)
cat("P(17 ≤ X ≤ 22):", round(p_d, 4), "\n")

# (e) P(|X − 19| ≤ 2) -> Equivalent to P(17 ≤ X ≤ 21)
p_e <- pnorm(21, mean = mean, sd = sd) - pnorm(17, mean = mean, sd = sd)
cat("P(|X − 19| ≤ 2):", round(p_e, 4), "\n")
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^