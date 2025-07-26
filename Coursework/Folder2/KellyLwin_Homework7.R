#-------------------------------------------------------------------------------
# Problem 4: The efficiency for a steel specimen immersed in a phosphating tank 
#bis the weight of the phosphate coating divided by the metal loss (both in mg/ft2). 
#An article gave the accompanying data on tank temperature (x) and efficiency 
# ratio (y).
#-------------------------------------------------------------------------------
# Sample data
x <- c(112.3, 97.0, 92.7, 86.0, 102.0, 99.2, 95.8, 103.5, 89.0, 86.7)
y <- c(75.5, 71.1, 57.6, 48.2, 74.4, 73.3, 68.3, 59.5, 58.1, 48.7)

# Create a data frame
data <- data.frame(x, y)

# Fit the linear model
model <- lm(y ~ x, data = data)

# Get the coefficients of the model
coefficients <- summary(model)$coefficients

# Print the coefficients
cat("Intercept:", round(coefficients[1, 1], 4), "\n")
cat("Slope:", round(coefficients[2, 1], 4), "\n")

# Calculate the coefficient of determination (R²)
r_squared <- summary(model)$r.squared

# Print R² rounded to four decimal places
cat("Coefficient of Determination (R²):", round(r_squared, 4), "\n")

# Get the summary of the model
model_summary <- summary(model)

# Estimate of the error standard deviation (residual standard error)
sigma_estimate <- model_summary$sigma

# Print the rounded estimate
cat("Estimated error standard deviation (σ):", round(sigma_estimate, 3), "MPa\n")
#-------------------------------------------------------------------------------
# Problem 6
#-------------------------------------------------------------------------------
# Given data
x <- c(100, 125, 125, 150, 150, 200, 200, 250, 250, 300, 300, 350, 400, 400)
y <- c(150, 140, 180, 210, 190, 320, 280, 400, 430, 440, 390, 600, 610, 670)

# Fit the linear regression model
model <- lm(y ~ x)

# Predicted values
y_hat <- predict(model)

# (a) Calculate SSE using the defining formula
SSE <- sum((y - y_hat)^2)

# (b) Calculate Total Sum of Squares (TSS)
TSS <- sum((y - mean(y))^2)

# Calculate R-squared
r_squared <- 1 - (SSE / TSS)

# Output results
cat("SSE (defining formula):", round(SSE, 2), "\n")
cat("Total Sum of Squares (TSS):", round(TSS, 2), "\n")
cat("R-squared:", round(r_squared, 2), "\n")

# Interpret R-squared
if (r_squared > 0.85) {
  cat("The simple linear regression model appears to do an effective job of explaining variation in emission rate.\n")
} else {
  cat("The simple linear regression model does not appear to do an effective job of explaining variation in emission rate.\n")
}
#-------------------------------------------------------------------------------
# Problem 9
#-------------------------------------------------------------------------------
# Parameters for the uniform distribution
a <- 6.5
b <- 21

# (a) Mean and variance
mean_depth <- (a + b) / 2
variance_depth <- ((b - a)^2) / 12

# Round variance to two decimal places
variance_depth_rounded <- round(variance_depth, 2)

# (b) CDF function for the uniform distribution
F <- function(x) {
  if (x < a) {
    return(0)
  } else if (x >= a && x < b) {
    return((x - a) / (b - a))
  } else {
    return(1)
  }
}

# (c) Probability that observed depth is at most 10
prob_at_most_10 <- round(F(10), 4)

# Probability that observed depth is between 10 and 15
prob_between_10_and_15 <- round(F(15) - F(10), 4)

# (d) Standard deviation
sd_depth <- sqrt(variance_depth)

# Mean value
mean_value <- mean_depth

# Probability that observed depth is within 1 standard deviation of the mean value
prob_within_1_sd <- round(F(mean_value + sd_depth) - F(mean_value - sd_depth), 4)

# Probability that observed depth is within 2 standard deviations of the mean value
prob_within_2_sd <- round(F(mean_value + 2 * sd_depth) - F(mean_value - 2 * sd_depth), 4)

# Output results
mean_depth
variance_depth_rounded
prob_at_most_10
prob_between_10_and_15
prob_within_1_sd
prob_within_2_sd
#-------------------------------------------------------------------------------
#
#-------------------------------------------------------------------------------
