# Bernoulli rv
# Format - includes X = 1 if "randomly selected" [...], X = 0 otherwise

# Function to draw the probability histogram
draw_probability_histogram <- function(X, p) {
  # Draw the probability histogram
  barplot(p, names.arg = X, 
          col = "lightblue", 
          main = "Probability Histogram of X",
          xlab = "Number of Students (X)", 
          ylab = "Probability",
          ylim = c(0, max(p) + 0.05))  # Adjust y-axis to fit the bars
  
  # Add grid lines for better visibility
  grid(nx = NA, ny = NULL)
  
  # Example usage
  # X <- 0:4
  # p <- c(0.25, 0.30, 0.20, 0.15, 0.10)
  # 
  # draw_probability_histogram(X, p)
}
#-------------------------------------------------------------------------------
# Define the values of X (number of components)
X <- 1:4

# Define the corresponding probabilities for each X
p <- c(0.35, 0.35, 0.15, 0.15)

# Part (a) - Calculate E(X)
E_X <- sum(X * p)
cat("Expected Value E(X):", E_X, "\n")

# Part (a) - Calculate E(5 - X)
E_5_minus_X <- sum((5 - X) * p)
cat("Expected Value E(5 - X):", E_5_minus_X, "\n")

# Part (b) - Compare flat fee of $65 with variable fee based on 150 / (5 - X)
# Calculate E(150 / (5 - X))
E_variable_fee <- sum((150 / (5 - X)) * p)
cat("Expected Value of variable fee (150 / (5 - X)):", E_variable_fee, "\n")
#-------------------------------------------------------------------------------
# Function to calculate expected value, expected value of the square, and variance
calculate_statistics <- function(values, probabilities) {
  # Check if probabilities sum to 1
  if (sum(probabilities) != 1) {
    stop("Probabilities must sum to 1.")
  }
  
  # Calculate E(X)
  E_X <- sum(values * probabilities)
  cat("Expected Value E(X):", E_X, "\n")
  
  # Calculate E(X^2)
  E_X2 <- sum(values^2 * probabilities)
  cat("Expected Value E(X^2):", E_X2, "\n")
  
  # Calculate Variance V(X)
  V_X <- E_X2 - E_X^2
  cat("Variance V(X):", V_X, "\n")
  
  # Calculate Standard Deviation sd(X)
  sd_X <- sqrt(V_X)
  cat("Standard Deviation sd(X):", sd_X, "\n")
  
  # x <- c(1, 2, 4, 8, 16)
  # p <- c(0.05, 0.15, 0.30, 0.35, 0.15)
  # 
  # calculate_statistics(x, p)
  
}

# Define the possible values of X (rated capacities)
X <- c(16, 18, 20)

# Define the corresponding probabilities
p <- c(0.1, 0.3, 0.6)

calculate_statistics(x, p)

# Part (b) - Calculate expected price E(Price) = 65 * E(X) - 650
E_price <- 65 * E_X - 650
cat("Expected Price:", E_price, "\n")

# Part (c) - Calculate variance of the price V(Price) = 65^2 * V(X)
V_price <- 65^2 * V_X
cat("Variance of Price:", V_price, "\n")

# Part (d) - Calculate expected actual capacity E(h(X)) where h(X) = X - 0.009 * X^2
E_actual_capacity <- sum((X - 0.009 * X^2) * p)
cat("Expected Actual Capacity E(h(X)):", round(E_actual_capacity, 4), "\n")
#-------------------------------------------------------------------------------
# Part (a) - Finding k
y_values <- 1:7
k <- 1 / sum(y_values) # k = 1 / (1 + 2 + ... + 7)
cat("Value of k:", k, "\n") # Should output 1/28

# Part (b) - Calculating probabilities
p <- k * y_values # p(y) = k * y

# Probability that at most 5 forms are required
P_at_most_5 <- sum(p[1:5])
cat("Probability that at most 5 forms are required:", P_at_most_5, "\n") # Should output 15/28

# Probability that between 4 and 6 forms (inclusive) are required
P_between_4_and_6 <- sum(p[4:6])
cat("Probability that between 4 and 6 forms are required:", P_between_4_and_6, "\n") # Should output 15/28

# Part (d) - Check if p(y) = y^2 / 138 can be a valid pmf
# Calculate the sum for p(y) = y^2 / 138
result <- sum((y_values^2) / 138)
result
#-------------------------------------------------------------------------------
# Function to calculate probabilities for a given scenario
calculate_probabilities <- function(max_y, range_start = NULL, range_end = NULL) {
  # Create a vector of y values
  y_values <- 1:max_y
  
  # Calculate k
  k <- 1 / sum(y_values)
  cat("Value of k:", k, "\n") # Print value of k
  
  # Calculate probabilities p(y) = k * y
  p <- k * y_values
  
  # Probability that at most 'range_end' forms are required
  if (!is.null(range_end)) {
    P_at_most <- sum(p[1:range_end])
    cat("Probability that at most", range_end, "forms are required:", P_at_most, "\n")
  }
  
  # Probability that between 'range_start' and 'range_end' forms are required
  if (!is.null(range_start) && !is.null(range_end)) {
    P_between <- sum(p[range_start:range_end])
    cat("Probability that between", range_start, "and", range_end, "forms are required:", P_between, "\n")
  }
  # Example usage:
  # Call the function with max_y, the upper limit for the range, and the proposed pmf for checking
  # calculate_probabilities(max_y = 7, range_end = 5, range_start = 4)
}
#-------------------------------------------------------------------------------
# Define the values of Y and their probabilities
y_values <- c(0, 1, 2, 3)
probabilities <- c(0.50, 0.25, 0.20, 0.05)

# Compute E(Y)
E_Y <- sum(y_values * probabilities)
cat("E(Y) =", E_Y, "\n")

# Compute E(Y^2)
E_Y_squared <- sum((y_values^2) * probabilities)

# Calculate expected surcharge
expected_surcharge <- 120 * E_Y_squared
cat("Expected surcharge =", expected_surcharge, "\n")
#-------------------------------------------------------------------------------