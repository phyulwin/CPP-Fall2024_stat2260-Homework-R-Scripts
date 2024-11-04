# Define the density function
f <- function(x) {
  ifelse(x >= 3 & x <= 5, 0.05 * x + 0.3, 0)
}

# Graph the pdf
curve(f, from = 2.5, to = 5.5, ylab = "Density", xlab = "X", main = "Probability Density Function")

# Verify that the total area under the density curve is 1
area <- integrate(f, 3, 5)$value
area  # This should output 1

# Calculate P(X ≤ 4)
P_X_leq_4 <- integrate(f, 3, 4)$value

# Calculate P(X < 4)
P_X_lt_4 <- integrate(f, 3, 4)$value  # Same calculation since the interval does not include the point

# Print the probabilities
P_X_leq_4
P_X_lt_4

# Calculate P(3.5 ≤ X ≤ 4.5)
P_3_5_to_4_5 <- integrate(f, 3.5, 4.5)$value

# Calculate P(X > 4.5)
P_X_gt_4_5 <- integrate(f, 4.5, 5)$value  # Only integrate from 4.5 to 5, since density is 0 after 5

# Print the probabilities
P_3_5_to_4_5
P_X_gt_4_5

#-------------------------------------------------------------------------------
#
#-------------------------------------------------------------------------------
# Define parameters for the uniform distribution
A <- -8
B <- 8

# Function to compute P(X < x)
P_less_than <- function(x) {
  if (x < A) {
    return(0)
  } else if (x > B) {
    return(1)
  } else {
    return((x - A) / (B - A))
  }
}

# Function to compute P(a < X < b)
P_between <- function(a, b) {
  if (a < A) {
    a <- A
  }
  if (b > B) {
    b <- B
  }
  if (a >= b) {
    return(0)
  }
  return((b - a) / (B - A))
}

# (a) Compute P(X < 0)
prob_a <- P_less_than(0)
print(paste("P(X < 0):", prob_a))

# (b) Compute P(-4 < X < 4)
prob_b <- P_between(-4, 4)
print(paste("P(-4 < X < 4):", prob_b))

# (c) Compute P(-6 ≤ X ≤ 7)
prob_c <- P_between(-6, 7)
rounded_prob_c <- round(prob_c, 2)
print(paste("P(-6 ≤ X ≤ 7):", rounded_prob_c))

# (d) Compute P(k < X < k + 4) for k in (-8, 8)
# You can set any value of k that satisfies the condition
k <- -6  # Choose a value for k (e.g., k = -6)
prob_d <- P_between(k, k + 4)
rounded_prob_d <- round(prob_d, 2)
print(paste("P(k < X < k + 4):", rounded_prob_d))
#-------------------------------------------------------------------------------
#
#-------------------------------------------------------------------------------

