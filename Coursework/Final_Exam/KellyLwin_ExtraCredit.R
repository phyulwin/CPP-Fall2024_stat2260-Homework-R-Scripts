#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Given parameters
lambda <- 0.7 # average rate of people per minute

# a) Probability that 2 people get in line in a minute
prob_2 <- dpois(2, lambda)
cat("a) Probability of 2 people getting in line:", round(prob_2, 4), "\n")

# b) Probability that more than 2 people get in line in a minute
prob_more_than_2 <- 1 - ppois(2, lambda)
cat("b) Probability of more than 2 people getting in line:", round(prob_more_than_2, 4), "\n")
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+# Parameters
total_cards <- 25  # Total number of cards
trap_cards <- 10   # Number of trap cards
non_trap_cards <- 15  # Number of non-trap cards
hand_size <- 7  # Number of cards in the hand

# Part (b): Probabilities
# Pr(Exactly 2 trap cards in your hand)
p_exactly_2 <- dhyper(2, trap_cards, non_trap_cards, hand_size)

# Pr(No more than 2 trap cards in your hand)
p_no_more_than_2 <- phyper(2, trap_cards, non_trap_cards, hand_size)

# Pr(At least 2 trap cards in your hand)
p_at_least_2 <- 1 - phyper(1, trap_cards, non_trap_cards, hand_size)

# Round probabilities to four decimal places
p_exactly_2 <- round(p_exactly_2, 4)
p_no_more_than_2 <- round(p_no_more_than_2, 4)
p_at_least_2 <- round(p_at_least_2, 4)

# Part (c): Mean and standard deviation
mean_trap_cards <- hand_size * (trap_cards / total_cards)
variance_trap_cards <- hand_size * (trap_cards / total_cards) * 
  (1 - trap_cards / total_cards) * 
  ((total_cards - hand_size) / (total_cards - 1))
sd_trap_cards <- sqrt(variance_trap_cards)

# Round mean and standard deviation to two decimal places
mean_trap_cards <- round(mean_trap_cards, 2)
sd_trap_cards <- round(sd_trap_cards, 2)

# Output results
cat("Pr(Exactly 2 trap cards in your hand) =", p_exactly_2, "\n")
cat("Pr(No more than 2 trap cards in your hand) =", p_no_more_than_2, "\n")
cat("Pr(At least 2 trap cards in your hand) =", p_at_least_2, "\n")
cat("Mean number of trap cards =", mean_trap_cards, "\n")
cat("Standard deviation of trap cards =", sd_trap_cards, "\n")
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Define the function and limits
lower_limit <- 0
upper_limit <- 2

# Perform the integral of k * x^2 from 0 to 2
k <- 1 / integrate(function(x) x^2, lower_limit, upper_limit)$value

# Print the value of k rounded to four decimal places
k_rounded <- round(k, 4)
k_rounded

# Define the limits of the PDF
lower_limit <- 0
upper_limit <- 2

# Solve for k by normalizing the PDF
integrate_function <- function(x) x^2  # x^2 part of the PDF
k <- 1 / integrate(integrate_function, lower_limit, upper_limit)$value

# Display the value of k
cat(sprintf("Normalization constant (k): %.4f\n", k))

# Function to calculate the probability for a given range
probability <- function(a, b) {
  integrate(function(x) k * x^2, a, b)$value
}

# (b) Probability that students show up within 1 minute (0 to 1)
prob_b <- probability(0, 1)
cat(sprintf("P(X <= 1): %.4f\n", prob_b))

# (c) Probability that students show up between 30 and 75 seconds (0.5 to 1.25 minutes)
prob_c <- probability(0.5, 1.25)
cat(sprintf("P(0.5 <= X <= 1.25): %.4f\n", prob_c))

# (d) Probability that students show up at least 75 seconds (1.25 to 2 minutes)
prob_d <- probability(1.25, 2)
cat(sprintf("P(X >= 1.25): %.4f\n", prob_d))
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Given data
n <- 356  # Total number of players
x <- 200  # Number of players who think it's a good game
alpha <- 0.05  # Significance level

# Calculate the sample proportion
p_hat <- x / n

# Calculate the z critical value for 95% confidence
z <- qnorm(1 - alpha / 2)

# Calculate the standard error
se <- sqrt(p_hat * (1 - p_hat) / n)

# Calculate the margin of error
margin_of_error <- z * se

# Calculate the confidence interval
lower_bound <- p_hat - margin_of_error
upper_bound <- p_hat + margin_of_error

# Print the results
cat("The 95% confidence interval is (", round(lower_bound, 3), ",", round(upper_bound, 3), ")\n")
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Given parameter
lambda <- 0.01417

# (a) Probabilities for the given distances
# Probability at most 100 meters
prob_at_most_100 <- pexp(100, rate = lambda)
# Probability at most 200 meters
prob_at_most_200 <- pexp(200, rate = lambda)
# Probability between 100 and 200 meters
prob_between_100_and_200 <- prob_at_most_200 - prob_at_most_100

# Round the probabilities to 4 decimal places
prob_at_most_100 <- round(prob_at_most_100, 4)
prob_at_most_200 <- round(prob_at_most_200, 4)
prob_between_100_and_200 <- round(prob_between_100_and_200, 4)

# (b) Probability that Z exceeds its mean by more than 2 standard deviations
mean_Z <- 1 / lambda  # Mean of exponential distribution
sd_Z <- 1 / lambda    # Standard deviation of exponential distribution (same as mean)
threshold <- mean_Z + 2 * sd_Z
prob_exceeds_threshold <- 1 - pexp(threshold, rate = lambda)

# Round to 4 decimal places
prob_exceeds_threshold <- round(prob_exceeds_threshold, 4)

# (c) Median distance
median_Z <- qexp(0.5, rate = lambda)

# Round to 2 decimal places
median_Z <- round(median_Z, 2)

# Print results
cat("Part (a):\n")
cat("Probability at most 100 meters:", prob_at_most_100, "\n")
cat("Probability at most 200 meters:", prob_at_most_200, "\n")
cat("Probability between 100 and 200 meters:", prob_between_100_and_200, "\n")

cat("\nPart (b):\n")
cat("Probability Z exceeds its mean by more than 2 standard deviations:", prob_exceeds_threshold, "\n")

cat("\nPart (c):\n")
cat("Median distance:", median_Z, "meters\n")
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Given data
y <- c(0, 1, 2, 3)
p_y <- c(0.40, 0.30, 0.25, 0.05)

# Derived probabilities
p_no_topping <- p_y[1]        # Probability of 0 toppings
p_at_least_one <- sum(p_y[-1]) # Probability of at least 1 topping

# Sample size
n <- 15

# (a) Probability that at least 10 have put no toppings
p_at_least_10_no_topping <- sum(dbinom(10:15, size = n, prob = p_no_topping))

# (b) Probability that fewer than half have put at least one topping
p_fewer_than_half_at_least_one <- sum(dbinom(0:7, size = n, prob = p_at_least_one))

# (c) Probability that the number with at least one topping is between 5 and 10, inclusive
p_5_to_10_at_least_one <- sum(dbinom(5:10, size = n, prob = p_at_least_one))

# Results
cat("Probability (a):", round(p_at_least_10_no_topping, 3), "\n")
cat("Probability (b):", round(p_fewer_than_half_at_least_one, 3), "\n")
cat("Probability (c):", round(p_5_to_10_at_least_one, 3), "\n")
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+# Given data
data <- c(2.82, 2.55, 2.69, 3.90, 2.36, 2.68, 3.88, 4.19, 3.83, 
          4.31, 3.43, 4.51, 2.48, 3.63, 2.87, 3.50, 2.94)

# Calculations
sum_x <- sum(data)
sum_x_squared <- sum(data^2)

# Sample variance and standard deviation
n <- length(data)
sample_variance <- sum((data - mean(data))^2) / (n - 1)
sample_sd <- sqrt(sample_variance)

# Results
cat("Σxi =", sum_x, "mm2\n")
cat("Σxi^2 =", sum_x_squared, "mm4\n")
cat("s^2 =", round(sample_variance, 4), "mm4\n")
cat("s =", round(sample_sd, 4), "mm2\n")
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++