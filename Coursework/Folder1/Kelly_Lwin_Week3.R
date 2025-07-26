# Function to calculate permutations: P(n, k) = n! / (n - k)!
permutations <- function(n, k) {
  return(factorial(n) / factorial(n - k))
}

# Function to calculate combinations: C(n, k) = n! / (k! * (n - k)!)
combinations <- function(n, k) {
  return(factorial(n) / (factorial(k) * factorial(n - k)))
}

# Function to calculate the probability of selecting less than a given number of items from groups
calculate_prob_less_than_given <- function(num_total, num_select, max_items) {
  total_probability <- 0
  
  # Loop through possible counts of items selected
  for (i in 0:(max_items - 1)) {
    if (i <= num_total && (num_select - i) <= num_total) {
      total_probability <- total_probability + 
        (combinations(num_total, i) * combinations(num_total, num_select - i))
    }
  }
  
  total_ways <- combinations(num_total + num_total, num_select)  # Total combinations for selection
  probability <- total_probability / total_ways
  
  cat("Probability of selecting less than", max_items, "items:", probability, "\n")
}

# Main function to execute different probability scenarios
calculate_probabilities <- function(num_total_a, num_total_b, num_select, max_items) {
  # Probability of selecting all from group A
  prob_all_a <- combinations(num_total_a, num_select) / combinations(num_total_a + num_total_b, num_select)
  cat("Probability of selecting all from group A:", prob_all_a, "\n")
  
  # Probability of selecting less than a given number of items from group B
  calculate_prob_less_than_given(num_total_b, num_select, max_items)
}

# Example usage:
# Assume we have 6 items from group A and 2 items from group B, selecting 5 items
calculate_probabilities(num_total_a = 6, num_total_b = 2, num_select = 5, max_items = 3)

# Example with different group sizes
calculate_probabilities(num_total_a = 8, num_total_b = 4, num_select = 5, max_items = 3)
