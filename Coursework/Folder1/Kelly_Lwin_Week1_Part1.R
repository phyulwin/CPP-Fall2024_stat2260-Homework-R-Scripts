# Function to calculate relative frequency
relative_frequency <- function(data) {
  # Table to count frequencies of each value
  freq_table <- table(data)
  
  # Total number of observations (n)
  total_observations <- length(data)
  
  # Relative frequency = frequency / total number of observations
  rel_freq <- freq_table / total_observations
  
  return(rel_freq)
  
  # Example usage:
  # data <- c(1, 2, 2, 3, 3, 3, 4, 4, 4, 4)
  # result <- relative_frequency(data)
  # print(result)
}

# These methods are the same: rep(x=0, times=2) and rep(0, 2).

# Define the function to create histograms
create_histogram <- function(data) {
  # Create a histogram for the second dataset
  hist(data, 
       breaks = seq(min(data), max(data), length.out = data_breaks), 
       right = FALSE,
       main = "Histogram of Data",
       xlab = "Data Values",
       ylab = "Frequency")
  # Example usage:
  # data <- sample(x = seq(0, 10, by = 0.5), size = 20, replace = TRUE)  # Sample data for histogram
  # create_histogram(data)
}

# Define the function to create histograms
create_histograms <- function(cont_data, data, cont_breaks = 4, data_breaks = 8) {
  # Create a histogram for the continuous data
  my_hist <- hist(cont_data, 
                  breaks = seq(from = min(cont_data), 
                               to = max(cont_data), 
                               length.out = cont_breaks + 1), 
                  right = FALSE)
  
  # Print the counts for the continuous data histogram
  cat("Counts for continuous data histogram:\n")
  print(my_hist$counts)
  
  # Create a histogram for the second dataset
  hist(data, 
       breaks = seq(min(data), max(data), length.out = data_breaks), 
       right = FALSE,
       main = "Histogram of Data",
       xlab = "Data Values",
       ylab = "Frequency")
  # Example usage:
  # cont_data <- rnorm(100, 10, 1)  # Generate some sample continuous data
  # data <- sample(x = seq(0, 10, by = 0.5), size = 20, replace = TRUE)  # Sample data for histogram
  # create_histograms(cont_data, data)
}

# Define the function to calculate relative frequency
relative_frequency <- function(data, value) {
  # Count the frequency of the specified value in the dataset
  f <- length(which(data == value)) 
  
  # Calculate the size of the dataset (number of observations)
  n <- length(data) 
  
  # Calculate and return the relative frequency
  return(f / n)
  # Example usage:
  # data <- c(6.5, 2.3, 6.5, 7.8, 6.5, 3.4)  # Sample data
  # result <- relative_frequency(data, 6.5)
  # print(result)
}

# EXAMPLE USAGE 
# Create a vector `stair_data` where each value represents a discrete count
stair_data <- c(rep(0, 2),      # 0 appears 2 times
                rep(1, 3),      # 1 appears 3 times
                rep(2, 6),      # 2 appears 6 times
                rep(3, 8),      # 3 appears 8 times
                rep(4, 5),      # 4 appears 5 times
                rep(5, 8),      # 5 appears 8 times
                rep(6, 16),     # 6 appears 16 times
                rep(7, 18),     # 7 appears 18 times
                rep(8, 9),      # 8 appears 9 times
                rep(9, 12),     # 9 appears 12 times
                rep(10, 13))    # 10 appears 13 times

# Create a frequency table for `stair_data`
table(stair_data)  # Display frequency of each value in stair_data
stair_dist = table(stair_data); stair_dist  # Store the frequency table in `stair_dist`

# Calculate and display the proportion of each value in the frequency table
prop.table(stair_dist)  # Returns the relative frequency of each value

# Create a bar plot of absolute frequencies
barplot(stair_dist)  # Visual representation of counts for each value

# Create a bar plot of relative frequencies (proportions)
barplot(prop.table(stair_dist))  # Visual representation of proportions for each value

# Generate a sequence of numbers from 0 to 10, incrementing by 1
seq(from = 0, to = 10, by = 1)  # Produces: 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10

# Generate 5 evenly spaced numbers from 0 to 10
seq(0, 10, length.out = 5)  # Produces: 0, 2.5, 5, 7.5, 10