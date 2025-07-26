calculate_stats <- function(x) {
  # Calculate and print the mean of the data
  xbar <- mean(x)
  cat("Mean: ", xbar, "\n")
  
  # Calculate and print the median of the data
  med <- median(x)
  cat("Median: ", med, "\n")
  
  # Calculate and print the sample variance (s^2)
  s2 <- sum((x - xbar)^2) / (length(x) - 1)
  cat("Sample Variance (s^2): ", s2, "\n")
  
  # Calculate and print the sample standard deviation (s)
  s <- sqrt(s2)
  cat("Sample Standard Deviation (s): ", s, "\n")
  
  # Example usage
  # x <- c(5,5,5,6,7,8)
  # calculate_stats(x)
  
  # sample var and sample sd built-in functions - var(x) and sd(x)
}

calculate_population_stats <- function(x) {
  # Can also use built-in functions 
  # pop.var <- pop_var(x)  # Calculate population variance
  # pop.sd <- sqrt(pop_var(x))  # Calculate population standard deviation (sqrt of variance)
  
  # Calculate the mean of the data
  xbar <- mean(x)
  
  # Calculate the population variance
  pop_var <- sum((x - xbar)^2) / length(x)
  
  # Calculate the population standard deviation (square root of variance)
  pop_sd <- sqrt(pop_var)
  
  # Print the results
  cat("Population Variance: ", pop_var, "\n")
  cat("Population Standard Deviation: ", pop_sd, "\n")
  
  # Example usage:
  # After running this, you can manually input your data when prompted
  # data_hw <- scan()
  # Calculate and print population variance and standard deviation for the input data
  # calculate_population_stats(data_hw)
}

generate_boxplot <- function(bp_data) {
  # Print the median of the data
  cat("Median of the data: ", median(bp_data), "\n")
  
  # Print the summary of the data (includes potential outliers)
  cat("Summary of the data:\n")
  print(summary(bp_data))
  
  # Create a horizontal boxplot and store the result in 'bp'
  bp <- boxplot(bp_data, horizontal = TRUE)
  
  # Print the five-number summary (FNS: Min, Q1, Median, Q3, Max)
  cat("\nFive-Number Summary (FNS):\n")
  print(bp$stats)
  
  # Print the outliers, if any
  if (length(bp$out) > 0) {
    cat("\nOutliers:\n")
    print(bp$out)
  } else {
    cat("\nNo outliers found.\n")
  }
  # Example usage
  # bp_data <- c(171,178,201,203,205,211,218,223,234,249,256,359,408)
  # generate_boxplot(bp_data)
}

find_outliers <- function(data) {
  # Create a boxplot for the data and store the boxplot statistics in 'bp'
  bp <- boxplot(data, horizontal = TRUE) 
  
  # Define the lower and upper limits based on the boxplot statistics
  fl <- bp$stats[2]; # Lower fence (first quartile)
  fu <- bp$stats[4]; # Upper fence (third quartile)
  
  # Calculate the interquartile range (IQR)
  fs <- fu - fl 
  
  # Define the mild outlier range using 1.5 times the IQR
  mild_range <- c(fl - 1.5 * fs, fu + 1.5 * fs); 
  # Define the extreme outlier range using 3 times the IQR
  ext_range  <- c(fl - 3 * fs, fu + 3 * fs); 
  
  # Output the mild and extreme outlier ranges
  mild_range
  ext_range
  
  # Identify and print extreme outliers
  cat("Extreme Outliers: ", 
      data[which(data <= ext_range[1] | data >= ext_range[2])]
  )
  
  # Identify and print mild outliers
  cat("\n   Mild Outliers: ", 
      data[which((data <= mild_range[1] | data >= mild_range[2]) & 
                   !(data <= ext_range[1] | data >= ext_range[2]))]
  )
}

find_fourths <- function(data){
  data <- sort(data)
  
  if(length(data) %% 2 == 1){
    fl <- median(data[1:ceiling(length(data)/2)])
    fu <- median(data[ceiling(length(data)/2):length(data)])
  }
  else{
    fl <- median(data[1:length(data)/2])
    fu <- median(data[(length(data)/2 + 1):length(data)])
  }
  
  cat(" Lower Fourth: ", fl)
  cat("\n Upper Fourth: ", fu)
}

# EXAMPLE USAGE
bp_data <- c(171,178,201,203,205,211,218,223,234,249,256,359,408)
generate_boxplot(bp_data)

find_fourths(bp_data)
find_outliers(bp_data)