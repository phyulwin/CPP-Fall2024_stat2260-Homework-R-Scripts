x <- c(5,5,5,6,7,8)
sum(x-mean(x)) # = 0

# var and sd in R
xbar <- mean(x)
s2 <- sum((x-xbar)^2)/(length(x)-1); s2
s <- sqrt(s2); s

# sample var and sample sd
var(x)
sd(x)

pop_var <- function(x){
  xbar <- mean(x)
  return(sum((x-xbar)^2)/(length(x)))
}

#10 25   67 10 100 102     401
# 
data_hw <- scan()

pop.var <- pop_var(x)
pop.sd <- sqrt(pop_var(x))

# sample variance and sd
var(x)
sd(x)

# keep in mind these commands above give SAMPLE variance and sd. 
#     If we wanted population, we would use the s2 and s above, but use 
#     length(x) instead of length(x)-1.

bp_data <- c(3,4,8,10,10,16,13,21,20,21,79)

# boxplot example
bp_data <- c(171,178,201,203,205,211,218,223,234,249,256,359,408)
median(bp_data)
summary(bp_data) # notice it includes outliers in FNS

boxplot(bp_data) # vertical! yuck
?boxplot
boxplot(bp_data, horizontal=T)
bp <- boxplot(bp_data, horizontal=T)
bp$stats # true FNS! use this on hw
bp$out # outliers!

fu <- bp$stats[4]
fl <- bp$stats[2]
fs <- fu-fl; fs
mild_outliers_range <- c(fl - 1.5*fs, fu + 1.5*fs); mild_outliers_range
ext_outliers_range <- c(fl - 3*fs, fu + 3*fs); ext_outliers_range

# getting actual outliers
bp_data[which(bp_data >= ext_outliers_range[2] | bp_data <= ext_outliers_range[1])]
bp_data[which(bp_data >= mild_outliers_range[2] | bp_data <= mild_outliers_range[1])]

find_outliers <- function(data){
  bp <- boxplot(data, horizontal=T)
  fl <- bp$stats[2]; fl
  fu <- bp$stats[4]; fu
  fs <- fu - fl
  
  mild_range <- c(fl - 1.5*fs, fu + 1.5*fs); mild_range
  ext_range  <- c(fl - 3*fs, fu + 3*fs); ext_range
  
  
  cat("Extreme Outliers: ", 
      data[which(data <=  ext_range[1] | data >=  ext_range[2])])
  
  cat("\n   Mild Outliers: ", 
      data[which((data <= mild_range[1] | data >= mild_range[2]) & 
                !(data <= ext_range[1] | data >= ext_range[2]))
           ]
      )
}
find_outliers(bp_data)
