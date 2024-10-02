# ctrl + enter runs a line your cursor is on
# text wrap is under tools -> global options -> 
#   code -> soft-wrap R source files, if you want it.

?c
?rep
rep(0,2) # repeats the number 0 two times. 
rep(2,1) # repeats the number 2 one time.

# explicitly stating the arguments lets you put 
#   them in any order. So this prints the number 0 twice,
#   even though the order is different from ?rep.
rep(times=2, x=0) 
rep(x=0, times=2)
rep(0, 2)

# you MUST combine all these different elements with 
#   the c() function.
# here we are just copying the data over from the example.
stair_data <- c(rep(0,2),
                rep(1,3),
                rep(2,6),
                rep(3,8),
                rep(4,5),
                rep(5,8),
                rep(6,16),
                rep(7,18),
                rep(8,9),
                rep(9,12),
                rep(10,13))
stair_data[c(1:5, 7:12)]
stair_data[c(1,2,3,4,5)]

table(stair_data) # shows the frequency distribution of the data

# assigning the frequency distribution to the variable 
#   "stair_dist"

?table # documentation for this one can be confusing.
stair_dist = table(stair_data); stair_dist

# this just does relative-frequency distribution. Note that 
#   this is the same as #prop.table(table(ex_data)), 
#   so you need to get the frequency distribution 
#   first, then the relative frequency after.
?prop.table
prop.table(stair_dist) # relative-frequency distribution

barplot(stair_dist) # how you would do a discrete histogram or barplot of categorical data
barplot(prop.table(stair_dist)) # relative frequency on vertical axis, same shape.


# Note that for the hist(ex_data, breaks=seq(...)) line below, the breaks 
#     should be like seq(from=min(data), to=max(data), length.out=bins+1). 
#     So minimum value in your dataset to the max value of the dataset, with 
#     length.out being desired number of bins + 1. 
# Desired number of bins is sometimes just sqrt(length(dataset)) but with a 
#     large data set this can be a large number of bins, so somewhere between 
#     5-20 is usually fine.

?seq
?hist
seq(from=0, to=10, by=1) # sequence of numbers from 0 to 10 incrementing by 2
seq(0, 10, length.out=5) # 5 numbers evenly spaced from 0 to 10

cont_data <- rnorm(100, 10, 1)

my_hist <- hist(cont_data, 
                breaks=seq(from=min(cont_data), 
                           to=max(cont_data), 
                           length.out=4+1), 
                right=F)

my_hist$counts






# ----- Simulation of data ----- #
?sample

# controls randomness so results are reproducible. If this is not run, then the 
#     next line will have a random data set each time.
set.seed(1)
rnorm(100)
data <- sample(x=seq(0, 10, by=0.5), size=20, replace=T)
double_data <- data*2

# a true/false vector corresponding to "data" indicating if the number at each 
#     index is equal to 6.5 or not.
data <= 6.5

which(data <= 6.5) # returns the index of numbers that are equal to 6.5 in data.
data[which(data <= 6.5)]
# -------------------------------------------------- #
# indexing in R does NOT start at 0, it starts at 1. #
# -------------------------------------------------- #

?length

# this is frequency of 6.5. You could store which(data == 6.5) 
#   into a variable above if you wanted, and just do 
#   length(that_variable)
f <- length(which(data == 6.5)) 

# size of the data set, number of observations.
n <- length(data) 

# this is relative frequency, so 10% of the data 
#   is equal to 6.5
f / n

# The right=F argument is how we should do our histograms in this class though 
#     it usually doesn't matter unless data is discrete and the bins are 
#     perfectly matched to the data values.
# right=F is saying our bins will be left-closed, right-open. So [0, 10) for 
#     example, instead of the default (0, 10].
hist(data, 
     breaks=seq(min(data), max(data), length.out=8), 
     right=F)

barplot(data)





# making a function to simulate the data of size n.
f <- function(n){
  #set.seed(1)
  data <- sample(x=seq(0, 10, by=0.5), size=n, replace=T)
  
  # this just prints the data on the function call. 
  #     if we assign the function call to a variable, 
  #     such as new_data <- f(20), then new_data will
  #     hold the value of what gets returned.
  return(data)
}
f(20)
new_data <- f(200)
new_data




# mean/median stuff 
(3+4+8+10+10+16+13+21+20+21+79)/11

# or...
x <- c(3,4,8,10,10,16,13,21,20,21,79)
sum(x)/length(x)

# or...
mean(x)
median(x)
