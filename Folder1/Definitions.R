# DEFINTIONS 
# population - the entire grp. of individuals of interest
# parameter - numeric summary of the population 
# sample - a subset of the population
# statistic - numeric summary of the sample 

# variables - are the characteristics an individual has
# numeric/ quantitative variables - have only numbered as their possible values [discrete / continuous]
# discrete variables - have values that can be counted or listed
# continuous variables - have values that can be any number in some internal measuring
# categorical/ qualitative - are variables which can be placed into non-numeric categories [nominal variables / ordinal variables]
# nominal variables - have no natural order
# ordinal variables - have a natural order

# histograms - are used to see shape and distribution of the data 
# frequency (f) - of a value in a dataset is how many times it occurs 
# relative frequency (r.f.) - is the proportion of time a value occurs r.f. = f/n

# histograms - are used to see shape and distribution of the data 
# frequency (f) - of a value in a dataset is how many times it occurs 
# relative frequency (r.f.) - is the proportion of time a value occurs r.f. = f/n
# a frequency distribution is a tabulation (table) of the frequencies

# Pr( a <= x <= b) = F(b) - F(a) where a-bar is the next largest value below a. 
# Cumulative Distribution Function (CDF)
# use when we want to know the probabilities of being at most som number 
# formula - F(x) = Pr( X <= x ) = EP(mu)

# Hypergeometric Distribution
# Assumptions: 
#   Finite Population size N
# Finite number of success in the pop. M
# Two outcomes  S or F
# Taking a sample of size n without replacement
# x = number of successes in sample 
# 
# code - h(x; n, M, N)
# formula 
# Pr(X=x) = (M x) (N-M n-x) / (N n)

# Binomial Distribution 
# If the following criteria are not met, then you have a binomial experiment. 
# - Fixed number of trails, n
# - Each trial has only 2 outcomes: S or F
# - Each trial is independent
# - Probability of success for each trial p is the same
# Then X = the number of successes is a binomial R.V. 
# Notation: X ~ Bin(n, P) 
# pmf : b(x, n, p)
# Probability of X successes in a binomial distribution is Pr(X = x) = (n x) p^x (1-p)^(n-x)

# If x is binomial: 
#   E[X] = np
# Var[X] = np(1--p)
# sigma of x = sqrt(np * (1-p))