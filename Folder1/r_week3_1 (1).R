# no function in base R for permutations.
perm <- function(n,k){
  return(factorial(n) / factorial(n-k))
}

# so 8 pick 5
factorial(8) / factorial(8-5)
perm(8,5)

# when order doesn't matter, use choose(n,k) for combinations
?choose

# 5 chairs, 8 people (6F and 2M)

# num of ways to seat a female in every chair
choose(6,5)

# probability of seating a female in every chair
choose(6,5) / choose(8,5)

# prob of seating 3F and 2M
(choose(6,3)*choose(2,2)) / choose(8,5)

# prob of less than 2M being sat
(choose(2,1)*choose(6,4) / choose(8,5)) + (choose(2,0)*choose(6,5) / choose(8,5))

# another way
sum(choose(2, 1:0) * choose(6,4:5) / choose(8,5))
