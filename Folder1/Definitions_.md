# Definitions

## Population
The entire group of individuals of interest.

## Parameter
Numeric summary of the population.

## Sample
A subset of the population.

## Statistic
Numeric summary of the sample.

---

# Variables

## Characteristics
Variables are the characteristics an individual has.

### Numeric/Quantitative Variables
- Have only numbers as their possible values.
  - **Discrete Variables:** Values that can be counted or listed.
  - **Continuous Variables:** Values that can be any number in some interval.

### Categorical/Qualitative Variables
- Variables that can be placed into non-numeric categories.
  - **Nominal Variables:** Have no natural order.
  - **Ordinal Variables:** Have a natural order.

---

# Histograms
Used to see the shape and distribution of the data.

## Frequency (f)
The number of times a value occurs in a dataset.

## Relative Frequency (r.f.)
The proportion of time a value occurs:  
\[ r.f. = \frac{f}{n} \]

## Frequency Distribution
A tabulation (table) of the frequencies.

---

# Cumulative Distribution Function (CDF)
Used when we want to know the probabilities of being at most some number.

## Formula
\[ \Pr(a \leq x \leq b) = F(b) - F(a) \]  
Where \( \bar{a} \) is the next largest value below \( a \).  
\[ F(x) = \Pr(X \leq x) = EP(\mu) \]

---

# Distributions

## Hypergeometric Distribution
### Assumptions
- Finite population size \( N \).
- Finite number of successes in the population \( M \).
- Two outcomes: Success (S) or Failure (F).
- Taking a sample of size \( n \) without replacement.
- \( x \): number of successes in the sample.

### Code
\[ h(x; n, M, N) \]

### Formula
\[ \Pr(X=x) = \frac{{\binom{M}{x} \cdot \binom{N-M}{n-x}}}{{\binom{N}{n}}} \]

---

## Binomial Distribution
### Criteria
For a binomial experiment:
- Fixed number of trials \( n \).
- Each trial has only 2 outcomes: Success (S) or Failure (F).
- Each trial is independent.
- Probability of success \( p \) is the same for each trial.

### Notation
\[ X \sim \text{Bin}(n, p) \]

### Probability Mass Function (pmf)
\[ b(x, n, p) \]  
Probability of \( X \) successes in a binomial distribution:  
\[ \Pr(X = x) = \binom{n}{x} p^x (1-p)^{(n-x)} \]

### If \( x \) is binomial:
- Expected Value:  
\[ E[X] = np \]
- Variance:  
\[ \text{Var}[X] = np(1-p) \]
- Standard Deviation:  
\[ \sigma(X) = \sqrt{np(1-p)} \]
