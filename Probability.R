# Descriptive statistics
library(combinat)
library(gtools)


# permuation and combinations ----
factorial(5)

# e.g. 7C3
choose(7,3)
combn(7,3)


# permutations
permutations(3,2)
nrow(permutations(3,2))


# descriptive statistics ----
x <- c(-2,-1,0,1,2)
px <- c(1/8,2/8,2/8,2/8,1/8)

meanp <- sum(x*px)
varp <- sum((x-meanp)^2*px)
meanp
varp

x <- c(2,14,2,6,43,2)
mean(x)
sd(x)
summary(x)
range(x)
median(x)
mode(x)
quantile(x)

# distribution ----
# normal - find probability
pnorm(60, mean=48, sd=8, lower.tail=TRUE)


# binom - find probability
pbinom(c(5,4), size = 14, prob = 0.13, lower.tail=TRUE)

# binom - density curve 
dbinom(0:10, size = 10, prob = 0.5)

# binom - find smallest number of x s.t. P(X <= x) > 0.95
qbinom(0.95, size = 10, 0.5)


# poiss - find 
# rate is always per time 
ppois(5, 2)
# CLT ---------

# for sample mean 
sample_mean <- 44 
n <- 64
pop_sd <- 16
sample_sd <- pop_sd/sqrt(n)

# P(X < 85)
pnorm(85, sample_mean, sample_sd)



# for proportion
p <- 0.6
n <- 150
sample_mean <- p #proportion 
sample_sd <- sqrt((p*(1-p))/n)
pnorm(0.52, sample_mean, sample_sd)
