# Confidence Interval and Hypothesis Testing 

#### packages ####
library(BSDA)
library(PASWR)

alpha_half <- 0.975 #0.995
alpha <- 0.95

# ------------------------------------------------------------------------------

#### Population Mean - Single
# n >= 30 | sigma is known   (use Z test, sigma) ---------------------------------
sample_mean <- 25
sd <- 10
n <- 50
alpha_half <- 0.975 #0.995
alpha <- 0.95

upper <- sample_mean + qnorm(alpha_half)*sd/sqrt(n)
lower <- sample_mean - qnorm(alpha_half)*sd/sqrt(n)
c(lower, upper)

Z <- (sample_mean - 25)/(sd/sqrt(n))
Z
qnorm(alpha_half)

# n <  30 | sigma is known   (use Z test, sigma) -------------------------------
X <- c(16.03,16.04,16.05,16.05,16.02,16.01,15.96,15.98,16.02,15.99)

n <- length(X)
sample_mean <- mean(X)
sd <- 0.020
alpha_half <- 0.975 #0.995
alpha <- 0.95

Z <- (sample_mean - 16)/(sd/sqrt(n))
Z
qnorm(alpha_half)

z.test(X, mu = 16, alternative="two.sided",
       sigma.x=sd,
       conf.level=alpha)

# n >= 30 | sigma is unknown (use Z test, s) -----------------------------------
sample_mean <- 25
sample_sd <- 10
n <- 50
alpha_half <- 0.975 #0.995

upper <- sample_mean + qnorm(alpha_half)*sample_sd/sqrt(n)
lower <- sample_mean - qnorm(alpha_half)*sample_sd/sqrt(n)
c(lower, upper)

Z <- (sample_mean - 22.23)/(sample_sd/sqrt(n))
Z
qnorm(alpha_half)


# n <  30 | sigma is unknown (use t test, s) ------------------------------------
S <- c(560, 500, 470, 660, 640)
n <- length(S)
alpha_half <- 0.975 #0.995
alpha <- 0.95

# CI
sample_mean <- mean(S)
sample_sd <- sqrt(var(S))
upper <- sample_mean + qt(alpha_half, n-1)*sample_sd/sqrt(n)
lower <- sample_mean - qt(alpha_half, n-1)*sample_sd/sqrt(n)
c(lower, upper)

t <-(sample_mean - 630)/(sample_sd/sqrt(n))
t
qt(alpha_half, n-1)

t.test(S, mu = 630, 
       alternative = "two.sided", paired=FALSE, 
       conf.level = alpha)

# ------------------------------------------------------------------------------

#### Population Mean - unpaired samples
# nx, ny >= 30 | sigma_x and sigma_y is known   (use Z test, sigma) ------------
sample_mean_X <- 25
sample_mean_Y <- 27
sd_X <- 5
sd_Y <- 8
n_X <- 50
n_Y <- 100
alpha <- 0.95
alpha_half <- 0.975

upper <- (sample_mean_X - sample_mean_Y) + qnorm(alpha_half)*sqrt((sd_X^2/n_X)+(sd_Y^2/n_Y))
lower <- (sample_mean_X - sample_mean_Y) - qnorm(alpha_half)*sqrt((sd_X^2/n_X)+(sd_Y^2/n_Y))
c(lower, upper)

Z <- (sample_mean_X - sample_mean_Y)/sqrt((sd_X^2/n_X)+(sd_Y^2/n_Y))
Z

qnorm(alpha_half)

# nx, ny <  30 | sigma_x and sigma_y is known   (use Z test, sigma) ------------
X <- c(16.03,16.04,16.05,16.05,16.02,16.01,15.96,15.98,16.02,15.99)
Y <- c(16.02,15.97,15.96,16.01,15.99,16.03,16.04,16.02,16.01,16.00)

sample_mean_X <- mean(X)
sample_mean_Y <- mean(Y)
sd_X <- 0.020
sd_Y <- 0.025
n_X <- length(X)
n_Y <- length(Y)
alpha <- 0.95
alpha_half <- 0.975

Z <- (sample_mean_X - sample_mean_Y)/sqrt((sd_X^2/n_X)+(sd_Y^2/n_Y))
Z
qnorm(alpha_half)

z.test(X, Y, alternative="two.sided",
       sigma.x=sd_X, sigma.y=sd_Y,
       conf.level=0.95)

# nx, ny >= 30 | sigma_x and sigma_y is unknown (use Z test, s) -------------
sample_mean_X <- 25
sample_mean_Y <- 27
sample_sd_X <- 5
sample_sd_Y <- 8
n_X <- 50
n_Y <- 100
alpha <- 0.95
alpha_half <- 0.975

upper <- (sample_mean_X - sample_mean_Y) + qnorm(alpha_half)*sqrt((sample_sd_X^2/n_X)+(sample_sd_Y^2/n_Y))
lower <- (sample_mean_X - sample_mean_Y) - qnorm(alpha_half)*sqrt((sample_sd_X^2/n_X)+(sample_sd_Y^2/n_Y))
c(lower, upper)

Z <- (sample_mean_X - sample_mean_Y)/sqrt((sample_sd_X^2/n_X)+(sample_sd_Y^2/n_Y))
Z

qnorm(alpha_half)

# nx, ny <  30 | sigma_x and sigma_y is unknown but equal (use t test, s) ------

# Method 1: not given samples 
sample_mean_X <- 3.27
sample_mean_Y <- 2.53
n_X <- 21
n_Y <- 25
sample_sd_X <- 1.30
sample_sd_Y <- 1.16

alpha_half <- 0.975 #0.995
alpha <- 0.95

Var_p <- ((n_X-1)*sample_sd_X^2+(n_Y-1)*sample_sd_Y^2)/((n_X-1)+(n_Y-1))
t <- (sample_mean_X-sample_mean_Y)/sqrt(Var_p*(1/n_X + 1/n_Y))

Var_p
t
qt(alpha_half,n_X+n_Y-2)

# Get CI 
upper <- (sample_mean_X-sample_mean_Y) + qt(alpha_half,n_X+n_Y-2)*sqrt(Var_p*(1/n_X + 1/n_Y))
lower <- (sample_mean_X-sample_mean_Y) - qt(alpha_half,n_X+n_Y-2)*sqrt(Var_p*(1/n_X + 1/n_Y))
c(lower, upper)

# Method 2: using t.test
X <- c(160, 162,162,165,170,172,172,172)
Y <- c(160, 160, 162, 163, 165, 167, 168, 169)

t.test(X,Y, 
       alternative = "two.sided", paired=FALSE, var.equal = TRUE, 
       conf.level = alpha)

# nx, ny <  30 | sigma_x and sigma_y is unknown but unequal (use t test, s) ----

# Method 2: using t.test
X <- c(160, 162, 162, 165, 170, 172, 172, 172)
Y <- c(160, 160, 162, 163, 165, 167, 168, 169)

t.test(X,Y, 
       alternative = "two.sided", paired=FALSE, var.equal = FALSE, 
       conf.level = alpha)

# ------------------------------------------------------------------------------

#### Population Mean - paired samples
# nx, ny >= 30 | sigma_x and sigma_y is known   (use Z test, sigma) ------------


# nx, ny <  30 | sigma_x and sigma_y is known   (use Z test, sigma) ------------


# nx, ny >= 30 | sigma_x and sigma_y is unknown (use Z test, s) -------------
before_mean <- 145
after_mean <- 147
sd <- 20
n <- 200 


# nx, ny <  30 | sigma_x and sigma_y is unknown   (use t test, s) -------------

# method 1 
before <- c(67, 24, 57, 55, 63, 54, 56, 68, 33, 43)
after  <- c(70, 38, 58, 58, 56, 67, 68, 77, 42, 38)
diff <- before - after
sd <- sd(diff)
n <- length(diff)
xbar <- mean(diff)
lower <- xbar - qt(0.975, n-1)*sd/sqrt(n)
upper <- xbar + qt(0.975, n-1)*sd/sqrt(n)
c(lower, upper)

t = xbar/(sd/sqrt(n))


#Method 2
before <- c(6,20,3,0,4)
after <- c(4,6,2,0,0)

t.test(before,after,alternative="two.sided",paired=TRUE,conf.level=0.95)

# method 3 
sd <- 0.54
n <- 20
xbar <- 3.18 - 3.1
# lower <- xbar - qt(0.95, n-1)*sd/sqrt(n)
# upper <- xbar + qt(0.95, n-1)*sd/sqrt(n)
# c(lower, upper)

t = xbar/(sd/sqrt(n))
t
qt(0.975, n-1)

# ------------------------------------------------------------------------------

#### Population Proportion - single------------------------
# Method 1 - CI
alpha_half <- 0.995 
alpha <- 0.99
p <- 0.21
n <- 100

upper <- p+qnorm(alpha_half)*sqrt((p*(1-p))/n)
lower <- p-qnorm(alpha_half)*sqrt((p*(1-p))/n)
c(lower,upper)

# Method 2 - prop.test
p_nr <- 21
n <- 100
prop.test(p_nr, n, conf.level =alpha, correct= FALSE)


#### Population Proportion - unpaired 2 samples------------------------
# Method 1: CI 
n_x <- 150
n_y <- 150
p_x <- 15/n_x
p_y <- 6/n_y
p_0 <- (n_x*p_x + n_y*p_y) / (n_x+n_y)
alpha_half <- 0.975 #0.995
alpha <- 0.95

lower <- (p_x-p_y) - qnorm(alpha_half) * sqrt(p_x*(1-p_x)/n_x + p_y*(1-p_y)/n_y)
upper <- (p_x-p_y) + qnorm(alpha_half) * sqrt(p_x*(1-p_x)/n_x + p_y*(1-p_y)/n_y)
c(lower, upper)


# Method 2: find Z
z <- (p_x - p_y)/sqrt(p_0*(1-p_0)/n_x + p_0*(1-p_0)/n_y)
z
qnorm(alpha)

# Method 3: prop.test 
prop.test(x=c(15,6),n=c(150,150),altern_xtive="less",conf.level = alpha, correct = FALSE)

