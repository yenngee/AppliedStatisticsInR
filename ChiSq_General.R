# CHI-SQ

#----> Case when Inputs are in matrix
################################ Creating input ###############################
# Method 1 
Input =("
Injection.area  No.severe  Severe
Thigh           4788       30
Arm             8916       76
")

Matriz = as.matrix(read.table(textConnection(Input),
                              header=TRUE,
                              row.names=1))

# Method 2 
R1 <- c(45,75, 23)
R2 <- c(37,126, 25)
R3 <- c(22, 43, 16)
rows <- 3

Matriz <- matrix(c(R1,R2,R3),nrow=rows, byrow =TRUE)

rownames(Matriz) <- c("Low Footfall","Medium Footfall","High Footfall")
colnames(Matriz) <- c("Low Price","Medium Price","High Price")

################################ use chisq.test ###############################
Result <- chisq.test(Matriz,correct=FALSE)
print(Result) 


################################ Interpretation ###############################
# H_0: There is no association 
# H_1: There is association / not independent 
  #### When p-value > alpha #### 
# > Pearson's Chi-squared test
# >
# > data:  Matriz
# > X-squared = 0.75758, df = 1, p-value = 0.3841
#
# Since p-value > 5%, we do not reject the null hypothesis. 
# We do not enough evidence to conclude that association exists between "Col" and "Row".

  #### When p-value < alpha ####
# > Pearson's Chi-squared test
# 
# > data:  Matriz
# > X-squared = 49.632, df = 2, p-value = 1.669e-11
#
# Since p-value < 5%, we do not reject the null hypothesis. 
# We have enough evidence to conclude that 
# association exists between "Col" and "Row". / "Col." is not independent of "Row."

Result$observed
# >                  Plan1 Plan2 Plan3
# > Salaried workers   160   140    40
# > Hourly workers      40    60    60

Result$expected
# >                  Plan1 Plan2 Plan3
# > Salaried workers   136   136    68
# > Hourly workers      64    64    32

Result$observed - Result$expected
# >                  Plan1 Plan2 Plan3
# > Salaried workers    24     4   -28
# > Hourly workers     -24    -4    28

# Further conclusion required e.g.: 
# Salaried workers tend to choose Plan 1 while Hourly workers tend to choose Plan 3



# ---> Goodness of fit test 
################################ Creating input ###############################
# uniform distribution 
freq <- c(290,250,238,257,265,230,192)
prob <- c(1/7,1/7,1/7,1/7,1/7,1/7,1/7)

# binomial distribution 
freq <- c(1,2,3,9,18,26,21,13,5,2,0)  # observed 
prob <- dbinom(0:10,10,0.5)           # get the distribution 


################################ use chisq.test ###############################
Result <- chisq.test(freq, p=prob,correct=FALSE)
print(Result) 
Result$expected
################################ Interpretation ###############################
# H_0: Distribution is ____
# H_1: Distribution is not ____ 

# > Chi-squared test for given probabilities
# > 
# > data:  call.freq
# > X-squared = 23.049, df = 6, p-value = 0.0007803

# Since p-value< 5%, we reject the null hypothesis. 
# We have enough evidence to conclude that the distribution of calls is not uniform