# REGRESSION 
library(MASS)
library(tidyverse)
library(broom) 
theme_set(theme_classic())

################################ Correlation #################################
X <- c(400,415,377,318,310,263,259,245,240,235,230,224)   # height
Y <- c(120,100,100,95,93,81,81,85,79,85,80,70)            # speed

cor(X,Y) #uses pearson
#cor(height,speed, method = "kendall")
#cor(height,speed, method = "spearman")

cor.test(X,Y) #checks for statistical significance 

############################## Linear Regression ##############################
#### Code template for 1 variable 
fit <- lm(Y~X)
summary(fit)


#### Code template for > 1 variable
# For all variables 
fit1 <- lm(y~x1+x2+x3+x4, data = my_data) # or lm(y~., data = my_data)
summary(fit1)

fit2 <- lm(y~1, data=my_data)             # intercept only 
summary(fit2)

stepwise <- stepAIC(fit, direction = "both", scope=list(upper=fit1, lower=fit2))
summary(stepwise)

backward <- stepAIC(fit1, direction = "backward")
summary(backward)

forward <- stepAIC(fit2, direction = "forward", scope=list(upper=fit1, lower=fit2))
summary(forward)


#### General Interpretation for straightforward LR
# 1) See if Intercept and X are statistically significant 
# 2) See if F-statistic / p-value (<0.05) is significant --> LR is statistically significant 
# 3) R-squared (or R-squared adjusted if >1 X) represents portion of total variation 
#    in the dependent variable that explained by variation in the independent variable
# 4) get the linear equation Y = mX + C etc

#### Interpretation for stepwise
# 1) finds the lowest AIC model 
