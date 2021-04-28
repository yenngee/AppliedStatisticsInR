# ADDITIONAL REGRESSION NOTES 

# Example and interpretation 
setwd('C:/Users/ngyen/OneDrive/MITB/Statistical Analysis in R/Session 09 - Regression')


#Call dataset
install.packages("datarium")
library(datarium)
data("marketing", package = "datarium")
sample_n(marketing, 3)

#Building the regression
model <- lm(sales ~ youtube, data = marketing)
model

#### Fitted values and residuals 
# youtube: X (Independent variable)
# sales: Y (Target variable)
# fitted: the fitted sale values
# resid: the residual errors
model.diag.metrics <- augment(model)
head(model.diag.metrics)

#### Regression diagnostics (based on regression assumptions)
par(mfrow = c(2, 2))
plot(model)

## Assumptions 
# 1) Linearity of data 
#    [Residuals vs Fitted] red line approximately horizontal at 0 is good 
#                          no fitted pattern 

# 2) Normality of residuals
#    [Normal Q-Q]

# 3) Homogeneity of residuals variance (resideuals with constant variance)
#    [Scale-Location] Horizontal line with equally spread points is good 

# 4) Independence of residuals error Terms 
#    [Residuals vs Leverage] no outliers that exceed 3 standard deviations 

# plots show top 3 most extreme data points labelled with row number

## Potential Problems
# 1) Non-linearity 
# 2) Non constant variance 
# 3) presence of influential values (like outliers in X and Y)

model2 <- lm(log(sales) ~ youtube, data = marketing)
par(mfrow = c(2, 2))
plot(model2)

# Add observations indices and drop some columns (.se.fit, .sigma) for simplification
model.diag.metrics <- model.diag.metrics %>%
  mutate(index = 1:nrow(model.diag.metrics)) %>%
  select(index, everything(), -.sigma)
# Inspect the data
head(model.diag.metrics, 4)

df2 <- data.frame(
  x = c(marketing$youtube, 500, 600),
  y = c(marketing$sales, 80, 100)
)
model2 <- lm(y ~ x, df2)
# Cook's distance
plot(model2, 4)
# Residuals vs Leverage
plot(model2, 5)







#Categorical variables with two levels
install.packages("carData")
library("carData")

# Load the data
data("Salaries", package = "carData")

# Inspect the data
sample_n(Salaries, 3)

# Compute the model
model <- lm(salary ~ sex, data = Salaries)
summary(model)

model.diag.metrics <- augment(model)
head(model.diag.metrics)

#Regression diagnostics
par(mfrow = c(2, 2))
plot(model)

contrasts(Salaries$sex)

#Categorical variables with more than two levels
model2 <- lm(salary ~ yrs.service + rank + discipline + sex, data = Salaries)
summary(model2)