# ANOVA

#### packages ####
library(car)
library(dplyr)
library(ggpubr) #graph

alpha_half <- 0.975 #0.995
alpha <- 0.95


#### Creating df -------------------------------------------------------------

#setwd('C:/Users/ngyen/OneDrive/MITB/Statistical Analysis in R/Session 08 - ANOVA')
#my_data <- read.csv("ANOVA1.csv")
my_data <- data.frame("X" = c(254,263,241,237,251,234,218,235,227,216,200,222,197,206,204),
                      "Group" = c(rep("Group1",5),rep("Group2",5),rep("Group3",5)))

group_by(my_data, Group) %>% 
  summarise(count = n(), mean = mean(X),sd = sd(X))


############################ Running ANOVA ------------------------------------------------------------

# Setting up ANOVA
res.aov <- aov(X ~ Group, data = my_data)

# Summary of the analysis
summary(res.aov)


# Identify which one(s) is different from others
TukeyHSD(res.aov)

#Check the homogeneity of variance assumption
# H0: the variance across groups is equal
# H1: the variance across groups is not equal
leveneTest(X ~ Group, data = my_data)


#Check the normality assumption
aov_residuals <- residuals (object = res.aov)
shapiro.test(x = aov_residuals)


#Kruskal-Wallis Test
my_data <- PlantGrowth

my_data$group <- ordered(my_data$group,
                         levels = c("ctrl", "trt1", "trt2"))

#Kruskal-Wallis test
kruskal.test(weight ~ group, data = my_data)

#Multiple pairwise-comparison between groups
pairwise.wilcox.test(PlantGrowth$weight, PlantGrowth$group,
                     p.adjust.method = "BH")


############################ Running two-way ANOVA -----------------------------

## Null Hypothesis
# There is no difference in the means of factor A
# There is no difference in means of factor B
# There is no interaction between factors A and B

## Conduct 2-way ANOVA in order to find out whether tooth length depends on supp and dose (α = 5%).
#### load data ----
data("ToothGrowth")
my_data <- ToothGrowth
set.seed(1234)
dplyr::sample_n(my_data, 10)
str(my_data)
my_data$dose <- factor(my_data$dose, levels = c(0.5, 1, 2), labels = c("D0.5", "D1", "D2")) 
# to change data type where necessary 

#### Generate frequency/mean table ----
table(my_data$supp, my_data$dose) #frequency
group_by(my_data, supp, dose) %>% 
  summarize(
    count = n(),
    mean = mean(len, na.rm = TRUE),
    sd = sd(len, na.rm=TRUE)
  )


#### visualize data----
c_pal <- c("#00AFBB", "#E7B800")
ggboxplot(my_data, x = "dose", y="len", color = "supp", palette = c_pal )
ggline(my_data, x = "dose", y="len", color = "supp", add = c("mean_se", "dotplot"), palette = c_pal)

#### compute aov ----
## assumption that two factors are independent (additive model)
res.aov2 <- aov(len ~ supp + dose, data= my_data)
summary(res.aov2)

## two variables might interact 
res.aov3 <- aov(len ~ supp * dose, data=my_data)
summary(res.aov3)
# in the situation where the interaction is not significant you should use the additive model.


# Interpretation 
# 1) for each variable, see if statistically significant. IF p<alpha, this implies that 
#    Xi is associated with Y
# 2) check if interactionn is statistically significant. IF p<alpha then X1 and Y depends on X2
# 

#### Tukey test (pairwise check)----
# to test of features with > 2 categories
TukeyHSD(res.aov3, which = "dose")
# if p-val < alpha, then can reject null hypothesis 

#### Levene's Test (variance homogeneity) ----
leveneTest(len ~ supp*dose, data = my_data)
# if p-val > alpha, then variance across groups is not statistically significant. 
# therefore can assume homogeneity of variances. 

# Shapiro-Wilk test (normality)----
aov_residuals <- residuals(object = res.aov3)
shapiro.test((x = aov_residuals))
# if p-val < alpha --> NOT NORMAL 

############################ Running two-way ANOVA [Unequal freq] --------------
my_anova <- aov(len ~ supp * dose, data = my_data)
Anova(my_anova, type = "III")