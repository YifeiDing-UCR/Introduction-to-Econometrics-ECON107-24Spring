#The Boston Housing Data Set
#For the course of this section, you will work with Boston, the Boston Housing data set which contains 506 observations on housing values in suburbs of Boston. Boston data set comes with the package MASS.
#Both the package MASS and AER are required for the interactive R exercises below, and they are already installed.
#Instructions:

#1. Load both the package and the data set.
# attach both packages and load the data set
library(AER)
library(MASS)

#2. Get yourself an overview over the data using function(s) known from the previous sessions.
data("Boston")
# obtain an overview over the data set
summary(Boston)
# or
str(Boston)
# or
head(Boston)

#3. Estimate a simple linear regression model that explains the median house value of districts (medv) by the percent of households with low socioeconomic status, lstat, and a constant.
#Save the model to bh_mod.
bh_mod <- lm(medv ~ lstat, data = Boston)

#4. Print a coefficient summary to the console that reports standard errors.
coeftest(bh_mod)

#5. Regress the median housing value in a district, medv, on the average age of the buildings, age,
#the per-capita crime rate, crim,
#the percentage of individuals with low socioeconomic status, lstat, and a constant.
mod <- lm(medv ~  age + crim + lstat, data = Boston)

#6. Print a coefficient summary to the console that reports standard errors for the augmented model.
coeftest_mod <- coeftest(mod)
coeftest_mod

#7. The R2 of the simple regression model is stored in R2_res. Save the multiple regression model’s
#R2 to R2_unres and check whether the augmented model yields a higher  R2.
R2_unres <- summary(mod)$r.squared
R2_res <- summary(bh_mod)$r.squared
R2_unres > R2_res

#8.Compute t-statistics for each coefficient. Assign them to `tstat`
tstat <- coef(summary(mod))[, "t value"]
tstat

#9.Compute  p-values manually for each coefficient using cumulative distribution function (CDF) of t-distribution and assign them to pval.
df <- df.residual(mod) # Get degrees of freedom
pval <- 2 * pt(abs(tstat), df = df, lower.tail = FALSE)
pval

#10 Check whether the hypotheses are rejected at the 5% significance level
significant <- pval < 0.05
significant
