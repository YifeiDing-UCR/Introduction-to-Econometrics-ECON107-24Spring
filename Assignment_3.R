#1 Load the mtcars dataset into R and inspect its structure.
data(mtcars)
help(mtcars)
str(mtcars)
head(mtcars)

#2 Run a simple linear regression model using 'disp' as our independent variable to predict 'mpg'
model <- lm(mpg ~ disp, data = mtcars)
summary(model)

#3 Visualize the relationship between our variables (disp vs. mpg) to get an intuitive grasp of the data.
plot(mpg ~ disp, data=mtcars, main="mpg vs. disp", xlab="disp", ylab="mpg")
abline(model)

#4 Check the Linearity and Homoskedasticity assumptions by plotting residuals versus fitted values and interpret the plot. 
plot(model,which=1)

#R automatically flagged 3 data points that have large residuals. 
#To assess the linearity assumption: The red line through your scatterplot is curved, the linearity assumption is not satisfied. 
#To assess the homoscedasticity assumption: we look to make sure that the residuals are equally spread around the y = 0 line.

#5 Check the Normality assumption by plotting the Histogram of residuals.
sresid <- resid(model)
hist_res <- hist(sresid,  breaks = 10, main="Histogram of Residuals", xlab="Residuals")

## Add a normal distribution curve (optional)
x <- seq(min(sresid), max(sresid), length.out = 100) #Generate x values for the normal distribution curve
residual_mean <- mean(sresid)
residual_sd <- sd(sresid)
curve(dnorm(x, mean = residual_mean, sd = residual_sd) * sum(hist_res$counts * diff(hist_res$breaks)), 
      col = "red", add = TRUE, lwd = 2)

#6  Check the Normality assumption by plotting the Q-Q plot of residuals. 
#QQ-plot compares the residuals to “ideal” normal observations along the 45-degree line.
plot(model,which=2)
#Interpretation: R automatically flagged those same 3 data points that have large residuals
#The points deviate from the diagonal line, it suggests deviations from normality in the residuals.


#7 Log-transform the independent variable 'disp', re-run the regression and interpret the coefficients.
mtcars$log_disp <- log(mtcars$disp)
model_log <- lm(mpg ~ log_disp, data = mtcars)
# or run: model_log <- lm(mpg ~ log(disp), data = mtcars)
summary(model_log)
# Interpretation: 1% increase in the independent variable increases or decreases the dependent variable by the coefficient/100 units


#8 Repeat Steps 3 to 6 and interpret the impact of the log-transform.
plot(mpg ~ log_disp, data=mtcars, main="mpg vs. log-disp", xlab="log-disp", ylab="mpg")
# or run: plot(mtcars$mpg ~ mtcars$log_disp, main="mpg vs. log-disp", xlab="log-disp", ylab="mpg")
abline(model_log)

plot(model_log,which=1)
plot(model_log,which=2)

#9 Log-transform both dependent 'mpg' and independent variable 'disp'
mtcars$log_mpg <- log(mtcars$mpg)
model_log_log <- lm(log_mpg ~ log_disp, data = mtcars) 
summary(model_log_log)
# Interpretation: the coefficient represents the % change in the dependent variable for a 1% change in independent variable

#10 Repeat Steps 3 to 6 and interpret the impact of the log-transform
plot(log_mpg ~ log_disp, data=mtcars, main="log-mpg vs. log-disp", xlab="log-disp", ylab="log-mpg")
abline(model_log_log)

plot(model_log_log,which=1)
plot(model_log_log,which=2)
