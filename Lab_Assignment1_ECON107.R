#To work with these data in R we begin by generating two vectors:
#one for the student-teacher ratios (STR) and one for test scores (TestScore),
#both containing the data from the table above.
# Create sample data
STR <- c(15, 17, 19, 20, 22, 23.5, 25)
TestScore <- c(680, 640, 670, 660, 630, 660, 635)

#Q1. Report mean and variance for STR and TestScore
mean(STR)
mean(TestScore)
#Q2. Report covariance and correlation coefficient
cov(STR, TestScore)
cor(STR, TestScore)
#Q3. Report estimator b_0 and b_1
linear_reg <- lm(TestScore~STR)
summary(linear_reg)
linear_reg$coefficients

#Q4 Total sum of residuals
sum(linear_reg$residuals)

#Q5 create a scatterplot of the data
plot(TestScore ~ STR,ylab="Test Score",pch=20)
# add the systematic relationship to the plot
abline(a = 713, b = -3)

