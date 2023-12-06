# Data
x <- c(41, 41, 42, 43, 54, 53, 57, 58, 63, 66, 67, 67, 67, 68, 69, 70, 70, 70, 70, 72, 73, 75, 75, 76, 76, 78, 79, 81, 85, 86, 86, 88)
y <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

# Task (i) - Estimating the Logit Function
logregre <- glm(y ~ x, family = binomial())
logregre_summary <- summary(logregre)
print(logregre_summary)

# Task (ii) - Hypothesis Testing
# Calculate the test statistic for beta1
beta1_estimate <- coef(logregre)["x"]
beta1_se <- summary(logregre)$coefficients["x", "Std. Error"]
test_statistic <- beta1_estimate / beta1_se

# # Critical value for a significance level of 0.05
z_critical <- qnorm(0.975) # two-tailed test
print(test_statistic)
print(z_critical)

# Task (iii) - Probability Estimation at x = 65
newdata <- data.frame(x = 65)
predicted_probability <- predict(logregre, newdata, type = "response")
print(predicted_probability)
