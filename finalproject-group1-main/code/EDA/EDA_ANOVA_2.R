# Load necessary libraries
library(dplyr)

# Read the dataset
data <- read.csv('data/WA_Fn-UseC_-HR-Employee-Attrition.csv')

# ANOVA for JobSatisfaction across Departments
anova_js_model <- aov(JobSatisfaction ~ Department, data = data)
anova_js_result <- summary(anova_js_model)

# Print ANOVA results
print("ANOVA for JobSatisfaction across Departments")
print(anova_js_result)

# "ANOVA for JobSatisfaction across Departments"
# Df Sum Sq Mean Sq F value Pr(>F)
# Department     2    1.2  0.6111   0.502  0.605
# Residuals   1467 1785.5  1.2171               