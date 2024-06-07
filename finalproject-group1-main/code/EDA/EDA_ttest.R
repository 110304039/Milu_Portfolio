# Load necessary libraries
library(dplyr)

# Read the dataset
data <- read.csv('data/WA_Fn-UseC_-HR-Employee-Attrition.csv')

# T-Test for Independent Samples between MonthlyIncome and Attrition
attrition_yes_income <- data %>% filter(Attrition == 'Yes') %>% pull(MonthlyIncome)
attrition_no_income <- data %>% filter(Attrition == 'No') %>% pull(MonthlyIncome)
t_test_income_result <- t.test(attrition_yes_income, attrition_no_income, var.equal = FALSE)

# Print T-Test results
print("T-Test for Independent Samples between MonthlyIncome and Attrition")
print(t_test_income_result)