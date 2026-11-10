# TASK 1
# Install and load the package used for fast data reading

library(data.table)

# Read the raw dataset and create a working copy
df_raw <- fread("datadays.csv")
df <- copy(df_raw)

# Basic structure inspection of the dataset
dim(df)          # number of rows and columns
names(df)        # variable names
head(df)         # first observations
str(df)          # structure and variable types
summary(df)      # general statistical summary

# Check missing values and categorical structure
colSums(is.na(df))          # count missing values per column
unique(df$position)         # inspect unique job position labels
unique(df$retention_risk)   # inspect unique retention risk labels

# Check duplicated observations and duplicated employee IDs
sum(duplicated(df))
sum(duplicated(df$id))

# Inspect numerical variable distributions
summary(df$salary)
summary(df$tech_score)
summary(df$soft_skill_score)
summary(df$experience_years)

# Detect invalid values based on logical ranges
sum(df$salary <= 0, na.rm = TRUE)                     # salary cannot be zero or negative
sum(df$tech_score < 0 | df$tech_score > 100, na.rm=TRUE)  # tech score must be between 0–100
sum(df$soft_skill_score < 0 | df$soft_skill_score > 10, na.rm=TRUE) # soft skill must be 0–10
sum(df$experience_years < 0, na.rm = TRUE)            # experience cannot be negative


# Explore relationship between salary and retention risk
aggregate(salary ~ retention_risk, df_raw, mean)     # group mean salary
aggregate(salary ~ retention_risk, df_raw, median)   # group median salary
boxplot(salary ~ retention_risk,
        data = df_raw,
        col = c("red","green","orange"),
        main = "Salary vs Retention Risk",
        xlab = "Retention Risk",
        ylab = "Salary")      # visualize distribution differences

# ANOVA test to check if salary differs across risk groups
anova_result <- aov(salary ~ retention_risk, data = df_raw)
summary(anova_result)


# Explore relationship between tech_score and retention risk
boxplot(tech_score ~ retention_risk,
        data = df_raw,
        col = c("red","green","orange"),
        main = "Tech Score vs Retention Risk",
        xlab = "Retention Risk",
        ylab = "Tech Score")

aggregate(tech_score ~ retention_risk, df_raw, mean)
aggregate(tech_score ~ retention_risk, df_raw, median)

# ANOVA test for tech score differences
anova_tech <- aov(tech_score ~ retention_risk, data = df_raw)
summary(anova_tech)


# Explore relationship between soft skills and retention risk
boxplot(soft_skill_score ~ retention_risk,
        data = df_raw,
        col = c("red","green","orange"),
        main = "Soft Skill vs Retention Risk",
        xlab = "Retention Risk",
        ylab = "Soft Skill Score")

aggregate(soft_skill_score ~ retention_risk, df_raw, mean)
aggregate(soft_skill_score ~ retention_risk, df_raw, median)

# ANOVA test for soft skill differences
anova_soft <- aov(soft_skill_score ~ retention_risk, data = df_raw)
summary(anova_soft)


# Replace logically invalid numeric values with NA
df$salary[df$salary <= 0] <- NA
df$tech_score[df$tech_score < 0 | df$tech_score > 100] <- NA
df$soft_skill_score[df$soft_skill_score < 0 | df$soft_skill_score > 10] <- NA


# Standardize categorical variables (remove spaces and unify case)
df$position <- trimws(tolower(df$position))
df$retention_risk <- trimws(tolower(df$retention_risk))

# Merge alternative spellings of positions
df$position[df$position %in% c("senior","sr","snr")] <- "Senior"
df$position[df$position %in% c("junior","jr","jnr")] <- "Junior"

# Standardize retention risk labels
df$retention_risk[df$retention_risk == "low"] <- "Low"
df$retention_risk[df$retention_risk == "medium"] <- "Medium"
df$retention_risk[df$retention_risk == "high"] <- "High"

# Verify cleaned categories
unique(df$position)
unique(df$retention_risk)

# Impute missing values using the variable median
df$salary[is.na(df$salary)] <- median(df$salary, na.rm = TRUE)
df$tech_score[is.na(df$tech_score)] <- median(df$tech_score, na.rm = TRUE)
df$soft_skill_score[is.na(df$soft_skill_score)] <- median(df$soft_skill_score, na.rm = TRUE)
df$experience_years[is.na(df$experience_years)] <- median(df$experience_years, na.rm = TRUE)

# Final check that no missing values remain
colSums(is.na(df))

# Check duplicates again after cleaning
sum(duplicated(df))

# Final dataset inspection
summary(df)

unique(df$position)
unique(df$retention_risk)

# Confirm that numeric variables are within valid ranges
range(df$tech_score)
range(df$soft_skill_score)
range(df$salary)
range(df$experience_years)

# Export the cleaned dataset for further analysis
write.csv(df, "datadays_clean.csv", row.names = FALSE)


# TASK 2

# Convert retention risk labels into numerical multipliers
# Low risk keeps full value, Medium and High reduce candidate value
df$retention_multiplier <- ifelse(df$retention_risk == "Low", 1.00,
                                  ifelse(df$retention_risk == "Medium", 0.85, 0.60))

# Check whether each retention category received the correct multiplier
table(df$retention_risk, df$retention_multiplier)

# Compute raw candidate value using the formula given in the case
# This combines tech score, soft skill score and experience into one value metric
df$candidate_value <- (df$tech_score * 0.6) *
  (df$soft_skill_score * 4) *
  (df$experience_years * 1.5)

# Inspect distribution of candidate values
summary(df$candidate_value)

# View a few examples to confirm the formula output
head(df[, c("id", "tech_score", "soft_skill_score", "experience_years", "candidate_value")])

# Adjust candidate value by retention multiplier to obtain final value
df$final_value <- df$candidate_value * df$retention_multiplier

# Inspect distribution of final values
summary(df$final_value)

# View a few rows to confirm final value calculation
head(df[, c("id", "candidate_value", "retention_multiplier", "final_value")])

# Compute value generated per salary unit
# This is used as a supporting efficiency metric, not as the final optimization target
df$value_per_salary <- df$final_value / df$salary


# Install and load the package used for linear optimization

library(lpSolve)

# Define main vectors for the optimization model
salary <- df$salary                  # budget-related variable
value <- df$final_value              # objective to maximize
senior <- ifelse(df$position == "Senior", 1, 0)   # indicator for senior employees

# Number of candidates in the dataset
n <- nrow(df)

# Inspect value-per-salary distribution
summary(df$value_per_salary)

# View the top 10 most cost-efficient candidates
head(df[order(-df$value_per_salary),
        c("id","salary","final_value","value_per_salary")],10)

# Build the first simple constraint matrix
# Constraint 1: total salary must stay within budget
# Constraint 2: at least one senior must be selected
const.mat <- rbind(
  salary,
  senior
)

# Define directions of the constraints
const.dir <- c("<=", ">=")

# Define right-hand side values of the constraints
# Budget = 50000, minimum seniors = 1
const.rhs <- c(50000, 1)

# Solve the first version of the optimization model
# Objective: maximize total final value under basic constraints
solution <- lp(
  direction = "max",
  objective.in = value,
  const.mat = const.mat,
  const.dir = const.dir,
  const.rhs = const.rhs,
  all.bin = TRUE
)

# Check solver status (0 means optimal solution found)
solution$status

# Extract selected employees from the initial solution
selected <- df[solution$solution == 1, ]

# Basic summary of the initial feasible team
nrow(selected)
sum(selected$salary)
sum(selected$final_value)
table(selected$position)


# After the first simple model, we added the remaining case constraints

# Create indicators for Junior employees and High retention risk employees
junior <- ifelse(df$position == "Junior", 1, 0)
tech <- df$tech_score
highrisk <- ifelse(df$retention_risk == "High", 1, 0)

# Transform junior ratio constraint into linear form
# junior ratio >= 30%
junior_ratio_constraint <- junior - 0.30

# Transform average tech score constraint into linear form
# average tech score >= 80
tech_constraint <- tech - 80

# Transform high-risk ratio constraint into linear form
# high-risk ratio <= 40%
highrisk_constraint <- highrisk - 0.40

# Build the full constraint matrix including all case requirements
const.mat <- rbind(
  salary,
  senior,
  junior_ratio_constraint,
  tech_constraint,
  highrisk_constraint
)

# Define directions of all full-model constraints
const.dir <- c("<=", ">=", ">=", ">=", "<=")

# Define right-hand side values of the transformed constraints
const.rhs <- c(50000, 1, 0, 0, 0)

# Solve the full optimization model with all constraints
solution_full <- lp(
  direction = "max",
  objective.in = value,
  const.mat = const.mat,
  const.dir = const.dir,
  const.rhs = const.rhs,
  all.bin = TRUE
)

# Check solver status for the full model
solution_full$status

# Extract the final optimal team from the full solution
selected_full <- df[solution_full$solution == 1, ]

# Report final team characteristics
nrow(selected_full)                                 # team size
sum(selected_full$salary)                           # total salary
sum(selected_full$final_value)                      # total final value
table(selected_full$position)                       # junior/senior composition
mean(selected_full$position == "Junior")            # junior ratio
mean(selected_full$tech_score)                      # average tech score
mean(selected_full$retention_risk == "High")        # high-risk ratio
selected_full$id                                    # selected candidate IDs





# TASK 3

# Q1
# Visualize the relationship between salary and final candidate value
plot(df$salary, df$final_value,
     main = "Salary vs Final Value",
     col = "blue",
     xlab = "Salary",
     ylab = "Final Value",
     pch = 19)

# Calculate Pearson correlation between salary and final value
cor(df$salary, df$final_value)

# Test whether the correlation is statistically significant
cor.test(df$salary, df$final_value)


# Q2
# Compare tech score distributions across retention risk groups
boxplot(tech_score ~ retention_risk,
        data = df,
        col = c("red","green","orange"),
        main = "Tech Score by Retention Risk",
        xlab = "Retention Risk",
        ylab = "Tech Score")

# Compute average tech score for each retention risk category
aggregate(tech_score ~ retention_risk, df, mean)

# Perform ANOVA to test whether tech scores differ significantly across risk groups
anova_tech_clean <- aov(tech_score ~ retention_risk, data = df)
summary(anova_tech_clean)

# Q3
# Select the top 10 candidates with the highest value-per-salary ratio
top10 <- df[order(-df$value_per_salary), ][1:10, ]

# Display the selected top 10 candidates
top10

# Check position distribution among the top 10 efficient candidates
table(top10$position)

# Show key variables for the top 10 candidates
top10[, c("id","position","salary","final_value","value_per_salary")]

# Q4
# Count invalid values in the raw dataset before cleaning
sum(df_raw$salary <= 0, na.rm = TRUE)
sum(df_raw$tech_score < 0 | df_raw$tech_score > 100, na.rm = TRUE)
sum(df_raw$soft_skill_score < 0 | df_raw$soft_skill_score > 10, na.rm = TRUE)

# Verify that invalid values have been removed after cleaning
sum(df$salary <= 0)
sum(df$tech_score < 0 | df$tech_score > 100)
sum(df$soft_skill_score < 0 | df$soft_skill_score > 10)

# Summarize the final value and value-per-salary distributions after cleaning
summary(df$final_value)
summary(df$value_per_salary)

# Visualize the distribution of final candidate values after cleaning
hist(df$final_value,
     main = "Distribution of Final Candidate Value",
     xlab = "Final Value",
     col = "lightblue",
     border = "white")

# Recalculate raw final values without cleaning for comparison
df_raw$retention_multiplier <- ifelse(df_raw$retention_risk == "Low", 1.00,
                                      ifelse(df_raw$retention_risk == "Medium", 0.85, 0.60))

# Compute raw candidate value using the original uncleaned variables
df_raw$candidate_value <- (df_raw$tech_score * 0.6) *
  (df_raw$soft_skill_score * 4) *
  (df_raw$experience_years * 1.5)

# Compute raw final value before cleaning adjustments
df_raw$final_value <- df_raw$candidate_value * df_raw$retention_multiplier

# Raw vs clean comparison of final_value
summary(df_raw$final_value)
summary(df$final_value)

# Compare mean final values before and after cleaning
mean(df_raw$final_value, na.rm = TRUE)
mean(df$final_value)

# Compare median final values before and after cleaning
median(df_raw$final_value, na.rm = TRUE)
median(df$final_value)

# Compare the number of usable final_value observations
sum(!is.na(df_raw$final_value))
sum(!is.na(df$final_value))

# Visualize how data cleaning affected the final value distribution
boxplot(df_raw$final_value, df$final_value,
        names = c("Raw Data", "Cleaned Data"),
        col = c("orange", "lightblue"),
        main = "Impact of Data Cleaning on Final Value",
        ylab = "Final Value")

# Final interpretation:
# ANOVA results show no statistically significant differences across retention risk groups.
# This suggests that the current retention risk labels are not strongly supported by the observed variables.


# TASK 4

# Scenario 1
# Reduce the budget constraint from 50000 to 40000
const.rhs <- c(40000, 1, 0, 0, 0)

# Re-run the optimization model under the lower budget
solution40 <- lp(
  direction = "max",
  objective.in = value,
  const.mat = const.mat,
  const.dir = const.dir,
  const.rhs = const.rhs,
  all.bin = TRUE
)

# Extract the new optimal team under the 40000 budget
selected40 <- df[solution40$solution == 1, ]

# Check team size under the reduced budget
nrow(selected40)

# Compute total salary of the new team
sum(selected40$salary)

# Compute total final value under the reduced budget
sum(selected40$final_value)

# Check team composition by position
table(selected40$position)

# Compute average tech score of the new team
mean(selected40$tech_score)

# Compute high-risk ratio of the new team
mean(selected40$retention_risk == "High")

# Compare original and reduced-budget team totals
sum(selected_full$salary)
sum(selected$final_value)

sum(selected40$salary)
sum(selected40$final_value)

# Display full optimization output and selected team details
solution40
selected40
sum(selected40$salary)
sum(selected40$final_value)
table(selected40$position)

# IDs in the original optimal team
selected_full$id

# IDs in the reduced-budget team
selected40$id

# Show which employees were removed when budget was reduced
setdiff(selected_full$id, selected40$id)

# Show which employees entered the team after budget reduction
setdiff(selected40$id, selected_full$id)


# Scenario 2
# Create a copy of the cleaned dataset for the new tech-weight scenario
library(data.table)
df_s2 <- copy(df)

# Recalculate candidate value using tech weight = 0.9 instead of 0.6
df_s2$candidate_value <- (df_s2$tech_score * 0.9) *
  (df_s2$soft_skill_score * 4) *
  (df_s2$experience_years * 1.5)

# Recalculate final value under the new weighting scheme
df_s2$final_value <- df_s2$candidate_value * df_s2$retention_multiplier

# Update objective vector for optimization
value <- df_s2$final_value

# Rebuild optimization inputs for scenario 2
salary2 <- df_s2$salary
value2 <- df_s2$final_value
senior2 <- ifelse(df_s2$position == "Senior", 1, 0)
junior2 <- ifelse(df_s2$position == "Junior", 1, 0)
tech2 <- df_s2$tech_score
highrisk2 <- ifelse(df_s2$retention_risk == "High", 1, 0)

# Rebuild transformed linear constraints
junior_ratio_constraint2 <- junior2 - 0.30
tech_constraint2 <- tech2 - 80
highrisk_constraint2 <- highrisk2 - 0.40

# Build full constraint matrix for scenario 2
const.mat2 <- rbind(
  salary2,
  senior2,
  junior_ratio_constraint2,
  tech_constraint2,
  highrisk_constraint2
)

# Constraint directions remain unchanged
const.dir2 <- c("<=", ">=", ">=", ">=", "<=")

# Use the original 50000 budget and original thresholds
const.rhs2 <- c(50000, 1, 0, 0, 0)

# Solve the optimization model with the updated tech weight
solution_s2 <- lp(
  direction = "max",
  objective.in = value2,
  const.mat = const.mat2,
  const.dir = const.dir2,
  const.rhs = const.rhs2,
  all.bin = TRUE
)

# Check solver status
solution_s2$status

# Extract the optimal team under scenario 2
selected_s2 <- df_s2[solution_s2$solution == 1, ]

# Summarize the scenario 2 team
nrow(selected_s2)
sum(selected_s2$salary)
sum(selected_s2$final_value)
table(selected_s2$position)
mean(selected_s2$position == "Junior")
mean(selected_s2$tech_score)
mean(selected_s2$retention_risk == "High")

# Compare selected IDs with the original Task 2 team
setdiff(selected_full$id, selected_s2$id)
setdiff(selected_s2$id, selected_full$id)
selected_s2$id

# Compare total final values between original and scenario 2 teams
sum(selected_full$final_value)
sum(selected_s2$final_value)


# Scenario 3
# Compare mean values of key variables across retention risk groups
aggregate(cbind(salary, tech_score, soft_skill_score, experience_years) ~ retention_risk,
          data = df,
          mean)

# Compare median values of key variables across retention risk groups
aggregate(cbind(salary, tech_score, soft_skill_score, experience_years) ~ retention_risk,
          data = df,
          median)

# Visualize salary differences by retention risk group
boxplot(salary ~ retention_risk, data = df,
        col = c("red","green","orange"),
        main = "Salary by Retention Risk",
        xlab = "Retention Risk",
        ylab = "Salary",
        border = "black")

# Visualize tech score differences by retention risk group
boxplot(tech_score ~ retention_risk, data = df,
        col = c("red","green","orange"),
        main = "Tech Score by Retention Risk",
        xlab = "Retention Risk",
        ylab = "Tech Score",
        border = "black")

# Visualize soft skill score differences by retention risk group
boxplot(soft_skill_score ~ retention_risk, data = df,
        col = c("red","green","orange"),
        main = "Soft Skill Score by Retention Risk",
        xlab = "Retention Risk",
        ylab = "Soft Skill Score",
        border = "black")

# Visualize experience differences by retention risk group
boxplot(experience_years ~ retention_risk, data = df,
        col = c("red","green","orange"),
        main = "Experience by Retention Risk",
        xlab = "Retention Risk",
        ylab = "Experience",
        border = "black")

# Test whether salary differs significantly across retention risk groups
summary(aov(salary ~ retention_risk, data = df))

# Test whether tech score differs significantly across retention risk groups
summary(aov(tech_score ~ retention_risk, data = df))

# Test whether soft skill score differs significantly across retention risk groups
summary(aov(soft_skill_score ~ retention_risk, data = df))

# Test whether experience differs significantly across retention risk groups
summary(aov(experience_years ~ retention_risk, data = df))






# TASK 5

# Retrieve the optimal team selected in Task 2
selected_full <- df[solution_full$solution == 1, ]

# Display the IDs of the employees in our optimal team
selected_full$id

# Summarize the main characteristics of our team
nrow(selected_full)                              # team size
sum(selected_full$salary)                        # total salary
sum(selected_full$final_value)                   # total final value
table(selected_full$position)                    # position composition
mean(selected_full$position == "Junior")         # junior ratio
mean(selected_full$tech_score)                   # average tech score
mean(selected_full$retention_risk == "High")     # high-risk ratio

# Create the pool of employees who are not in the optimal team
alt_pool <- df[!df$id %in% selected_full$id, ]

# Initialize an empty table to store feasible alternative one-swap teams
one_swap_results <- data.frame()

# Search all possible one-for-one swaps:
# remove one employee from our team and replace with one from the alternative pool
for(out_id in selected_full$id){
  for(in_id in alt_pool$id){
    
    # Build competitor team IDs by replacing one selected employee
    competitor_ids <- selected_full$id
    competitor_ids[competitor_ids == out_id] <- in_id
    
    # Extract competitor team from the dataset
    competitor <- df[df$id %in% competitor_ids, ]
    
    # Calculate main team metrics for the competitor team
    total_salary <- sum(competitor$salary)
    total_value <- sum(competitor$final_value)
    avg_tech <- mean(competitor$tech_score)
    junior_ratio <- mean(competitor$position == "Junior")
    highrisk_ratio <- mean(competitor$retention_risk == "High")
    n_senior <- sum(competitor$position == "Senior")
    
    # Check whether the competitor team satisfies all Task 2 constraints
    # and is still weaker than our optimal team
    feasible <- total_salary <= 50000 &&
      n_senior >= 1 &&
      junior_ratio >= 0.30 &&
      avg_tech >= 80 &&
      highrisk_ratio <= 0.40 &&
      total_value < sum(selected_full$final_value)
    
    # Store the feasible competitor team in the results table
    if(feasible){
      one_swap_results <- rbind(one_swap_results, data.frame(
        out_id = out_id,
        in_id = in_id,
        total_salary = total_salary,
        total_final_value = total_value,
        avg_tech = avg_tech,
        junior_ratio = junior_ratio,
        highrisk_ratio = highrisk_ratio,
        stringsAsFactors = FALSE
      ))
    }
  }
}

# Display the strongest feasible alternative teams
head(one_swap_results[order(-one_swap_results$total_final_value), ], 10)

# Select the best one-swap competitor team
best_swap <- one_swap_results[which.max(one_swap_results$total_final_value), ]
best_swap

# Rebuild the final competitor team based on the best swap
competitor_ids <- selected_full$id
competitor_ids[competitor_ids == best_swap$out_id] <- best_swap$in_id

competitor <- df[df$id %in% competitor_ids, ]

# Display competitor team IDs
competitor$id

# Summarize the competitor team's characteristics
nrow(competitor)                                 # team size
sum(competitor$salary)                           # total salary
sum(competitor$final_value)                      # total final value
table(competitor$position)                       # position composition
mean(competitor$position == "Junior")            # junior ratio
mean(competitor$tech_score)                      # average tech score
mean(competitor$retention_risk == "High")        # high-risk ratio

# Create a side-by-side comparison table for our team and the competitor team
comparison <- data.frame(
  Metric = c("Team Size",
             "Total Salary",
             "Total Final Value",
             "Average Tech Score",
             "Junior Ratio",
             "High Risk Ratio",
             "Number of Seniors"),
  
  Our_Team = c(
    nrow(selected_full),
    sum(selected_full$salary),
    round(sum(selected_full$final_value), 1),
    round(mean(selected_full$tech_score), 2),
    round(mean(selected_full$position == "Junior"), 2),
    round(mean(selected_full$retention_risk == "High"), 2),
    sum(selected_full$position == "Senior")
  ),
  
  Competitor = c(
    nrow(competitor),
    sum(competitor$salary),
    round(sum(competitor$final_value), 1),
    round(mean(competitor$tech_score), 2),
    round(mean(competitor$position == "Junior"), 2),
    round(mean(competitor$retention_risk == "High"), 2),
    sum(competitor$position == "Senior")
  )
)

# Show comparison table
comparison

# Compute the value gap between our team and the competitor team
value_gap <- sum(selected_full$final_value) - sum(competitor$final_value)
round(value_gap,1)

# Identify which employee was replaced in the competitor team
setdiff(selected_full$id, competitor$id)
setdiff(competitor$id, selected_full$id)

# Final interpretation:
# The best feasible nearby competitor is obtained through a one-for-one replacement.
# Even with this minimal change, total final value decreases, which supports the strength of our original optimal team.
# In our result, the best nearby competitor is formed by replacing employee 445 with employee 4.
