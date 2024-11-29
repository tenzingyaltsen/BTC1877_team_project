# BTC1877 Team Project

#### Preparation ####
# Install and load packages.
install.packages("readxl")
install.packages("funModeling")
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("knitr")
install.packages("mice")
library(readxl)
library(funModeling)
library(dplyr)
library(tidyr)
library(ggplot2)
library(knitr)
library(mice)

# Import data.
raw_data <- read_excel("transfusion_data.xlsx")

#' For simplicity, let's consider only highlighted variables and variables of
#' interest.
working <- raw_data[,c(1,3:15,25,26,28:41,46,47,49,50,57,60:65,77:92,116,117)]

# Assess NAs.
which(is.na(working))
# Lots of NAs and blank spaces.
status(working)
#' Massive NA values for Pre_Fibrinogen, RBC timepoints, FFP timepoints, 
#' Plt timepoints and Cryo timepoints. Remove these variables.
#' QUESTION: What's our rationale for removing massively missing values?
#' As opposed to imputing (i.e., what is the threshold for imputation?).
missing_variables <- c(26,42:44,46:48,50:52,54:56)
missing_table <- working %>%
  summarise(across(all_of(missing_variables), list(
    na_count = ~sum(is.na(.x)),
    na_proportion = ~mean(is.na(.x))
    ))) %>%
  pivot_longer(everything(), names_to = c("Variable", ".value"), 
               names_pattern = "(.*)_(.*)$")
print(missing_table, n=Inf)
working <- working[,-c(26,42:44,46:48,50:52,54:56)]

# Refactor variables.
# Some character variables should be factors.
working <- working %>%
  mutate(across(c(Type, ALIVE_30DAYS_YN, ALIVE_90DAYS_YN, ALIVE_12MTHS_YN, 
                  `Massive Transfusion`, COPD), as.factor))
# Some character variables should be logical.
working <- working %>%
  mutate(across(c(`Gender (male)`, COPD, `alpha1-Antitrypsin Deficiency`, 
                  `Cystic Fibrosis`, `Idiopathic Pulmonary Hypertension`, 
                  `Interstitial Lung Disease`, Pulm_Other, 
                  `First Lung Transplant`, `Redo Lung Transplant`, 
                  `ExVIVO Lung Perfusion`, `Preoperative ECLS`, ECLS_ECMO, 
                  ECLS_CPB), ~ as.logical(.)))
# Some character and POSIXCT variables should be Date type.
working$DEATH_DATE <- as.Date(working$DEATH_DATE, format = "%d-%b-%Y")
working$`OR Date` <- as.Date(working$`OR Date`, format = "%d-%b-%Y")
str(working)

#### EDA ####
# Generate descriptive statistics for numeric variables.
which(sapply(working, is.numeric))
num_variables <- c(5:8,19:26,30:34,39:45)
num_table <- working %>%
  summarise(across(all_of(num_variables), list(
    mean = ~mean(.x, na.rm = TRUE),
    median = ~median(.x, na.rm = TRUE),
    sd = ~sd(.x, na.rm = TRUE),
    min = ~min(.x, na.rm = TRUE),
    max = ~max(.x, na.rm = TRUE),
    iqr = ~IQR(.x, na.rm = TRUE)
  ))) %>%
  pivot_longer(everything(), names_to = c("Variable", ".value"), 
               names_pattern = "(.*)_(.*)$")
# Print table.
num_table %>%
  kable(
    format = "markdown",  # Use "markdown" for plain text, "html" for R Markdown rendering
    caption = "Summary Statistics for Numerical Variables",
    digits = 2  # Number of decimal places
  )

# Generate descriptive statistics for factor variables.
which(sapply(working, is.factor))
factor_variables <- c(3,36,37,38,46)
factor_table <- working %>%
  select(all_of(factor_variables)) %>%  
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
  group_by(variable, value) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(variable) %>%
  mutate(proportion = n / sum(n))
# Print table.
factor_table %>%
  arrange(variable, desc(n)) %>%  # Optional: Arrange by variable and count
  kable(
    format = "markdown",  # Use "markdown" for plain text, "html" for R Markdown
    caption = "Summary of Factor Variables",
    digits = 2,  # For proportion formatting
    col.names = c("Variable", "Value", "Count", "Proportion")  # Rename columns
  )

# Generate descriptive statistics for logical variables.
which(sapply(working, is.logical))
logical_variables <- c(4,9:18,27:29)
logical_table <- working %>%
  select(all_of(logical_variables)) %>%  
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
  group_by(variable, value) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(variable) %>%
  mutate(proportion = n / sum(n))
# Print table.
logical_table %>%
  arrange(variable, desc(n)) %>%  # Optional: Sort by variable and count
  kable(
    format = "markdown",  # Use "markdown" for plain text; "html" for R Markdown
    caption = "Summary of Logical Variables",
    digits = 2,  # Number of decimal places for proportions
    col.names = c("Variable", "Value", "Count", "Proportion")  # Rename columns for clarity
  )

# Generate descriptive statistics (NAs) for date variables.
which(sapply(working, inherits, "Date"))
date_variables <- c(2,35)
date_table <- working %>%
  select(all_of(date_variables)) %>%  
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
  mutate(value = ifelse(is.na(value), "NA", "Not NA")) %>%  # Group values as "NA" or "Not NA"
  group_by(variable, value) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(variable) %>%
  mutate(proportion = n / sum(n))
# Print table.
date_table %>%
  arrange(variable, desc(n)) %>%  # Optional: Sort by variable and count
  kable(
    format = "markdown",  # Use "markdown" for plain text, "html" for R Markdown
    caption = "Summary of Date Variables (NA vs Not NA)",
    digits = 2,  # Number of decimal places for proportions
    col.names = c("Variable", "Value", "Count", "Proportion")  # Rename columns for clarity
  )

#' Create histogram for each numerical variable and bar chart for each
#' categorical variable in data set.
for (var in names(working)) {
  if (is.numeric(working[[var]]) && var != "Study ID #") {
    histogram <- ggplot(working, aes(x = .data[[var]])) +
      geom_histogram(fill = "skyblue", binwidth = 2, na.rm = TRUE) +
      labs(title = var, x = var, y = "Count") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, size = 18))
    print(histogram)
  } else if (is.factor(working[[var]]) || is.logical(working[[var]])) {
    barchart <- ggplot(working, aes(x = .data[[var]])) +
      geom_bar(fill = "salmon") +
      labs(title = var, x = var, y = "Frequency") +
      theme_minimal() +
      theme(plot.title = element_text(, hjust = 0.5, size = 18))
    print(barchart)
  }
}

#### Variable Selection from Literature Review ####
working <- working[,-c(5,6,14,17,26)]

#### Imputation of Variables with Missingness ####
# Create table for variables with missingness.
missing_variables <- c(15,21,29)
imputation_table <- working %>%
  dplyr::select(all_of(missing_variables)) %>%
  summarise_all(~sum(is.na(.))) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Number_of_NAs") %>%
  mutate(Proportion_of_NAs = Number_of_NAs / nrow(working))
# Print table.
imputation_table %>%
  kable(caption = "Summary of Missing Values by Variable",
        col.names = c("Variable", "Number of NAs", "Proportion of NAs"))

# Create imputation data set with dates removed, as they cannot used for predictions.
impute_data <- working[,-c(2,30)]
# Replace spaces with underscores, not accepted by mice function.
colnames(impute_data) <- gsub(" ", "_", colnames(impute_data))  
# Remove non-alphanumeric characters, not accepted by mice function.
colnames(impute_data) <- gsub("[^[:alnum:]_]", "", colnames(impute_data)) 
# Impute and create mids object.
imp_stoch <- mice(impute_data, method = "pmm", seed = 123, m = 1, print = FALSE)
summary(imp_stoch)
# Plot the newly imputed values.
xyplot(imp_stoch, LAS_score ~ Age)
xyplot(imp_stoch, Pre_PTT ~ Age)
xyplot(imp_stoch, Duration_of_ICU_Stay_days ~ Age)
# Unable to impute this variable due to collinearity.
# Convert from mids object to data frame.
imp_stoch_df <- complete(imp_stoch)
# Move imputed variables back to working copy.
working$`LAS score` <- imp_stoch_df$LAS_score
working$Pre_PTT <- imp_stoch_df$Pre_PTT
status(working)
# Remove observation with un-imputable value.
which(is.na(working$`Duration of ICU Stay (days)`))
working <- working[-85,]

#### Question 1 ####
# Creating a new data set for classification.
# Adding an "RBC Transfusion" column to the data frame:

#If "Total 24hr RBC" value was greater than 0, indicate it as 1 (for transfusion), if its 0, then indicate it as 0 (no transfusion)
working_class <- working %>%
  mutate(
    RBC_Transfusion = if_else(`Total 24hr RBC` > 0, 1, 0))

#Converting the variable I just created to factor
working_class$RBC_Transfusion <- as.factor(working_class$RBC_Transfusion)

#Converting logical(TRUE/FALSE) to factors
working_class <- working_class %>%
  mutate_if(is.logical, as.factor)

#selecting the predictors were focusing on, including the outcome
working_class <- working_class %>%
  select(
    RBC_Transfusion, Type, `Gender (male)`, Age, BMI, COPD,
    `alpha1-Antitrypsin Deficiency`, `Cystic Fibrosis`,
    `Idiopathic Pulmonary Hypertension`, `Interstitial Lung Disease`,
    `First Lung Transplant`, `Redo Lung Transplant`, `Preoperative ECLS`,
    `LAS score`, Pre_Hb, Pre_Hct, Pre_Platelets, Pre_PT, Pre_INR, Pre_PTT, ECLS_ECMO, ECLS_CPB
  ) %>%
  rename_with(~ gsub(" ", "_", .x))

#renaming potential problematic columns
working_class <- working_class %>%
  rename(alpha1_Antitrypsin_Deficiency = `alpha1-Antitrypsin_Deficiency`)

colnames(working_class) <- gsub("[^A-Za-z0-9_]", "_", colnames(working_class))


library(glmnet)
library(pROC) 
library(tree) 

### Creating a LASSO model for classification ###

set.seed(65)

train_1 <- sample(nrow(working_class),round(nrow(working_class)/2))

#created a vector with predictor features
#excluded the first column as it corresponds to the intercept and it is always 1
x1 <- model.matrix(RBC_Transfusion ~.,working_class)[train_1,-1]

#created a vector with the response values
y1 <- working_class$RBC_Transfusion[train_1]

#creating lasso model, binomial family
lasso_model1 <- glmnet(x1,y1,family="binomial")

#plotting lambda vs coefficient weight plot 
plot(lasso_model1,label = T, xvar = "lambda")

#Using cross validation to tune the model 
cv_lasso1 <- cv.glmnet(x1,y1,alpha=1,family = "binomial", type.measure = "auc", nfolds = 10)

plot(cv_lasso1)
#Lambda that maximizes the AUC for this model 
cv_lasso1$lambda.min

#Extract coefficients
coef_min1 <- coef(cv_lasso1, s = "lambda.min")
coef_min1
coef(cv_lasso1, s = "lambda.1se")

#testing in the test set

pred_lasso1 <- as.numeric(predict(lasso_model1, newx = model.matrix(RBC_Transfusion ~.,working_class)[-train_1,-1], s=cv_lasso1$lambda.1se, type = "response"))



#plotting the ROC curve
myroc1 <- roc(RBC_Transfusion ~ pred_lasso1, data=working_class[-train_1,])

plot(myroc1)

# extracting the Area Under the Curve, a measure of discrimination
auc.lasso1 <- myroc1$auc
auc.lasso1


#Making a loop to compare the performance of lasso and pruned tree models. This will help decide which model to go, by assessing and testing the models performances. 

#Making a data frame to store AUC values for each model within each iteration
auc_table <- data.frame(
  Iteration = integer(),
  AUC_LASSO = numeric(),
  AUC_Pruned_Tree = numeric()
)

#Loop to repeat the process 5 times with different seeds set for each time
for (i in 1:5) {
  # Different seed for each iteration
  set.seed(60 + i)
  
  #Splitting the data into training and test sets
  train_data <- sample(nrow(working_class), round(nrow(working_class) / 2))
  
  # Creating predictor matrix and response vector for the LASSO model
  x_train <- model.matrix(RBC_Transfusion ~ ., working_class)[train_data, -1]
  y_train <- working_class$RBC_Transfusion[train_data]
  
  # LASSO model 
  # Fitting the LASSO model
  lasso_model <- glmnet(x_train, y_train, family = "binomial")
  
  # Cross-validation for lambda selection
  cv_lasso <- cv.glmnet(x_train, y_train, alpha = 1, family = "binomial", type.measure = "auc", nfolds = 10)
  optimal_lambda <- cv_lasso$lambda.1se
  
  # Predict using the test set
  pred_lasso <- predict(lasso_model, newx = model.matrix(RBC_Transfusion ~ ., working_class)[-train_data, -1], s = optimal_lambda, type = "response")
  
  # Calculating AUC for LASSO
  roc_lasso <- roc(working_class$RBC_Transfusion[-train_data], as.numeric(pred_lasso))
  auc_lasso <- roc_lasso$auc
  
  # CART
  # Making a classification tree
  tree_model <- tree(RBC_Transfusion ~ ., data = working_class, subset = train_data)
  
  #Cross-validation for pruning
  cv_res <- cv.tree(tree_model, FUN = prune.tree)
  
  #Determining the best size of the tree based on minimum deviance
  best_size <- cv_res$size[which.min(cv_res$dev)]
  
  best_size <- ifelse(
    best_size == 1, 2,
    best_size
  )
  
  # Prune the tree using the best size
  pruned_tree <- prune.tree(tree_model, best = best_size)
  
  
  
  # Predicting on the test set for the pruned tree
  preds_pruned <- predict(pruned_tree, newdata = working_class[-train_data, ], type = "vector")
  pred_probs_pruned <- as.numeric(preds_pruned[, 2])
  
  # Calculating AUC for pruned tree
  roc_pruned_tree <- roc(working_class$RBC_Transfusion[-train_data], pred_probs_pruned)
  auc_pruned_tree <- roc_pruned_tree$auc
  
  # Updating the AUC values from each model to the table created earlier (outside the loop)
  auc_table <- rbind(
    auc_table,
    data.frame(
      Iteration = i,
      AUC_LASSO = auc_lasso,
      AUC_Pruned_Tree = auc_pruned_tree
    )
  )
}

# Printing result table with AUC values
print(auc_table)

#highest AUC value was for the AUC of the LASSO model at iteration 5 (seed = 15)



#Part 2 of Question 1: We want to look at the amount of RBCs in transfusions

#Converting logical(TRUE/FALSE) to factors
working_cont <- working %>%
  mutate_if(is.logical, as.factor)

#Response stays the same, we are using the same predictor set 
#selecting the predictors were focusing on, including the outcome
working_cont <- working_cont %>%
  select(
    `Total 24hr RBC`, Type, `Gender (male)`, Age, BMI, COPD,
    `alpha1-Antitrypsin Deficiency`, `Cystic Fibrosis`,
    `Idiopathic Pulmonary Hypertension`, `Interstitial Lung Disease`,
    `First Lung Transplant`, `Redo Lung Transplant`, `Preoperative ECLS`,
    `LAS score`, Pre_Hb, Pre_Hct, Pre_Platelets, Pre_PT, Pre_INR, Pre_PTT, ECLS_ECMO, ECLS_CPB
  ) %>%
  rename_with(~ gsub(" ", "_", .x))

#renaming potential problematic columns
working_cont <- working_cont %>%
  rename(alpha1_Antitrypsin_Deficiency = `alpha1-Antitrypsin_Deficiency`)

colnames(working_cont) <- gsub("[^A-Za-z0-9_]", "_", colnames(working_cont))



### Creating Regression LASSO ###

set.seed(65)

train_2 <- sample(nrow(working_cont),round(nrow(working_cont)/2))

#created a vector with predictor features
#excluded the first column as it corresponds to the intercept and it is always 1
x2 <- model.matrix(Total_24hr_RBC ~.,working_cont)[train_2,-1]

#created a vector with the response values
y2 <- working_cont$Total_24hr_RBC[train_2]

#creating lasso model, binomial family
lasso_model2 <- glmnet(x2,y2,family="gaussian")

#plotting lambda vs coefficient weight plot 
plot(lasso_model2,label = T, xvar = "lambda")

#Using cross validation to tune the model 
cv_lasso2 <- cv.glmnet(x2,y2,alpha=1,family = "gaussian", type.measure = "mse", nfolds = 10)

plot(cv_lasso2)
#Lambda that maximizes the AUC for this model 
cv_lasso2$lambda.min

#Extract coefficients
coef_min2 <- coef(cv_lasso2, s = "lambda.min")
coef_min2
coef(cv_lasso2, s = "lambda.1se")

#testing in the test set

pred_lasso2 <- as.numeric(predict(lasso_model2, newx = model.matrix(Total_24hr_RBC ~.,working_cont)[-train_2,-1], s=cv_lasso2$lambda.1se, type = "response"))

#calculating test MSE
y_test <- working_cont$Total_24hr_RBC[-train_2]

test_mse <- mean((y_test - pred_lasso2)^2)
test_mse

#calculating training MSE

pred_lasso3 <- as.numeric(predict(lasso_model2, newx = x2, s = cv_lasso2$lambda.1se, type = "response"))

training_MSE <- mean((y2 - pred_lasso3)^2)
training_MSE

#### Question 2 ####
# Create new data set.
#' If "Total 24hr RBC" value was greater than 0, indicate it as 1 
#' (for transfusion), if its 0, then indicate it as 0 (no transfusion).
working_outcomes <- working %>%
  mutate(
    RBC_Transfusion = if_else(`Total 24hr RBC` > 0, 1, 0))
# Convert to factor.
working_outcomes$RBC_Transfusion <- as.factor(working_outcomes$RBC_Transfusion)
# Create time variable, for time to death in days.
working_outcomes$time <- as.numeric(working_outcomes$DEATH_DATE - working_outcomes$`OR Date`)
# Create status variable, to indicate death or censored.
working_outcomes$status <- ifelse(!is.na(working_outcomes$DEATH_DATE),1,0)

library(survival)
# Create survival fit.
sf <- survfit(Surv(time, status==1) ~ 1, data=working_outcomes)
print(sf)
# Plot survival curves.
plot(sf, xlab = "days from operation", ylab = "Survival")
plot(sf, xscale = 365.25, xlab = "years from operation", ylab="Survival") 

# Create stratified survival fit.
sf2 <- survfit(Surv(time, status == 1) ~ RBC_Transfusion, data = working_outcomes)
print(sf2)
# Plot stratified survival curves.
plot(sf2, xscale = 365.25, xlab = "years from operation", ylab="Survival", 
     col= 1:2) 
plot(sf2, xscale = 365.25, xlab = "years from operation", ylab="Survival", 
     col=1:2, conf.int = 0.95) 
legend("bottomright",legend = c("no transfusion", "transfusion"),lty = 1, col = 1:2)

# We can also perform a Log-rank test to compare the two survival curves.
# First, we need to check the PH assumption.
# Typical stratified KM curve:
plot(survfit(Surv(time, status==1) ~ RBC_Transfusion, 
             data=working_outcomes), fun = "S")
# There is no obvious deviation from the PH assumption.
# Also, plot a cloglog plot against log(t).
plot(survfit(Surv(time, status==1) ~ RBC_Transfusion, 
             data = working_outcomes), fun = "cloglog")
# Curves are parallel so PH seems to hold, the log-rank test is valid.

# Perform the log rank test.
survdiff(Surv(time, status==1) ~ RBC_Transfusion, data = working_outcomes)
# We do not see strong evidence that the two curves are not identical.