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
# Generate descriptive statistics (NAs) for logical variables.
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

# Variable Selection from Literature Review #
working <- working[,-c(5,6,14,17,26)]

#### Imputation of Variables with Missingness ####
# Create table for variables with missingness.
missing_variables <- c(15,21,29)
imputation_table <- working %>%
  select(all_of(missing_variables)) %>%
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

#### Data Analysis ####