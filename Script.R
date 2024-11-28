# BTC1877 Team Project

#### Preparation ####

# Install and load packages.
install.packages("readxl")
install.packages("funModeling")
install.packages("dplyr")
install.packages("ggplot2")
library(funModeling)
library(readxl)
library(ggplot2)
library(dplyr)

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
    mean = ~mean(.x,na.rm = T), median = ~median(.x,na.rm = T),
    sd = ~sd(.x, na.rm = T), min = ~min(.x, na.rm = T),
    max = ~max(.x, na.rm = T), iqr = ~IQR(.x, na.rm = T)))) %>%
  pivot_longer(everything(), names_to = c("Variable", ".value"), 
               names_pattern = "(.*)_(.*)$")
print(num_table, n=Inf)

# Generate descriptive statistics for categorical variables.
which(sapply(working, is.factor))
which(sapply(working, is.logical))
which(sapply(working, inherits, "Date"))
factor_variables <- c(3,36,37,38,46)
logical_variables <- c(4,9:18,27:29)
date_variables <- c(2,35)

factor_table <- working %>%
  select(all_of(factor_variables)) %>%  
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
  group_by(variable, value) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(variable) %>%
  mutate(proportion = n / sum(n))
print(factor_table, n = Inf)

logical_table <- working %>%
  select(all_of(logical_variables)) %>%  
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
  group_by(variable, value) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(variable) %>%
  mutate(proportion = n / sum(n))
print(logical_table, n = Inf)

date_table <- working %>%
  select(all_of(date_variables)) %>%  
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
  mutate(value = ifelse(is.na(value), "NA", "Not NA")) %>%  # Group values as "NA" or "Not NA"
  group_by(variable, value) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(variable) %>%
  mutate(proportion = n / sum(n))
print(date_table, n = Inf)

#' Create histogram for each numerical variable and bar chart for each
#' categorical variable in data set.
for (var in names(working)) {
  if (is.numeric(working[[var]]) && var != "Study ID #") {
    histogram <- ggplot(working, aes(x = .data[[var]])) +
      geom_histogram(fill = "skyblue", binwidth = 30) +
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