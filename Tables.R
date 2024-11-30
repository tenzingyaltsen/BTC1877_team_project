library(flextable)
library(webshot2)
library(htmltools)

#### Massive Missingness ####
Do this

#### Numerical Variables ####
# Note that "STUDY ID #" is removed.
data <- data.frame(
  Variable = c("Age", "BMI", "LAS score", "Pre_PTT", 
               "Intra_Fresh Frozen Plasma", "Intra_Packed Cells", 
               "Intra_Platelets", "Intra_Cryoprecipitate", 
               "Duration of ICU Stay (days)", "ICU_LOS", 
               "HOSPITAL_LOS", "RBC 72hr Total", "FFP 72hr Total", 
               "Plt 72hr Total", "Cryo 72hr Total", "Total 24hr RBC"),
  mean = c(56.26, 24.11, 36.91, 26.07, 
           0.61, 2.27, 0.43, 2.71, 7.29, 9.24, 
           33.60, 0.80, 0.19, 0.14, 0.47, 2.67),
  median = c(61.00, 24.37, 34.00, 24.50, 
             0.00, 1.00, 0.00, 0.00, 4.00, 3.62, 
             22.84, 0.00, 0.00, 0.00, 0.00, 1.00),
  sd = c(14.80, 3.98, 9.15, 7.99, 
         2.54, 4.23, 1.39, 7.38, 11.45, 29.25, 
         42.36, 2.23, 0.98, 0.84, 2.76, 4.98),
  min = c(19.00, 15.54, 24.00, 17.70, 
          0.00, 0.00, 0.00, 0.00, 1.00, 0.63, 
          9.66, 0.00, 0.00, 0.00, 0.00, 0.00),
  max = c(76.00, 31.78, 90.00, 93.50, 
          26.00, 36.00, 11.00, 50.00, 99.00, 380.86, 
          381.52, 23.00, 8.00, 10.00, 30.00, 38.00),
  iqr = c(20.00, 6.80, 5.62, 4.55, 
          0.00, 3.00, 0.00, 0.00, 6.00, 5.66, 
          20.38, 1.00, 0.00, 0.00, 0.00, 4.00)
)
# Create a flextable.
ft <- flextable(data) %>%
  add_header_row(values = "Numerical Variables", 
                 colwidths = ncol(data)) %>%
  set_header_labels(
    Variable = "Variable",
    mean = "Mean",
    median = "Median",
    sd = "Standard Deviation",
    min = "Minimum",
    max = "Maximum",
    iqr = "Interquartile Range"
  ) %>%
  theme_vanilla() %>%
  set_table_properties(width = 1, layout = "autofit") %>%
  bg(bg = "white", part = "all")  
# Save as an image.
save_as_image(ft, path = "numerical_variables.png")

#### Factor Variables ####
data <- data.frame(
  Variable = c("ALIVE_12MTHS_YN", "ALIVE_12MTHS_YN", 
               "ALIVE_30DAYS_YN", "ALIVE_30DAYS_YN", 
               "ALIVE_90DAYS_YN", "ALIVE_90DAYS_YN", 
               "Massive Transfusion", "Massive Transfusion", 
               "Type", "Type", "Type"),
  Value = c("Y", "N", 
            "Y", "N", 
            "Y", "N", 
            "0", "1", 
            "Bilateral", "Single Left Lung", "Single Right Lung"),
  Count = c(169, 23, 
            189, 3, 
            183, 9, 
            183, 9, 
            157, 18, 17),
  Proportion = c(0.88, 0.12, 
                 0.98, 0.02, 
                 0.95, 0.05, 
                 0.95, 0.05, 
                 0.82, 0.09, 0.09)
)
# Create a flextable
ft <- flextable(data) %>%
  add_header_row(values = "Factor Variables", 
                 colwidths = ncol(data)) %>%
  set_header_labels(
    Variable = "Variable",
    Value = "Value",
    Count = "Count",
    Proportion = "Proportion"
  ) %>%
  theme_vanilla() %>%  
  set_table_properties(width = 1, layout = "autofit") %>%
  bg(bg = "white", part = "all")  
# Export the table as an image
save_as_image(ft, path = "factor_variables.png")

#### Logical Variables ####
data <- data.frame(
  Variable = c(
    "COPD", "COPD", 
    "Cystic Fibrosis", "Cystic Fibrosis", 
    "ECLS_CPB", "ECLS_CPB", 
    "ECLS_ECMO", "ECLS_ECMO", 
    "Gender (male)", "Gender (male)", 
    "Idiopathic Pulmonary Hypertension", "Idiopathic Pulmonary Hypertension", 
    "Interstitial Lung Disease", "Interstitial Lung Disease", 
    "Preoperative ECLS", "Preoperative ECLS", 
    "Redo Lung Transplant", "Redo Lung Transplant", 
    "alpha1-Antitrypsin Deficiency", "alpha1-Antitrypsin Deficiency"
  ),
  Value = c(
    "FALSE", "TRUE", 
    "FALSE", "TRUE", 
    "FALSE", "TRUE", 
    "TRUE", "FALSE", 
    "TRUE", "FALSE", 
    "FALSE", "TRUE", 
    "FALSE", "TRUE", 
    "FALSE", "TRUE", 
    "FALSE", "TRUE", 
    "FALSE", "TRUE"
  ),
  Count = c(
    134, 58, 
    162, 30, 
    190, 2, 
    101, 91, 
    104, 88, 
    186, 6, 
    100, 92, 
    185, 7, 
    182, 10, 
    183, 9
  ),
  Proportion = c(
    0.70, 0.30, 
    0.84, 0.16, 
    0.99, 0.01, 
    0.53, 0.47, 
    0.54, 0.46, 
    0.97, 0.03, 
    0.52, 0.48, 
    0.96, 0.04, 
    0.95, 0.05, 
    0.95, 0.05
  )
)
# Create a flextable
ft <- flextable(data) %>%
  add_header_row(values = "Logical Variables", 
                 colwidths = ncol(data)) %>%
  set_header_labels(
    Variable = "Variable",
    Value = "Value",
    Count = "Count",
    Proportion = "Proportion"
  ) %>%
  theme_vanilla() %>%  
  set_table_properties(width = 1, layout = "autofit") %>%
  bg(bg = "white", part = "all")  
# Export the table as an image
save_as_image(ft, path = "logical_variables.png")

#### Missingness for Date Variables ####
date_table <- data.frame(
  Variable = c("DEATH_DATE", "DEATH_DATE", "OR Date"),
  Value = c("NA", "Not NA", "Not NA"),
  Count = c(160, 32, 192),
  Proportion = c(0.83, 0.17, 1.00)
)
# Create a flextable
ft <- flextable(date_table) %>%
  add_header_row(values = "Missingness for Date Variables", 
                 colwidths = ncol(date_table)) %>%
  set_header_labels(
    Variable = "Variable",
    Value = "Value",
    Count = "Count",
    Proportion = "Proportion"
  ) %>%
  theme_vanilla() %>%  
  set_table_properties(width = 1, layout = "autofit") %>%
  bg(bg = "white", part = "all")  
# Export the table as an image
save_as_image(ft, path = "date_variables.png")

#### Missingness for Imputation Variable Selection ####
na_summary <- data.frame(
  Variable = c("LAS score", "Pre_PTT", "Duration of ICU Stay (days)"),
  Number_of_NAs = c(12, 1, 1),
  Proportion_of_NAs = c(0.0625, 0.0052083, 0.0052083)
)
# Create a flextable object
ft <- flextable(na_summary) %>%
  add_header_row(values = "Missingness of Imputed Variables", 
                 colwidths = ncol(na_summary)) %>%
  set_caption("Missingness of Imputed Variables") %>%
  align(align = "center", part = "all") %>%
  theme_vanilla() %>%  
  set_table_properties(width = 1, layout = "autofit") %>%
  bg(bg = "white", part = "all")  
# Export the table as an image
save_as_image(ft, "imputation_variables.png")

#### Question 1: AUC Table ####
data <- data.frame(
  Iteration = c(1, 2, 3, 4, 5),
  AUC_LASSO = c(0.7600733, 0.7183333, 0.8820971, 0.7393813, 0.7236070),
  AUC_Pruned_Tree = c(0.7138278, 0.6797619, 0.8237179, 0.6622807, 0.6219453)
)
# Create a flextable object
ft <- flextable(data) %>%
  add_header_row(values = "Model Comparison via AUC", 
                 colwidths = ncol(data)) %>%
  set_caption("Model Comparison via AUC") %>%
  align(align = "center", part = "all") %>%
  theme_vanilla() %>%  
  set_table_properties(width = 1, layout = "autofit") %>%
  bg(bg = "white", part = "all")  
# Export the table as an image
save_as_image(ft, "AUC_table.png")

#### Question 1: Coefficients for Full Lasso Classification Model####
data <- data.frame(
  Variable = c("(Intercept)", "(Intercept)", 
               "TypeSingle Left Lung", "TypeSingle Right Lung", 
               "Gender__male_TRUE", "Age", "BMI", 
               "COPDTRUE", "alpha1_Antitrypsin_DeficiencyTRUE", 
               "Cystic_FibrosisTRUE", "Idiopathic_Pulmonary_HypertensionTRUE", 
               "Interstitial_Lung_DiseaseTRUE", "Redo_Lung_TransplantTRUE", 
               "Preoperative_ECLSTRUE", "LAS_score", "Pre_Hb", 
               "Pre_Hct", "Pre_Platelets", "Pre_PT", "Pre_INR", 
               "Pre_PTT", "ECLS_ECMOTRUE", "ECLS_CPBTRUE"),
  Coefficient = c(3.10014438, NA, 
                  NA, NA, 
                  NA, NA, NA, 
                  NA, NA, 
                  NA, NA, 
                  NA, NA, 
                  NA, NA, -0.02307781, 
                  NA, NA, NA, NA, 
                  NA, 0.47488129, NA)
)
ft <- flextable(data) %>%
  add_header_row(values = "Lasso Classification Model Weights", 
                 colwidths = ncol(data)) %>%
  set_caption("Lasso Classification Model Weights") %>%
  align(align = "center", part = "all") %>%
  theme_vanilla() %>%  
  set_table_properties(width = 1, layout = "autofit") %>%
  bg(bg = "white", part = "all")  
# Export the table as an image
save_as_image(ft, "lasso_classification_model_weights.png")

#### Question 1: Coefficients for Full Lasso Regression Model####
data <- data.frame(
  Variable = c(
    "(Intercept)", "TypeSingle Left Lung", "TypeSingle Right Lung", 
    "Gender__male_TRUE", "Age", "BMI", 
    "COPDTRUE", "alpha1_Antitrypsin_DeficiencyTRUE", 
    "Cystic_FibrosisTRUE", "Idiopathic_Pulmonary_HypertensionTRUE", 
    "Interstitial_Lung_DiseaseTRUE", "Redo_Lung_TransplantTRUE", 
    "Preoperative_ECLSTRUE", "LAS_score", 
    "Pre_Hb", "Pre_Hct", "Pre_Platelets", 
    "Pre_PT", "Pre_INR", "Pre_PTT", 
    "ECLS_ECMOTRUE", "ECLS_CPBTRUE"
  ),
  Coefficient = c(
    7.69718445, NA, NA, 
    NA, NA, NA, 
    NA, NA, 
    NA, NA, 
    NA, 4.96511536, 
    2.39094343, NA, 
    NA, -13.76478524, NA, 
    NA, NA, NA, 
    0.09694887, 0.50889569
  )
)
ft <- flextable(data) %>%
  add_header_row(values = "Lasso Regression Model Weights", 
                 colwidths = ncol(data)) %>%
  set_caption("Lasso Regression Model Weights") %>%
  align(align = "center", part = "all") %>%
  theme_vanilla() %>%  
  set_table_properties(width = 1, layout = "autofit") %>%
  bg(bg = "white", part = "all")  
# Export the table as an image
save_as_image(ft, "lasso_regression_model_weights.png")

#### Question 1: MSEs for Lasso Regression Models ####
data <- data.frame(
  Model_Type = c("Null", "Lasso Regression"),
  MSE = c(24.76188, 16.86478)
)
ft <- flextable(data) %>%
  add_header_row(values = "MSEs", 
                 colwidths = ncol(data)) %>%
  set_caption("Lasso Regression Model Weights") %>%
  align(align = "center", part = "all") %>%
  theme_vanilla() %>%  
  set_table_properties(width = 1, layout = "autofit") %>%
  bg(bg = "white", part = "all")  
# Export the table as an image
save_as_image(ft, "lasso_regression_and_null_models_MSE.png")
