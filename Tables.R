library(flextable)
library(webshot2)
library(htmltools)

#### Numerical Variables ####
data <- data.frame(
  Variable = c("Height", "Weight", "Age", "BMI", "LAS score", "Pre_Hb", 
               "Pre_Hct", "Pre_Platelets", "Pre_PT", "Pre_INR", "Pre_PTT", 
               "Pre_Creatinine", "Intra_Fresh Frozen Plasma", "Intra_Packed Cells",
               "Intra_Platelets", "Intra_Cryoprecipitate", "Duration of ICU Stay (days)",
               "ICU_LOS", "HOSPITAL_LOS", "RBC 72hr Total", "FFP 72hr Total",
               "Plt 72hr Total", "Cryo 72hr Total", "Total 24hr RBC"),
  mean = c(166.69, 67.44, 56.26, 24.11, 36.91, 127.84, 0.39, 264.03, 12.95, 
           1.11, 26.07, 74.55, 0.61, 2.27, 0.43, 2.71, 7.29, 9.24, 33.60, 
           0.80, 0.19, 0.14, 0.47, 2.67),
  median = c(167.00, 67.10, 61.00, 24.37, 34.00, 131.00, 0.40, 243.50, 11.60, 
             1.00, 24.50, 69.00, 0.00, 1.00, 0.00, 0.00, 4.00, 3.62, 22.84, 
             0.00, 0.00, 0.00, 0.00, 1.00),
  sd = c(10.11, 14.60, 14.80, 3.98, 9.15, 19.59, 0.05, 103.30, 4.70, 0.47, 
         7.99, 20.66, 2.54, 4.23, 1.39, 7.38, 11.45, 29.25, 42.36, 2.23, 
         0.98, 0.84, 2.76, 4.98),
  min = c(140.00, 33.10, 19.00, 15.54, 24.00, 73.00, 0.22, 89.00, 9.60, 0.80, 
          17.70, 37.00, 0.00, 0.00, 0.00, 0.00, 1.00, 0.63, 9.66, 0.00, 0.00, 
          0.00, 0.00, 0.00),
  max = c(192.00, 107.80, 76.00, 31.78, 90.00, 168.00, 0.51, 882.00, 37.40, 
          3.70, 93.50, 169.00, 26.00, 36.00, 11.00, 50.00, 99.00, 380.86, 
          381.52, 23.00, 8.00, 10.00, 30.00, 38.00),
  iqr = c(16.00, 22.12, 20.00, 6.80, 5.62, 25.00, 0.06, 98.75, 1.43, 0.20, 
          4.55, 21.25, 0.00, 3.00, 0.00, 0.00, 6.00, 5.66, 20.38, 1.00, 
          0.00, 0.00, 0.00, 4.00)
)
# Create a flextable.
ft <- flextable(data) %>%
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
  Variable = c("ALIVE_12MTHS_YN", "ALIVE_12MTHS_YN", "ALIVE_30DAYS_YN", "ALIVE_30DAYS_YN", 
               "ALIVE_90DAYS_YN", "ALIVE_90DAYS_YN", "Massive Transfusion", "Massive Transfusion", 
               "Type", "Type", "Type"),
  Value = c("Y", "N", "Y", "N", "Y", "N", "0", "1", "Bilateral", "Single Left Lung", "Single Right Lung"),
  Count = c(169, 23, 189, 3, 183, 9, 183, 9, 157, 18, 17),
  Proportion = c(0.88, 0.12, 0.98, 0.02, 0.95, 0.05, 0.95, 0.05, 0.82, 0.09, 0.09)
)
# Create a flextable
ft <- flextable(data) %>%
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
logical_table <- data.frame(
  Variable = c(
    "COPD", "COPD", "Cystic Fibrosis", "Cystic Fibrosis", "ECLS_CPB", "ECLS_CPB",
    "ECLS_ECMO", "ECLS_ECMO", "ExVIVO Lung Perfusion", "ExVIVO Lung Perfusion",
    "First Lung Transplant", "First Lung Transplant", "Gender (male)", "Gender (male)",
    "Idiopathic Pulmonary Hypertension", "Idiopathic Pulmonary Hypertension",
    "Interstitial Lung Disease", "Interstitial Lung Disease", "Intraoperative ECLS",
    "Intraoperative ECLS", "Preoperative ECLS", "Preoperative ECLS", "Pulm_Other",
    "Pulm_Other", "Redo Lung Transplant", "Redo Lung Transplant",
    "alpha1-Antitrypsin Deficiency", "alpha1-Antitrypsin Deficiency"
  ),
  Value = c(
    "FALSE", "TRUE", "FALSE", "TRUE", "FALSE", "TRUE",
    "TRUE", "FALSE", "FALSE", "TRUE", "TRUE", "FALSE",
    "TRUE", "FALSE", "FALSE", "TRUE", "FALSE", "TRUE",
    "TRUE", "FALSE", "FALSE", "TRUE", "FALSE", "TRUE",
    "FALSE", "TRUE", "FALSE", "TRUE"
  ),
  Count = c(
    134, 58, 162, 30, 190, 2, 101, 91, 115, 77, 181, 11, 104, 88,
    186, 6, 100, 92, 103, 89, 185, 7, 177, 15, 182, 10, 183, 9
  ),
  Proportion = c(
    0.70, 0.30, 0.84, 0.16, 0.99, 0.01, 0.53, 0.47, 0.60, 0.40, 0.94, 0.06,
    0.54, 0.46, 0.97, 0.03, 0.52, 0.48, 0.54, 0.46, 0.96, 0.04, 0.92, 0.08,
    0.95, 0.05, 0.95, 0.05
  )
)
# Create a flextable
ft <- flextable(logical_table) %>%
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
  set_caption("Summary of Missing Values by Variable") %>%
  align(align = "center", part = "all") %>%
  theme_vanilla() %>%  
  set_table_properties(width = 1, layout = "autofit") %>%
  bg(bg = "white", part = "all")  
# Export the table as an image
save_as_image(ft, "imputation_variables.png")