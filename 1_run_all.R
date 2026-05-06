# Master script to run the entire analysis pipeline
cat("===== Starting machine learning pipeline =====\n")

source("R/00_setup.R")
source("R/01_data_prep.R")
source("R/02_variable_selection.R")
source("R/03_model_training.R")
source("R/04_evaluation.R")
source("R/05_shap_analysis.R")
source("R/06_cv_sensitivity.R")
source("R/07_final_output.R")

cat("===== All analyses completed. Results saved in 'output/' directory =====\n")
