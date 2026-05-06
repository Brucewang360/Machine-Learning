# Machine-Learning
Analysis code for Oral Microbiota and Sarcopenia Prediction
# Machine Learning for Sarcopenia Prediction – Feature Selection & Model Comparison

This repository contains the complete R code for predicting sarcopenia using clinical and microbiome data.  
An elastic net is used for feature selection, followed by training and comparison of five machine learning models: Logistic Regression, Naive Bayes, Random Forest, SVM, and XGBoost.

# Project Structure

ML_Sarcopenia/
├── data/ # Place your Excel file (1_Metadata.xlsx) here
├── output/ # All results will be saved here (created automatically)
├── R/ # R scripts for each step
├── run_all.R # Master script to run the entire pipeline
├── renv.lock # Package versions (optional, use renv)
└── README.md

# Requirements

- R >= 4.2.0
- Required packages (see `00_setup.R`). Install them via:
  ```r
  install.packages(c("pacman", "caret", "readxl", "nnet", "e1071", "randomForest", 
                     "xgboost", "pROC", "dplyr", "ROSE", "ggplot2", "binom", 
                     "smotefamily", "glmnet", "openxlsx", "pls", "ggbeeswarm", 
                     "grid", "plotROC", "tidyverse", "rmda", "cowplot", 
                     "RColorBrewer", "svglite", "tidyr", "usdm", "corrplot", 
                     "car", "recipes", "themis"))

# How to Run
Clone this repository.
Place your data file 1_Metadata.xlsx inside the data/ folder.
Open R (preferably RStudio) and run:
source("run_all.R")

# Outputs
All output files (figures, tables) will be written to the output/ directory, including:

Tables: Model performance summary, logistic regression coefficients, 10-fold CV results, threshold comparison, etc.
Figures: ROC curves (training/test), calibration curves, decision curves, confusion matrices, SHAP plots, VIF plot, correlation heatmap, elastic net paths, perturbation analysis, etc.
Reproducibility

All random seeds are fixed (seed_split = 242, seed_cv = 242, seed_smote = 242).
The code applies CLR transformation to microbial abundances (pseudocount = 1e-6) and SMOTE upsampling (over_ratio = 1) on the training set.

# Citation
If you use this code, please cite our manuscript (reference to be added).

# License
MIT License (or as you prefer).
