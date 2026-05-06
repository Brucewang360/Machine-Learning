# 0. Load required packages ------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  caret, readxl, nnet, e1071, randomForest, xgboost, pROC, dplyr, ROSE,
  ggplot2, binom, smotefamily, glmnet, openxlsx, pls, ggbeeswarm, grid,
  plotROC, tidyverse, rmda, cowplot, RColorBrewer, svglite, tidyr,
  usdm, corrplot, car, recipes, themis
)

# Global parameters
do_clr <- TRUE          # Apply CLR transformation to microbial abundances
pseudocount <- 1e-6     # Pseudocount for CLR
alpha_enet <- 0.5       # Elastic net mixing parameter
seed_split <- 242       # Seed for data partitioning
seed_cv <- 242          # Seed for elastic net cross-validation
seed_smote <- 242       # Seed for SMOTE upsampling

# Variable names (as defined in the original data)
ordinal_vars <- c()
continuous_vars <- c()
binary_vars <- c()
microbiome_vars <- c()

# Output directory
output_dir <- "output"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Custom ggplot theme for publication
theme_sci_large <- function() {
  theme_bw(base_family = "Times New Roman") +
    theme(
      plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 20, face = "bold"),
      axis.text = element_text(size = 18, color = "black"),
      legend.title = element_text(size = 18, face = "bold"),
      legend.text = element_text(size = 16),
      panel.grid = element_blank(),
      axis.line = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1.2),
      axis.ticks = element_line(color = "black", linewidth = 1.0)
    )
}

cat("00_setup.R completed\n")
