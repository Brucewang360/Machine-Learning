# 22. 10-fold cross-validation (SMOTE + internal scaling) for all models
load("output/temp_split.RData")
load("output/temp_selected_vars.RData")
load("output/temp_models.RData")   # contains trainData_ml and testData_ml (but CV uses trainData_ml directly)

set.seed(seed_cv)
folds <- createFolds(trainData_ml$Diagnose, k = 10)
num_vars <- selected_vars

cv_auc <- list(LR = numeric(10), NB = numeric(10), RF = numeric(10), SVM = numeric(10), XGB = numeric(10))

for (i in 1:10) {
  cat("CV Fold", i, "of 10\n")
  fold_test_idx <- folds[[i]]
  cv_train_raw <- trainData_ml[-fold_test_idx, ]
  cv_valid_raw <- trainData_ml[ fold_test_idx, ]
  
  # Impute missing values with training fold median
  impute_median_fold <- function(df, vars) {
    for (v in vars) {
      med_val <- median(df[[v]], na.rm = TRUE)
      df[[v]][is.na(df[[v]])] <- med_val
    }
    df
  }
  cv_train_raw <- impute_median_fold(cv_train_raw, num_vars)
  cv_valid_raw <- impute_median_fold(cv_valid_raw, num_vars)
  
  # Standardize based on training fold mean/sd
  train_means <- sapply(cv_train_raw[, num_vars], mean, na.rm = TRUE)
  train_sds   <- sapply(cv_train_raw[, num_vars], sd, na.rm = TRUE)
  train_sds[train_sds == 0] <- 1
  
  cv_train_scaled <- cv_train_raw
  cv_valid_scaled <- cv_valid_raw
  for (v in num_vars) {
    cv_train_scaled[[v]] <- (cv_train_scaled[[v]] - train_means[v]) / train_sds[v]
    cv_valid_scaled[[v]] <- (cv_valid_scaled[[v]] - train_means[v]) / train_sds[v]
  }
  
  # SMOTE balancing (only training fold)
  smote_recipe <- recipe(Diagnose ~ ., data = cv_train_scaled) %>%
    step_smote(Diagnose, over_ratio = 1)
  cv_train_bal <- prep(smote_recipe) %>% bake(new_data = NULL)
  
  train_labels_cv <- as.numeric(cv_train_bal$Diagnose) - 1
  valid_labels_cv <- as.numeric(cv_valid_scaled$Diagnose) - 1
  
  # Logistic regression
  lr_cv <- glm(Diagnose ~ ., data = cv_train_bal, family = binomial)
  pred_lr <- predict(lr_cv, cv_valid_scaled, type = "response")
  cv_auc$LR[i] <- auc(roc(valid_labels_cv, pred_lr, quiet = TRUE))
  
  # Naive Bayes
  nb_cv <- naiveBayes(Diagnose ~ ., data = cv_train_bal)
  pred_nb <- predict(nb_cv, cv_valid_scaled, type = "raw")[, 2]
  cv_auc$NB[i] <- auc(roc(valid_labels_cv, pred_nb, quiet = TRUE))
  
  # Random Forest
  rf_cv <- randomForest(Diagnose ~ ., data = cv_train_bal, ntree = 100)
  pred_rf <- predict(rf_cv, cv_valid_scaled, type = "prob")[, 2]
  cv_auc$RF[i] <- auc(roc(valid_labels_cv, pred_rf, quiet = TRUE))
  
  # SVM
  svm_cv <- svm(Diagnose ~ ., data = cv_train_bal, probability = TRUE)
  pred_svm <- attr(predict(svm_cv, cv_valid_scaled, probability = TRUE), "probabilities")[, 2]
  cv_auc$SVM[i] <- auc(roc(valid_labels_cv, pred_svm, quiet = TRUE))
  
  # XGBoost
  xgb_train_mat <- model.matrix(~ . - Diagnose - 1, data = cv_train_bal)
  xgb_valid_mat <- model.matrix(~ . - Diagnose - 1, data = cv_valid_scaled)
  dtrain_cv <- xgb.DMatrix(data = xgb_train_mat, label = train_labels_cv)
  xgb_cv <- xgboost(data = dtrain_cv,
                    params = list(objective = "binary:logistic", eval_metric = "logloss"),
                    nrounds = 100, verbose = 0)
  pred_xgb <- predict(xgb_cv, xgb_valid_mat)
  cv_auc$XGB[i] <- auc(roc(valid_labels_cv, pred_xgb, quiet = TRUE))
}

# Summarise CV results
cv_summary <- data.frame(
  Model = names(cv_auc),
  Mean_AUC = sapply(cv_auc, mean),
  SD_AUC   = sapply(cv_auc, sd)
)
cv_summary$MeanSD <- sprintf("%.3f ± %.3f", cv_summary$Mean_AUC, cv_summary$SD_AUC)
write.xlsx(cv_summary, file.path(output_dir, "10Fold_CV_All_Models.xlsx"), overwrite = TRUE)

# Boxplot
cv_df <- do.call(rbind, lapply(names(cv_auc), function(m) {
  data.frame(Model = m, AUC = cv_auc[[m]], Fold = 1:10)
}))
label_df <- cv_summary %>%
  mutate(y_pos = Mean_AUC + 0.05)

cv_plot <- ggplot(cv_df, aes(x = Model, y = AUC, fill = Model)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 21, outlier.size = 2) +
  geom_jitter(width = 0.2, size = 1.5, alpha = 0.6) +
  geom_text(data = label_df,
            aes(x = Model, y = y_pos, label = MeanSD),
            size = 4.5, family = "Times New Roman", fontface = "bold", vjust = 0) +
  scale_fill_manual(values = colors[models_order]) +
  labs(title = "10-Fold Cross-Validation AUC", x = "", y = "AUC") +
  theme_sci_large() + theme(legend.position = "none")
ggsave(file.path(output_dir, "10Fold_CV_All_Models_Boxplot.svg"), cv_plot, width = 7, height = 6)

# 23. Optimal threshold (F1-maximizing) for LR -------------------------
roc_obj_lr <- roc(test_labels_clean, tmp$LR[ok])
f1_df <- coords(roc_obj_lr, x = "all", ret = c("threshold", "sensitivity", "specificity"), drop = FALSE)
f1_df$F1 <- 2 * f1_df$sensitivity * f1_df$specificity / (f1_df$sensitivity + f1_df$specificity)
optimal_threshold <- f1_df$threshold[which.max(f1_df$F1)]

# 24. Single-variable perturbation sensitivity analysis -----------------
sd_vec <- sapply(trainData_ml[, numeric_vars_ml], sd)
all_perturb <- list()

for (v in numeric_vars_ml) {
  delta_seq <- seq(-2, 2, by = 0.1) * sd_vec[v]
  all_perturb[[v]] <- map_df(delta_seq, function(d) {
    new_data <- testData_ml
    new_data[[v]] <- new_data[[v]] + d
    probs <- predict(lr_model, new_data, type = "response")
    n <- length(probs)
    mean_prob <- mean(probs)
    sd_prob <- sd(probs)
    ci_lower <- mean_prob - 1.96 * sd_prob / sqrt(n)
    ci_upper <- mean_prob + 1.96 * sd_prob / sqrt(n)
    tibble(Variable = v,
           Delta_SD = d / sd_vec[v],
           Mean_Prob = mean_prob,
           CI_lower = ci_lower,
           CI_upper = ci_upper)
  })
}
all_perturb_df <- bind_rows(all_perturb)

summary_perturb_plot <- ggplot(all_perturb_df, aes(x = Delta_SD, y = Mean_Prob, color = Variable)) +
  geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper, fill = Variable), alpha = 0.2, colour = NA) +
  geom_line(size = 1.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "darkgray") +
  labs(title = "Prediction Sensitivity to Feature Perturbation",
       x = "Shift in Standard Deviation (SD) units",
       y = "Mean Predicted Probability") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  theme_sci_large() +
  theme(legend.position = "bottom")
ggsave(file.path(output_dir, "All_Variables_Perturbation_Summary.svg"), summary_perturb_plot, width = 10, height = 8)

# Export impact ranking
impact_df <- all_perturb_df %>%
  group_by(Variable) %>%
  summarise(Impact_Score = max(Mean_Prob) - min(Mean_Prob),
            Direction = ifelse(Mean_Prob[Delta_SD == 2] > Mean_Prob[Delta_SD == -2],
                               "Positive", "Negative")) %>%
  arrange(desc(Impact_Score))
write.xlsx(impact_df, file.path(output_dir, "Variable_Sensitivity_Impact_Ranking.xlsx"), overwrite = TRUE)

cat("06_cv_sensitivity.R completed\n")
