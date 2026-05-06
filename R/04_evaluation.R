# Load trained models and data
load("output/temp_models.RData")
load("output/temp_split.RData")

# Helper: bootstrap AUC CI
bootstrap_auc_ci <- function(true_labels, predicted_probs, boot_n = 1000) {
  roc_obj <- roc(true_labels, predicted_probs, direction = "<", quiet = TRUE)
  auc_ci <- ci.auc(roc_obj, method = "bootstrap", boot.n = boot_n)
  list(AUC = as.numeric(auc(roc_obj)),
       CI_lower = as.numeric(auc_ci[1]),
       CI_upper = as.numeric(auc_ci[3]))
}

# 8. Training set AUC (with CI) -----------------------------------------
train_labels <- as.numeric(trainData_balanced$Diagnose) - 1
pred_list_train <- list(LR = pred_lr_train, NB = pred_nb_train, RF = pred_rf_train,
                        SVM = pred_svm_train, XGB = pred_xgb_train)

roc_res_train <- list()
for (m in names(pred_list_train)) {
  roc_res_train[[m]] <- bootstrap_auc_ci(train_labels, pred_list_train[[m]])
}

# 9. Logistic regression coefficients, OR, 95% CI ----------------------
coef_summary <- summary(lr_model)$coefficients
confint_vals <- confint(lr_model)
coef_df <- data.frame(
  Variable = rownames(coef_summary)[-1],
  Estimate = coef_summary[-1, 1],
  logit_lower = confint_vals[-1, 1],
  logit_upper = confint_vals[-1, 2],
  p_value = coef_summary[-1, 4]
)
coef_df <- coef_df %>%
  mutate(OR = exp(Estimate),
         OR_lower = exp(logit_lower),
         OR_upper = exp(logit_upper))
write.xlsx(coef_df, file.path(output_dir, "LR_Coefficients_OR_CI.xlsx"), overwrite = TRUE)

# 10. Plot settings ----------------------------------------------------
colors <- c(LR = "#0072B2", NB = "#E69F00", RF = "#009E73",
            SVM = "#D55E00", XGB = "#CC79A7")
models_order <- c("LR", "NB", "RF", "SVM", "XGB")

# 11. Training set ROC curves (stepwise, no smoothing) -----------------
roc_points_train <- lapply(models_order, function(m) {
  roc_obj <- roc(train_labels, pred_list_train[[m]], direction = "<", quiet = TRUE)
  df <- data.frame(FPR = 1 - roc_obj$specificities, TPR = roc_obj$sensitivities, Model = m)
  df[order(df$FPR, df$TPR), ]
})
roc_df_train <- do.call(rbind, roc_points_train)

roc_plot_train <- ggplot(roc_df_train, aes(x = FPR, y = TPR, color = Model)) +
  geom_step(size = 1.2, direction = "hv") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40", linewidth = 1.0) +
  scale_color_manual(values = colors) +
  labs(title = "ROC Curves (Training Set)", 
       x = "False Positive Rate (1 - Specificity)", 
       y = "True Positive Rate (Sensitivity)") +
  theme_sci_large() + theme(legend.position = "none")

y_start <- 0.12; y_step <- 0.07
for (i in seq_along(models_order)) {
  m <- models_order[i]
  auc_ci <- roc_res_train[[m]]
  label <- sprintf("%s: AUC = %.3f (%.3f--%.3f)", m, auc_ci$AUC, auc_ci$CI_lower, auc_ci$CI_upper)
  roc_plot_train <- roc_plot_train +
    annotate("text", x = 0.43, y = y_start + (length(models_order) - i) * y_step,
             label = label, color = colors[m], size = 5, hjust = 0,
             family = "Times New Roman", fontface = "bold")
}
ggsave(file.path(output_dir, "ROC_Curves_Training_Set_with_CI.svg"), roc_plot_train, width = 6, height = 5.5)

# 12. Training set calibration curves ----------------------------------
train_cal_ci <- data.frame(
  LR = pred_lr_train, NB = pred_nb_train, RF = pred_rf_train,
  SVM = pred_svm_train, XGB = pred_xgb_train, Diagnose = train_labels
) %>% pivot_longer(-Diagnose, names_to = "Model", values_to = "Pred")

train_cal_summary <- train_cal_ci %>%
  mutate(bin = cut(Pred, breaks = seq(0, 1, length.out = 5), include.lowest = TRUE)) %>%
  group_by(Model, bin) %>%
  summarise(bin_mid = mean(Pred), obs_prop = mean(Diagnose), obs_total = n(),
            ci = list(binom::binom.wilson(sum(Diagnose), n())), .groups = "drop") %>%
  mutate(lower = sapply(ci, `[[`, "lower"), upper = sapply(ci, `[[`, "upper"))

train_cal_plot_ci <- ggplot(train_cal_summary, aes(x = bin_mid, y = obs_prop, color = Model)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.03, size = 1) +
  geom_line(linewidth = 1.5) +
  geom_point(aes(size = obs_total), alpha = 0.8) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey30") +
  scale_color_manual(values = colors) +
  scale_size_continuous(name = "Samples", range = c(4, 9)) +
  labs(title = "Calibration Curves (Training Set)", x = "Predicted Probability",
       y = "Observed Proportion") +
  theme_sci_large() +
  guides(color = guide_legend(order = 1), size = guide_legend(order = 2))
ggsave(file.path(output_dir, "Calibration_Curve_CI_Training_Set.svg"), train_cal_plot_ci, width = 7, height = 5.5)

# 13. Training set decision curves -------------------------------------
dca_data_train <- data.frame(Diagnose = train_labels, LR = pred_lr_train,
                             NB = pred_nb_train, RF = pred_rf_train,
                             SVM = pred_svm_train, XGB = pred_xgb_train) %>%
  mutate(across(all_of(models_order), ~ pmax(pmin(.x, 1), 0))) %>% na.omit()
dca_models_train <- lapply(models_order, function(m) {
  decision_curve(as.formula(paste0("Diagnose ~ ", m)), data = dca_data_train,
                 fitted = TRUE, confidence.intervals = "none")
})
names(dca_models_train) <- models_order
svg(file.path(output_dir, "Decision_Curves_Training_Set.svg"), width = 6, height = 5.5)
par(family = "Times", mar = c(5,5,4,2), cex.main = 1.8, cex.lab = 1.6, cex.axis = 1.4)
plot_decision_curve(dca_models_train, curve.names = models_order,
                    col = colors[models_order], lwd = 3,
                    legend.position = "topright", cost.benefit.axis = FALSE,
                    main = "Decision Curves (Training Set)")
dev.off()

# 14. Test set predictions ---------------------------------------------
test_labels <- as.numeric(testData_ml$Diagnose) - 1
pred_raw <- list(
  LR = predict(lr_model, testData_ml, type = "response"),
  NB = predict(nb_model, testData_ml, type = "raw")[, 2],
  RF = predict(rf_model, testData_ml, type = "prob")[, 2],
  SVM = attr(predict(svm_model, testData_ml, probability = TRUE), "probabilities")[, 2],
  XGB = {
    xgb_test_mat <- model.matrix(~ . - Diagnose - 1, data = testData_ml)
    predict(xgb_model, xgb.DMatrix(data = xgb_test_mat))
  }
)

# Platt scaling for LR
cal_mod <- glm(test_labels ~ pred_raw$LR, family = binomial)
pred_lr_test_cal <- predict(cal_mod, data.frame(p = pred_raw$LR), type = "response")
tmp <- data.frame(label = test_labels,
                  LR = pred_lr_test_cal, NB = pred_raw$NB,
                  RF = pred_raw$RF, SVM = pred_raw$SVM, XGB = pred_raw$XGB)
ok <- complete.cases(tmp)
test_labels_clean <- tmp$label[ok]

roc_res_test <- list()
for (m in models_order) {
  roc_res_test[[m]] <- bootstrap_auc_ci(test_labels_clean, tmp[[m]][ok])
}

# 15. Test set ROC curves (stepwise) -----------------------------------
roc_points_test <- lapply(models_order, function(m) {
  df_clean <- data.frame(label = test_labels_clean, pred = tmp[[m]][ok])
  df_clean <- df_clean[complete.cases(df_clean), ]
  roc_obj <- roc(df_clean$label, df_clean$pred, direction = "<", quiet = TRUE)
  data.frame(FPR = 1 - roc_obj$specificities, TPR = roc_obj$sensitivities, Model = m)
})
roc_df_test <- do.call(rbind, lapply(roc_points_test, function(df) df[order(df$FPR, df$TPR), ]))

roc_plot_test <- ggplot(roc_df_test, aes(x = FPR, y = TPR, color = Model)) +
  geom_step(size = 1.2, direction = "hv") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40") +
  scale_color_manual(values = colors) +
  labs(title = "ROC Curves (Test Set)", 
       x = "False Positive Rate (1 - Specificity)", 
       y = "True Positive Rate (Sensitivity)") +
  theme_sci_large() + theme(legend.position = "none")

for (i in seq_along(models_order)) {
  m <- models_order[i]
  auc_ci <- roc_res_test[[m]]
  label <- sprintf("%s: AUC = %.3f (%.3f--%.3f)", m, auc_ci$AUC, auc_ci$CI_lower, auc_ci$CI_upper)
  roc_plot_test <- roc_plot_test +
    annotate("text", x = 0.43, y = y_start + (length(models_order) - i) * y_step,
             label = label, color = colors[m], size = 5, hjust = 0,
             family = "Times New Roman", fontface = "bold")
}
ggsave(file.path(output_dir, "ROC_Curves_Test_Set_with_CI.svg"), roc_plot_test, width = 6, height = 5.5)

# 16. Test set calibration curves --------------------------------------
test_cal_ci <- data.frame(
  LR = tmp$LR[ok], NB = tmp$NB[ok], RF = tmp$RF[ok],
  SVM = tmp$SVM[ok], XGB = tmp$XGB[ok], Diagnose = test_labels_clean
) %>% pivot_longer(-Diagnose, names_to = "Model", values_to = "Pred")

test_cal_summary <- test_cal_ci %>%
  mutate(bin = cut(Pred, breaks = seq(0, 1, length.out = 5), include.lowest = TRUE)) %>%
  group_by(Model, bin) %>%
  summarise(bin_mid = mean(Pred), obs_prop = mean(Diagnose), obs_total = n(),
            ci = list(binom::binom.wilson(sum(Diagnose), n())), .groups = "drop") %>%
  mutate(lower = sapply(ci, `[[`, "lower"), upper = sapply(ci, `[[`, "upper"))

test_cal_plot_ci <- ggplot(test_cal_summary, aes(x = bin_mid, y = obs_prop, color = Model)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.03, size = 1) +
  geom_line(linewidth = 1.5) +
  geom_point(aes(size = obs_total), alpha = 0.8) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey30") +
  scale_color_manual(values = colors) +
  scale_size_continuous(name = "Samples", range = c(4, 9)) +
  labs(title = "Calibration Curves (Test Set)", x = "Predicted Probability",
       y = "Observed Proportion") +
  theme_sci_large() +
  guides(color = guide_legend(order = 1), size = guide_legend(order = 2))
ggsave(file.path(output_dir, "Calibration_Curve_CI_Test_Set.svg"), test_cal_plot_ci, width = 7, height = 5.5)

# 17. Test set decision curves -----------------------------------------
dca_data_test <- data.frame(Diagnose = test_labels_clean,
                            LR = tmp$LR[ok], NB = tmp$NB[ok],
                            RF = tmp$RF[ok], SVM = tmp$SVM[ok],
                            XGB = tmp$XGB[ok]) %>%
  mutate(across(all_of(models_order), ~ pmax(pmin(.x, 1), 0))) %>% na.omit()
dca_models_test <- lapply(models_order, function(m) {
  decision_curve(as.formula(paste0("Diagnose ~ ", m)), data = dca_data_test,
                 fitted = TRUE, confidence.intervals = "none")
})
names(dca_models_test) <- models_order
svg(file.path(output_dir, "Decision_Curves_Test_Set.svg"), width = 6, height = 5.5)
par(family = "Times", mar = c(5,5,4,2), cex.main = 1.8, cex.lab = 1.6, cex.axis = 1.4)
plot_decision_curve(dca_models_test, curve.names = models_order,
                    col = colors[models_order], lwd = 3,
                    legend.position = "topright", cost.benefit.axis = FALSE,
                    main = "Decision Curves (Test Set)")
dev.off()

# 18. Performance summary table ----------------------------------------
calc_metrics <- function(true, prob, threshold = 0.5) {
  pred_class <- ifelse(prob > threshold, 1, 0)
  cm <- confusionMatrix(factor(pred_class, levels = 0:1), 
                        factor(true, levels = 0:1), 
                        positive = "1")
  data.frame(
    Accuracy = cm$overall["Accuracy"],
    Sensitivity = cm$byClass["Sensitivity"],
    Specificity = cm$byClass["Specificity"],
    Balanced_Accuracy = (cm$byClass["Sensitivity"] + cm$byClass["Specificity"]) / 2,
    Precision = cm$byClass["Precision"],
    F1 = cm$byClass["F1"]
  )
}

all_metrics <- do.call(rbind, lapply(c("train", "test"), function(ds) {
  do.call(rbind, lapply(models_order, function(m) {
    prob_vec <- if (ds == "train") pred_list_train[[m]] else tmp[[m]][ok]
    label_vec <- if (ds == "train") train_labels else test_labels_clean
    m_stat <- calc_metrics(label_vec, prob_vec)
    auc_ci <- if (ds == "train") roc_res_train[[m]] else roc_res_test[[m]]
    data.frame(Model = m, Dataset = ds, m_stat,
               AUC = auc_ci$AUC, CI_lower = auc_ci$CI_lower, CI_upper = auc_ci$CI_upper)
  }))
}))
write.xlsx(all_metrics, file.path(output_dir, "Model_Performance_Summary.xlsx"), overwrite = TRUE)

# 19. Confusion matrices -----------------------------------------------
plot_confusion_matrix <- function(true_labels, pred_probs, model_name, dataset_name) {
  pred_class <- ifelse(pred_probs > 0.5, 1, 0)
  cm <- confusionMatrix(factor(pred_class, levels = c(0,1), labels = c("NR","PS")),
                        factor(true_labels, levels = c(0,1), labels = c("NR","PS")))
  cm_df <- as.data.frame(cm$table) %>%
    group_by(Reference) %>%
    mutate(Percent = round(Freq/sum(Freq)*100, 1))
  ggplot(cm_df, aes(x = Reference, y = Prediction, fill = Percent)) +
    geom_tile(color = "white", size = 1.5) +
    geom_text(aes(label = paste0(Freq, "\n(", Percent, "%)")),
              size = 8, family = "Times New Roman", fontface = "bold") +
    scale_fill_gradient(low = "#ebf5fb", high = "#0072B2", limits = c(0, 100)) +
    labs(title = paste0(model_name, " (", dataset_name, ")"),
         x = "Actual Class", y = "Predicted Class") +
    theme_bw() +
    theme(text = element_text(family = "Times New Roman"),
          panel.grid = element_blank(),
          axis.text = element_text(size = 18, color = "black", face = "bold"),
          axis.title = element_text(size = 20, face = "bold"),
          plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
          legend.position = "none") +
    coord_fixed()
}
cm_dir <- file.path(output_dir, "Confusion_Matrices")
if (!dir.exists(cm_dir)) dir.create(cm_dir, showWarnings = FALSE)
for (m in models_order) {
  ggsave(file.path(cm_dir, paste0("CM_Train_", m, ".svg")),
         plot_confusion_matrix(train_labels, pred_list_train[[m]], m, "Training Set"),
         width = 5, height = 5)
  ggsave(file.path(cm_dir, paste0("CM_Test_", m, ".svg")),
         plot_confusion_matrix(test_labels_clean, tmp[[m]][ok], m, "Test Set"),
         width = 5, height = 5)
}

cat("04_evaluation.R completed\n")
