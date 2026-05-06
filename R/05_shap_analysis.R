# 20. SHAP analysis for logistic regression (approximation) ------------
load("output/temp_models.RData")  # contains lr_model, trainData_balanced, testData_ml

# Coefficients and model matrices
lr_coefs <- coef(lr_model)[-1]
train_mat <- model.matrix(lr_model, trainData_balanced)[, -1]
test_mat <- model.matrix(lr_model, testData_ml)[, -1]

# Approximate SHAP values: (test - train_mean) * coefficient
shap_mat <- sweep(test_mat, 2, colMeans(train_mat), "-") |> sweep(2, lr_coefs, "*")
shap_df <- as.data.frame(shap_mat) %>%
  mutate(Sample = row_number()) %>%
  pivot_longer(-Sample, names_to = "Feature", values_to = "SHAP")

# Feature importance (mean absolute SHAP)
importance_merged <- shap_df %>%
  group_by(Feature) %>%
  summarise(mean_abs_SHAP = mean(abs(SHAP))) %>%
  left_join(data.frame(Feature = names(lr_coefs), Coeff = lr_coefs), by = "Feature") %>%
  mutate(Direction = factor(Coeff > 0, levels = c(TRUE, FALSE), 
                            labels = c("Increases Risk", "Decreases Risk"))) %>%
  arrange(desc(mean_abs_SHAP))

# Bar plot of importance
imp_plot <- ggplot(importance_merged, aes(x = reorder(Feature, mean_abs_SHAP), y = mean_abs_SHAP, fill = Direction)) +
  geom_col(width = 0.7, color = "black", size = 0.5) +
  scale_fill_manual(values = c("Increases Risk" = "#E74C3C", "Decreases Risk" = "#0072B2")) +
  coord_flip() +
  labs(title = "Feature Importance (Logistic Regression)", x = "", y = "Mean(|SHAP value|)") +
  theme_sci_large() +
  theme(legend.position = "right", legend.title = element_blank(), legend.text = element_text(size = 16))

# Beeswarm plot
shap_df$Feature <- factor(shap_df$Feature, levels = rev(importance_merged$Feature))
bee_plot <- ggplot(shap_df, aes(x = SHAP, y = Feature, color = SHAP)) +
  ggbeeswarm::geom_quasirandom(width = 0.3, size = 2.5, alpha = 0.7) +
  scale_color_gradient2(low = "#0072B2", mid = "grey90", high = "#E74C3C", midpoint = 0) +
  labs(title = "SHAP Summary Plot", x = "SHAP Value (Impact on Prediction)", y = "Features") +
  theme_sci_large() +
  theme(panel.grid.major = element_line(color = "grey90"),
        legend.text = element_text(size = 14))

# Combine plots
combined_shap <- cowplot::plot_grid(bee_plot, imp_plot, ncol = 2, rel_widths = c(1, 1.15),
                                    labels = c("A", "B"), label_size = 22, label_fontfamily = "Times New Roman")
ggsave(file.path(output_dir, "LR_Combined_Importance_SHAP.svg"), combined_shap, width = 15, height = 7)

# Export SHAP summary table
wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "Feature_Importance")
openxlsx::writeData(wb, "Feature_Importance", importance_merged)
openxlsx::addWorksheet(wb, "Detailed_SHAP_Values")
openxlsx::writeData(wb, "Detailed_SHAP_Values", shap_df)
openxlsx::saveWorkbook(wb, file.path(output_dir, "LR_SHAP_Summary.xlsx"), overwrite = TRUE)

cat("05_shap_analysis.R completed\n")
