# 26. Threshold comparison table ---------------------------------------
thresholds <- seq(0.1, 0.9, by = 0.05)
if (exists("pred_lr_test_cal") && exists("test_labels_clean")) {
  thresh_results <- do.call(rbind, lapply(thresholds, function(t) {
    pred_class <- ifelse(pred_lr_test_cal > t, 1, 0)
    pred_f <- factor(pred_class, levels = c(0, 1), labels = c("Normal", "Sarcopenia"))
    true_f <- factor(test_labels_clean, levels = c(0, 1), labels = c("Normal", "Sarcopenia"))
    cm <- confusionMatrix(pred_f, true_f, positive = "Sarcopenia")
    data.frame(Threshold = t,
               Sensitivity = cm$byClass["Sensitivity"],
               Specificity = cm$byClass["Specificity"],
               PPV = cm$byClass["Pos Pred Value"],
               NPV = cm$byClass["Neg Pred Value"],
               Accuracy = cm$overall["Accuracy"],
               F1 = cm$byClass["F1"])
  }))
  write.xlsx(thresh_results, file.path(output_dir, "LR_Threshold_Comparison.xlsx"), overwrite = TRUE)
}

# 27. Confusion matrix for LR using optimal threshold -----------------
plot_cm_optimal <- function(true_labels, pred_probs, threshold, model_name, dataset_name) {
  pred_class <- ifelse(pred_probs > threshold, 1, 0)
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
    labs(title = paste0(model_name, " (Test Set)"),
         subtitle = paste0("Threshold = ", round(threshold, 3)),
         x = "Actual Class", y = "Predicted Class") +
    theme_bw() +
    theme(text = element_text(family = "Times New Roman"),
          panel.grid = element_blank(),
          axis.text = element_text(size = 18, color = "black", face = "bold"),
          axis.title = element_text(size = 20, face = "bold"),
          plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 16, hjust = 0.5),
          legend.position = "none") +
    coord_fixed()
}
if (exists("optimal_threshold") && exists("pred_lr_test_cal")) {
  p_cm_lr_opt <- plot_cm_optimal(test_labels_clean, pred_lr_test_cal,
                                 optimal_threshold, "LR Model", "Test Set")
  ggsave(file.path(cm_dir, "CM_Test_LR_Optimal_Threshold.svg"),
         p_cm_lr_opt, width = 5, height = 5)
}

# 28. Elastic net visualization (coefficient path & CV plot) ----------
save_svg <- function(filename, plot_expr, width = 6, height = 6) {
  svglite(file.path(output_dir, filename), width = width, height = height)
  par(family = "Times", cex.axis = 1.2, cex.lab = 1.4, cex.main = 1.5,
      mar = c(5, 5, 4, 2) + 0.1)
  plot_expr
  dev.off()
  cat(filename, "saved\n")
}

save_svg("elastic_net_coefficient_path.svg", {
  colors_path <- brewer.pal(min(9, ncol(x)), "Set1")
  plot(cv_fit$glmnet.fit, xvar = "lambda", label = FALSE,
       col = colors_path, lwd = 2, main = "Coefficient Path")
  abline(v = log(lambda_1se), col = "#E74C3C", lty = 2, lwd = 2)
  abline(v = log(cv_fit$lambda.min), col = "#2ECC71", lty = 2, lwd = 2)
  legend("topright",
         legend = c("lambda.1se", "lambda.min"),
         col = c("#E74C3C", "#2ECC71"),
         lty = 2, lwd = 2, cex = 1.2, bty = "n")
})

save_svg("elastic_net_cross_validation.svg", {
  plot(cv_fit, col = "#0072B2", lwd = 2, main = "Cross-Validation Error")
  abline(v = log(lambda_1se), col = "#E74C3C", lty = 2, lwd = 2)
  abline(v = log(cv_fit$lambda.min), col = "#2ECC71", lty = 2, lwd = 2)
  legend("topright",
         legend = c("lambda.1se", "lambda.min"),
         col = c("#E74C3C", "#2ECC71"),
         lty = 2, lwd = 2, cex = 1.2, bty = "n")
})

cat("07_final_output.R completed\n")
