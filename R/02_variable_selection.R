# Load split data
load("output/temp_split.RData")

# 3.1 Preprocessing for elastic net ------------------------------------
trainData <- trainData_raw %>%
  mutate(across(all_of(c(continuous_vars, microbiome_vars)),
                ~ ifelse(is.na(.x), median(.x, na.rm = TRUE), .x)))

ordinal_encoded <- trainData %>%
  select(all_of(ordinal_vars)) %>%
  mutate(across(everything(), ~ as.numeric(.x)))

dummy_vars <- trainData %>% select(Diagnose) %>% mutate(.dummy = 1) %>% select(-Diagnose, -.dummy)

train_means <- trainData %>%
  summarise(across(all_of(c(continuous_vars, microbiome_vars)), mean, na.rm = TRUE))
train_sds <- trainData %>%
  summarise(across(all_of(c(continuous_vars, microbiome_vars)), sd, na.rm = TRUE))

continuous_scaled <- trainData %>%
  select(all_of(c(continuous_vars, microbiome_vars))) %>%
  mutate(across(everything(),
                ~ (.x - train_means[[cur_column()]]) / train_sds[[cur_column()]]))

count_original <- trainData %>% select(Diagnose) %>% mutate(.dummy = 1) %>% select(-Diagnose, -.dummy)

x_train_df <- bind_cols(
  trainData %>% select(all_of(binary_vars)),
  dummy_vars,
  ordinal_encoded,
  count_original,
  continuous_scaled
)
x <- as.matrix(x_train_df)
y <- trainData$Diagnose

# 3.2 VIF plot (using car::vif) -----------------------------------------
vif_data <- as.data.frame(x_train_df)
is_constant <- sapply(vif_data, function(col) length(unique(col)) == 1)
if (any(is_constant)) {
  cat("Removing constant columns:", names(vif_data)[is_constant], "\n")
  vif_data <- vif_data[, !is_constant, drop = FALSE]
}
combo_info <- findLinearCombos(vif_data)
if (!is.null(combo_info$remove)) {
  cat("Removing linearly dependent columns:", colnames(vif_data)[combo_info$remove], "\n")
  vif_data <- vif_data[, -combo_info$remove, drop = FALSE]
}
dummy_lm <- lm(1:nrow(vif_data) ~ ., data = vif_data)
vif_values <- vif(dummy_lm)
vif_df <- data.frame(Variable = names(vif_values), VIF = vif_values) %>%
  arrange(desc(VIF))

vif_plot <- ggplot(vif_df, aes(x = reorder(Variable, VIF), y = VIF, fill = VIF > 5)) +
  geom_col(width = 0.7, color = "black") +
  geom_hline(yintercept = 5, linetype = "dashed", color = "#E74C3C", linewidth = 1) +
  scale_fill_manual(values = c("FALSE" = "#0072B2", "TRUE" = "#E74C3C"), guide = "none") +
  geom_text(aes(label = round(VIF, 2)), hjust = -0.2, size = 3.5, family = "Times New Roman") +
  labs(title = "VIF of All Features Entering Elastic Net",
       x = "", y = "VIF Value") +
  coord_flip() +
  theme_sci_large()
ggsave(file.path(output_dir, "VIF_Plot_All_Features.svg"), vif_plot, width = 10, height = 10)

# 4. Elastic net cross-validation and variable selection (lambda.1se) ---
set.seed(seed_cv)
cv_fit <- cv.glmnet(x, y, family = "binomial", alpha = alpha_enet, nfolds = 10,
                    lambda = exp(seq(-15, -1, length = 100)))
lambda_1se <- cv_fit$lambda.1se
cat(sprintf("\nlambda.1se = %.6f\n", lambda_1se))

coef_matrix <- as.matrix(coef(cv_fit, s = lambda_1se))
selected_vars <- rownames(coef_matrix)[which(coef_matrix != 0)][-1]
cat("Variables selected by lambda.1se:", paste(selected_vars, collapse = ", "), "\n")

# Correlation heatmap of selected variables (with significance)
cor_data <- trainData_raw[, selected_vars, drop = FALSE] %>%
  mutate(across(where(is.ordered), as.numeric))
cor_mat <- cor(cor_data, method = "spearman", use = "pairwise.complete.obs")
p_mat <- cor.mtest(cor_data, method = "spearman")$p  

svg(file.path(output_dir, "Correlation_Heatmap_Selected_Vars_with_p.svg"),
    width = 8, height = 6)
corrplot(cor_mat,
         method = "square",
         type = "upper",
         order = "hclust",
         tl.col = "black",
         tl.srt = 45,
         tl.cex = 1.2,
         number.cex = 0.9,
         addCoef.col = "black",
         p.mat = p_mat,
         sig.level = 0.05,
         insig = "blank",
         pch.cex = 1.5,
         col = colorRampPalette(c("#0072B2", "white", "#E74C3C"))(200),
         title = "Correlation Heatmap of Selected Features",
         mar = c(0, 0, 2, 0))
dev.off()

save(selected_vars, cv_fit, lambda_1se, file = "output/temp_selected_vars.RData")
cat("02_variable_selection.R completed\n")
