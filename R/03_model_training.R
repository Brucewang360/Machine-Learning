load("output/temp_split.RData")
load("output/temp_selected_vars.RData")

# 5. Build ML dataset (only selected variables; ordinal -> numeric) ---
data_ml <- data %>%
  select(Diagnose, all_of(selected_vars)) %>%
  mutate(across(where(is.ordered), as.numeric))
trainData_ml <- data_ml[trainIndex, ]
testData_ml  <- data_ml[-trainIndex, ]
numeric_vars_ml <- selected_vars

# 6. ML preprocessing (median imputation, centering/scaling) -----------
impute_median <- function(x) {
  if (is.numeric(x)) {
    x[is.na(x)] <- median(x, na.rm = TRUE)
  } else if (is.factor(x)) {
    x[is.na(x)] <- names(sort(table(x), decreasing = TRUE))[1]
  }
  x
}
trainData_ml <- trainData_ml %>% mutate(across(everything(), impute_median))
testData_ml  <- testData_ml  %>% mutate(across(everything(), impute_median))

preprocess_params <- preProcess(trainData_ml[, numeric_vars_ml, drop = FALSE],
                                method = c("center", "scale"))
trainData_ml[, numeric_vars_ml] <- predict(preprocess_params, trainData_ml[, numeric_vars_ml])
testData_ml[, numeric_vars_ml]  <- predict(preprocess_params, testData_ml[, numeric_vars_ml])

# Address class imbalance (SMOTE upsampling, only training set)
set.seed(seed_smote)
my_recipe <- recipe(Diagnose ~ ., data = trainData_ml) %>%
  step_smote(Diagnose, over_ratio = 1)
trainData_balanced <- prep(my_recipe) %>% bake(new_data = NULL)
cat("Training set distribution after SMOTE:\n"); print(table(trainData_balanced$Diagnose))

# 7. Train all models -------------------------------------------------
train_labels <- as.numeric(trainData_balanced$Diagnose) - 1

# Logistic regression
lr_model <- glm(Diagnose ~ ., data = trainData_balanced, family = binomial)
pred_lr_train <- predict(lr_model, trainData_balanced, type = "response")

# Naive Bayes
nb_model <- naiveBayes(Diagnose ~ ., data = trainData_balanced)
pred_nb_train <- predict(nb_model, trainData_balanced, type = "raw")[, 2]

# Random Forest
rf_model <- randomForest(Diagnose ~ ., data = trainData_balanced, ntree = 500)
pred_rf_train <- predict(rf_model, trainData_balanced, type = "prob")[, 2]

# SVM
svm_model <- svm(Diagnose ~ ., data = trainData_balanced,
                 type = "C-classification", kernel = "radial", probability = TRUE)
pred_svm_train <- attr(predict(svm_model, trainData_balanced, probability = TRUE),
                       "probabilities")[, 2]

# XGBoost
xgb_train_mat <- model.matrix(~ . - Diagnose - 1, data = trainData_balanced)
dtrain <- xgb.DMatrix(data = xgb_train_mat, label = train_labels)
xgb_model <- xgboost(data = dtrain,
                     params = list(objective = "binary:logistic",
                                   eval_metric = "logloss",
                                   scale_pos_weight = sum(train_labels == 0) / sum(train_labels == 1),
                                   max_depth = 3, eta = 0.1),
                     nrounds = 100, verbose = 0)
pred_xgb_train <- predict(xgb_model, dtrain)

save(lr_model, nb_model, rf_model, svm_model, xgb_model,
     pred_lr_train, pred_nb_train, pred_rf_train, pred_svm_train, pred_xgb_train,
     train_labels, trainData_balanced, testData_ml, trainData_ml,
     file = "output/temp_models.RData")
cat("03_model_training.R completed\n")
