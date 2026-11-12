# =========================================================
# Task 5 - Predictive Statistical Modelling
# =========================================================

setwd("C:/Users/desha/Desktop/Y3S1/IT3081/Group Assignment")
getwd()

library(tidyverse)
library(glmnet)
library(broom)
library(car)

set.seed(42)

df <- readRDS("C:/Users/desha/Desktop/Y3S1/IT3081/Group Assignment/Dataset/fbc_anemia_analysis.rds")
out <- "C:/Users/desha/Desktop/Y3S1/IT3081/Group Assignment/Task 05/Outputs"
out_plots <- file.path(out, "plots")

sink(file.path(out, "task5_modelling_console_log.txt"), split = TRUE)
cat("=========================================================\n")
cat("TASK 5: PREDICTIVE STATISTICAL MODELLING - console output\n")
cat("=========================================================\n\n")

# =========================================================
# 1. TRAIN / TEST SPLIT
# =========================================================
n <- nrow(df)
train_idx <- sample(seq_len(n), size = floor(0.75 * n))
train <- df[train_idx, ]
test  <- df[-train_idx, ]

cat("Train n:", nrow(train), " Test n:", nrow(test), "\n")
cat("Train anaemia rate:", round(mean(train$anemia=="Yes"),4),
    " Test anaemia rate:", round(mean(test$anemia=="Yes"),4), "\n\n")

# =========================================================
# 2. MODEL 1: Logistic regression - demographic predictors only
# =========================================================
cat("\n---------------------------------------------------------\n")
cat("MODEL 1: Logistic Regression (GLM, binomial) - Demographic only\n")
cat("Predictors: age_at_test, sex\n")
cat("---------------------------------------------------------\n")

m1 <- glm(anemia ~ age_at_test + sex, data = train, family = binomial(link = "logit"))
print(summary(m1))
cat("\nOdds ratios:\n")
print(exp(coef(m1)))
cat("\n95% CI (odds ratio scale):\n")
print(exp(confint(m1)))

# =========================================================
# 3. MODEL 2: Logistic regression - demographic + lab indices
#    (excludes HB - see design note above)
# =========================================================
cat("\n---------------------------------------------------------\n")
cat("MODEL 2: Logistic Regression (GLM, binomial) - Demographic + Labs\n")
cat("Predictors: age_at_test, sex, MCH, MCHC, MCV, PCV, RBC\n")
cat("(HB excluded - deterministic of the label; institution excluded - Task 1/4 justification)\n")
cat("---------------------------------------------------------\n")

m2 <- glm(anemia ~ age_at_test + sex + MCH + MCHC + MCV + PCV + RBC,
          data = train, family = binomial(link = "logit"))
print(summary(m2))
cat("\nOdds ratios:\n")
print(exp(coef(m2)))

cat("\n-- Multicollinearity check: Variance Inflation Factors (VIF) --\n")
print(vif(m2))

# =========================================================
# 4. MODEL 3: Logistic regression with age x sex interaction
# =========================================================
cat("\n---------------------------------------------------------\n")
cat("MODEL 3: Logistic Regression - Demographic + Labs + age x sex interaction\n")
cat("Justified by Task 4 Test G (significant two-way ANOVA interaction)\n")
cat("---------------------------------------------------------\n")

m3 <- glm(anemia ~ age_at_test * sex + MCH + MCHC + MCV + PCV + RBC,
          data = train, family = binomial(link = "logit"))
print(summary(m3))

cat("\n-- Likelihood ratio test: Model 2 vs Model 3 (is interaction justified?) --\n")
print(anova(m2, m3, test = "Chisq"))

cat("\n-- Model comparison: AIC/BIC --\n")
cat(sprintf("Model 1 (demo only):        AIC=%.1f  BIC=%.1f\n", AIC(m1), BIC(m1)))
cat(sprintf("Model 2 (demo+labs):        AIC=%.1f  BIC=%.1f\n", AIC(m2), BIC(m2)))
cat(sprintf("Model 3 (demo+labs+inter):  AIC=%.1f  BIC=%.1f\n", AIC(m3), BIC(m3)))

# =========================================================
# 5. PERFORMANCE EVALUATION on held-out TEST set (Models 1-3)
# =========================================================
cat("\n---------------------------------------------------------\n")
cat("TEST-SET PERFORMANCE: Models 1-3\n")
cat("---------------------------------------------------------\n")

evaluate_model <- function(model, newdata, actual, threshold = 0.5, label = "") {
  pred_prob <- predict(model, newdata = newdata, type = "response")
  pred_class <- factor(ifelse(pred_prob > threshold, "Yes", "No"), levels = c("No","Yes"))
  actual <- factor(actual, levels = c("No","Yes"))

  cm <- table(Predicted = pred_class, Actual = actual)
  acc <- sum(diag(cm)) / sum(cm)
  sens <- cm["Yes","Yes"] / sum(cm[,"Yes"])       # recall / sensitivity
  spec <- cm["No","No"] / sum(cm[,"No"])
  prec <- cm["Yes","Yes"] / sum(cm["Yes",])

  # AUC (simple rank-based / Mann-Whitney calculation, no extra package needed)
  pos <- pred_prob[actual == "Yes"]
  neg <- pred_prob[actual == "No"]
  auc <- mean(sample(pos, 2000, replace=TRUE) > sample(neg, 2000, replace=TRUE))

  cat(sprintf("\n[%s]\n", label))
  print(cm)
  cat(sprintf("Accuracy=%.4f  Sensitivity(Recall)=%.4f  Specificity=%.4f  Precision=%.4f  AUC(approx)=%.4f\n",
              acc, sens, spec, prec, auc))
  invisible(list(acc=acc, sens=sens, spec=spec, prec=prec, auc=auc, pred_prob=pred_prob))
}

set.seed(1)
res1 <- evaluate_model(m1, test, test$anemia, label = "Model 1: Demographic only")
res2 <- evaluate_model(m2, test, test$anemia, label = "Model 2: Demographic + Labs")
res3 <- evaluate_model(m3, test, test$anemia, label = "Model 3: Demographic + Labs + Interaction")

# =========================================================
# 6. REGULARIZED MODELS: Ridge, LASSO, Elastic Net (glmnet)
# =========================================================
cat("\n---------------------------------------------------------\n")
cat("REGULARIZED MODELS: Ridge / LASSO / Elastic Net\n")
cat("Same predictor set as Model 2, standardized internally by glmnet\n")
cat("---------------------------------------------------------\n")

predictor_formula <- ~ age_at_test + sex + MCH + MCHC + MCV + PCV + RBC - 1
x_train <- model.matrix(predictor_formula, data = train)
x_test  <- model.matrix(predictor_formula, data = test)
y_train <- ifelse(train$anemia == "Yes", 1, 0)
y_test  <- ifelse(test$anemia == "Yes", 1, 0)

# Cross-validated lambda selection for each alpha
cv_ridge <- cv.glmnet(x_train, y_train, family = "binomial", alpha = 0, nfolds = 10)
cv_lasso <- cv.glmnet(x_train, y_train, family = "binomial", alpha = 1, nfolds = 10)
cv_enet  <- cv.glmnet(x_train, y_train, family = "binomial", alpha = 0.5, nfolds = 10)

cat("\nRidge: lambda.min =", cv_ridge$lambda.min, " lambda.1se =", cv_ridge$lambda.1se, "\n")
cat("LASSO: lambda.min =", cv_lasso$lambda.min, " lambda.1se =", cv_lasso$lambda.1se, "\n")
cat("Elastic Net (alpha=0.5): lambda.min =", cv_enet$lambda.min, " lambda.1se =", cv_enet$lambda.1se, "\n")

cat("\n-- Ridge coefficients (lambda.min) --\n")
print(coef(cv_ridge, s = "lambda.min"))

cat("\n-- LASSO coefficients (lambda.min) -- (note which shrink to exactly zero)\n")
print(coef(cv_lasso, s = "lambda.min"))

cat("\n-- Elastic Net coefficients (lambda.min) --\n")
print(coef(cv_enet, s = "lambda.min"))

evaluate_glmnet <- function(cvfit, x, y_actual_factor, label) {
  pred_prob <- as.numeric(predict(cvfit, newx = x, s = "lambda.min", type = "response"))
  pred_class <- factor(ifelse(pred_prob > 0.5, "Yes", "No"), levels = c("No","Yes"))
  actual <- factor(y_actual_factor, levels = c("No","Yes"))
  cm <- table(Predicted = pred_class, Actual = actual)
  acc <- sum(diag(cm)) / sum(cm)
  sens <- cm["Yes","Yes"] / sum(cm[,"Yes"])
  spec <- cm["No","No"] / sum(cm[,"No"])
  prec <- cm["Yes","Yes"] / sum(cm["Yes",])
  pos <- pred_prob[actual == "Yes"]; neg <- pred_prob[actual == "No"]
  set.seed(1); auc <- mean(sample(pos, 2000, replace=TRUE) > sample(neg, 2000, replace=TRUE))
  cat(sprintf("\n[%s]\n", label))
  print(cm)
  cat(sprintf("Accuracy=%.4f  Sensitivity=%.4f  Specificity=%.4f  Precision=%.4f  AUC(approx)=%.4f\n",
              acc, sens, spec, prec, auc))
  invisible(list(acc=acc, sens=sens, spec=spec, prec=prec, auc=auc))
}

res_ridge <- evaluate_glmnet(cv_ridge, x_test, test$anemia, "Ridge Regression (alpha=0)")
res_lasso <- evaluate_glmnet(cv_lasso, x_test, test$anemia, "LASSO Regression (alpha=1)")
res_enet  <- evaluate_glmnet(cv_enet,  x_test, test$anemia, "Elastic Net (alpha=0.5)")

# =========================================================
# 7. FINAL COMPARISON TABLE
# =========================================================
cat("\n---------------------------------------------------------\n")
cat("FINAL MODEL COMPARISON TABLE\n")
cat("---------------------------------------------------------\n")

comparison <- tibble(
  Model = c("1. Logistic (Demographic only)", "2. Logistic (Demo+Labs)",
            "3. Logistic (Demo+Labs+Interaction)", "4. Ridge", "5. LASSO", "6. Elastic Net"),
  Accuracy = c(res1$acc, res2$acc, res3$acc, res_ridge$acc, res_lasso$acc, res_enet$acc),
  Sensitivity = c(res1$sens, res2$sens, res3$sens, res_ridge$sens, res_lasso$sens, res_enet$sens),
  Specificity = c(res1$spec, res2$spec, res3$spec, res_ridge$spec, res_lasso$spec, res_enet$spec),
  Precision = c(res1$prec, res2$prec, res3$prec, res_ridge$prec, res_lasso$prec, res_enet$prec),
  AUC_approx = c(res1$auc, res2$auc, res3$auc, res_ridge$auc, res_lasso$auc, res_enet$auc)
)
print(comparison, width = Inf)
write_csv(comparison, file.path(out, "task5_model_comparison.csv"))

# =========================================================
# 8. VISUALIZATIONS
# =========================================================
dir.create(out_plots, showWarnings = FALSE, recursive = TRUE)
theme_set(theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face="bold", size=13),
        plot.subtitle = element_text(color="gray40", size=10)))

# ROC-style visualization: predicted probability distributions by actual class (Model 3)
roc_df <- tibble(prob = res3$pred_prob, actual = test$anemia)
p_dens <- ggplot(roc_df, aes(x = prob, fill = actual)) +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "gray40") +
  scale_fill_manual(values = c("No"="#2E86AB","Yes"="#D64550"), name = "Actual anaemia") +
  labs(title = "Model 3 predicted probability distribution by true anaemia status",
       subtitle = "Better separation = better discriminative model performance",
       x = "Predicted probability of anaemia", y = "Density")
ggsave(file.path(out_plots, "13_predicted_prob_distribution.png"), p_dens, width=8, height=5, dpi=150)

# Coefficient comparison: logistic vs LASSO (which vars survive shrinkage)
lasso_coef <- as.matrix(coef(cv_lasso, s = "lambda.min"))
lasso_df <- tibble(term = rownames(lasso_coef), lasso = lasso_coef[,1]) %>% filter(term != "(Intercept)")
logistic_df <- tidy(m2) %>% filter(term != "(Intercept)") %>% select(term, estimate)
coef_compare <- full_join(logistic_df, lasso_df, by = "term") %>%
  rename(logistic = estimate) %>%
  pivot_longer(c(logistic, lasso), names_to = "model", values_to = "coefficient")

p_coef <- ggplot(coef_compare, aes(x = reorder(term, coefficient), y = coefficient, fill = model)) +
  geom_col(position = "dodge") +
  coord_flip() +
  scale_fill_manual(values = c("logistic"="#2E86AB","lasso"="#D64550")) +
  labs(title = "Coefficient comparison: Logistic regression vs LASSO",
       subtitle = "LASSO shrinkage may zero out weaker predictors",
       x = NULL, y = "Coefficient (log-odds scale)")
ggsave(file.path(out_plots, "14_coefficient_comparison.png"), p_coef, width=8, height=5.5, dpi=150)

# Model comparison bar chart
comp_long <- comparison %>% pivot_longer(-Model, names_to = "metric", values_to = "value")
p_comp <- ggplot(comp_long, aes(x = Model, y = value, fill = metric)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(title = "Model performance comparison (test set)", x = NULL, y = "Score") +
  theme(legend.position = "bottom")
ggsave(file.path(out_plots, "15_model_comparison_bars.png"), p_comp, width=9, height=6, dpi=150)

sink()
cat("Task 5 script complete. Console log saved to task5_modelling_console_log.txt\n")
cat("Plots saved to:", out_plots, "\n")
