# =========================================================
# Task 8 - Critical Evaluation of Bayesian Statistical Methods
# =========================================================

library(tidyverse)
library(e1071)      # naiveBayes()
library(arm)        # bayesglm() - weakly informative Cauchy prior (Gelman et al. 2008)
library(broom)

set.seed(42)
df <- readRDS("C:/Users/desha/Desktop/Y3S1/IT3081/Group Assignment/Dataset/fbc_anemia_analysis.rds")
out <- "C:/Users/desha/Desktop/Y3S1/IT3081/Group Assignment/Task 08/Outputs"
out_plots <- file.path(out, "plots")

sink(file.path(out, "task8_bayesian_console_log.txt"), split = TRUE)
cat("=========================================================\n")
cat("TASK 8: BAYESIAN STATISTICAL METHODS - console output\n")
cat("=========================================================\n\n")

# Same train/test split and predictor set as Task 5, for direct comparability
n <- nrow(df)
train_idx <- sample(seq_len(n), size = floor(0.75 * n))
train <- df[train_idx, ]
test  <- df[-train_idx, ]

cat("Train n:", nrow(train), " Test n:", nrow(test), "\n")
cat("Predictors: age_at_test, sex, MCH, MCHC, MCV, PCV, RBC (same as Task 5 Model 2)\n\n")

evaluate <- function(pred_prob, actual, label) {
  pred_class <- factor(ifelse(pred_prob > 0.5, "Yes", "No"), levels = c("No","Yes"))
  actual <- factor(actual, levels = c("No","Yes"))
  cm <- table(Predicted = pred_class, Actual = actual)
  acc <- sum(diag(cm)) / sum(cm)
  sens <- cm["Yes","Yes"] / sum(cm[,"Yes"])
  spec <- cm["No","No"] / sum(cm[,"No"])
  prec <- cm["Yes","Yes"] / sum(cm["Yes",])
  pos <- pred_prob[actual=="Yes"]; neg <- pred_prob[actual=="No"]
  set.seed(1); auc <- mean(sample(pos,2000,replace=TRUE) > sample(neg,2000,replace=TRUE))
  cat(sprintf("\n[%s]\n", label))
  print(cm)
  cat(sprintf("Accuracy=%.4f Sensitivity=%.4f Specificity=%.4f Precision=%.4f AUC(approx)=%.4f\n",
              acc, sens, spec, prec, auc))
  invisible(list(acc=acc, sens=sens, spec=spec, prec=prec, auc=auc))
}

# =========================================================
# A. NAIVE BAYES CLASSIFIER
# =========================================================
cat("\n---------------------------------------------------------\n")
cat("A. NAIVE BAYES CLASSIFIER (e1071::naiveBayes)\n")
cat("---------------------------------------------------------\n")
cat("Assumption being tested: predictors are conditionally independent given class.\n")
cat("We know from Task 3/5 this is VIOLATED (MCH-MCV r=0.88, HB-PCV r=0.96,\n")
cat("MCV in our predictor set has VIF=461). This is a deliberate stress-test\n")
cat("of Naive Bayes under known assumption violation.\n\n")

nb_model <- naiveBayes(anemia ~ age_at_test + sex + MCH + MCHC + MCV + PCV + RBC,
                        data = train)

nb_pred_prob <- predict(nb_model, newdata = test, type = "raw")[, "Yes"]
res_nb <- evaluate(nb_pred_prob, test$anemia, "Naive Bayes")

cat("\n-- Comparison: Logistic regression (Task 5 Model 2) on same split --\n")
glm_model <- glm(anemia ~ age_at_test + sex + MCH + MCHC + MCV + PCV + RBC,
                  data = train, family = binomial)
glm_pred_prob <- predict(glm_model, newdata = test, type = "response")
res_glm <- evaluate(glm_pred_prob, test$anemia, "Logistic Regression (for comparison)")

# =========================================================
# B. BAYESIAN LOGISTIC REGRESSION (arm::bayesglm, Cauchy prior)
# =========================================================
cat("\n---------------------------------------------------------\n")
cat("B. BAYESIAN LOGISTIC REGRESSION (arm::bayesglm)\n")
cat("Weakly informative Cauchy(0, 2.5) prior on coefficients (Gelman et al. 2008)\n")
cat("---------------------------------------------------------\n")
cat("NOTE ON METHOD CHOICE: We first attempted full MCMC sampling via\n")
cat("MCMCpack::MCMClogit. This FAILED - the Metropolis sampler achieved a\n")
cat("0% acceptance rate regardless of proposal tuning, prior tightness, or\n")
cat("subsample size. Diagnosis: our predictor set produces near-perfect\n")
cat("class separation (consistent with Task 5's 92%+ accuracy using these\n")
cat("same variables, and the recurring 'fitted probabilities numerically 0\n")
cat("or 1' warning from glm() itself), which creates an extremely peaked\n")
cat("likelihood surface that a basic random-walk Metropolis sampler cannot\n")
cat("navigate.\n")
cat("We therefore use arm::bayesglm, which implements exactly the published\n")
cat("solution to this problem (Gelman, Jakulin, Pittau & Su, 2008): a\n")
cat("weakly informative Cauchy prior that regularizes coefficients away from\n")
cat("the separation boundary, using a fast approximate (EM-based) posterior\n")
cat("mode estimate rather than full MCMC sampling.\n\n")

bayes_glm <- bayesglm(anemia ~ age_at_test + sex + MCH + MCHC + MCV + PCV + RBC,
                       data = train, family = binomial,
                       prior.scale = 2.5, prior.df = 1)

cat("-- Bayesian GLM summary (posterior mode estimate under Cauchy prior) --\n")
print(summary(bayes_glm))

cat("\n-- Comparison: Bayesian (Cauchy prior) vs. Frequentist MLE (Task 5 Model 2) --\n")
freq_glm <- glm(anemia ~ age_at_test + sex + MCH + MCHC + MCV + PCV + RBC,
                 data = train, family = binomial)
comparison_coef <- tibble(
  term = names(coef(freq_glm)),
  frequentist_MLE = coef(freq_glm),
  frequentist_SE = summary(freq_glm)$coefficients[, "Std. Error"],
  bayesian_posterior_mode = coef(bayes_glm),
  bayesian_SE = summary(bayes_glm)$coefficients[, "Std. Error"]
)
print(comparison_coef)
write_csv(comparison_coef, file.path(out, "task8_bayesian_vs_frequentist_coefs.csv"))

cat("\n-- Predictive performance: Bayesian GLM (Cauchy prior) --\n")
bayes_pred_prob <- predict(bayes_glm, newdata = test, type = "response")
res_bayes_glm <- evaluate(bayes_pred_prob, test$anemia, "Bayesian Logistic Regression (Cauchy prior)")

# =========================================================
# C. BAYESIAN DECISION MAKING: expected-loss threshold selection
# =========================================================
cat("\n---------------------------------------------------------\n")
cat("C. BAYESIAN DECISION MAKING: Expected-loss threshold analysis\n")
cat("---------------------------------------------------------\n")
cat("Standard 0.5 cutoff treats false negatives and false positives as\n")
cat("equally costly. In a clinical screening context this is rarely true:\n")
cat("missing a true anaemia case (false negative) delays treatment, while\n")
cat("a false positive triggers an unnecessary but low-cost follow-up test.\n")
cat("We illustrate Bayesian decision theory: choosing the threshold that\n")
cat("minimizes EXPECTED LOSS given asymmetric costs, using the posterior\n")
cat("predictive probabilities from Model B above.\n\n")

# Illustrative cost ratio: missing a true case (FN) costs 5x more than
# an unnecessary follow-up test (FP) - a defensible clinical assumption,
# stated explicitly as an assumption for the client to calibrate.
cost_FN <- 5
cost_FP <- 1

thresholds <- seq(0.05, 0.95, by = 0.025)
actual_bin <- ifelse(test$anemia == "Yes", 1, 0)

expected_loss <- map_dfr(thresholds, function(t) {
  pred_bin <- ifelse(as.numeric(bayes_pred_prob) > t, 1, 0)
  FN <- sum(pred_bin == 0 & actual_bin == 1)
  FP <- sum(pred_bin == 1 & actual_bin == 0)
  TP <- sum(pred_bin == 1 & actual_bin == 1)
  TN <- sum(pred_bin == 0 & actual_bin == 0)
  total_loss <- FN * cost_FN + FP * cost_FP
  tibble(threshold = t, FN = FN, FP = FP, TP = TP, TN = TN,
         total_loss = total_loss,
         mean_loss = total_loss / length(actual_bin))
})

print(expected_loss, n = Inf)

optimal_t <- expected_loss$threshold[which.min(expected_loss$total_loss)]
default_t_loss <- expected_loss$total_loss[expected_loss$threshold == 0.5]
optimal_t_loss <- min(expected_loss$total_loss)

cat(sprintf("\nDefault threshold (0.5): total loss = %d\n", default_t_loss))
cat(sprintf("Optimal threshold (minimizes expected loss, cost ratio FN:FP=%d:1): %.3f, total loss = %d\n",
            cost_FN, optimal_t, optimal_t_loss))
cat(sprintf("Loss reduction from threshold optimization: %.1f%%\n",
            100 * (default_t_loss - optimal_t_loss) / default_t_loss))

write_csv(expected_loss, file.path(out, "task8_expected_loss_by_threshold.csv"))

# =========================================================
# D. VISUALIZATIONS
# =========================================================
dir.create(out_plots, showWarnings = FALSE, recursive = TRUE)
theme_set(theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face="bold", size=13),
        plot.subtitle = element_text(color="gray40", size=10)))

# Expected loss curve
p_loss <- ggplot(expected_loss, aes(x = threshold, y = total_loss)) +
  geom_line(color = "#2E86AB", linewidth = 1) +
  geom_point(color = "#2E86AB", size = 1.5) +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = optimal_t, linetype = "dashed", color = "#D64550") +
  annotate("text", x = 0.5, y = max(expected_loss$total_loss), label = "Default (0.5)",
           hjust = -0.05, size = 3.3, color = "gray40") +
  annotate("text", x = optimal_t, y = max(expected_loss$total_loss)*0.92, label = "Optimal",
           hjust = -0.1, size = 3.3, color = "#D64550") +
  labs(title = "Bayesian decision making: expected loss by classification threshold",
       subtitle = sprintf("Cost ratio assumed: missing a true case = %dx cost of a false alarm", cost_FN),
       x = "Classification threshold", y = "Total expected loss (test set)")
ggsave(file.path(out_plots, "19_bayesian_decision_expected_loss.png"), p_loss, width=8.5, height=5.5, dpi=150)

# Coefficient comparison: Bayesian posterior mode vs Frequentist MLE, with error bars
coef_plot_df <- comparison_coef %>% filter(term != "(Intercept)") %>%
  transmute(term,
            frequentist_MLE = frequentist_MLE, frequentist_SE,
            bayesian_posterior_mode = bayesian_posterior_mode, bayesian_SE) %>%
  pivot_longer(c(frequentist_MLE, bayesian_posterior_mode), names_to="method", values_to="estimate") %>%
  mutate(se = ifelse(method=="frequentist_MLE", frequentist_SE, bayesian_SE)) %>%
  dplyr::select(term, method, estimate, se)

p_coef <- ggplot(coef_plot_df, aes(x = reorder(term, estimate), y = estimate, fill = method)) +
  geom_col(position = position_dodge(width=0.8), width=0.7) +
  geom_errorbar(aes(ymin=estimate-1.96*se, ymax=estimate+1.96*se),
                position = position_dodge(width=0.8), width=0.25) +
  coord_flip() +
  scale_fill_manual(values = c("frequentist_MLE"="#2E86AB","bayesian_posterior_mode"="#D64550")) +
  labs(title = "Bayesian (Cauchy prior) vs. Frequentist MLE coefficients",
       subtitle = "Error bars = 95% CI/credible interval; Bayesian estimates modestly shrunk toward zero",
       x = NULL, y = "Coefficient (log-odds scale)")
ggsave(file.path(out_plots, "20_bayesian_vs_frequentist_coefs.png"), p_coef, width=8.5, height=5.5, dpi=150)

sink()
cat("Task 8 script complete. Plots saved to:", out_plots, "\n")
