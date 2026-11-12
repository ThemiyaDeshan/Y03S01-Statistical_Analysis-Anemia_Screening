# =========================================================
# Task 7 - Critical Evaluation of Principal Component Analysis
# =========================================================

setwd("C:/Users/desha/Desktop/Y3S1/IT3081/Group Assignment")
getwd()

library(tidyverse)
library(broom)

set.seed(42)
df <- readRDS("C:/Users/desha/Desktop/Y3S1/IT3081/Group Assignment/Dataset/fbc_anemia_analysis.rds")
out <- "C:/Users/desha/Desktop/Y3S1/IT3081/Group Assignment/Task 07/Outputs"
out_plots <- file.path(out, "plots")

sink(file.path(out, "task7_pca_console_log.txt"), split = TRUE)
cat("=========================================================\n")
cat("TASK 7: PCA CRITICAL EVALUATION - console output\n")
cat("=========================================================\n\n")

# =========================================================
# 1. PREPARE DATA: the 6 FBC parameters, standardized
# =========================================================
fbc_vars <- c("HB", "MCH", "MCHC", "MCV", "PCV", "RBC")
X <- df %>% select(all_of(fbc_vars))

cat("Variables entering PCA:", paste(fbc_vars, collapse=", "), "\n")
cat("n =", nrow(X), "\n\n")

cat("-- Correlation matrix (reference: same as Task 3 finding) --\n")
print(round(cor(X), 3))

# =========================================================
# 2. RUN PCA (standardized / correlation-based, since variables
#    are on very different scales - g/dL, pg, %, fL, x10^12/L)
# =========================================================
pca <- prcomp(X, scale. = TRUE, center = TRUE)

cat("\n-- PCA summary: variance explained --\n")
print(summary(pca))

cat("\n-- Eigenvalues (variance of each PC) --\n")
eigenvalues <- pca$sdev^2
names(eigenvalues) <- paste0("PC", 1:length(eigenvalues))
print(round(eigenvalues, 3))
cat("\n(Kaiser rule: retain components with eigenvalue > 1)\n")
cat("Components with eigenvalue > 1:", sum(eigenvalues > 1), "\n")

cat("\n-- Loadings (how original variables combine into each PC) --\n")
print(round(pca$rotation, 3))

# Variance explained table
var_explained <- tibble(
  PC = paste0("PC", 1:length(eigenvalues)),
  Eigenvalue = eigenvalues,
  Prop_Variance = eigenvalues / sum(eigenvalues),
  Cumulative_Variance = cumsum(eigenvalues / sum(eigenvalues))
)
print(var_explained)
write_csv(var_explained, file.path(out, "task7_pca_variance_explained.csv"))

# =========================================================
# 3. INTERPRET PC1 and PC2 (what do they represent clinically?)
# =========================================================
cat("\n-- PC1 loadings sorted by magnitude --\n")
print(sort(abs(pca$rotation[,1]), decreasing = TRUE))
cat("\n-- PC2 loadings sorted by magnitude --\n")
print(sort(abs(pca$rotation[,2]), decreasing = TRUE))

# =========================================================
# 4. COMPARISON: Logistic regression using ORIGINAL variables
#    vs. using PRINCIPAL COMPONENTS (Principal Component Regression)
#    Same train/test split logic as Task 5, for fair comparison
# =========================================================
cat("\n---------------------------------------------------------\n")
cat("COMPARISON: Original-variable logistic regression vs PCA-based\n")
cat("(Principal Component Regression / Logistic PCR)\n")
cat("---------------------------------------------------------\n")

set.seed(42)
n <- nrow(df)
train_idx <- sample(seq_len(n), size = floor(0.75 * n))

df_pca <- df %>%
  bind_cols(as_tibble(pca$x))  # attach PC scores to the dataframe

train <- df_pca[train_idx, ]
test  <- df_pca[-train_idx, ]

# Model A: original variables (mirrors Task 5 Model 2, demo+labs, no interaction for fair comparison)
m_orig <- glm(anemia ~ age_at_test + sex + MCH + MCHC + MCV + PCV + RBC,
              data = train, family = binomial)

# Model B: first 2 PCs only (explains most variance - see output above)
m_pc2 <- glm(anemia ~ age_at_test + sex + PC1 + PC2,
             data = train, family = binomial)

# Model C: all 6 PCs (same information as original vars, just rotated/uncorrelated)
m_pc_all <- glm(anemia ~ age_at_test + sex + PC1 + PC2 + PC3 + PC4 + PC5 + PC6,
                 data = train, family = binomial)

cat("\n-- AIC comparison --\n")
cat(sprintf("Original variables (7 predictors): AIC = %.1f\n", AIC(m_orig)))
cat(sprintf("First 2 PCs only (4 predictors):    AIC = %.1f\n", AIC(m_pc2)))
cat(sprintf("All 6 PCs (7 predictors, rotated):  AIC = %.1f\n", AIC(m_pc_all)))

evaluate <- function(model, newdata, label) {
  pred_prob <- predict(model, newdata = newdata, type = "response")
  pred_class <- factor(ifelse(pred_prob > 0.5, "Yes", "No"), levels = c("No","Yes"))
  actual <- factor(newdata$anemia, levels = c("No","Yes"))
  cm <- table(Predicted = pred_class, Actual = actual)
  acc <- sum(diag(cm)) / sum(cm)
  sens <- cm["Yes","Yes"] / sum(cm[,"Yes"])
  spec <- cm["No","No"] / sum(cm[,"No"])
  pos <- pred_prob[actual=="Yes"]; neg <- pred_prob[actual=="No"]
  set.seed(1); auc <- mean(sample(pos,2000,replace=TRUE) > sample(neg,2000,replace=TRUE))
  cat(sprintf("[%s] Accuracy=%.4f Sensitivity=%.4f Specificity=%.4f AUC(approx)=%.4f\n",
              label, acc, sens, spec, auc))
  invisible(list(acc=acc, sens=sens, spec=spec, auc=auc))
}

cat("\n-- Test-set performance --\n")
res_orig  <- evaluate(m_orig, test, "Original variables")
res_pc2   <- evaluate(m_pc2, test, "First 2 PCs only")
res_pcall <- evaluate(m_pc_all, test, "All 6 PCs")

# Check multicollinearity is actually resolved among PCs (should be ~0 by construction)
cat("\n-- Correlation among PC1-PC6 (should be ~0, confirming PCA removes multicollinearity) --\n")
print(round(cor(df_pca %>% select(PC1:PC6)), 3))

# =========================================================
# 5. VISUALIZATIONS
# =========================================================
dir.create(out_plots, showWarnings = FALSE, recursive = TRUE)
theme_set(theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face="bold", size=13),
        plot.subtitle = element_text(color="gray40", size=10)))

# Scree plot
scree_df <- var_explained %>% mutate(PC_num = 1:n())
p_scree <- ggplot(scree_df, aes(x = PC_num, y = Prop_Variance)) +
  geom_col(fill = "#2E86AB", alpha = 0.8) +
  geom_line(aes(y = Cumulative_Variance), color = "#D64550", linewidth = 1) +
  geom_point(aes(y = Cumulative_Variance), color = "#D64550", size = 2.5) +
  geom_hline(yintercept = 0.9, linetype = "dashed", color = "gray40") +
  scale_x_continuous(breaks = 1:6) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "PCA Scree Plot: FBC red-cell parameters",
       subtitle = "Bars = individual variance explained; red line = cumulative (dashed = 90% threshold)",
       x = "Principal Component", y = "Proportion of variance explained")
ggsave(file.path(out_plots, "16_pca_scree_plot.png"), p_scree, width=8, height=5.5, dpi=150)

# Biplot-style: PC1 vs PC2 scores colored by anemia status (sample for readability)
set.seed(1)
sample_idx <- sample(seq_len(nrow(df_pca)), 3000)
biplot_df <- df_pca[sample_idx, ]

p_biplot <- ggplot(biplot_df, aes(x = PC1, y = PC2, color = anemia)) +
  geom_point(alpha = 0.4, size = 1) +
  scale_color_manual(values = c("No"="#2E86AB","Yes"="#D64550")) +
  labs(title = "PC1 vs PC2 scores, colored by anaemia status",
       subtitle = "3,000-point sample for readability",
       x = paste0("PC1 (", round(var_explained$Prop_Variance[1]*100,1), "% variance)"),
       y = paste0("PC2 (", round(var_explained$Prop_Variance[2]*100,1), "% variance)"),
       color = "Anaemic")
ggsave(file.path(out_plots, "17_pca_pc1_pc2_scatter.png"), p_biplot, width=8, height=6, dpi=150)

# Loadings plot
loadings_df <- as_tibble(pca$rotation[,1:2], rownames = "variable")
p_load <- ggplot(loadings_df, aes(x = PC1, y = PC2, label = variable)) +
  geom_segment(aes(x=0, y=0, xend=PC1, yend=PC2), arrow = arrow(length=unit(0.25,"cm")), color="#2E86AB") +
  geom_text(vjust = -0.5, size = 4, fontface = "bold") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray70") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray70") +
  labs(title = "PCA Loadings: how each variable contributes to PC1 and PC2",
       x = "PC1 loading", y = "PC2 loading")
ggsave(file.path(out_plots, "18_pca_loadings.png"), p_load, width=7, height=6.5, dpi=150)

sink()
cat("Task 7 script complete. Plots saved to:", out_plots, "\n")
