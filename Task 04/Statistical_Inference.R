# =========================================================
# Task 4 - Statistical Inference
# =========================================================

setwd("C:/Users/desha/Desktop/Y3S1/IT3081/Group Assignment")
getwd()

library(tidyverse)
library(car)     # for Levene's test
library(broom)   # for tidy() output

df <- readRDS("C:/Users/desha/Desktop/Y3S1/IT3081/Group Assignment/Dataset/fbc_anemia_analysis.rds")
out <- "C:/Users/desha/Desktop/Y3S1/IT3081/Group Assignment/Task 04/Outputs"

sink(file.path(out, "task4_inference_console_log.txt"), split = TRUE)

cat("=========================================================\n")
cat("TASK 4: STATISTICAL INFERENCE - full console output\n")
cat("=========================================================\n\n")

# =========================================================
# A. COMPARISON OF MEANS: HB by Sex
# =========================================================
cat("\n---------------------------------------------------------\n")
cat("TEST A: Comparison of Means - HB by Sex (Welch two-sample t-test)\n")
cat("---------------------------------------------------------\n")
cat("H0: mean HB (Female) = mean HB (Male)\n")
cat("H1: mean HB (Female) != mean HB (Male)\n\n")

df %>% group_by(sex) %>%
  summarise(n=n(), mean_HB=mean(HB), sd_HB=sd(HB)) %>% print()

test_A <- t.test(HB ~ sex, data = df, var.equal = FALSE)
print(test_A)

# =========================================================
# B. COMPARISON OF MEANS: HB, young women (18-39) vs elderly men (80+)
# =========================================================
cat("\n---------------------------------------------------------\n")
cat("TEST B: Comparison of Means - HB: Women 18-39 vs Men 80+\n")
cat("---------------------------------------------------------\n")
cat("H0: mean HB (Female 18-39) = mean HB (Male 80+)\n")
cat("H1: mean HB (Female 18-39) != mean HB (Male 80+)\n\n")

sub_B <- df %>% filter(
  (age_group == "18-39 (Young adult)" & sex == "Female") |
    (age_group == "80+ (Elderly)" & sex == "Male")
) %>% mutate(grp = paste(age_group, sex))

sub_B %>% group_by(grp) %>% summarise(n=n(), mean_HB=mean(HB), sd_HB=sd(HB)) %>% print()

test_B <- t.test(HB ~ grp, data = sub_B, var.equal = FALSE)
print(test_B)

# =========================================================
# C. COMPARISON OF VARIANCES: HB variance across institutions
# =========================================================
cat("\n---------------------------------------------------------\n")
cat("TEST C: Comparison of Variances - HB across institutions\n")
cat("---------------------------------------------------------\n")
cat("H0: variance of HB is equal across all 4 institutions\n")
cat("H1: variance of HB differs across at least one institution\n\n")

df %>% group_by(institution) %>%
  summarise(n=n(), var_HB=var(HB), sd_HB=sd(HB)) %>% print()

cat("\n-- Levene's test (robust to non-normality) --\n")
test_C_levene <- leveneTest(HB ~ institution, data = df)
print(test_C_levene)

cat("\n-- Bartlett's test (classical, assumes normality) --\n")
test_C_bartlett <- bartlett.test(HB ~ institution, data = df)
print(test_C_bartlett)

# =========================================================
# D. COMPARISON OF PROPORTIONS: Anaemia prevalence, Female vs Male
# =========================================================
cat("\n---------------------------------------------------------\n")
cat("TEST D: Comparison of Proportions - Anaemia: Female vs Male\n")
cat("---------------------------------------------------------\n")
cat("H0: proportion anaemic (Female) = proportion anaemic (Male)\n")
cat("H1: proportion anaemic (Female) != proportion anaemic (Male)\n\n")

tab_D <- table(df$sex, df$anemia)
print(tab_D)
prop_D <- df %>% group_by(sex) %>% summarise(n=n(), n_anemic=sum(anemia=="Yes"), prop=n_anemic/n)
print(prop_D)

test_D <- prop.test(x = c(prop_D$n_anemic[prop_D$sex=="Female"], prop_D$n_anemic[prop_D$sex=="Male"]),
                    n = c(prop_D$n[prop_D$sex=="Female"], prop_D$n[prop_D$sex=="Male"]))
print(test_D)

# =========================================================
# E. COMPARISON OF PROPORTIONS: Anaemia prevalence across institutions
# =========================================================
cat("\n---------------------------------------------------------\n")
cat("TEST E: Comparison of Proportions - Anaemia across institutions (Chi-square)\n")
cat("---------------------------------------------------------\n")
cat("H0: anaemia status is independent of institution\n")
cat("H1: anaemia status is NOT independent of institution (association exists)\n\n")

tab_E <- table(df$institution, df$anemia)
print(tab_E)
print(prop.table(tab_E, margin = 1))

test_E <- chisq.test(tab_E)
print(test_E)
cat("\nExpected counts:\n")
print(test_E$expected)
cat("\nStandardized residuals (which cells drive the association):\n")
print(round(test_E$stdres, 2))

# Effect size: Cramer's V
n_total <- sum(tab_E)
cramers_v <- sqrt(test_E$statistic / (n_total * (min(dim(tab_E)) - 1)))
cat("\nCramer's V (effect size):", round(cramers_v, 4), "\n")

# =========================================================
# F. ONE-WAY ANOVA: HB across age groups
# =========================================================
cat("\n---------------------------------------------------------\n")
cat("TEST F: One-way ANOVA - HB across age groups\n")
cat("---------------------------------------------------------\n")
cat("H0: mean HB is equal across all age groups\n")
cat("H1: at least one age group's mean HB differs\n\n")

df %>% group_by(age_group) %>%
  summarise(n=n(), mean_HB=mean(HB), sd_HB=sd(HB)) %>% print(n=Inf)

aov_F <- aov(HB ~ age_group, data = df)
print(summary(aov_F))

cat("\n-- Effect size (eta squared) --\n")
ss <- summary(aov_F)[[1]]
eta_sq <- ss[1,"Sum Sq"] / sum(ss[,"Sum Sq"])
cat("Eta-squared:", round(eta_sq, 4), "\n")

cat("\n-- Post-hoc: Tukey HSD (pairwise comparisons) --\n")
tukey_F <- TukeyHSD(aov_F)
print(tukey_F)

cat("\n-- ANOVA assumption check: residual normality (Shapiro on sample) --\n")
set.seed(123)
resid_sample <- sample(residuals(aov_F), 5000)  # Shapiro max n=5000
print(shapiro.test(resid_sample))


sink()
cat("Task 4 script complete. Full console log saved to task4_inference_console_log.txt\n")
