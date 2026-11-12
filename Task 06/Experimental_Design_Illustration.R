# =========================================================
# Task 6 - Critical Evaluation of Experimental Design
# =========================================================

setwd("C:/Users/desha/Desktop/Y3S1/IT3081/Group Assignment")
getwd()

library(tidyverse)

set.seed(42)
df <- readRDS("C:/Users/desha/Desktop/Y3S1/IT3081/Group Assignment/Dataset/fbc_anemia_analysis.rds")
out <- "C:/Users/desha/Desktop/Y3S1/IT3081/Group Assignment/Task 06/Outputs"


pool <- df %>% filter(anemia == "Yes") %>%
  select(case_id, institution, sex, age_group, age_at_test)

cat("Eligible (anaemic) patient pool for illustrative trial allocation:", nrow(pool), "\n\n")

# =========================================================
# A. COMPLETELY RANDOMIZED DESIGN (CRD) - pure random allocation
# =========================================================
cat("=========================================================\n")
cat("A. CRD: Pure random 1:1 allocation, no blocking\n")
cat("=========================================================\n")

crd <- pool %>%
  mutate(treatment_CRD = sample(rep(c("Intervention","Control"), length.out = n())))

cat("\nOverall allocation balance:\n")
print(table(crd$treatment_CRD))

cat("\n-- Resulting institution balance under CRD (left to chance) --\n")
print(table(crd$institution, crd$treatment_CRD))
cat("\nInstitution proportions per arm (ideally should match each other):\n")
print(round(prop.table(table(crd$institution, crd$treatment_CRD), margin = 2), 3))

# =========================================================
# B. RANDOMIZED COMPLETE BLOCK DESIGN (RCBD) - blocked by institution
# =========================================================
cat("\n=========================================================\n")
cat("B. RCBD: Randomization performed WITHIN each institution block\n")
cat("=========================================================\n")

rcbd <- pool %>%
  group_by(institution) %>%
  mutate(treatment_RCBD = sample(rep(c("Intervention","Control"), length.out = n()))) %>%
  ungroup()

cat("\nOverall allocation balance:\n")
print(table(rcbd$treatment_RCBD))

cat("\n-- Resulting institution balance under RCBD (guaranteed by design) --\n")
print(table(rcbd$institution, rcbd$treatment_RCBD))
cat("\nInstitution proportions per arm (should now match closely by construction):\n")
print(round(prop.table(table(rcbd$institution, rcbd$treatment_RCBD), margin = 2), 3))

# =========================================================
# C. COMPARISON: does blocking actually improve balance here?
# =========================================================
cat("\n=========================================================\n")
cat("C. Quantifying the balance improvement: CRD vs RCBD\n")
cat("=========================================================\n")

imbalance_metric <- function(tab) {
  # Max absolute difference in institution proportion between the two arms
  props <- prop.table(tab, margin = 2)
  max(abs(props[, 1] - props[, 2]))
}

crd_tab  <- table(crd$institution, crd$treatment_CRD)
rcbd_tab <- table(rcbd$institution, rcbd$treatment_RCBD)

cat(sprintf("\nMax institution-proportion imbalance between arms:\n  CRD:  %.4f\n  RCBD: %.4f\n",
            imbalance_metric(crd_tab), imbalance_metric(rcbd_tab)))
cat("(RCBD imbalance should be at or near zero by construction; CRD imbalance is left to chance\n")
cat(" and will vary each time this script is re-run with a different seed.)\n")

# Save illustrative allocation tables
write_csv(as.data.frame.matrix(crd_tab), file.path(out, "task6_CRD_allocation_illustration.csv"))
write_csv(as.data.frame.matrix(rcbd_tab), file.path(out, "task6_RCBD_allocation_illustration.csv"))

cat("\nScript complete. This is a design illustration only - no actual intervention was tested.\n")
