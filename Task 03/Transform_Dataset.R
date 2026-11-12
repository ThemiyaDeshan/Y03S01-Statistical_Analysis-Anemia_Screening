setwd("C:/Users/desha/Desktop/Y3S1/IT3081/Group Assignment")
getwd()

library(tidyverse)

# ---- 1. Load data ----
raw <- read_csv("C:/Users/desha/Desktop/Y3S1/IT3081/Group Assignment/fbc_anemia_clean.csv")

cat("Initial dimensions:", nrow(raw), "rows x", ncol(raw), "cols\n")

# ---- 2. Basic structure check ----
glimpse(raw)

# ---- 3. Missingness check ----
missing_summary <- raw %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_missing")
print(missing_summary)

# ---- 4. Data quality issue: implausible ages ----
implausible_age <- raw %>% filter(age_at_test > 110)
cat("\nRows with implausible age (>110):", nrow(implausible_age), "\n")
print(implausible_age %>% select(case_id, institution, sex, age_at_test, birth_year, bill_date, HB, anemia))

# Decision: DROP these rows
# Rationale: age >110 is not clinically plausible; imputing/capping would fabricate data.
# Dropping Only 12/27046 rows (0.044%) affected - negligible power loss, does not bias remaining sample.

df <- raw %>% filter(age_at_test <= 110)

cat("\nRows after dropping implausible ages:", nrow(df), "\n")
cat("Rows removed:", nrow(raw) - nrow(df), "\n")

# ---- 5. Duplicate check ----
cat("\nDuplicate case_id count:", sum(duplicated(df$case_id)), "\n")

# ---- 6. Range sanity check on hematology parameters ----
range_check <- df %>%
  summarise(across(c(HB, MCH, MCHC, MCV, PCV, RBC),
                    list(min = ~min(.), max = ~max(.), mean = ~mean(.), median = ~median(.)),
                    .names = "{.col}_{.fn}"))
print(t(range_check))

# ---- 7. Re-code types ----
df <- df %>%
  mutate(
    institution = factor(institution),
    sex = factor(sex, levels = c("F", "M"), labels = c("Female", "Male")),
    anemia = factor(anemia, levels = c("N", "Y"), labels = c("No", "Yes")),
    anemia_severity = factor(anemia_severity,
                              levels = c("Normal", "Mild", "Moderate", "Severe"),
                              ordered = TRUE),
    bill_date = as.Date(bill_date, format = "%m/%d/%Y"),
    age_group = case_when(
      age_at_test < 5   ~ "0-4 (Child, U5)",
      age_at_test < 12  ~ "5-11 (Child)",
      age_at_test < 15  ~ "12-14 (Adolescent)",
      age_at_test < 18  ~ "15-17 (Adolescent)",
      age_at_test < 40  ~ "18-39 (Young adult)",
      age_at_test < 60  ~ "40-59 (Middle-aged)",
      age_at_test < 80  ~ "60-79 (Older adult)",
      TRUE ~ "80+ (Elderly)"
    ),
    age_group = factor(age_group, levels = c(
      "0-4 (Child, U5)", "5-11 (Child)", "12-14 (Adolescent)", "15-17 (Adolescent)",
      "18-39 (Young adult)", "40-59 (Middle-aged)", "60-79 (Older adult)", "80+ (Elderly)"
    ))
  )

cat("\nbill_date parse failures:", sum(is.na(df$bill_date)), "\n")

# ---- 8. Save analysis-ready dataset ----
saveRDS(df, "C:/Users/desha/Desktop/Y3S1/IT3081/Group Assignment/Dataset/fbc_anemia_analysis.rds")
write_csv(df, "C:/Users/desha/Desktop/Y3S1/IT3081/Group Assignment/Dataset/fbc_anemia_analysis_ready.csv")

cat("\n=== Final analysis-ready dataset ===\n")
cat("Rows:", nrow(df), " Cols:", ncol(df), "\n")
glimpse(df)
