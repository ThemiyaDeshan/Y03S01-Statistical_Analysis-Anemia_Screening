# =========================================================
# Task 3 - Dataset Understanding and Descriptive Analysis
# =========================================================

setwd("C:/Users/desha/Desktop/Y3S1/IT3081/Group Assignment")
getwd()

library(tidyverse)

df <- readRDS("C:/Users/desha/Desktop/Y3S1/IT3081/Group Assignment/Dataset/fbc_anemia_analysis.rds")
outlier_plots <- "C:/Users/desha/Desktop/Y3S1/IT3081/Group Assignment/Task 03/Outputs/Plots"
dir.create(outlier_plots, showWarnings = FALSE, recursive = TRUE)

theme_set(theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 13),
        panel.grid.minor = element_blank()))

# =========================================================
# 1. DATASET DESCRIPTION
# =========================================================
cat("========== DATASET DESCRIPTION ==========\n")
cat("Rows:", nrow(df), "\n")
cat("Cols:", ncol(df), "\n")
cat("Unique case_id:", n_distinct(df$case_id), "\n")
cat("Date range:", as.character(min(df$bill_date)), "to", as.character(max(df$bill_date)), "\n")
cat("Institutions:", paste(levels(df$institution), collapse=", "), "\n")

# =========================================================
# 2. VARIABLE DESCRIPTIONS (structure)
# =========================================================
var_desc <- tibble(
  variable = names(df),
  type = sapply(df, function(x) class(x)[1]),
  n_unique = sapply(df, function(x) n_distinct(x)),
  example = sapply(df, function(x) as.character(x[1]))
)
print(var_desc, n = Inf)
write_csv(var_desc, "C:/Users/desha/Desktop/Y3S1/IT3081/Group Assignment/Task 03/Outputs/variable_descriptions.csv")

# =========================================================
# 3. MISSING VALUE ANALYSIS
# =========================================================
cat("\n========== MISSING VALUES ==========\n")
missing_tbl <- df %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_missing") %>%
  mutate(pct_missing = round(100 * n_missing / nrow(df), 3))
print(missing_tbl, n = Inf)
write_csv(missing_tbl, "C:/Users/desha/Desktop/Y3S1/IT3081/Group Assignment/Task 03/Outputs/missing_value_summary.csv")

# =========================================================
# 4. DESCRIPTIVE STATISTICS - numeric variables
# =========================================================
cat("\n========== DESCRIPTIVE STATS: numeric FBC parameters ==========\n")
numeric_vars <- c("age_at_test", "HB", "MCH", "MCHC", "MCV", "PCV", "RBC")

desc_stats <- df %>%
  summarise(across(all_of(numeric_vars),
    list(
      n = ~sum(!is.na(.)),
      mean = ~mean(., na.rm=TRUE),
      sd = ~sd(., na.rm=TRUE),
      median = ~median(., na.rm=TRUE),
      min = ~min(., na.rm=TRUE),
      max = ~max(., na.rm=TRUE),
      q1 = ~quantile(., 0.25, na.rm=TRUE),
      q3 = ~quantile(., 0.75, na.rm=TRUE),
      skew = ~ (mean(., na.rm=TRUE) - median(., na.rm=TRUE)) / sd(., na.rm=TRUE)
    ), .names = "{.col}__{.fn}")) %>%
  pivot_longer(everything(), names_to = c("variable", "stat"), names_sep = "__") %>%
  pivot_wider(names_from = stat, values_from = value)

print(desc_stats, n = Inf, width = Inf)
write_csv(desc_stats, "C:/Users/desha/Desktop/Y3S1/IT3081/Group Assignment/Task 03/Outputs/descriptive_statistics_numeric.csv")

# Categorical summaries
cat("\n========== CATEGORICAL SUMMARIES ==========\n")
cat_summary <- function(var) {
  df %>% count(.data[[var]]) %>% mutate(pct = round(100*n/sum(n),1))
}
cat("\n--- Sex ---\n"); print(cat_summary("sex"))
cat("\n--- Institution ---\n"); print(cat_summary("institution"))
cat("\n--- Anemia (Y/N) ---\n"); print(cat_summary("anemia"))
cat("\n--- Anemia severity ---\n"); print(cat_summary("anemia_severity"))
cat("\n--- Age group ---\n"); print(cat_summary("age_group"))

# Cross-tab
cat("\n--- Anemia prevalence by sex ---\n")
df %>% group_by(sex) %>% summarise(n=n(), anemia_rate = round(100*mean(anemia=="Yes"),1)) %>% print()

cat("\n--- Anemia prevalence by institution ---\n")
df %>% group_by(institution) %>% summarise(n=n(), anemia_rate = round(100*mean(anemia=="Yes"),1)) %>% print()

cat("\n--- Anemia prevalence by age group ---\n")
df %>% group_by(age_group) %>% summarise(n=n(), anemia_rate = round(100*mean(anemia=="Yes"),1)) %>% print()

cat("\n--- Anemia prevalence by age group x sex ---\n")
df %>% group_by(age_group, sex) %>% summarise(n=n(), anemia_rate = round(100*mean(anemia=="Yes"),1), .groups="drop") %>% print(n=Inf)

# =========================================================
# 5. OUTLIER DETECTION (IQR method + clinical range flags)
# =========================================================
cat("\n========== OUTLIER DETECTION (IQR x 1.5 method) ==========\n")
outlier_summary <- map_dfr(numeric_vars, function(v) {
  x <- df[[v]]
  q1 <- quantile(x, 0.25, na.rm=TRUE)
  q3 <- quantile(x, 0.75, na.rm=TRUE)
  iqr <- q3 - q1
  lower <- q1 - 1.5*iqr
  upper <- q3 + 1.5*iqr
  n_out <- sum(x < lower | x > upper, na.rm=TRUE)
  tibble(variable = v, lower_fence = round(lower,2), upper_fence = round(upper,2),
         n_outliers = n_out, pct_outliers = round(100*n_out/length(x),2))
})
print(outlier_summary)
write_csv(outlier_summary, "C:/Users/desha/Desktop/Y3S1/IT3081/Group Assignment/Task 03/Outputs/outlier_summary.csv")

# Save boxplots for outlier visualization
df_long <- df %>% select(case_id, all_of(numeric_vars)) %>%
  pivot_longer(-case_id, names_to="parameter", values_to="value") %>%
  filter(parameter != "age_at_test")

p_box <- ggplot(df_long, aes(x=parameter, y=value)) +
  geom_boxplot(fill="#2E86AB", alpha=0.6, outlier.color="#D64550", outlier.alpha=0.4) +
  facet_wrap(~parameter, scales="free", nrow=1) +
  labs(title="Distribution and outliers: FBC red-cell parameters", x=NULL, y=NULL) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
ggsave(file.path(outlier_plots, "01_boxplots_fbc_params.png"), p_box, width=12, height=4.5, dpi=150)

cat("\nScript complete. Plots saved to:", outlier_plots, "\n")
