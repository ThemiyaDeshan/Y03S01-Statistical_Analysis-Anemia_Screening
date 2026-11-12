# =========================================================
# Task 9 - Time Series Analysis
#
# NOTE: Task 9 explicitly does NOT require performing time series
# analysis (no trend decomposition, no ARIMA fitting, no forecasting).
# This script is a small EXPLORATORY check of what our own bill_date
# field actually looks like over time, used only to ground the Task 9
# discussion in our dataset's real limitations rather than purely
# hypothetical ones. It stops well short of any time series modelling.
# =========================================================

setwd("C:/Users/desha/Desktop/Y3S1/IT3081/Group Assignment")
getwd()

library(tidyverse)

df <- readRDS("C:/Users/desha/Desktop/Y3S1/IT3081/Group Assignment/Dataset/fbc_anemia_analysis.rds")
out <- "C:/Users/desha/Desktop/Y3S1/IT3081/Group Assignment/Task 09/Outputs"
out_plots <- file.path(out, "plots")
dir.create(out_plots, showWarnings = FALSE, recursive = TRUE)

sink(file.path(out, "task9_time_exploration_console_log.txt"), split = TRUE)
cat("=========================================================\n")
cat("TASK 9: TIME DIMENSION EXPLORATION (not time series analysis)\n")
cat("=========================================================\n\n")

df <- df %>% mutate(year = lubridate::year(bill_date))

cat("Date range:", as.character(min(df$bill_date)), "to", as.character(max(df$bill_date)), "\n")
cat("N with valid bill_date:", sum(!is.na(df$bill_date)), "of", nrow(df), "(100%% complete)\n\n")

cat("-- Case volume by year (pooled across institutions) --\n")
df %>% count(year) %>% print(n = Inf)

cat("\n-- CRITICAL FINDING: institution coverage is NOT consistent across years --\n")
coverage <- df %>% count(institution, year) %>%
  pivot_wider(names_from = year, values_from = n, values_fill = 0)
print(coverage, width = Inf)

cat("\nThis table shows each institution contributes data in DIFFERENT,\n")
cat("largely NON-OVERLAPPING year ranges:\n")
cat("  Institution_A: 2023 only\n")
cat("  Institution_B: 2015-2023 (near-continuous, but with volume swings)\n")
cat("  Institution_C: 2014-2015 only\n")
cat("  Institution_D: 2018, 2020-2023 (missing 2014-2017, 2019)\n")
cat("This means any pooled year-over-year trend would be confounded with\n")
cat("institutions entering/leaving the sample, NOT a genuine underlying\n")
cat("time trend in anaemia prevalence. This is the central data-quality\n")
cat("limitation discussed in Task 9.\n\n")

cat("-- Annual anaemia prevalence, POOLED across institutions (naive, misleading) --\n")
df %>% group_by(year) %>%
  summarise(n = n(), anemia_rate = round(100*mean(anemia=="Yes"), 1)) %>%
  print(n = Inf)

cat("\n-- Annual anaemia prevalence, Institution_B ONLY (the one institution --\n")
cat("-- with near-continuous coverage, used as an illustrative comparison) --\n")
df %>% filter(institution == "Institution_B") %>%
  group_by(year) %>%
  summarise(n = n(), anemia_rate = round(100*mean(anemia=="Yes"), 1)) %>%
  print(n = Inf)

# =========================================================
# VISUALIZATION: what naive pooled trend looks like vs. reality
# =========================================================
theme_set(theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face="bold", size=13),
        plot.subtitle = element_text(color="gray40", size=10),
        legend.position = "bottom"))

yearly_by_inst <- df %>%
  count(institution, year) %>%
  complete(institution, year = full_seq(year, 1), fill = list(n = 0))

p_coverage <- ggplot(yearly_by_inst, aes(x = year, y = institution, fill = n)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = ifelse(n == 0, "", n)), size = 3, color = "white") +
  scale_fill_gradient(low = "#D6E4F0", high = "#2E86AB", na.value = "gray95") +
  labs(title = "Case volume by institution and year: coverage is highly uneven",
       subtitle = "Each institution contributes data in different, mostly non-overlapping periods",
       x = NULL, y = NULL, fill = "Cases") +
  scale_x_continuous(breaks = 2014:2023)
ggsave(file.path(out_plots, "21_time_coverage_by_institution.png"), p_coverage, width=9, height=4, dpi=150)

# Naive pooled trend (misleading) vs Institution_B only (more honest)
pooled_trend <- df %>% group_by(year) %>%
  summarise(anemia_rate = mean(anemia=="Yes")*100, n = n(), series = "All institutions pooled (confounded)")
instB_trend <- df %>% filter(institution == "Institution_B") %>% group_by(year) %>%
  summarise(anemia_rate = mean(anemia=="Yes")*100, n = n(), series = "Institution_B only (near-continuous)")
trend_compare <- bind_rows(pooled_trend, instB_trend)

p_trend <- ggplot(trend_compare, aes(x = year, y = anemia_rate, color = series)) +
  geom_line(linewidth = 1) +
  geom_point(aes(size = n), alpha = 0.7) +
  scale_color_manual(values = c("All institutions pooled (confounded)"="#D64550",
                                  "Institution_B only (near-continuous)"="#2E86AB")) +
  scale_x_continuous(breaks = 2014:2023) +
  labs(title = "Naive pooled 'trend' vs. single-institution series",
       subtitle = "Point size = number of cases that year. Pooled line's swings partly reflect\nwhich institutions were contributing data, not a genuine population trend.",
       x = NULL, y = "Anaemia prevalence (%)", color = NULL, size = "Cases (n)") +
  theme(legend.box = "vertical")
ggsave(file.path(out_plots, "22_naive_vs_honest_trend.png"), p_trend, width=9, height=6, dpi=150)

sink()
cat("Script complete (exploratory only - no time series model was fitted).\n")
cat("Plots saved to:", out_plots, "\n")
