# =========================================================
# Task 3 - Visualizations
# =========================================================

setwd("C:/Users/desha/Desktop/Y3S1/IT3081/Group Assignment")
getwd()

library(tidyverse)

df <- readRDS("C:/Users/desha/Desktop/Y3S1/IT3081/Group Assignment/Dataset/fbc_anemia_analysis.rds")
out_plots <- "C:/Users/desha/Desktop/Y3S1/IT3081/Group Assignment/Task 03/Outputs/Plots"
dir.create(out_plots, showWarnings = FALSE, recursive = TRUE)

pal <- c("No" = "#2E86AB", "Yes" = "#D64550")
theme_set(theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(color="gray40", size=10),
        panel.grid.minor = element_blank(),
        legend.position = "bottom"))

# ---- 1. HB distribution overall, colored by anemia status ----
p1 <- ggplot(df, aes(x = HB, fill = anemia)) +
  geom_histogram(binwidth = 0.5, alpha = 0.85, position = "identity", color="white", linewidth=0.1) +
  scale_fill_manual(values = pal, name = "Anaemic") +
  labs(title = "Distribution of Haemoglobin (HB) levels",
       subtitle = "Roughly unimodal, slightly right-skewed; dashed lines mark WHO thresholds (11/12/13 g/dL)",
       x = "Haemoglobin (g/dL)", y = "Number of cases") +
  geom_vline(xintercept = c(11,12,13), linetype="dashed", color="gray50", linewidth=0.3)
ggsave(file.path(out_plots, "02_hb_distribution.png"), p1, width=8, height=5, dpi=150)

# ---- 2. Anemia prevalence by sex ----
sex_summary <- df %>% group_by(sex) %>% summarise(rate = mean(anemia=="Yes")*100, n=n())
p2 <- ggplot(sex_summary, aes(x=sex, y=rate, fill=sex)) +
  geom_col(width=0.55, show.legend = FALSE) +
  geom_text(aes(label=paste0(round(rate,1),"%\n(n=",n,")")), vjust=-0.3, size=3.8) +
  scale_fill_manual(values=c("Female"="#D64550","Male"="#2E86AB")) +
  ylim(0, max(sex_summary$rate)*1.25) +
  labs(title="Anaemia prevalence by sex", x=NULL, y="Anaemia prevalence (%)")
ggsave(file.path(out_plots, "03_anemia_by_sex.png"), p2, width=6, height=5, dpi=150)

# ---- 3. Anemia prevalence by age group x sex ----
age_sex_summary <- df %>%
  group_by(age_group, sex) %>%
  summarise(rate = mean(anemia=="Yes")*100, n=n(), .groups="drop")

p3 <- ggplot(age_sex_summary, aes(x=age_group, y=rate, color=sex, group=sex)) +
  geom_line(linewidth=1.1) +
  geom_point(size=2.8) +
  scale_color_manual(values=c("Female"="#D64550","Male"="#2E86AB")) +
  labs(title="Anaemia prevalence by age group and sex",
       subtitle="Large sex gap in reproductive years narrows and reverses in older age",
       x=NULL, y="Anaemia prevalence (%)", color=NULL) +
  theme(axis.text.x = element_text(angle=30, hjust=1))
ggsave(file.path(out_plots, "04_anemia_age_sex_interaction.png"), p3, width=9, height=5.5, dpi=150)

# ---- 4. Anemia prevalence by institution ----
inst_summary <- df %>% group_by(institution) %>% summarise(rate=mean(anemia=="Yes")*100, n=n())
p4 <- ggplot(inst_summary, aes(x=reorder(institution, -rate), y=rate, fill=institution)) +
  geom_col(show.legend = FALSE, width=0.6) +
  geom_text(aes(label=paste0(round(rate,1),"%\n(n=",n,")")), vjust=-0.3, size=3.5) +
  scale_fill_brewer(palette = "Blues", direction = -1) +
  ylim(0, max(inst_summary$rate)*1.25) +
  labs(title="Anaemia prevalence by institution", x=NULL, y="Anaemia prevalence (%)")
ggsave(file.path(out_plots, "05_anemia_by_institution.png"), p4, width=7, height=5, dpi=150)

# ---- 5. Anemia severity distribution ----
sev_summary <- df %>% count(anemia_severity) %>% mutate(pct = 100*n/sum(n))
p5 <- ggplot(sev_summary, aes(x=anemia_severity, y=n, fill=anemia_severity)) +
  geom_col(show.legend=FALSE, width=0.6) +
  geom_text(aes(label=paste0(n," (",round(pct,1),"%)")), vjust=-0.3, size=3.8) +
  scale_fill_manual(values=c("Normal"="#8FBF9F","Mild"="#F2C572","Moderate"="#E67E22","Severe"="#C0392B")) +
  labs(title="Anaemia severity distribution", subtitle="WHO-classified severity across all cases",
       x=NULL, y="Number of cases") +
  scale_y_continuous(expand = expansion(mult=c(0,0.15)))
ggsave(file.path(out_plots, "06_severity_distribution.png"), p5, width=7, height=5, dpi=150)

# ---- 6. Sample size by institution ----
p6 <- ggplot(df %>% count(institution), aes(x=reorder(institution,-n), y=n, fill=institution)) +
  geom_col(show.legend=FALSE, width=0.6) +
  geom_text(aes(label=scales::comma(n)), vjust=-0.3, size=3.8) +
  scale_fill_brewer(palette="Blues", direction=-1) +
  scale_y_continuous(labels = scales::comma, expand=expansion(mult=c(0,0.15))) +
  labs(title="Sample size by institution", subtitle="Highlights institutional imbalance in the dataset",
       x=NULL, y="Number of cases")
ggsave(file.path(out_plots, "07_institution_sample_sizes.png"), p6, width=7, height=5, dpi=150)

# ---- 7. Correlation heatmap of FBC parameters ----
num_vars <- df %>% select(HB, MCH, MCHC, MCV, PCV, RBC)
cor_mat <- cor(num_vars)
cor_df <- as.data.frame(as.table(cor_mat))
names(cor_df) <- c("Var1","Var2","Correlation")

p7 <- ggplot(cor_df, aes(Var1, Var2, fill=Correlation)) +
  geom_tile(color="white") +
  geom_text(aes(label=round(Correlation,2)), size=3.5) +
  scale_fill_gradient2(low="#2E86AB", mid="white", high="#D64550", midpoint=0, limits=c(-1,1)) +
  labs(title="Correlation between FBC red-cell parameters", x=NULL, y=NULL) +
  theme(axis.text.x = element_text(angle=45, hjust=1))
ggsave(file.path(out_plots, "08_correlation_heatmap.png"), p7, width=6.5, height=5.5, dpi=150)

# ---- 8. Age distribution ----
p8 <- ggplot(df, aes(x=age_at_test)) +
  geom_histogram(binwidth=5, fill="#2E86AB", alpha=0.8, color="white", linewidth=0.1) +
  labs(title="Age distribution of patients", x="Age at test (years)", y="Number of cases")
ggsave(file.path(out_plots, "09_age_distribution.png"), p8, width=8, height=5, dpi=150)

cat("All plots saved to:", out_plots, "\n")
list.files(out_plots)
