# Appendix C: Data Analysis Code (cleaned and structured)

# =============== Load Data ===============
library(tidyverse)
library(ggplot2)
library(broom)
library(patchwork)
library(ggcorrplot)

master_data <- readRDS("~/Desktop/ResearchProjectData/master_data.rds")

# =============== Select Relevant Variables ===============
selected_vars <- c("IID", "cohort", "dem.dob_age_cop", "dem.sex_cop_numeric", "dem.bmi_signup_cop",
                   "an.total_score", "bn.total_score", "bed.total_score",
                   "phq9.total_score", "gad7.total_score", "SU_sums", "PW_sums")

data <- master_data %>%
  select(all_of(selected_vars))

# =============== Apply Filtering Criteria ===============
data <- data %>%
  filter(dem.dob_age_cop > 17 & dem.dob_age_cop < 80,          # Age range
         dem.sex_cop_numeric %in% c(0,1),                      # Valid sex entries
         dem.bmi_signup_cop > 10 & dem.bmi_signup_cop < 60)    # Valid BMI

# Remove rows missing all key variables
data <- data %>%
  filter(!(is.na(an.total_score) & is.na(bn.total_score) & is.na(bed.total_score) &
             is.na(SU_sums) & is.na(PW_sums) & is.na(phq9.total_score) & is.na(gad7.total_score)))

# Remove negative values
score_vars <- c("an.total_score", "bn.total_score", "bed.total_score", "phq9.total_score",
                "gad7.total_score", "SU_sums", "PW_sums")
data <- data %>% filter(across(all_of(score_vars), ~is.na(.) | . >= 0))

# =============== Descriptives ===============
data$ED_total_score <- rowSums(data[, c("an.total_score", "bn.total_score", "bed.total_score")], na.rm = TRUE)

# Create z-scores
z_vars <- c("ED_total_score", "an.total_score", "bn.total_score", "bed.total_score",
            "phq9.total_score", "gad7.total_score", "SU_sums", "PW_sums",
            "dem.dob_age_cop", "dem.bmi_signup_cop")
data <- data %>% mutate(across(all_of(z_vars), scale, .names = "{.col}_z"))

# =============== Descriptive Summary Stats ===============
total_N <- nrow(data)
summary_stats <- data %>%
  summarise(
    perc_female = mean(dem.sex_cop_numeric == 1, na.rm = TRUE)*100,
    age_mean = mean(dem.dob_age_cop, na.rm = TRUE),
    bmi_mean = mean(dem.bmi_signup_cop, na.rm = TRUE),
    depression = mean(phq9.total_score >= 10, na.rm = TRUE)*100,
    anxiety = mean(gad7.total_score >= 10, na.rm = TRUE)*100,
    anorexia = mean(an.total_score > 0, na.rm = TRUE)*100,
    bulimia = mean(bn.total_score > 0, na.rm = TRUE)*100,
    bed = mean(bed.total_score > 0, na.rm = TRUE)*100,
    any_ed = mean((an.total_score > 0 | bn.total_score > 0 | bed.total_score > 0), na.rm = TRUE)*100,
    suicidality = mean(SU_sums > 0, na.rm = TRUE)*100,
    high_stress = mean(PW_sums > median(PW_sums, na.rm = TRUE), na.rm = TRUE)*100
  )

print(summary_stats)

# =============== Correlations ===============
cor_data <- data %>% select(ED_total_score_z, an.total_score_z, bn.total_score_z, bed.total_score_z, SU_sums_z, PW_sums_z)
cor_matrix <- cor(cor_data, use = "pairwise.complete.obs")

ggcorrplot(cor_matrix, method = "circle", type = "lower", lab = TRUE,
           colors = c("#D0E1F9", "white", "#F8766D"),
           title = "Correlation Matrix of Key Variables")

# =============== Regression Models ===============
model_total <- lm(SU_sums_z ~ ED_total_score_z + dem.dob_age_cop_z + dem.sex_cop_numeric + dem.bmi_signup_cop_z, data = data)
summary(model_total)

# Separate ED Subtypes
model_an <- lm(SU_sums_z ~ an.total_score_z + dem.dob_age_cop_z + dem.sex_cop_numeric + dem.bmi_signup_cop_z, data = data)
model_bn <- lm(SU_sums_z ~ bn.total_score_z + dem.dob_age_cop_z + dem.sex_cop_numeric + dem.bmi_signup_cop_z, data = data)
model_bed <- lm(SU_sums_z ~ bed.total_score_z + dem.dob_age_cop_z + dem.sex_cop_numeric + dem.bmi_signup_cop_z, data = data)

# Interaction models with PW
model_interaction <- lm(SU_sums_z ~ ED_total_score_z * PW_sums_z + phq9.total_score_z + gad7.total_score_z + dem.dob_age_cop_z + dem.sex_cop_numeric + dem.bmi_signup_cop_z, data = data)

# =============== Visualisation of Interactions ===============
data <- data %>% mutate(PW_category = ifelse(PW_sums_z < median(PW_sums_z, na.rm = TRUE), "Low PW", "High PW"))

plot_interaction <- function(x_var, label) {
  ggplot(data, aes(x = .data[[x_var]], y = SU_sums_z, color = PW_category)) +
    geom_smooth(method = "lm", se = FALSE) +
    labs(title = label, x = "ED Symptom Severity (z)", y = "Suicidality (z)") +
    scale_color_manual(values = c("Low PW" = "#E69F00", "High PW" = "#C44E52")) +
    theme_minimal()
}

plot_AN <- plot_interaction("an.total_score_z", "Anorexia Nervosa")
plot_BN <- plot_interaction("bn.total_score_z", "Bulimia Nervosa")
plot_BED <- plot_interaction("bed.total_score_z", "Binge Eating Disorder")

(plot_AN + plot_BN + plot_BED) +
  plot_layout(guides = "collect") +
  plot_annotation(title = "Interaction: COVID Stress x ED Severity on Suicidality")