library(survey)
library(dplyr)

# STEP 1: Filter out rows with missing values in key variables
nhanes_filtered <- `kupdat05_nhanes data` %>%
  filter(
    !is.na(age),
    !is.na(female),
    !is.na(race),
    !is.na(dm_self_reported),
    !is.na(height),
    !is.na(weight),
    !is.na(bmi)
  )

# STEP 2: Create survey design object
nhanes_design <- svydesign(
  ids = ~psu,
  strata = ~pseudostratum,
  weights = ~mec2yweight,
  data = nhanes_filtered,
  nest = TRUE
)

# STEP 3: N (Unweighted)
n_unweighted <- nrow(nhanes_filtered)
cat("N (Unweighted):", n_unweighted, "\n\n")

# Helper function for categorical summary with 95% CI
format_pct_ci <- function(x) {
  data <- as.data.frame(x)
  data$category <- rownames(data)
  data$mean_pct <- 100 * data$mean
  data$ci_l <- 100 * (data$mean - 1.96 * data$SE)
  data$ci_u <- 100 * (data$mean + 1.96 * data$SE)
  data <- data[, c("category", "mean_pct", "ci_l", "ci_u", "SE")]
  rownames(data) <- NULL
  data
}

# -------- AGE GROUPS --------
age_stats <- svymean(~factor(age), nhanes_design, na.rm = TRUE)
age_df <- format_pct_ci(age_stats)
# Extract numeric age
age_df$age_num <- as.numeric(gsub("factor\\(age\\)", "", age_df$category))
age_df$age_group <- cut(
  age_df$age_num,
  breaks = c(17, 44, 64, Inf),
  labels = c("18 to 44", "45 to 64", "65 plus"),
  right = TRUE
)
# Group and sum for Table 1 age categories
age_grouped <- age_df %>%
  filter(!is.na(age_group)) %>%
  group_by(age_group) %>%
  summarise(
    mean_pct = sum(mean_pct),
    pooled_se = sqrt(sum((SE*100)^2)),
    ci_l = mean_pct - 1.96 * pooled_se,
    ci_u = mean_pct + 1.96 * pooled_se
  ) %>%
  mutate(
    mean_pct = round(mean_pct, 1),
    ci_l = round(ci_l, 1),
    ci_u = round(ci_u, 1)
  )

cat("\nAge category (%):\n")
for(i in 1:nrow(age_grouped)) {
  cat(sprintf("%-10s: %.1f (%.1f, %.1f)\n", age_grouped$age_group[i], age_grouped$mean_pct[i], age_grouped$ci_l[i], age_grouped$ci_u[i]))
}

# -------- MEN (%) --------
men_stats <- svymean(~I(female == 0), nhanes_design, na.rm = TRUE)
men_df <- format_pct_ci(men_stats)
cat("\nMen (%%):\n")
cat(sprintf("Men: %.1f (%.1f, %.1f)\n", men_df$mean_pct[2], men_df$ci_l[2], men_df$ci_u[2])) # TRUE = men

# -------- RACE AND ETHNICITY --------
race_stats <- svymean(~factor(race), nhanes_design, na.rm = TRUE)
race_df <- format_pct_ci(race_stats)
# Clean up labels for table
race_df$category <- gsub("factor\\(race\\)", "", race_df$category)
cat("\nRace and Ethnicity (%%):\n")
for (race_cat in c("Hispanic", "NH Black", "NH White", "NH Other")) {
  row <- race_df[race_df$category == race_cat, ]
  cat(sprintf("%-10s: %.1f (%.1f, %.1f)\n", race_cat, row$mean_pct, row$ci_l, row$ci_u))
}

# -------- DIABETES (%) --------
dm_stats <- svymean(~factor(dm_self_reported), nhanes_design, na.rm = TRUE)
dm_df <- format_pct_ci(dm_stats)
dm_df$category <- ifelse(dm_df$category == "factor(dm_self_reported)1", "Yes", "No")
cat("\nDiabetes (%%):\n")
for(i in 1:nrow(dm_df)) {
  cat(sprintf("%-3s: %.1f (%.1f, %.1f)\n", dm_df$category[i], dm_df$mean_pct[i], dm_df$ci_l[i], dm_df$ci_u[i]))
}

# -------- HEIGHT, WEIGHT, BMI --------
height_stats <- svymean(~height, nhanes_design, na.rm = TRUE)
weight_stats <- svymean(~weight, nhanes_design, na.rm = TRUE)
bmi_stats <- svymean(~bmi, nhanes_design, na.rm = TRUE)
height_sd <- sqrt(svyvar(~height, nhanes_design, na.rm = TRUE))
weight_sd <- sqrt(svyvar(~weight, nhanes_design, na.rm = TRUE))
bmi_sd <- sqrt(svyvar(~bmi, nhanes_design, na.rm = TRUE))
cat("\nHeight (cm):\n")
cat(sprintf("%.1f (%.1f)\n", coef(height_stats), height_sd))
cat("Weight (kg):\n")
cat(sprintf("%.1f (%.1f)\n", coef(weight_stats), weight_sd))
cat("Body Mass Index (kg/m²):\n")
cat(sprintf("%.1f (%.1f)\n", coef(bmi_stats), bmi_sd))

# -------- BMI CATEGORIES --------
nhanes_filtered <- nhanes_filtered %>%
  mutate(bmi_cat = case_when(
    bmi < 18.5 ~ "<18.5",
    bmi >= 18.5 & bmi < 25 ~ "18.5–24.9",
    bmi >= 25 & bmi < 30 ~ "25.0–29.9",
    bmi >= 30 ~ "≥30"
  ))
nhanes_design <- update(nhanes_design, bmi_cat = nhanes_filtered$bmi_cat)
bmi_cat_stats <- svymean(~factor(bmi_cat), nhanes_design, na.rm = TRUE)
bmi_cat_df <- format_pct_ci(bmi_cat_stats)
bmi_cat_df$category <- gsub("factor\\(bmi_cat\\)", "", bmi_cat_df$category)
cat("\nBMI Category (%%):\n")
for (cat_label in c("<18.5", "18.5–24.9", "25.0–29.9", "≥30")) {
  row <- bmi_cat_df[bmi_cat_df$category == cat_label, ]
  cat(sprintf("%-10s: %.1f (%.1f, %.1f)\n", cat_label, row$mean_pct, row$ci_l, row$ci_u))
}