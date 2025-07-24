library(arrow)
library(dplyr)
library(survey)

# STEP 1: Load BRFSS 2022 Parquet Data
parquet_path <- "/Users/carolinechizak/Library/CloudStorage/OneDrive-SharedLibraries-Emory/Varghese, Jithin Sam - BRFSS/data/year=2022/"
brfss2022 <- open_dataset(parquet_path, format = "parquet") %>% collect()

# STEP 2: Clean and Recode Variables
brfss_clean <- brfss2022 %>%
  mutate(
    female = case_when(
      `_SEX` == 2 ~ 1,
      `_SEX` == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    race = case_when(
      `_RACEPR1` == 1 ~ "NH White",
      `_RACEPR1` == 2 ~ "NH Black",
      `_RACEPR1` == 7 ~ "Hispanic",
      `_RACEPR1` %in% c(3, 4, 5, 6, 8, 9) ~ "NH Other"
    ),
    age_group = case_when(
      `_AGEG5YR` %in% 1:5 ~ "18 to 44",
      `_AGEG5YR` %in% 6:9 ~ "45 to 64",
      `_AGEG5YR` %in% 10:13 ~ "65 plus"
    ),
    bmi = `_BMI5` / 100,
    height = HTM4,   # already in cm
    weight = WTKG3,  # already in kg
    diabetes = case_when(
      DIABETE4 == 1 ~ "Yes",
      DIABETE4 %in% c(2, 3) ~ "No"
    ),
    rural = as.numeric(`_METSTAT` == 1) # 1 = rural, 0 = not rural (per BRFSS codebook)
  ) %>%
  filter(
    !is.na(female), !is.na(age_group), !is.na(race),
    !is.na(height), !is.na(weight), !is.na(bmi), !is.na(diabetes), !is.na(rural)
  )

# STEP 3: Create Survey Design Object
options(survey.lonely.psu = "adjust")
brfss_design <- svydesign(
  ids = ~`_PSU`,
  strata = ~`_STSTR`,
  weights = ~`_LLCPWT`,
  data = brfss_clean,
  nest = TRUE
)

# Helper function for summary output
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

# STEP 4: N (Unweighted)
n_unweighted <- nrow(brfss_clean)
cat("N (Unweighted):", n_unweighted, "\n\n")

# STEP 5: Age Category (%)
age_stats <- svymean(~factor(age_group), brfss_design, na.rm = TRUE)
age_df <- format_pct_ci(age_stats)
age_df$category <- gsub("factor\\(age_group\\)", "", age_df$category)
cat("\nAge category (%):\n")
for(i in 1:nrow(age_df)) {
  cat(sprintf("%-10s: %.1f (%.1f, %.1f)\n", age_df$category[i], age_df$mean_pct[i], age_df$ci_l[i], age_df$ci_u[i]))
}

# STEP 6: Men (%)
men_stats <- svymean(~I(female == 0), brfss_design, na.rm = TRUE)
men_df <- format_pct_ci(men_stats)
cat("\nMen (%%):\n")
cat(sprintf("Men: %.1f (%.1f, %.1f)\n", men_df$mean_pct[2], men_df$ci_l[2], men_df$ci_u[2])) # TRUE = men

# STEP 7: Race and Ethnicity (%)
race_stats <- svymean(~factor(race), brfss_design, na.rm = TRUE)
race_df <- format_pct_ci(race_stats)
race_df$category <- gsub("factor\\(race\\)", "", race_df$category)
cat("\nRace and Ethnicity (%%):\n")
for (race_cat in c("Hispanic", "NH Black", "NH White", "NH Other")) {
  row <- race_df[race_df$category == race_cat, ]
  cat(sprintf("%-10s: %.1f (%.1f, %.1f)\n", race_cat, row$mean_pct, row$ci_l, row$ci_u))
}

# STEP 8: Rural Residence (%)
rural_stats <- svymean(~rural, brfss_design, na.rm = TRUE)
rural_pct <- 100 * coef(rural_stats)[1]      # [1] is percent rural, [2] is non-rural
rural_se <- 100 * SE(rural_stats)[1]
rural_ci_l <- rural_pct - 1.96 * rural_se
rural_ci_u <- rural_pct + 1.96 * rural_se
cat(sprintf("\nRural residence (%%): %.1f (%.1f, %.1f)\n", rural_pct, rural_ci_l, rural_ci_u))

# STEP 9: Diabetes (%)
dm_stats <- svymean(~factor(diabetes), brfss_design, na.rm = TRUE)
dm_df <- format_pct_ci(dm_stats)
dm_df$category <- gsub("factor\\(diabetes\\)", "", dm_df$category)
cat("\nDiabetes (%%):\n")
for(i in 1:nrow(dm_df)) {
  cat(sprintf("%-3s: %.1f (%.1f, %.1f)\n", dm_df$category[i], dm_df$mean_pct[i], dm_df$ci_l[i], dm_df$ci_u[i]))
}

# STEP 10: Height, Weight, BMI (mean ± SD)
height_stats <- svymean(~height, brfss_design, na.rm = TRUE)
weight_stats <- svymean(~weight, brfss_design, na.rm = TRUE)
bmi_stats <- svymean(~bmi, brfss_design, na.rm = TRUE)
height_sd <- sqrt(svyvar(~height, brfss_design, na.rm = TRUE))
weight_sd <- sqrt(svyvar(~weight, brfss_design, na.rm = TRUE))
bmi_sd <- sqrt(svyvar(~bmi, brfss_design, na.rm = TRUE))
cat("\nHeight (cm):\n")
cat(sprintf("%.1f (%.1f)\n", coef(height_stats), height_sd))
cat("Weight (kg):\n")
cat(sprintf("%.1f (%.1f)\n", coef(weight_stats), weight_sd))
cat("Body Mass Index (kg/m²):\n")
cat(sprintf("%.1f (%.1f)\n", coef(bmi_stats), bmi_sd))

# STEP 11: BMI Categories (%)
brfss_clean <- brfss_clean %>%
  mutate(bmi_cat = case_when(
    bmi < 18.5 ~ "<18.5",
    bmi >= 18.5 & bmi < 25 ~ "18.5–24.9",
    bmi >= 25 & bmi < 30 ~ "25.0–29.9",
    bmi >= 30 ~ "≥30"
  ))
brfss_design <- update(brfss_design, bmi_cat = brfss_clean$bmi_cat)
bmi_cat_stats <- svymean(~factor(bmi_cat), brfss_design, na.rm = TRUE)
bmi_cat_df <- format_pct_ci(bmi_cat_stats)
bmi_cat_df$category <- gsub("factor\\(bmi_cat\\)", "", bmi_cat_df$category)
cat("\nBMI Category (%%):\n")
for (cat_label in c("<18.5", "18.5–24.9", "25.0–29.9", "≥30")) {
  row <- bmi_cat_df[bmi_cat_df$category == cat_label, ]
  cat(sprintf("%-10s: %.1f (%.1f, %.1f)\n", cat_label, row$mean_pct, row$ci_l, row$ci_u))
}