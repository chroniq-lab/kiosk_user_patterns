library(dplyr)
library(survey)

# 1. Load NHANES data from CSV
file_path <- "~/Emory/Varghese, Jithin Sam - Pursuant User Profiles/working/processed/kupdat05_nhanes data.csv"
nhanes <- read.csv(file_path)

# 2. Analytic sample: 2021–2023, adults, not pregnant, BMI/ht/wt available
nhanes_analytic <- nhanes %>%
  filter(
    year == "20212023",
    age >= 18,
    is.na(pregnant) | pregnant != 1, # exclude pregnant women, keep all men and non-pregnant
    !is.na(bmi),
    !is.na(height),
    !is.na(weight)
  ) %>%
  mutate(
    sex = if_else(female == 1, "Women", "Men"),
    age_cat = cut(age, c(17, 44, 64, Inf), labels = c("18 to 44", "45 to 64", "65 plus")),
    race_cat = case_when(
      race == "Hispanic" ~ "Hispanic",
      race == "NH White" ~ "NH White",
      race == "NH Black" ~ "NH Black",
      TRUE ~ "NH Other"
    )
  )

cat("N (Unweighted):", nrow(nhanes_analytic), "\n\n")

# 3. Survey design object
nhanes_design <- svydesign(
  ids = ~psu, strata = ~pseudostratum, weights = ~mec2yweight,
  data = nhanes_analytic, nest = TRUE
)

# 4. Helper function for printing weighted % (w/ 95% CI)
print_prop <- function(var, label, design, val) {
  est <- svymean(~I(var == val), design, na.rm = TRUE)
  pct <- 100 * coef(est)[2]
  se <- 100 * SE(est)[2]
  cat(sprintf("%s: %.1f (%.1f, %.1f)\n", label, pct, pct - 1.96 * se, pct + 1.96 * se))
}

# Age categories
cat("Age category (%):\n")
for(lvl in levels(nhanes_analytic$age_cat)) {
  sub <- nhanes_analytic %>% filter(!is.na(age_cat))
  d <- svydesign(ids = ~psu, strata = ~pseudostratum, weights = ~mec2yweight, data = sub, nest = TRUE)
  print_prop(sub$age_cat, lvl, d, lvl)
}

# Sex
cat("\nMen (%):\n")
sub <- nhanes_analytic %>% filter(!is.na(sex))
d <- svydesign(ids = ~psu, strata = ~pseudostratum, weights = ~mec2yweight, data = sub, nest = TRUE)
print_prop(sub$sex, "Men", d, "Men")

# Race/Ethnicity
cat("\nRace and Ethnicity (%):\n")
for(lvl in c("Hispanic", "NH Black", "NH White", "NH Other")) {
  sub <- nhanes_analytic %>% filter(!is.na(race_cat))
  d <- svydesign(ids = ~psu, strata = ~pseudostratum, weights = ~mec2yweight, data = sub, nest = TRUE)
  print_prop(sub$race_cat, lvl, d, lvl)
}

# Diabetes
if ("dm_self_reported" %in% names(nhanes_analytic)) {
  cat("\nDiabetes (%):\n")
  sub <- nhanes_analytic %>% filter(!is.na(dm_self_reported))
  d <- svydesign(ids = ~psu, strata = ~pseudostratum, weights = ~mec2yweight, data = sub, nest = TRUE)
  print_prop(sub$dm_self_reported, "Yes", d, 1)
  print_prop(sub$dm_self_reported, "No", d, 0)
}

# Height, Weight, BMI (mean ± SD)
cat("\nHeight (cm):\n")
sub <- nhanes_analytic %>% filter(!is.na(height))
d <- svydesign(ids = ~psu, strata = ~pseudostratum, weights = ~mec2yweight, data = sub, nest = TRUE)
mean_val <- svymean(~height, d)
sd_val <- sqrt(svyvar(~height, d))
cat(sprintf("%.1f (%.1f)\n", coef(mean_val), sd_val))

cat("Weight (kg):\n")
sub <- nhanes_analytic %>% filter(!is.na(weight))
d <- svydesign(ids = ~psu, strata = ~pseudostratum, weights = ~mec2yweight, data = sub, nest = TRUE)
mean_val <- svymean(~weight, d)
sd_val <- sqrt(svyvar(~weight, d))
cat(sprintf("%.1f (%.1f)\n", coef(mean_val), sd_val))

cat("Body Mass Index (kg/m²):\n")
sub <- nhanes_analytic %>% filter(!is.na(bmi))
d <- svydesign(ids = ~psu, strata = ~pseudostratum, weights = ~mec2yweight, data = sub, nest = TRUE)
mean_val <- svymean(~bmi, d)
sd_val <- sqrt(svyvar(~bmi, d))
cat(sprintf("%.1f (%.1f)\n", coef(mean_val), sd_val))

# BMI categories
bmi_tab <- nhanes_analytic %>%
  filter(!is.na(bmi)) %>%
  mutate(bmi_cat = factor(case_when(
    bmi < 18.5 ~ "<18.5",
    bmi >= 18.5 & bmi < 25 ~ "18.5-24.9",
    bmi >= 25 & bmi < 30 ~ "25.0-29.9",
    bmi >= 30 ~ "≥30"
  ), levels = c("<18.5", "18.5-24.9", "25.0-29.9", "≥30")))
d <- svydesign(ids = ~psu, strata = ~pseudostratum, weights = ~mec2yweight, data = bmi_tab, nest = TRUE)
cat("\nBMI Category (%):\n")
for(lvl in levels(bmi_tab$bmi_cat)) {
  print_prop(bmi_tab$bmi_cat, lvl, d, lvl)
}