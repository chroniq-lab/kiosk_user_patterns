library(dplyr)
library(survey)

# STEP 1: Read analytic dataset
file_path <- "/Users/carolinechizak/Library/CloudStorage/OneDrive-SharedLibraries-Emory/Varghese, Jithin Sam - Pursuant User Profiles/working/processed/kupdat05_nhanes data.csv"
nhanes <- read.csv(file_path)

# STEP 2: Filter to only NHANES 2021–2023
nhanes <- nhanes %>% filter(year == "20212023")

# STEP 3: Exclude pregnant, recode variables, create grouping vars
nhanes <- nhanes %>%
  mutate(
    sex = if_else(female == 1, "Women", "Men"),
    age_cat = cut(age, breaks = c(17, 44, 64, Inf), labels = c("18-44", "45-64", "65 and above")),
    race_cat = case_when(
      race == "Hispanic" ~ "Hispanic",
      race == "NH White" ~ "Non-Hispanic White",
      race == "NH Black" ~ "Non-Hispanic Black",
      race == "NH Asian" ~ "Non-Hispanic Asian",
      TRUE ~ "Non-Hispanic Other"
    ),
    overweight = factor(if_else(bmi >= 25 & bmi < 30, 1, 0), levels = c(0, 1)),
    obesity    = factor(if_else(bmi >= 30, 1, 0), levels = c(0, 1))
  ) %>%
  filter(
    age >= 18,
    is.na(pregnant) | pregnant != 1,
    !is.na(female), !is.na(race), !is.na(height), !is.na(weight), !is.na(bmi)
  )

# STEP 4: Create survey design object
nhanes_design <- svydesign(
  ids = ~psu,
  strata = ~pseudostratum,
  weights = ~mec2yweight,
  data = nhanes,
  nest = TRUE
)

# STEP 5: Prevalence helper
get_prev <- function(var, design, subset = NULL) {
  if (!is.null(subset)) {
    design <- subset(design, subset)
  }
  fml <- as.formula(paste0("~", var))
  est <- svymean(fml, design, na.rm = TRUE)
  pct <- 100 * coef(est)[2] # Prevalence of '1'
  se <- 100 * SE(est)[2]
  ci <- c(pct - 1.96 * se, pct + 1.96 * se)
  if (is.na(pct)) return("NA (NA, NA)")
  sprintf("%.1f (%.1f, %.1f)", pct, ci[1], ci[2])
}

# ---------- TOTAL ----------
cat("Total\n")
cat("Unweighted N:", nrow(nhanes), "\n")
cat("Overweight (%):", get_prev("overweight", nhanes_design), "\n")
cat("Obesity (%):", get_prev("obesity", nhanes_design), "\n\n")

# ---------- BY SEX ----------
for (sx in c("Women", "Men")) {
  cat(sx, "\n")
  cat("Unweighted N:", sum(nhanes$sex == sx, na.rm = TRUE), "\n")
  cat("Overweight (%):", get_prev("overweight", nhanes_design, nhanes$sex == sx), "\n")
  cat("Obesity (%):", get_prev("obesity", nhanes_design, nhanes$sex == sx), "\n\n")
}

# ---------- BY AGE ----------
for (ag in levels(nhanes$age_cat)) {
  cat(ag, "\n")
  cat("Unweighted N:", sum(nhanes$age_cat == ag, na.rm = TRUE), "\n")
  cat("Overweight (%):", get_prev("overweight", nhanes_design, nhanes$age_cat == ag), "\n")
  cat("Obesity (%):", get_prev("obesity", nhanes_design, nhanes$age_cat == ag), "\n\n")
}

# ---------- BY RACE/ETHNICITY ----------
race_order <- c("Hispanic", "Non-Hispanic White", "Non-Hispanic Black", "Non-Hispanic Asian", "Non-Hispanic Other")
for (rc in race_order) {
  cat(rc, "\n")
  cat("Unweighted N:", sum(nhanes$race_cat == rc, na.rm = TRUE), "\n")
  cat("Overweight (%):", get_prev("overweight", nhanes_design, nhanes$race_cat == rc), "\n")
  cat("Obesity (%):", get_prev("obesity", nhanes_design, nhanes$race_cat == rc), "\n\n")
}