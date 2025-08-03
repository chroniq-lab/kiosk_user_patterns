library(arrow)
library(dplyr)
library(survey)

# --- 1. Load and Prepare BRFSS Analytic Dataset ---
parquet_path <- "/Users/carolinechizak/Library/CloudStorage/OneDrive-SharedLibraries-Emory/Varghese, Jithin Sam - BRFSS/data/year=2022/"
brfss2022 <- open_dataset(parquet_path, format = "parquet") %>% collect()

brfss_clean <- brfss2022 %>%
  mutate(
    female = case_when(
      `_SEX` == 2 ~ 1,
      `_SEX` == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    sex = if_else(female == 1, "Women", "Men"),
    race_cat = case_when(
      `_RACEPR1` == 1 ~ "Non-Hispanic White",
      `_RACEPR1` == 2 ~ "Non-Hispanic Black",
      `_RACEPR1` == 4 ~ "Non-Hispanic Asian",
      `_RACEPR1` == 7 ~ "Hispanic",
      `_RACEPR1` %in% c(3, 5, 6, 8) ~ "Non-Hispanic Other"
      # 3 = AI/AN, 5 = NH/PI, 6 = Multiracial, 8 = Other
    ),
    age_cat = case_when(
      `_AGEG5YR` %in% 1:5 ~ "18-44",
      `_AGEG5YR` %in% 6:9 ~ "45-64",
      `_AGEG5YR` %in% 10:13 ~ "65 and above"
    ),
    bmi = `_BMI5` / 100,
    height = HTM4,
    weight = WTKG3 / 100,
    overweight = ifelse(bmi >= 25 & bmi < 30, 1, 0),
    obesity = ifelse(bmi >= 30, 1, 0),
    pregnant_num = as.numeric(PREGNANT),
    residence = case_when(
      `_METSTAT` == 1 ~ "Urban",
      `_METSTAT` == 2 ~ "Rural"
    )
  ) %>%
  filter(
    !is.na(female), !is.na(age_cat), !is.na(race_cat), !is.na(residence),
    !is.na(bmi), !is.na(height), !is.na(weight),
    is.na(pregnant_num) | pregnant_num == 0
  )

# --- 2. Survey Design ---
options(survey.lonely.psu = "adjust")
brfss_design <- svydesign(
  ids = ~`_PSU`,
  strata = ~`_STSTR`,
  weights = ~`_LLCPWT`,
  data = brfss_clean,
  nest = TRUE
)

# --- 3. Helper for Prevalence Output ---
get_prev <- function(var, design, subgroup = NULL) {
  if (!is.null(subgroup)) {
    design <- subset(design, subgroup)
  }
  est <- svymean(~get(var), design, na.rm = TRUE)
  pct <- 100 * coef(est)
  se <- 100 * SE(est)
  ci_l <- pct - 1.96 * se
  ci_u <- pct + 1.96 * se
  sprintf("%.1f (%.1f, %.1f)", pct, ci_l, ci_u)
}

# --- 4. Output Table (BRFSS) ---

# ---------- TOTAL ----------
cat("Total\n")
cat("Unweighted N:", nrow(brfss_clean), "\n")
cat("Overweight (%):", get_prev("overweight", brfss_design), "\n")
cat("Obesity (%):", get_prev("obesity", brfss_design), "\n\n")

# ---------- BY SEX ----------
for (sx in c("Women", "Men")) {
  cat(sx, "\n")
  cat("Unweighted N:", sum(brfss_clean$sex == sx, na.rm = TRUE), "\n")
  cat("Overweight (%):", get_prev("overweight", brfss_design, brfss_clean$sex == sx), "\n")
  cat("Obesity (%):", get_prev("obesity", brfss_design, brfss_clean$sex == sx), "\n\n")
}

# ---------- BY AGE ----------
for (ag in c("18-44", "45-64", "65 and above")) {
  cat(ag, "\n")
  cat("Unweighted N:", sum(brfss_clean$age_cat == ag, na.rm = TRUE), "\n")
  cat("Overweight (%):", get_prev("overweight", brfss_design, brfss_clean$age_cat == ag), "\n")
  cat("Obesity (%):", get_prev("obesity", brfss_design, brfss_clean$age_cat == ag), "\n\n")
}

# ---------- BY RACE/ETHNICITY ----------
race_order <- c("Hispanic", "Non-Hispanic White", "Non-Hispanic Black", "Non-Hispanic Asian", "Non-Hispanic Other")
for (rc in race_order) {
  cat(rc, "\n")
  cat("Unweighted N:", sum(brfss_clean$race_cat == rc, na.rm = TRUE), "\n")
  cat("Overweight (%):", get_prev("overweight", brfss_design, brfss_clean$race_cat == rc), "\n")
  cat("Obesity (%):", get_prev("obesity", brfss_design, brfss_clean$race_cat == rc), "\n\n")
}

# ---------- BY RESIDENCE (URBAN/RURAL) ----------
for (res in c("Urban", "Rural")) {
  cat(res, "\n")
  cat("Unweighted N:", sum(brfss_clean$residence == res, na.rm = TRUE), "\n")
  cat("Overweight (%):", get_prev("overweight", brfss_design, brfss_clean$residence == res), "\n")
  cat("Obesity (%):", get_prev("obesity", brfss_design, brfss_clean$residence == res), "\n\n")
}