library(arrow)
library(dplyr)

# 1) Load BRFSS dataset (edit path as needed)
parquet_path <- "/Users/carolinechizak/Library/CloudStorage/OneDrive-SharedLibraries-Emory/Varghese, Jithin Sam - BRFSS/data/year=2022/"
brfss_raw <- open_dataset(parquet_path, format = "parquet") %>% collect()

# 2) Recode variables if needed
brfss_recoded <- brfss_raw %>%
  mutate(
    female = case_when(
      `_SEX` == 2 ~ 1,
      `_SEX` == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    age = `_AGE80`,  # Or your actual age variable
    bmi = `_BMI5` / 100,
    height = HTM4,   # already in cm
    weight = WTKG3,  # already in kg
    # Replace PREGNANT below with the actual pregnancy variable name in BRFSS
    pregnant = PREGNANT  # usually 1 = yes, 2 = no, 7/9 = don't know/refused/missing
  )

# Helper function for counts by sex
count_by_sex <- function(df, step_name) {
  df %>%
    mutate(sex = case_when(
      female == 1 ~ "Female",
      female == 0 ~ "Male",
      TRUE ~ NA_character_
    )) %>%
    filter(!is.na(sex)) %>%
    count(Step = step_name, sex) %>%
    group_by(Step) %>%
    mutate(Total = sum(n)) %>%
    ungroup()
}

# STEP 1: All adults ≥18
step1 <- brfss_recoded %>% filter(!is.na(age), age >= 18)
c1 <- count_by_sex(step1, "All BRFSS adults ≥18")

# STEP 2: Not pregnant (usually pregnant == 2 means not pregnant)
step2 <- step1 %>% filter(is.na(pregnant) | pregnant == 2)
c2 <- count_by_sex(step2, "Not pregnant")

# STEP 3: BMI, height & weight available
step3 <- step2 %>% filter(!is.na(bmi), !is.na(height), !is.na(weight))
c3 <- count_by_sex(step3, "BMI, height & weight available")

# Combine and print
brfss_counts <- bind_rows(c1, c2, c3)
print(brfss_counts)