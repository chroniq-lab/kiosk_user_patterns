# --- Load Libraries ---
library(tidyverse)
library(arrow)       # For parquet files
library(survey)      # For weighted estimates
library(sf)
library(ggplot2)
library(patchwork)

# --- Load State Boundaries Shapefile ---
states <- readRDS("/Users/carolinechizak/Library/CloudStorage/OneDrive-SharedLibraries-Emory/Varghese, Jithin Sam - Pursuant User Profiles/working/shapefiles/state_boundaries_2022.rds")

# --- BRFSS 2022 State Estimates ---
parquet_path <- "/Users/carolinechizak/Library/CloudStorage/OneDrive-SharedLibraries-Emory/Varghese, Jithin Sam - BRFSS/data/year=2022/"
brfss2022 <- open_dataset(parquet_path, format = "parquet") %>% collect()
brfss2022 <- brfss2022 %>%
  mutate(
    bmi = as.numeric(`_BMI5`) / 100,
    overweight = if_else(bmi >= 25 & bmi < 30, 1, 0, missing = NA_real_),
    obesity = if_else(bmi >= 30, 1, 0, missing = NA_real_),
    state_fips = str_pad(as.character(state), 2, pad = "0")
  ) %>%
  filter(!is.na(bmi), !is.na(state), !is.na(`_LLCPWT`))
options(survey.lonely.psu = "adjust")
brfss_design <- svydesign(ids = ~1, weights = ~`_LLCPWT`, data = brfss2022)
brfss_state_est <- svyby(
  ~overweight + obesity, by = ~state_fips, brfss_design, svymean, na.rm = TRUE
) %>% as_tibble() %>%
  mutate(overweight = overweight * 100, obesity = obesity * 100)
states_df <- st_drop_geometry(states) %>% select(state_fips = STATEFP, NAME)
brfss_state_est <- left_join(brfss_state_est, states_df, by = "state_fips")

# --- Cosmos State Estimates (Unweighted) ---
cosmos_state_raw <- read_csv("/Users/carolinechizak/kiosk_user_patterns/data/kupdat08_cosmos state stratified estimates.csv")
cosmos_state <- cosmos_state_raw %>%
  filter(Stratification == "state") %>%
  mutate(state_fips = str_pad(as.character(state_fips), 2, pad = "0")) %>%
  select(state_fips, overweight, obesity, State) %>%
  rename(NAME = State)
# Check: should be 51 rows for states+DC

# --- Pursuant State Estimates (Modeled) ---
pursuant_state_raw <- read_csv("/Users/carolinechizak/Library/CloudStorage/OneDrive-SharedLibraries-Emory/Varghese, Jithin Sam - Pursuant User Profiles/working/model/ktana08/glmer_gbdcompare_20242025_state_overweight estimates.csv")
pursuant_state_obesity <- read_csv("/Users/carolinechizak/Library/CloudStorage/OneDrive-SharedLibraries-Emory/Varghese, Jithin Sam - Pursuant User Profiles/working/model/ktana08/glmer_gbdcompare_20242025_state_obesity estimates.csv")
pursuant_state <- pursuant_state_raw %>%
  filter(age_group_gbd_consolidated == "25plus") %>%
  select(state_fips = state_code, overweight = prevalence) %>%
  mutate(overweight = overweight * 100)
pursuant_state_obesity <- pursuant_state_obesity %>%
  filter(age_group_gbd_consolidated == "25plus") %>%
  select(state_fips = state_code, obesity = prevalence) %>%
  mutate(obesity = obesity * 100)
pursuant_state <- left_join(pursuant_state, pursuant_state_obesity, by = "state_fips")
pursuant_state <- left_join(states_df, pursuant_state, by = "state_fips")

# --- Merge Data with State Boundaries (for plotting) ---
map_brfss    <- left_join(states, brfss_state_est, by = c("STATEFP" = "state_fips"))
map_cosmos   <- left_join(states, cosmos_state,    by = c("NAME"))
map_pursuant <- left_join(states, pursuant_state,  by = c("STATEFP" = "state_fips"))

# --- Common Fill (Unified Color Scale) ---
common_fill_over <- scale_fill_viridis_c(limits = c(0, 100), name = "Overweight (%)", direction = -1)
common_fill_obes <- scale_fill_viridis_c(limits = c(0, 100), name = "Obesity (%)", direction = -1)

# --- Plot Panels (add state outlines) ---
brfss_over_panel <- ggplot(map_brfss) +
  geom_sf(aes(fill = overweight), color = "black", size = 0.2) +
  common_fill_over + theme_void() +
  ggtitle("BRFSS 2022: Overweight") +
  theme(plot.title = element_text(size = 10))

cosmos_over_panel <- ggplot(map_cosmos) +
  geom_sf(aes(fill = overweight), color = "black", size = 0.2) +
  common_fill_over + theme_void() +
  ggtitle("Cosmos 2024–25: Overweight") +
  theme(plot.title = element_text(size = 10))

pursuant_over_panel <- ggplot(map_pursuant) +
  geom_sf(aes(fill = overweight), color = "black", size = 0.2) +
  common_fill_over + theme_void() +
  ggtitle("Pursuant 2024–25: Overweight") +
  theme(plot.title = element_text(size = 10))

brfss_obes_panel <- ggplot(map_brfss) +
  geom_sf(aes(fill = obesity), color = "black", size = 0.2) +
  common_fill_obes + theme_void() +
  ggtitle("BRFSS 2022: Obesity") +
  theme(plot.title = element_text(size = 10))

cosmos_obes_panel <- ggplot(map_cosmos) +
  geom_sf(aes(fill = obesity), color = "black", size = 0.2) +
  common_fill_obes + theme_void() +
  ggtitle("Cosmos 2024–25: Obesity") +
  theme(plot.title = element_text(size = 10))

pursuant_obes_panel <- ggplot(map_pursuant) +
  geom_sf(aes(fill = obesity), color = "black", size = 0.2) +
  common_fill_obes + theme_void() +
  ggtitle("Pursuant 2024–25: Obesity") +
  theme(plot.title = element_text(size = 10))

# --- Assemble 2x3 Panel ---
final_panel <- (brfss_over_panel | cosmos_over_panel | pursuant_over_panel) /
  (brfss_obes_panel | cosmos_obes_panel | pursuant_obes_panel) +
  plot_annotation(
    title = "Figure 1. State prevalence of overweight and obesity",
    theme = theme(plot.title = element_text(size = 16))
  )

# --- Save to JPEG File ---
ggsave("figure1_state_prevalence.jpeg", plot = final_panel, width = 14, height = 8, dpi = 300, bg = "white", device = "jpeg")