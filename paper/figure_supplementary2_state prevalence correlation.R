# --- Load necessary libraries ---
library(tidyverse)
library(ggrepel)
library(cowplot)

# --- 1. US States + DC List ---
state_list <- c(state.name, "District of Columbia")

# --- 2. State FIPS-to-name crosswalk (for Pursuant) ---
state_xwalk <- tibble(
  state_code = sprintf("%02d", 1:51), # "01" to "51"
  state_names = state_list
)

# --- 3. Load and prepare GBD Data (obesity) ---
gbd <- read_csv("/Users/carolinechizak/Library/CloudStorage/OneDrive-SharedLibraries-Emory/Varghese, Jithin Sam - Pursuant User Profiles/working/raw/GBD/IHME_USA_OVERWEIGHT_OBESITY_PREVALENCE_1990_2050/IHME_USA_OVERWEIGHT_OBESITY_PREVALENCE_1990_2050_AGES_25_125_OB_Y2024M11D07.CSV")

gbd_state <- gbd %>%
  filter(year_id == 2024, sex == "Both", location_name %in% state_list) %>%
  select(state_gbd = location_name, gbd_obesity = mean_prev) %>%
  mutate(gbd_obesity = gbd_obesity * 100) %>%
  distinct(state_gbd, .keep_all = TRUE)

# --- 4. Load and prepare Cosmos State Data ---
cosmos <- read_csv("/Users/carolinechizak/kiosk_user_patterns/data/kupdat08_cosmos state stratified estimates.csv")
cosmos_state <- cosmos %>%
  filter(Stratification == "state", State %in% state_list) %>%
  select(state_names = State, cosmos_obesity = obesity) %>%
  distinct(state_names, .keep_all = TRUE)

# --- 5. Merge for Panel A: GBD vs Cosmos (by state name) ---
cosmos_gbd <- cosmos_state %>%
  left_join(gbd_state, by = c("state_names" = "state_gbd"))

cosmos_gbd_clean <- cosmos_gbd %>%
  drop_na(gbd_obesity, cosmos_obesity)

# --- 6. Prepare Pursuant State Data with crosswalk ---
pursuant <- read_csv("/Users/carolinechizak/Library/CloudStorage/OneDrive-SharedLibraries-Emory/Varghese, Jithin Sam - Pursuant User Profiles/working/model/ktana08/glmer_gbdcompare_20242025_state_obesity estimates.csv")

pursuant_state <- pursuant %>%
  filter(age_group_gbd_consolidated == "25plus") %>%
  mutate(state_code = sprintf("%02d", as.numeric(state_code))) %>%
  left_join(state_xwalk, by = "state_code") %>%
  mutate(pursuant_obesity = prevalence * 100) %>%
  select(state_names, pursuant_obesity) %>%
  filter(state_names %in% state_list) %>%
  distinct(state_names, .keep_all = TRUE)

# --- 7. Merge for Panel B: GBD vs Pursuant (by state name) ---
pursuant_gbd <- pursuant_state %>%
  left_join(gbd_state, by = c("state_names" = "state_gbd"))

pursuant_gbd_clean <- pursuant_gbd %>%
  drop_na(gbd_obesity, pursuant_obesity)

# --- 8. Panel A: Plot ---
rho_cosmos <- cor.test(cosmos_gbd_clean$cosmos_obesity, cosmos_gbd_clean$gbd_obesity, method = "spearman")

p1 <- ggplot(cosmos_gbd_clean, aes(x = gbd_obesity, y = cosmos_obesity, label = state_names)) +
  geom_point(size = 2, color = "#1f77b4") +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  ggrepel::geom_text_repel(size = 2.8) +
  labs(
    x = "GBD 2024 Obesity Prevalence (%)",
    y = "Cosmos 2024-2025 Obesity Prevalence (%)",
    title = "A. GBD 2024 vs Cosmos 2024-2025"
  ) +
  annotate("text", x = Inf, y = -Inf, hjust = 1.05, vjust = -0.5,
           label = paste0("Spearman’s ρ = ", round(rho_cosmos$estimate, 2)),
           size = 3.5, fontface = "italic") +
  theme_bw(base_size = 12)

# --- 9. Panel B: Plot ---
rho_pursuant <- cor.test(pursuant_gbd_clean$pursuant_obesity, pursuant_gbd_clean$gbd_obesity, method = "spearman")

p2 <- ggplot(pursuant_gbd_clean, aes(x = gbd_obesity, y = pursuant_obesity, label = state_names)) +
  geom_point(size = 2, color = "#ff7f0e") +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  ggrepel::geom_text_repel(size = 2.8) +
  labs(
    x = "GBD 2024 Obesity Prevalence (%)",
    y = "Pursuant 2024-2025 Obesity Prevalence (%)",
    title = "B. GBD 2024 vs Pursuant 2024-2025"
  ) +
  annotate("text", x = Inf, y = -Inf, hjust = 1.05, vjust = -0.5,
           label = paste0("Spearman’s ρ = ", round(rho_pursuant$estimate, 2)),
           size = 3.5, fontface = "italic") +
  theme_bw(base_size = 12)

# --- 10. Combine Panels ---
fig_sup2 <- plot_grid(p1, p2, nrow = 1, align = "hv", labels = c("", ""))

# --- 11. Show and Save Figure ---
print(fig_sup2)
ggsave("supplementary_figure2_gbd_correlation.jpg", fig_sup2, width = 13, height = 5, dpi = 350)