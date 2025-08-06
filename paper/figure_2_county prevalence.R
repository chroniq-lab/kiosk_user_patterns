# --- Load Libraries ---
library(tidyverse)
library(sf)
library(ggplot2)
library(patchwork)

# --- Load County Boundaries Shapefile ---
counties <- readRDS("/Users/carolinechizak/Library/CloudStorage/OneDrive-SharedLibraries-Emory/Varghese, Jithin Sam - Pursuant User Profiles/working/shapefiles/county_boundaries_2022.rds") %>%
  rename(fips = GEOID) %>%
  mutate(fips = as.character(fips),
         fips = str_pad(fips, 5, pad = "0"))

# --- Extract State Boundaries ---
states <- counties %>%
  group_by(STATEFP) %>%
  summarize(geometry = sf::st_union(geometry)) %>%
  ungroup()

# --- Load PLACES Data (Obesity Only) ---
places <- read_csv("/Users/carolinechizak/kiosk_user_patterns/data/kupdat09_places county estimates.csv") %>%
  mutate(fips = str_pad(as.character(fips), 5, pad = "0"))

# --- Load Cosmos Data ---
cosmos <- read_csv("/Users/carolinechizak/kiosk_user_patterns/data/kupdat08_cosmos county estimates.csv") %>%
  mutate(fips = str_pad(as.character(fips), 5, pad = "0"))

# --- Load Pursuant Data (Obesity & Overweight, separate files) ---
pursuant_obesity <- read_csv("/Users/carolinechizak/Library/CloudStorage/OneDrive-SharedLibraries-Emory/Varghese, Jithin Sam - Pursuant User Profiles/working/model/ktana07/glmer_20242025_county_obesity estimates.csv")
pursuant_overweight <- read_csv("/Users/carolinechizak/Library/CloudStorage/OneDrive-SharedLibraries-Emory/Varghese, Jithin Sam - Pursuant User Profiles/working/model/ktana07/glmer_20242025_county_overweight estimates.csv")

pursuant_obesity <- pursuant_obesity %>%
  mutate(fips = str_pad(as.character(FIPS), 5, pad = "0"))
pursuant_overweight <- pursuant_overweight %>%
  mutate(fips = str_pad(as.character(FIPS), 5, pad = "0"))

pursuant <- left_join(
  pursuant_obesity %>% select(fips, Obesity = prevalence),
  pursuant_overweight %>% select(fips, Overweight = prevalence),
  by = "fips"
) %>%
  mutate(
    Obesity = Obesity * 100,
    Overweight = Overweight * 100
  )

# --- Merge All Data with County Boundaries ---
map_places    <- left_join(counties, places, by = "fips")
map_cosmos    <- left_join(counties, cosmos, by = "fips")
map_pursuant  <- left_join(counties, pursuant, by = "fips")

# --- Plot Panels (titles size 10, reversed color scale, state outlines, unified color axis) ---
cosmos_over <- ggplot(map_cosmos) +
  geom_sf(aes(fill = Overweight), color = NA) +
  geom_sf(data = states, fill = NA, color = "black", size = 0.3) +
  scale_fill_viridis_c(limits = c(0, 100), name = "Overweight (%)", direction = -1) +
  theme_void() +
  ggtitle("Cosmos 2024-25: Overweight") +
  theme(plot.title = element_text(size = 10))

pursuant_over <- ggplot(map_pursuant) +
  geom_sf(aes(fill = Overweight), color = NA) +
  geom_sf(data = states, fill = NA, color = "black", size = 0.3) +
  scale_fill_viridis_c(limits = c(0, 100), name = "Overweight (%)", direction = -1) +
  theme_void() +
  ggtitle("Pursuant 2024-25: Overweight") +
  theme(plot.title = element_text(size = 10))

places_obes <- ggplot(map_places) +
  geom_sf(aes(fill = obesity), color = NA) +
  geom_sf(data = states, fill = NA, color = "black", size = 0.3) +
  scale_fill_viridis_c(limits = c(0, 100), name = "Obesity (%)", direction = -1) +
  theme_void() +
  ggtitle("PLACES 2024: Obesity") +
  theme(plot.title = element_text(size = 10))

cosmos_obes <- ggplot(map_cosmos) +
  geom_sf(aes(fill = Obesity), color = NA) +
  geom_sf(data = states, fill = NA, color = "black", size = 0.3) +
  scale_fill_viridis_c(limits = c(0, 100), name = "Obesity (%)", direction = -1) +
  theme_void() +
  ggtitle("Cosmos 2024-25: Obesity") +
  theme(plot.title = element_text(size = 10))

pursuant_obes <- ggplot(map_pursuant) +
  geom_sf(aes(fill = Obesity), color = NA) +
  geom_sf(data = states, fill = NA, color = "black", size = 0.3) +
  scale_fill_viridis_c(limits = c(0, 100), name = "Obesity (%)", direction = -1) +
  theme_void() +
  ggtitle("Pursuant 2024-25: Obesity") +
  theme(plot.title = element_text(size = 10))

# --- Assemble Panel (no GBD, just 2x3 with leftmost cell blank on row 1) ---
empty_panel <- ggplot() + theme_void() # Empty panel for top-left

final_panel <- (empty_panel | cosmos_over | pursuant_over) /
  (places_obes | cosmos_obes | pursuant_obes) +
  plot_annotation(title = "Figure 2. County prevalence of overweight and obesity",
                  theme = theme(plot.title = element_text(size = 16)))

# --- Save Directly to JPEG File ---
ggsave("figure2_county_prevalence.jpeg", plot = final_panel, width = 16, height = 8, dpi = 300, bg = "white", device = "jpeg")