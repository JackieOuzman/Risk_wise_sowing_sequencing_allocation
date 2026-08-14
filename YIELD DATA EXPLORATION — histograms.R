# ===============================================================================
# YIELD DATA EXPLORATION — histograms of expected yield by crop and decile
# ===============================================================================

# ===============================================================================
# SECTION 1 — LIBRARIES
# ===============================================================================
library(readxl)
library(dplyr)
library(ggplot2)

# ===============================================================================
# SECTION 2 — LOAD DATA
# ===============================================================================
file_path <- "D:/work/RiskWise/early_sowing/Tool/Jackie_working/"
#file_name <- "EP_yld_long_format.xlsx"
file_name <- "EP_yld_long_format_MOCK.xlsx"

yield_long <- read_excel(paste0(file_path, file_name), sheet = "Yield data long format") 

print(dim(yield_long))
print(head(yield_long))
print(unique(yield_long$crop))
print(unique(yield_long$decile_band))


# ===============================================================================
# SECTION 3 — DENSITY CURVES: YIELD DISTRIBUTION BY CROP, ZONE, AND DECILE
# ===============================================================================
worked_crops <- c("Wheat", "Barley", "Beans", "Lupins")

yield_density_plot_zone <- yield_long %>%
  filter(crop %in% worked_crops) %>%
  mutate(crop = factor(crop, levels = worked_crops)) %>%
  ggplot(aes(x = yield_t_per_ha, colour = decile_band, fill = decile_band)) +
  geom_density(alpha = 0.3, linewidth = 0.8) +
  facet_grid(frost_zone ~ crop, scales = "free") +
  labs(title = "Yield distribution by crop, zone, and decile",
       subtitle = file_name,
       x = "Yield (t/ha)", y = "Density", fill = "Decile", colour = "Decile") +
  theme_minimal(base_size = 11)

print(yield_density_plot_zone)

ggsave(paste0(file_path, paste0("yield_distribution_by_crop_zone_decile",file_name,".png")),
       plot = yield_density_plot_zone, width = 14, height = 9, dpi = 300)
