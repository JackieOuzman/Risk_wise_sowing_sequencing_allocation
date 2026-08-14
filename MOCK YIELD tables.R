# ===============================================================================
# MOCK YIELD SCENARIO — deliberately different crop-response shapes by decile
# ===============================================================================
# ===============================================================================
# SECTION 1 — LIBRARIES
# ===============================================================================
library(readxl)
library(dplyr)
library(writexl)

# ===============================================================================
# SECTION 2 — LOAD ORIGINAL YIELD DATA
# ===============================================================================
file_path <- "D:/work/RiskWise/early_sowing/Tool/Jackie_working/"
file_name <- "EP_yld_long_format.xlsx"
yield_long <- read_excel(paste0(file_path, file_name), sheet = "Yield data long format")

# ===============================================================================
# SECTION 3 — DEFINE MOCK ADJUSTMENT FACTORS (Wheat, Barley, Beans, Lupins only)
# ===============================================================================
wheat_zone_factor <- tribble(
  ~decile_band, ~frost_zone, ~factor,
  "D1-3", "Red",   0.50,
  "D4-6", "Red",   0.90,
  "D7-9", "Red",   1.20
)

barley_late_week_factor <- tribble(
  ~decile_band, ~factor,
  "D1-3", 0.40,
  "D4-6", 0.80,
  "D7-9", 1.10
)
late_weeks <- 8:11   # last 4 weeks of the 11-week program

beans_zone_factor <- tribble(
  ~decile_band, ~frost_zone, ~factor,
  "D1-3", "Amber", 1.20,
  "D1-3", "Green", 0.70,
  "D7-9", "Amber", 0.90,
  "D7-9", "Green", 1.20
)

lupins_zone_factor <- tribble(
  ~decile_band, ~frost_zone, ~factor,
  "D1-3", "Red",   0.40,
  "D4-6", "Red",   0.85,
  "D7-9", "Red",   1.30
)

# ===============================================================================
# SECTION 4 — APPLY ADJUSTMENTS TO BUILD MOCK YIELD TABLE
# ===============================================================================
mock_yield_long <- yield_long

mock_yield_long <- mock_yield_long %>%
  left_join(wheat_zone_factor, by = c("decile_band", "frost_zone")) %>%
  mutate(yield_t_per_ha = if_else(crop == "Wheat" & !is.na(factor),
                                  yield_t_per_ha * factor, yield_t_per_ha)) %>%
  select(-factor)

mock_yield_long <- mock_yield_long %>%
  left_join(barley_late_week_factor, by = "decile_band") %>%
  mutate(yield_t_per_ha = if_else(crop == "Barley" & `week of sowing program window` %in% late_weeks & !is.na(factor),
                                  yield_t_per_ha * factor, yield_t_per_ha)) %>%
  select(-factor)

mock_yield_long <- mock_yield_long %>%
  left_join(beans_zone_factor, by = c("decile_band", "frost_zone")) %>%
  mutate(yield_t_per_ha = if_else(crop == "Beans" & !is.na(factor),
                                  yield_t_per_ha * factor, yield_t_per_ha)) %>%
  select(-factor)

mock_yield_long <- mock_yield_long %>%
  left_join(lupins_zone_factor, by = c("decile_band", "frost_zone")) %>%
  mutate(yield_t_per_ha = if_else(crop == "Lupins" & !is.na(factor),
                                  yield_t_per_ha * factor, yield_t_per_ha)) %>%
  select(-factor)

# ===============================================================================
# SECTION 5 — SAVE MOCK YIELD TABLE
# ===============================================================================
write_xlsx(list(`Yield data long format` = mock_yield_long),
           paste0(file_path, "EP_yld_long_format_MOCK.xlsx"))
