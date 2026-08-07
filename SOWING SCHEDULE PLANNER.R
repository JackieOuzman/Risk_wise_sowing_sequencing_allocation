# ===============================================================================
#   SOWING SCHEDULE PLANNER — MILP SEQUENCING MODEL (R version)
# ===============================================================================
#   
#   PURPOSE
# -------
#   Determines the optimal ha-per-crop-per-week sowing plan that maximises
# total expected yield, subject to:
#   - each crop's total ha sown must hit its target
#   - weekly sowing can't exceed daily capacity x days available that week
# - a crop, once started, must run to completion before another crop can
# start (at most one "handover" week where two crops are both active)
# - a crop can only be sown once across the season (no starting, stopping,
#                                                   then restarting later)

# BACKGROUND
# ----------
#   Translated from an Excel Solver model: MILP_model_v1_sequencing_allocation.xlsx,
# sheet "Solver sequencing allocation4-6". The Excel version proved the logic
# out (yield table, decision variables, all four Check blocks) but hit Excel
# free Solver's 200-variable ceiling (this model has 220 changing cells: 165
# ha-cells + 55 active-cells). Moved to OpenSolver (CBC engine) to remove that
# limit, which caught a real formulation issue along the way: the original
# "start flag" logic used MAX(), which isn't valid for a linear solver even
# though it behaved correctly with binary active-cells — fixed by making start
# flags real decision variables constrained by start >= active_now - active_prev
# instead. Then blocked from running OpenSolver itself by CSIRO's enterprise
# macro policy — rather than wait on an IT ticket, rebuilding the same proven
# model structure here in R using ompr.
# 
# SOLVER
# ------
# Package: ompr + ompr.roi, backend ROI.plugin.glpk (GLPK)
# 
# DATA SOURCE
# -----------
# Inputs read directly from the Excel workbook via readxl — same ranges the
# Excel formulas used, no manual re-entry.
# 
# BUILD PLAN
# ----------
#   1. Read inputs from Excel                                   <- next step
#   2. Define index sets (crops, zones, weeks)
#   3. Define decision variables (ha, active, start)
#   4. Add Check 1  — ha <= capacity x active
#   5. Add Check 2 (revised) — start >= active_now - active_prev
#   6. Add Check 3  — no restarting
#   7. Add Check 4  — max 2 crops active at once
#   8. Add crop target constraints
#   9. Set objective — maximise total expected yield
#   10. Solve and extract results
# ===============================================================================


library(readxl)
library(dplyr)
library(tidyr)


# ===========================================================================
# USER INPUTS — edit these directly, no longer reading from the Excel Inputs tab
# ===========================================================================
# --- Yield decile to use ------------------------------------------------
target_decile <- "D4-6"    # matches your Excel D4-6/average-season model; change here to test other deciles later

# --- Farm settings -----------------------------------------------------
site_name <- "Lock, Eyre Peninsula"
cropping_area_ha <- 1000            # <- total cropping area, ha (fill this in)
daily_capacity_ha <- 15         # ha/day

# --- Sowing window definitions (fixed 10-day windows) -----------------------
window_start_all <- as.Date(c(
  "2026-04-08", "2026-04-18", "2026-04-28", "2026-05-08", "2026-05-18",
  "2026-05-28", "2026-06-07", "2026-06-17", "2026-06-27", "2026-07-07",
  "2026-07-17"
))
window_end_all <- window_start_all + 9   # each window spans 10 days inclusive

# --- Program start: set EITHER a week number OR an exact date, not both -----
# Option A: start on a whole week's Monday (clean full week, no partial days)
program_start_week <- 1        # <- set a week number 1-11, or NA if using Option B

# Option B: start on a specific calendar date (allows a partial first week)
program_start_date <- NA       # <- e.g. as.Date("2026-04-13"), or NA if using Option A

# Resolve into a single date used by everything below
if (!is.na(program_start_week)) {
  program_start_date <- window_start_all[program_start_week]
}
program_start_date <- as.Date(program_start_date)

# --- Days available to sow in each of the 11 fixed windows -------------------
effective_start <- pmax(window_start_all, program_start_date)
days_available_all <- pmax(0, pmin(10, as.numeric(window_end_all - effective_start + 1)))
names(days_available_all) <- 1:11

print(days_available_all)

# --- Active weeks = any week with days available > 0 -----------------------
active_weeks <- as.numeric(names(days_available_all[days_available_all > 0]))
days_available <- days_available_all[as.character(active_weeks)]

print(active_weeks)

# --- Frost zone areas (% of farm) --------------------------------------
zone_pct <- c(Green = 0.60, Amber = 0.20, Red = 0.20)
zone_ha  <- zone_pct * cropping_area_ha   # will resolve once cropping_area_ha is set

# --- Crops & area (ha = 0 to exclude a crop) ----------------------------
crop_targets <- c(
  Wheat  = 400,
  Barley = 400,
  Canola = 0
)

# Legume 2 crops — choose which two are active this season
legume_targets <- c(
  Lupins = 100,
  Beans  = 100
)

# "Red zone?" flag per crop — TRUE means this crop is allowed in the Red zone
red_zone_allowed <- c(
  Wheat  = FALSE,
  Barley = FALSE,
  Canola = FALSE,
  Lupins = FALSE,
  Beans  = FALSE
)

# --- Combine into one active-crop target list ---------------------------
# (drops any crop with ha = 0, same as your Excel "0 to exclude" rule)
all_targets <- c(crop_targets, legume_targets)
active_crops <- names(all_targets[all_targets > 0])
crop_targets_final <- all_targets[active_crops]

print(crop_targets_final)
print(active_crops)

# ===========================================================================

# --- Sowing window definitions (fixed 10-day windows) -----------------------
window_start_all <- as.Date(c(
  "2026-04-08", "2026-04-18", "2026-04-28", "2026-05-08", "2026-05-18",
  "2026-05-28", "2026-06-07", "2026-06-17", "2026-06-27", "2026-07-07",
  "2026-07-17"
))
window_end_all <- window_start_all + 9   # each window spans 10 days inclusive

# --- Proposed program start date ---------------------------------------------
program_start_date <- as.Date("2026-04-13")   # <- set the actual start date here

# --- Days available to sow in each of the 11 fixed windows -------------------
effective_start <- pmax(window_start_all, program_start_date)
days_available_all <- pmax(0, pmin(10, as.numeric(window_end_all - effective_start + 1)))
names(days_available_all) <- 1:11

print(days_available_all)

# --- Active weeks = any week with days available > 0 -----------------------
active_weeks <- as.numeric(names(days_available_all[days_available_all > 0]))
days_available <- days_available_all[as.character(active_weeks)]

print(active_weeks)


# ===========================================================================
file_path <- "D:/work/RiskWise/early_sowing/Tool/Jackie_working/"
file_name <- "EP_yld_long_format.xlsx"

yield_long <- read_excel(paste0(file_path, file_name), sheet = "Yield data long format")

# Keep only Decile 4-6, and only the crops/weeks actually in use
yield_d46 <- yield_long %>%
  filter(decile_band == target_decile,
         crop %in% active_crops,
         `week of sowing program window` %in% active_weeks) %>%
  mutate(crop_zone = paste(crop, frost_zone)) %>%
  select(crop_zone, week = `week of sowing program window`, yield_t_per_ha)

yield_matrix <- yield_d46 %>%
  pivot_wider(names_from = week, values_from = yield_t_per_ha) %>%
  as.data.frame()

rownames(yield_matrix) <- yield_matrix$crop_zone
yield_matrix <- as.matrix(yield_matrix[, -1])

print(dim(yield_matrix))
print(rownames(yield_matrix))
# ===========================================================================
### Capacity Vector 
# ===========================================================================
# --- Weekly sowing capacity (ha) -------------------------------------------
capacity_vec <- daily_capacity_ha * days_available
print(capacity_vec)

