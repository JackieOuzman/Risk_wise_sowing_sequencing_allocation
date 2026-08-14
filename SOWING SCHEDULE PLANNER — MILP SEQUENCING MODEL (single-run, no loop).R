
# ===============================================================================
#   SOWING SCHEDULE PLANNER — MILP SEQUENCING MODEL (single-run, no loop)
#   MODEL CHECK: Baseline, D1-3
# ===============================================================================

library(readxl)
library(dplyr)
library(tidyr)
library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)
library(ggplot2)

# ===========================================================================
# USER INPUTS
# ===========================================================================
file_path <- "D:/work/RiskWise/early_sowing/Tool/Jackie_working/"
file_name <- "EP_yld_long_format.xlsx"    # Baseline

target_decile <- "D1-3"

site_name <- "Lock, Eyre Peninsula"
cropping_area_ha <- 1000
daily_capacity_ha <- 15

program_start_date <- as.Date("2026-04-08")

window_date <- as.Date(c(
  "2026-04-08", "2026-04-18", "2026-04-28", "2026-05-08", "2026-05-18",
  "2026-05-28", "2026-06-07", "2026-06-17", "2026-06-27", "2026-07-07",
  "2026-07-17"
))
window_week <- 1:11
window_days <- rep(10, 11)

sowing_calendar <- data.frame(date = window_date, week = window_week, days = window_days)
active_calendar <- sowing_calendar[sowing_calendar$date >= program_start_date, ]

zone_pct <- c(Green = 0.60, Amber = 0.20, Red = 0.20)
zone_ha  <- zone_pct * cropping_area_ha

crop_targets <- c(Wheat = 400, Barley = 400, Canola = 0)
legume_targets <- c(Lupins = 100, Beans = 100)

red_zone_excluded_crop <- "Wheat"

all_targets <- c(crop_targets, legume_targets)
active_crops <- names(all_targets[all_targets > 0])
crop_targets_final <- all_targets[active_crops]

capacity_vec <- daily_capacity_ha * active_calendar$days
names(capacity_vec) <- active_calendar$week

crops <- active_crops
zones <- c("Green", "Amber", "Red")
weeks <- active_calendar$week

n_crops <- length(crops)
n_zones <- length(zones)
n_weeks <- length(weeks)

# ===========================================================================
# YIELD TABLE
# ===========================================================================
yield_long <- read_excel(paste0(file_path, file_name), sheet = "Yield data long format")

yield_d_step <- yield_long %>%
  filter(decile_band == target_decile,
         crop %in% active_crops,
         `week of sowing program window` %in% active_calendar$week) %>%
  mutate(crop_zone = paste(crop, frost_zone)) %>%
  select(crop_zone, week = `week of sowing program window`, yield_t_per_ha)

yield_matrix <- yield_d_step %>%
  pivot_wider(names_from = week, values_from = yield_t_per_ha) %>%
  as.data.frame()
rownames(yield_matrix) <- yield_matrix$crop_zone
yield_matrix <- as.matrix(yield_matrix[, -1])

yield_array <- array(NA_real_, dim = c(n_crops, n_zones, n_weeks))
for (ci in 1:n_crops) for (zi in 1:n_zones) for (wi in 1:n_weeks) {
  yield_array[ci, zi, wi] <- yield_matrix[paste(crops[ci], zones[zi]), as.character(weeks[wi])]
}

cat("Missing yield lookups (should be 0):", sum(is.na(yield_array)), "\n")

# ===========================================================================
# MODEL
# ===========================================================================
model <- MILPModel() %>%
  add_variable(ha[c, z, w], c = 1:n_crops, z = 1:n_zones, w = 1:n_weeks, type = "continuous", lb = 0) %>%
  add_variable(active[c, w], c = 1:n_crops, w = 1:n_weeks, type = "binary") %>%
  add_variable(start[c, w], c = 1:n_crops, w = 1:n_weeks, type = "continuous", lb = 0) %>%
  add_constraint(sum_expr(ha[c, z, w], z = 1:n_zones) <= capacity_vec[w] * active[c, w],
                 c = 1:n_crops, w = 1:n_weeks) %>%
  add_constraint(sum_expr(ha[c, z, w], c = 1:n_crops, z = 1:n_zones) <= capacity_vec[w],
                 w = 1:n_weeks) %>%
  add_constraint(start[c, 1] >= active[c, 1], c = 1:n_crops) %>%
  add_constraint(start[c, w] >= active[c, w] - active[c, w - 1], c = 1:n_crops, w = 2:n_weeks) %>%
  add_constraint(sum_expr(start[c, w], w = 1:n_weeks) <= 1, c = 1:n_crops) %>%
  add_constraint(sum_expr(active[c, w], c = 1:n_crops) <= 2, w = 1:n_weeks) %>%
  add_constraint(sum_expr(ha[c, z, w], z = 1:n_zones, w = 1:n_weeks) == crop_targets_final[c],
                 c = 1:n_crops) %>%
  add_constraint(sum_expr(ha[c, z, w], c = 1:n_crops, w = 1:n_weeks) <= zone_ha[z], z = 1:n_zones)

if (!is.na(red_zone_excluded_crop)) {
  model <- model %>%
    add_constraint(ha[c, z, w] == 0, c = 1:n_crops, z = 1:n_zones, w = 1:n_weeks,
                   zones[z] == "Red", crops[c] == red_zone_excluded_crop)
}

terms_expr <- list()
for (ci in 1:n_crops) for (zi in 1:n_zones) for (wi in 1:n_weeks) {
  coef <- yield_array[ci, zi, wi]
  terms_expr[[length(terms_expr) + 1]] <- expr(!!coef * ha[!!ci, !!zi, !!wi])
}
obj_expr <- reduce(terms_expr, function(a, b) expr(!!a + !!b))

model <- inject(set_objective(model, !!obj_expr, sense = "max"))

# ===========================================================================
# SOLVE
# ===========================================================================
result <- solve_model(model, with_ROI(solver = "glpk", verbose = FALSE))
cat("Status:", result$status, "| Objective:", result$objective_value, "\n")

ha_solution <- get_solution(result, ha[c, z, w])
ha_solution$crop <- crops[ha_solution$c]
ha_solution$zone <- zones[ha_solution$z]
ha_solution$week <- weeks[ha_solution$w]

sowing_plan <- ha_solution[ha_solution$value > 0, c("crop", "zone", "week", "value")]
sowing_plan <- sowing_plan[order(sowing_plan$crop, sowing_plan$week), ]

sowing_plan_dated <- sowing_plan %>%
  left_join(active_calendar[, c("week", "date")], by = "week")

print(sowing_plan_dated)


# ===========================================================================
# SAVE THE SOLVED PLAN — for the manual check
# ===========================================================================
check_path <- "D:/work/RiskWise/early_sowing/Tool/Jackie_working/model check/"
scenario_name <- tools::file_path_sans_ext(file_name)
run_label <- paste0(scenario_name, "_", target_decile)

write.csv(sowing_plan_dated, paste0(check_path, run_label, "_sowing_plan.csv"), row.names = FALSE)
cat("Saved:", paste0(run_label, "_sowing_plan.csv"), "\n")

# ===========================================================================
# MANUAL CHECK — bypass ompr entirely
# ===========================================================================


manual_total <- sum(ha_solution$value * yield_array[cbind(ha_solution$c, ha_solution$z, ha_solution$w)])
cat("Manual (clean, row-aligned):", manual_total, "\n")

cat("\n=== FINAL CHECK ===\n")
cat("Solver-reported objective:", result$objective_value, "\n")
cat("Manually calculated (same session, same yield_array):", manual_total, "\n")


### checks
sowing_plan_dated
active_solution <- get_solution(result, active[c, w])
active_solution$crop <- crops[active_solution$c]
active_solution$week <- weeks[active_solution$w]

cat("--- Wheat active[c,w] by week ---\n")
print(active_solution[active_solution$crop == "Wheat", c("week", "value")])

start_solution <- get_solution(result, start[c, w])
start_solution$crop <- crops[start_solution$c]
start_solution$week <- weeks[start_solution$w]

cat("\n--- Wheat start[c,w] by week ---\n")
print(start_solution[start_solution$crop == "Wheat", c("week", "value")])
