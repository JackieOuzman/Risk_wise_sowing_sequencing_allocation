# ===============================================================================
#   SOWING SCHEDULE PLANNER — SINGLE CROP TEST (Wheat only)
#   MODEL CHECK: isolate the objective-reporting bug with the simplest possible case
# ===============================================================================

library(readxl)
library(dplyr)
library(tidyr)
library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)

# ===========================================================================
# USER INPUTS — Wheat only
# ===========================================================================
file_path <- "D:/work/RiskWise/early_sowing/Tool/Jackie_working/"
file_name <- "EP_yld_long_format.xlsx"    # Baseline

target_decile <- "D1-3"

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

# --- Only Wheat is active -----------------------------------------------
crop_targets_final <- c(Wheat = 400)
red_zone_excluded_crop <- "Wheat"

capacity_vec <- daily_capacity_ha * active_calendar$days
names(capacity_vec) <- active_calendar$week

crops <- names(crop_targets_final)   # just "Wheat"
zones <- c("Green", "Amber", "Red")
weeks <- active_calendar$week

n_crops <- length(crops)   # 1
n_zones <- length(zones)   # 3
n_weeks <- length(weeks)   # 11

# ===========================================================================
# YIELD TABLE
# ===========================================================================
yield_long <- read_excel(paste0(file_path, file_name), sheet = "Yield data long format")

yield_d_step <- yield_long %>%
  filter(decile_band == target_decile,
         crop %in% crops,
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
cat("\n--- Yield array (Wheat only, all zones/weeks) ---\n")
print(yield_array[1,,])   # rows = zones, cols = weeks — eyeball this against the Excel file

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
  add_constraint(sum_expr(ha[c, z, w], c = 1:n_crops, w = 1:n_weeks) <= zone_ha[z], z = 1:n_zones) %>%
  add_constraint(ha[c, z, w] == 0, c = 1:n_crops, z = 1:n_zones, w = 1:n_weeks,
                 zones[z] == "Red", crops[c] == red_zone_excluded_crop)

model <- model %>%
  set_objective(sum_expr(yield_array[cbind(c, z, w)] * ha[c, z, w],
                         c = 1:n_crops, z = 1:n_zones, w = 1:n_weeks), sense = "max")


# ===========================================================================
# SOLVE
# ===========================================================================
result <- solve_model(model, with_ROI(solver = "glpk", verbose = FALSE))
cat("\nStatus:", result$status, "| Solver-reported objective:", result$objective_value, "\n")

ha_solution <- get_solution(result, ha[c, z, w])
ha_solution$crop <- crops[ha_solution$c]
ha_solution$zone <- zones[ha_solution$z]
ha_solution$week <- weeks[ha_solution$w]

sowing_plan <- ha_solution[ha_solution$value > 0, c("crop", "zone", "week", "value")]
sowing_plan <- sowing_plan[order(sowing_plan$week), ]
cat("\n--- Solved plan (Wheat only) ---\n")
print(sowing_plan)

# ===========================================================================
# MANUAL CHECK — clean, row-aligned, easy to hand-verify against yield_array above
# ===========================================================================
manual_total <- sum(ha_solution$value * yield_array[cbind(ha_solution$c, ha_solution$z, ha_solution$w)])

cat("\n=== FINAL CHECK ===\n")
cat("Solver-reported objective:", result$objective_value, "\n")
cat("Manually calculated:", manual_total, "\n")



#  ===========================================================================
# Simplest possible version — a single variable, no index sets at all:
#  ===========================================================================
c_fixed <- 1; z_fixed <- 1; w_fixed <- 1
coef_fixed <- yield_array[c_fixed, z_fixed, w_fixed]
cat("Coefficient at [1,1,1]:", coef_fixed, "\n")

model_min <- MILPModel() %>%
  add_variable(x, type = "continuous", lb = 0, ub = 10) %>%
  set_objective(coef_fixed * x, sense = "max")

result_min <- solve_model(model_min, with_ROI(solver = "glpk", verbose = FALSE))
cat("Solver-reported:", result_min$objective_value, "\n")

x_value <- get_solution(result_min, x)
cat("x_value:", x_value, "\n")
cat("Manual (coef_fixed * x):", coef_fixed * x_value, "\n")


# ===========================================================================
# Two-term version — does a simple sum of two hand-written terms still work?
# ===========================================================================
model_min2 <- MILPModel() %>%
  add_variable(x1, type = "continuous", lb = 0, ub = 10) %>%
  add_variable(x2, type = "continuous", lb = 0, ub = 10) %>%
  set_objective(yield_array[1,1,1] * x1 + yield_array[1,1,2] * x2, sense = "max")

result_min2 <- solve_model(model_min2, with_ROI(solver = "glpk", verbose = FALSE))

x1_value <- get_solution(result_min2, x1)
x2_value <- get_solution(result_min2, x2)

cat("Solver-reported:", result_min2$objective_value, "\n")
cat("x1:", x1_value, "| x2:", x2_value, "\n")

manual_min2 <- (x1_value * yield_array[1,1,1]) + (x2_value * yield_array[1,1,2])
cat("Manual:", manual_min2, "\n")


# ===========================================================================
# Three-index-set version — using sum_expr's real expansion, but tiny (9 terms)
# ===========================================================================
model_3idx <- MILPModel() %>%
  add_variable(ha[c, z, w], c = 1:1, z = 1:3, w = 1:3, type = "continuous", lb = 0, ub = 10) %>%
  set_objective(sum_expr(yield_array[cbind(c, z, w)] * ha[c, z, w],
                         c = 1:1, z = 1:3, w = 1:3), sense = "max")

result_3idx <- solve_model(model_3idx, with_ROI(solver = "glpk", verbose = FALSE))
cat("Solver-reported:", result_3idx$objective_value, "\n")

sol_3idx <- get_solution(result_3idx, ha[c, z, w])
print(sol_3idx)

manual_3idx <- sum(sol_3idx$value * yield_array[cbind(sol_3idx$c, sol_3idx$z, sol_3idx$w)])
cat("Manual:", manual_3idx, "\n")


# ===========================================================================
# Two-index-set version — combine zone+week into one index, avoid 3D lookup
# ===========================================================================
zw_combinations <- expand.grid(z = 1:3, w = 1:3)
n_zw <- nrow(zw_combinations)

coef_zw <- sapply(1:n_zw, function(i) yield_array[1, zw_combinations$z[i], zw_combinations$w[i]])

model_2idx <- MILPModel() %>%
  add_variable(ha2[zw], zw = 1:n_zw, type = "continuous", lb = 0, ub = 10) %>%
  set_objective(sum_expr(coef_zw[zw] * ha2[zw], zw = 1:n_zw), sense = "max")

result_2idx <- solve_model(model_2idx, with_ROI(solver = "glpk", verbose = FALSE))
cat("Solver-reported:", result_2idx$objective_value, "\n")

sol_2idx <- get_solution(result_2idx, ha2[zw])
manual_2idx <- sum(sol_2idx$value * coef_zw[sol_2idx$zw])
cat("Manual:", manual_2idx, "\n")


# ===========================================================================
# Decisive test — force competition, see which cell the solver actually favours
# ===========================================================================
model_choice <- MILPModel() %>%
  add_variable(ha2[zw], zw = 1:n_zw, type = "continuous", lb = 0, ub = 10) %>%
  add_constraint(sum_expr(ha2[zw], zw = 1:n_zw) <= 10) %>%   # only 10 ha total to allocate across all 9 cells
  set_objective(sum_expr(coef_zw[zw] * ha2[zw], zw = 1:n_zw), sense = "max")

result_choice <- solve_model(model_choice, with_ROI(solver = "glpk", verbose = FALSE))
sol_choice <- get_solution(result_choice, ha2[zw])

cat("True coefficients (zw index : value):\n")
print(data.frame(zw = 1:n_zw, coef = coef_zw))

cat("\nWhich cell got the 10 ha?\n")
print(sol_choice[sol_choice$value > 0, ])

cat("\nTrue best coefficient is at zw =", which.max(coef_zw), "with value", max(coef_zw), "\n")


library(rlang)

model_fixed <- MILPModel() %>%
  add_variable(ha2[zw], zw = 1:n_zw, type = "continuous", lb = 0, ub = 10) %>%
  add_constraint(sum_expr(ha2[zw], zw = 1:n_zw) <= 10)

terms_expr <- list()
for (zi in 1:3) for (wi in 1:3) {
  coef <- yield_array[1, zi, wi]
  idx <- which(zw_combinations$z == zi & zw_combinations$w == wi)
  terms_expr[[length(terms_expr) + 1]] <- expr(!!coef * ha2[!!idx])
}


##################################################################################
library(rlang)

model_fixed <- MILPModel() %>%
  add_variable(ha2[zw], zw = 1:n_zw, type = "continuous", lb = 0, ub = 10) %>%
  add_constraint(sum_expr(ha2[zw], zw = 1:n_zw) <= 10)

terms_expr <- list()
for (zi in 1:3) for (wi in 1:3) {
  coef <- yield_array[1, zi, wi]
  idx <- which(zw_combinations$z == zi & zw_combinations$w == wi)
  terms_expr[[length(terms_expr) + 1]] <- expr(!!coef * ha2[!!idx])
}
obj_expr <- reduce(terms_expr, function(a, b) expr(!!a + !!b))

model_fixed <- inject(set_objective(model_fixed, !!obj_expr, sense = "max"))

result_fixed <- solve_model(model_fixed, with_ROI(solver = "glpk", verbose = FALSE))
sol_fixed <- get_solution(result_fixed, ha2[zw])

cat("Which cell got the 10 ha now?\n")
print(sol_fixed[sol_fixed$value > 0, ])
cat("\nShould be zw = 7 (the true best, coef 2.6306)\n")

