# ===============================================================================
# PACKAGES
# ===============================================================================
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)
library(rlang)
library(purrr)


# ===============================================================================
# SECTION 1 — USER INPUTS
# ===============================================================================
#file_path <- "D:/work/RiskWise/early_sowing/Tool/Jackie_working/"
getwd()
file_path <- "C:/Users/ouz001/working_from_home_post_Sep2022/Risk_wise_sowing_sequencing_allocation/files/"
file_name <- "EP_yld_long_format.xlsx"
#file_name <- "EP_yld_long_format_MOCK.xlsx"

# --- Simulation name (used to group all output files together) -------------
simulation_name <- "baseline_v1"    # <- change this each time you run a new scenario
#simulation_name <- "MOCK_Yldsv1"    # <- change this each time you run a new scenario

site_name <- "Lock, Eyre Peninsula"
cropping_area_ha <- 1000
daily_capacity_ha <- 15

program_start_date <- as.Date("2026-04-08") # 2026-04-08 (YYYMMDD)

zone_pct <- c(Green = 0.60, Amber = 0.20, Red = 0.20)
zone_ha  <- zone_pct * cropping_area_ha

crop_targets <- c(Wheat = 400, Barley = 400, Canola = 0)
legume_targets <- c(Lupins = 100, Beans = 100)

red_zone_excluded_crop <- "Beans"

deciles_to_run <- c("D1-3", "D4-6", "D7-9")

# --- Grain prices ($/t) — editable defaults, choose which price scenario to use ---
grain_price_table <- tribble(
  ~crop,     ~Low, ~Average, ~High,
  "Wheat",    200,  315,      400,
  "Barley",   200,  285,      380,
  "Canola",   500,  700,      1100,
  "Lentils",  500,  650,      1000,
  "Beans",    400,  500,      700,
  "Lupins",   200,  400,      600,
  "Peas",     300,  400,      800
)

price_scenario <- "Average"   # <- change to "Low" or "High" to use a different price scenario

grain_price <- setNames(grain_price_table[[price_scenario]], grain_price_table$crop)

# --- Variable costs ($/ha) by crop AND decile — editable defaults -----------
variable_cost_table <- tribble(
  ~crop,     ~`D1-3`, ~`D4-6`, ~`D7-9`,
  "Wheat",    284,     392,     473,
  "Barley",   230,     339,     402,
  "Canola",   324,     418,     469,
  "Lentils",  241,     267,     312,
  "Beans",    222,     238,     259,
  "Lupins",   194,     221,     265,
  "Peas",     184,     199,     214
)

# --- What should the optimiser maximise? -------------------------------------
optimise_for <- "yield"   # <- change to "gm" to optimise for gross margin instead #yield


# ===============================================================================
# SECTION 2 — CALENDAR, INDEX SETS, CAPACITY
# ===============================================================================
window_date <- as.Date(c(
  "2026-04-08", "2026-04-18", "2026-04-28", "2026-05-08", "2026-05-18",
  "2026-05-28", "2026-06-07", "2026-06-17", "2026-06-27", "2026-07-07",
  "2026-07-17"
))
window_week <- 1:11
window_days <- rep(10, 11)

sowing_calendar <- data.frame(date = window_date, week = window_week, days = window_days)
active_calendar <- sowing_calendar[sowing_calendar$date >= program_start_date, ]

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

# --- Feasibility check: is there enough sowing capacity for this start date? ---
check_feasibility <- function(capacity_vec, crop_targets_final) {
  total_capacity <- sum(capacity_vec)
  total_target <- sum(crop_targets_final)
  list(
    feasible = total_capacity >= total_target,
    total_capacity = total_capacity,
    total_target = total_target,
    shortfall = max(0, total_target - total_capacity)
  )
}

feasibility <- check_feasibility(capacity_vec, crop_targets_final)

cat("Total available sowing capacity:", feasibility$total_capacity, "ha over", n_weeks, "weeks\n")
cat("Total crop area to sow:", feasibility$total_target, "ha\n")

if (!feasibility$feasible) {
  stop(paste0(
    "Not enough sowing capacity for this program start date.\n",
    "  Program start: ", program_start_date, " leaves only ", n_weeks,
    " weeks (", feasibility$total_capacity, " ha of capacity)\n",
    "  Total crop area required: ", feasibility$total_target, " ha\n",
    "  Shortfall: ", feasibility$shortfall, " ha\n",
    "Try an earlier program_start_date, or reduce your crop targets."
  ))
}

yield_long <- read_excel(paste0(file_path, file_name), sheet = "Yield data long format")

cat("Reading yield file:", paste0(file_path, file_name), "\n")
cat("Check — Wheat Red D1-3, week 1 yield:",
    yield_long %>% filter(crop == "Wheat", frost_zone == "Red", decile_band == "D1-3",
                          `week of sowing program window` == 1) %>% pull(yield_t_per_ha),
    "\n")

cat("Setup complete. n_crops =", n_crops, "| n_zones =", n_zones, "| n_weeks =", n_weeks, "\n")


# ===============================================================================
# SECTION 3 — MAIN LOOP
# ===============================================================================
all_results <- list()
run_summary <- data.frame()

for (target_decile in deciles_to_run) {
  
  cat("\n=== Solving for decile:", target_decile, "===\n")
  
  # --- Yield table for this decile --------------------------------------
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
  
  get_yield <- function(c, z, w) {
    yield_array[cbind(c, z, w)]
  }
  
  if (sum(is.na(yield_array)) > 0) stop(paste("Missing yield lookups for", target_decile))
  
  if (sum(is.na(yield_array)) > 0) stop(paste("Missing yield lookups for", target_decile))
  
  # --- Build the gross margin array ($/ha) for this decile -----------------
  var_cost_decile <- setNames(variable_cost_table[[target_decile]], variable_cost_table$crop)
  
  gm_array <- array(NA_real_, dim = c(n_crops, n_zones, n_weeks))
  for (ci in 1:n_crops) for (zi in 1:n_zones) for (wi in 1:n_weeks) {
    gm_array[ci, zi, wi] <- yield_array[ci, zi, wi] * grain_price[crops[ci]] - var_cost_decile[crops[ci]]
  }
  
  # --- Choose which array drives the objective this run ---------------------
  objective_array <- if (optimise_for == "gm") gm_array else yield_array
  
  yield_vec <- c()
  for (ci in 1:n_crops) for (zi in 1:n_zones) for (wi in 1:n_weeks) {
    yield_vec[paste(ci, zi, wi, sep = "_")] <- yield_array[ci, zi, wi]
  }
  
  
  
  # --- Build a FRESH model -------------------------------------------------
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
  
  
  
  # --- Check 5: built explicitly with real loop indices — avoids ompr's
  #     dependent-range (ww = 1:w) expansion, which silently mishandles
  #     ranges that depend on another loop variable (same root cause as
  #     last week's objective bug)
  for (c1 in 1:n_crops) {
    for (c2 in 1:n_crops) {
      if (c1 == c2) next
      for (w in 1:n_weeks) {
        cum_ha_terms <- list()
        for (ww in 1:w) for (z in 1:n_zones) {
          cum_ha_terms[[length(cum_ha_terms) + 1]] <- expr(ha[!!c1, !!z, !!ww])
        }
        cum_ha_expr <- Reduce(function(a, b) expr(!!a + !!b), cum_ha_terms)
        
        cum_start_terms <- list()
        for (www in 1:w) {
          cum_start_terms[[length(cum_start_terms) + 1]] <- expr(start[!!c1, !!www])
        }
        cum_start_expr <- Reduce(function(a, b) expr(!!a + !!b), cum_start_terms)
        
        target_val <- crop_targets_final[c1]
        
        constraint_expr <- expr(
          !!cum_ha_expr >= !!target_val * (start[!!c2, !!w] + !!cum_start_expr - 1)
        )
        
        model <- inject(add_constraint(model, !!constraint_expr))
      }
    }
  }
 
  
  if (!is.na(red_zone_excluded_crop)) {
    model <- model %>%
      add_constraint(ha[c, z, w] == 0, c = 1:n_crops, z = 1:n_zones, w = 1:n_weeks,
                     zones[z] == "Red", crops[c] == red_zone_excluded_crop)
  }
  
  terms_expr <- list()
  for (ci in 1:n_crops) for (zi in 1:n_zones) for (wi in 1:n_weeks) {
    coef <- objective_array[ci, zi, wi]
    terms_expr[[length(terms_expr) + 1]] <- expr(!!coef * ha[!!ci, !!zi, !!wi])
  }
  obj_expr <- reduce(terms_expr, function(a, b) expr(!!a + !!b))
  
  
  
  model <- inject(set_objective(model, !!obj_expr, sense = "max"))
  
  # --- Solve --------------------------------------------------------------
  result <- solve_model(model, with_ROI(solver = "glpk", verbose = FALSE))
  cat("Status:", result$status, "| Objective:", result$objective_value, "\n")
  
  if (result$status != "success") { cat("SKIPPING", target_decile, "\n"); next }
  
  # --- Extract the sowing plan ----------------------------------------------
  ha_solution <- get_solution(result, ha[c, z, w])
  ha_solution$crop <- crops[ha_solution$c]
  ha_solution$zone <- zones[ha_solution$z]
  ha_solution$week <- weeks[ha_solution$w]
  
  manual_obj <- sum(sapply(1:nrow(ha_solution), function(i) {
    get_yield(ha_solution$c[i], ha_solution$z[i], ha_solution$w[i]) * ha_solution$value[i]
  }))
  cat(target_decile, "— manual objective check:", manual_obj,
      "| solver-reported objective:", result$objective_value, "\n")
  
  # --- Always calculate BOTH totals from the final solution, regardless of
  #     which one drove the optimisation ---------------------------------
  total_expected_yield <- sum(sapply(1:nrow(ha_solution), function(i) {
    yield_array[ha_solution$c[i], ha_solution$z[i], ha_solution$w[i]] * ha_solution$value[i]
  }))
  total_expected_gm <- sum(sapply(1:nrow(ha_solution), function(i) {
    gm_array[ha_solution$c[i], ha_solution$z[i], ha_solution$w[i]] * ha_solution$value[i]
  }))
  cat(target_decile, "— Expected yield:", total_expected_yield, "t | Expected GM: $", total_expected_gm, "\n")
  
  
  sowing_plan <- ha_solution[ha_solution$value > 0, c("crop", "zone", "week", "value")]
  sowing_plan <- sowing_plan[order(sowing_plan$crop, sowing_plan$week), ]
  sowing_plan_dated <- sowing_plan %>% left_join(active_calendar[, c("week", "date")], by = "week")
  
  write.csv(sowing_plan_dated,
            paste0(file_path, simulation_name, "_sowing_plan_", target_decile, ".csv"),
            row.names = FALSE)
  cat("Saved", simulation_name, "_sowing_plan_", target_decile, ".csv\n", sep = "")
  
  # --- Weekly sowing sequence table: one row per week, one column per zone ----
  zone_summary <- sowing_plan_dated %>%
    group_by(week, date, zone) %>%
    summarise(entry = paste0(crop, ": ", value, " ha", collapse = " + "), .groups = "drop")
  
  weekly_table <- zone_summary %>%
    pivot_wider(names_from = zone, values_from = entry) %>%
    arrange(week)
  
  cat("\n--- Sowing sequence,", target_decile, "---\n")
  print(weekly_table)
  
  write.csv(weekly_table,
            paste0(file_path, simulation_name, "_sowing_sequence_", target_decile, ".csv"),
            row.names = FALSE)
  cat("Saved", simulation_name, "_sowing_sequence_", target_decile, ".csv\n", sep = "")
  
  # --- Build one summary row: inputs + outputs for this decile run -----------
  crop_check <- sowing_plan %>%
    group_by(crop) %>%
    summarise(ha_sown = sum(value), .groups = "drop")
  
  crop_wide <- as.data.frame(t(setNames(crop_check$ha_sown, crop_check$crop)))
  
  run_row <- data.frame(
    simulation_name = simulation_name,
    decile = target_decile,
    site_name = site_name,
    cropping_area_ha = cropping_area_ha,
    daily_capacity_ha = daily_capacity_ha,
    program_start_date = as.character(program_start_date),
    zone_green_ha = zone_ha["Green"],
    zone_amber_ha = zone_ha["Amber"],
    zone_red_ha = zone_ha["Red"],
    red_zone_excluded_crop = ifelse(is.na(red_zone_excluded_crop), "None", red_zone_excluded_crop),
    
    total_ha_sown = sum(sowing_plan$value),
    optimise_for = optimise_for,
    price_scenario = price_scenario,
    
    expected_yield_t = total_expected_yield,
    expected_gm_dollars = total_expected_gm

  )
  
  run_row <- cbind(run_row, crop_wide)
  
  run_summary <- bind_rows(run_summary, run_row)
  
  all_results[[target_decile]] <- list(sowing_plan = sowing_plan_dated, objective = result$objective_value)
  
}   # <-- end of the decile loop


# ===============================================================================
# SECTION 4 — COMPARISON + SAVE COMBINED SUMMARY
# ===============================================================================
cat("\n=== DECILE COMPARISON ===\n")
for (d in names(all_results)) cat(d, ":", all_results[[d]]$objective, "\n")

run_filename <- paste0(simulation_name, "_run_summary.csv")
write.csv(run_summary, paste0(file_path, run_filename), row.names = FALSE)
write.csv(grain_price_table, paste0(file_path, simulation_name, "_grain_prices.csv"), row.names = FALSE)
write.csv(variable_cost_table, paste0(file_path, simulation_name, "_variable_costs.csv"), row.names = FALSE)

print(run_summary)
cat("\nSaved:", run_filename, "\n")
 
