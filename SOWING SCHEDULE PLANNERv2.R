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

#install.packages("ompr")
#install.packages("ompr.roi")
#install.packages("ROI.plugin.glpk")

library(readxl)
library(dplyr)
library(tidyr)

library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)
library(ggplot2)


# ===========================================================================
file_path <- "D:/work/RiskWise/early_sowing/Tool/Jackie_working/"
file_name <- "EP_yld_long_format.xlsx"
# ===========================================================================

# ===========================================================================
# USER INPUTS — edit these directly, no longer reading from the Excel Inputs tab
# ===========================================================================
# --- Yield decile to use ------------------------------------------------
target_decile <- "D1-3"    # matches your Excel D4-6/average-season model; change here to test other deciles later
#target_decile <- "D4-6"
#target_decile <- "D7-9"
# --- Farm settings -----------------------------------------------------
site_name <- "Lock, Eyre Peninsula"
cropping_area_ha <- 1000            # <- total cropping area, ha (fill this in)
daily_capacity_ha <- 15         # ha/day

# --- Program start date (user input) -----------------------------------------
program_start_date <- as.Date("2026-04-08")   # <- change this to whatever the user selects



# --- Sowing window calendar: date, week, days --------------------------------
window_date <- as.Date(c(
  "2026-04-08", "2026-04-18", "2026-04-28", "2026-05-08", "2026-05-18",
  "2026-05-28", "2026-06-07", "2026-06-17", "2026-06-27", "2026-07-07",
  "2026-07-17"
))

window_week <- 1:11

window_days <- rep(10, 11)   # each window is 10 days long, before any program-start adjustment

sowing_calendar <- data.frame(
  date = window_date,
  week = window_week,
  days = window_days
)

# --- Trim the calendar to only the weeks from the start date onward ----------
active_calendar <- sowing_calendar[sowing_calendar$date >= program_start_date, ]

print(active_calendar)



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

# --- Crop excluded from the Red zone (only one, all others are allowed) -----
# Set to NA if every crop is allowed in the Red zone.
red_zone_excluded_crop <- "Wheat"    # <- change to a crop name, or NA for "no restriction"

# ===========================================================================
# Solver tables etc
# ===========================================================================
# --- Combine into one active-crop target list ---------------------------
# (drops any crop with ha = 0, same as your Excel "0 to exclude" rule)
all_targets <- c(crop_targets, legume_targets)
active_crops <- names(all_targets[all_targets > 0])
crop_targets_final <- all_targets[active_crops]

print(crop_targets_final)
print(active_crops)

# ===========================================================================
# --- Yield table  ---------------------------

yield_long <- read_excel(paste0(file_path, file_name), sheet = "Yield data long format")

# Keep only Decile target, and only the crops/weeks actually in use
yield_d46 <- yield_long %>%
  filter(decile_band == target_decile,
         crop %in% active_crops,
         `week of sowing program window` %in% active_calendar$week) %>%
  mutate(crop_zone = paste(crop, frost_zone)) %>%
  select(crop_zone, week = `week of sowing program window`, yield_t_per_ha)

yield_matrix <- yield_d46 %>%
  pivot_wider(names_from = week, values_from = yield_t_per_ha) %>%
  as.data.frame()

rownames(yield_matrix) <- yield_matrix$crop_zone
yield_matrix <- as.matrix(yield_matrix[, -1])

print(dim(yield_matrix))
print(rownames(yield_matrix))


# --- Weekly sowing capacity (ha) -------------------------------------------
capacity_vec <- daily_capacity_ha * active_calendar$days
names(capacity_vec) <- active_calendar$week

print(capacity_vec)


# --- Index sets for the model ------------------------------------------
crops <- active_crops                    # e.g. "Wheat" "Barley" "Lupins" "Beans"
zones <- c("Green", "Amber", "Red")
weeks <- active_calendar$week            # e.g. 4 5 6 7 8 9 10 11

n_crops <- length(crops)
n_zones <- length(zones)
n_weeks <- length(weeks)

print(n_crops)
print(n_zones)
print(n_weeks)

# --- Model ------------------------------------------
model <- MILPModel() %>%
  add_variable(ha[c, z, w], c = 1:n_crops, z = 1:n_zones, w = 1:n_weeks, type = "continuous", lb = 0) %>%
  add_variable(active[c, w], c = 1:n_crops, w = 1:n_weeks, type = "binary") %>%
  add_variable(start[c, w], c = 1:n_crops, w = 1:n_weeks, type = "continuous", lb = 0)

print(model)

# --- Model Check 1 ------------------------------------------
#For each crop, in each week, this constraint says: 
#the total hectares sown across all three zones this week can't exceed that week's 
#sowing capacity — and if the crop isn't active this week, it can't be sown at all.

model <- model %>%
  add_constraint(
    sum_expr(ha[c, z, w], z = 1:n_zones) <= capacity_vec[w] * active[c, w],
    c = 1:n_crops, w = 1:n_weeks
  )

print(model)

# --- Model Check 1b shared weekly capacity -----------------------------------------
# Check 1 only limits each crop's OWN ha against capacity individually — it
# doesn't stop two crops sharing a handover week from together exceeding what
# the sowing rig can physically cover. This adds the missing combined cap:
# total ha across ALL crops and zones, per week, can't exceed that week's
# real capacity.

model <- model %>%
  add_constraint(
    sum_expr(ha[c, z, w], c = 1:n_crops, z = 1:n_zones) <= capacity_vec[w],
    w = 1:n_weeks
  )

print(model)


# --- Model Check 2 ------------------------------------------
# For each crop, in each week, we ask: is this the first week this crop turned on? 
# If yes, start gets forced to 1. If not (either the crop was already active last week, 
# or it's not active at all this week), 
# start is free to sit at 0


model <- model %>%
  add_constraint(
    start[c, 1] >= active[c, 1],
    c = 1:n_crops
  ) %>%
  add_constraint(
    start[c, w] >= active[c, w] - active[c, w - 1],
    c = 1:n_crops, w = 2:n_weeks
  )

print(model)


# --- Model Check 3 ------------------------------------------
# This adds up every start flag across the whole season for a given crop, and caps that total at 1. 
# Since start only ever gets forced to 1 in the exact week a crop switches from off to on (that's what Check 2 does), 
# summing them tells you how many times that crop turned on across the entire program. 
# Capping the sum at 1 means: once a crop has been sown and then stops, 
#it's locked out from being sown again later in the season — no starting, finishing, and restarting.

model <- model %>%
  add_constraint(
    sum_expr(start[c, w], w = 1:n_weeks) <= 1,
    c = 1:n_crops
  )

print(model)


# --- Model Check 3 ------------------------------------------
# For each week, this adds up how many crops are switched "active" at the same time, 
# and caps that total at 2. Most weeks this will just enforce "only one crop can be sown at a time" in practice
# — but it deliberately allows a brief overlap of two crops in the same week, 
# which is what makes a clean handover possible: 
# crop A finishing its last few hectares in the same week crop B begins its first. 
# Without this cap, nothing would stop the solver deciding three or four crops could all share a week, 
# which doesn't reflect how one sowing rig can actually work.

model <- model %>%
  add_constraint(
    sum_expr(active[c, w], c = 1:n_crops) <= 2,
    w = 1:n_weeks
  )

print(model)



# --- Model Target constraints ------------------------------------------
# For each crop, this adds up every hectare sown across all three zones and every week of the program, 
# and forces that total to equal exactly the target ha you set in your inputs 
# (400 for Wheat, 400 for Barley, 100 for Lupins, 100 for Beans). 
# This is the constraint that actually makes the model do its job — everything else (the four checks) 
# governs how and when sowing can happen, 
# but this is what guarantees the full crop program actually gets planted, not just some feasible subset of it.

model <- model %>%
  add_constraint(
    sum_expr(ha[c, z, w], z = 1:n_zones, w = 1:n_weeks) == crop_targets_final[c],
    c = 1:n_crops
  )

print(model)
# --- Model zone cap constraints ------------------------------------------
# For each zone, this adds up every hectare sown there across all crops and
# all weeks, and caps that total at how much of that zone actually exists on
# the farm (zone_ha). Without this, the solver has no reason not to put
# everything in whichever zone yields best — this is what forces it to
# actually spread sowing across Green/Amber/Red in proportion to your real
# farm, not just the highest-yielding option.

model <- model %>%
  add_constraint(
    sum_expr(ha[c, z, w], c = 1:n_crops, w = 1:n_weeks) <= zone_ha[z],
    z = 1:n_zones
  )

print(model)

# --- Model red-zone restriction -----------------------------------------
# Forces ha to zero for the one excluded crop in the Red zone (skipped
# entirely if red_zone_excluded_crop is NA — every crop allowed in Red)

if (!is.na(red_zone_excluded_crop)) {
  model <- model %>%
    add_constraint(
      ha[c, z, w] == 0,
      c = 1:n_crops, z = 1:n_zones, w = 1:n_weeks,
      zones[z] == "Red",
      crops[c] == red_zone_excluded_crop
    )
}

print(model)

# --- Model objective  ------------------------------------------

# This is the line that actually hands your finished model over to the solver and says "go find the answer." 
# model is everything you've built — every decision variable, every constraint, the objective. with_ROI(solver = "glpk", ...)
# ifically to use GLPK (the free solver engine you installed) to do the searching, rather than any other backend. 
# verbose = TRUE just means GLPK will print out its progress as it works, rather than solving silently — useful to watch, 
# especially the first time, so you can see it's actively working through the possibilities rather than stuck.

# Build yield lookups, then flatten into a single-key vector (avoids 3D array
# indexing inside sum_expr, which appears to be what's breaking)
yield_array <- array(NA_real_, dim = c(n_crops, n_zones, n_weeks))

for (ci in 1:n_crops) {
  for (zi in 1:n_zones) {
    for (wi in 1:n_weeks) {
      crop_zone_label <- paste(crops[ci], zones[zi])
      week_label <- as.character(weeks[wi])
      yield_array[ci, zi, wi] <- yield_matrix[crop_zone_label, week_label]
    }
  }
}

print(sum(is.na(yield_array)))   # must print 0 — confirmed already, should still be 0

# Flatten into a named vector keyed by "c_z_w", same style as capacity_vec[w]
yield_vec <- c()
for (ci in 1:n_crops) {
  for (zi in 1:n_zones) {
    for (wi in 1:n_weeks) {
      key <- paste(ci, zi, wi, sep = "_")
      yield_vec[key] <- yield_array[ci, zi, wi]
    }
  }
}

model <- model %>%
  set_objective(
    sum_expr(yield_vec[paste(c, z, w, sep = "_")] * ha[c, z, w],
             c = 1:n_crops, z = 1:n_zones, w = 1:n_weeks),
    sense = "max"
  )

print(model)

# --- Model solve  ------------------------------------------

result <- solve_model(model, with_ROI(solver = "glpk", verbose = TRUE))

print(result)

# --- Model program  ------------------------------------------
#library(ompr)

ha_solution <- get_solution(result, ha[c, z, w])

# Map the integer indices back to real crop/zone/week names
ha_solution$crop <- crops[ha_solution$c]
ha_solution$zone <- zones[ha_solution$z]
ha_solution$week <- weeks[ha_solution$w]

# Keep only the combinations that actually got sown (drop the zero rows)
sowing_plan <- ha_solution[ha_solution$value > 0, c("crop", "zone", "week", "value")]
sowing_plan <- sowing_plan[order(sowing_plan$crop, sowing_plan$week), ]

print(sowing_plan)
arrange(sowing_plan,week)

# ===========================================================================
# Results and program etc
# ===========================================================================
###1. Wide weekly table
# Attach real calendar dates to the solved plan
sowing_plan_dated <- sowing_plan %>%
  left_join(active_calendar[, c("week", "date")], by = "week")

# --- Weekly table: one row per week, one column per zone --------------------
zone_summary <- sowing_plan_dated %>%
  group_by(week, date, zone) %>%
  summarise(entry = paste0(crop, ": ", value, " ha", collapse = " + "), .groups = "drop")

weekly_table <- zone_summary %>%
  pivot_wider(names_from = zone, values_from = entry) %>%
  arrange(week)

print(weekly_table)


#2. Balance checks — farm total, per-crop targets, weekly capacity

# --- Farm-wide total sown vs cropping area ----------------------------------
total_sown <- sum(sowing_plan$value)
cat("Site:", site_name, "| Decile:", target_decile,
    "| Total ha sown:", total_sown, "/ Cropping area:", cropping_area_ha, "\n")

# --- Per-crop totals vs targets ---------------------------------------------
crop_check <- sowing_plan %>%
  group_by(crop) %>%
  summarise(ha_sown = sum(value), .groups = "drop") %>%
  mutate(target_ha = crop_targets_final[crop],
         diff = ha_sown - target_ha)

print(crop_check)

# --- Weekly capacity check ---------------------------------------------------
weekly_totals <- sowing_plan_dated %>%
  group_by(week, date) %>%
  summarise(total_ha_sown = sum(value), .groups = "drop") %>%
  mutate(capacity_ha = capacity_vec[as.character(week)],
         capacity_slack = capacity_ha - total_ha_sown)

print(weekly_totals)

# --- Weekly totals, to add as an extra column on the chart -----------------
weekly_totals <- sowing_plan_dated %>%
  group_by(week) %>%
  summarise(ha = sum(value), .groups = "drop")

crop_priority <- c("Wheat", "Barley", "Canola")
crop_order <- c(intersect(crop_priority, crops), setdiff(crops, crop_priority))

n_crop_cols <- length(crop_order)
weekly_totals$x_pos <- n_crop_cols + 1   # sits one column to the right of the last crop


# --- Crop totals (across all weeks), for the bottom row --------------------
crop_totals <- sowing_plan_dated %>%
  group_by(crop) %>%
  summarise(ha = sum(value), .groups = "drop")

crop_totals$crop <- factor(crop_totals$crop, levels = crop_order)
crop_totals$x_pos <- as.numeric(crop_totals$crop)
crop_totals$y_pos <- n_weeks + 1   # sits one row below the last week


# --- Grand total (bottom-right corner: farm total ha) -----------------------
grand_total <- data.frame(
  x_pos = n_crop_cols + 1,
  y_pos = n_weeks + 1,
  ha = sum(sowing_plan_dated$value)
)


# ===========================================================================
# Gantt-style chart, with Total column AND Total row added
# ===========================================================================
gantt_data <- sowing_plan_dated %>%
  group_by(crop, zone, week) %>%
  summarise(ha = sum(value), .groups = "drop")

gantt_data$crop <- factor(gantt_data$crop, levels = crop_order)
gantt_data$crop_num <- as.numeric(gantt_data$crop)

gantt_data <- gantt_data %>%
  group_by(crop, week) %>%
  mutate(n_seg = n(),
         seg_index = row_number(),
         seg_width = 0.8 / n_seg,
         x_pos = crop_num - 0.4 + seg_width * (seg_index - 0.5)) %>%
  ungroup()

p <- ggplot() +
  geom_tile(data = gantt_data, aes(x = x_pos, y = week, fill = zone, width = seg_width),
            color = "white", linewidth = 0.4, height = 0.8) +
  geom_text(data = gantt_data, aes(x = x_pos, y = week, label = sprintf("%.0f", ha)),
            color = "white", fontface = "bold", size = 3) +
  geom_tile(data = weekly_totals, aes(x = x_pos, y = week),
            fill = "grey70", color = "white", linewidth = 0.4, width = 0.8, height = 0.8) +
  geom_text(data = weekly_totals, aes(x = x_pos, y = week, label = sprintf("%.0f", ha)),
            color = "white", fontface = "bold", size = 3) +
  geom_tile(data = crop_totals, aes(x = x_pos, y = y_pos),
            fill = "grey70", color = "white", linewidth = 0.4, width = 0.8, height = 0.8) +
  geom_text(data = crop_totals, aes(x = x_pos, y = y_pos, label = sprintf("%.0f", ha)),
            color = "white", fontface = "bold", size = 3) +
  geom_tile(data = grand_total, aes(x = x_pos, y = y_pos),
            fill = "grey70", color = "white", linewidth = 0.4, width = 0.8, height = 0.8) +
  geom_text(data = grand_total, aes(x = x_pos, y = y_pos, label = sprintf("%.0f", ha)),
            color = "white", fontface = "bold", size = 3) +
  scale_fill_manual(values = c(Green = "#2E7D32", Amber = "#F9A825", Red = "#C62828")) +
  scale_x_continuous(breaks = 1:(n_crop_cols + 1), labels = c(crop_order, "Total"),
                     position = "top", expand = expansion(add = 0.5)) +
  scale_y_reverse(breaks = 1:(n_weeks + 1), labels = c(as.character(1:n_weeks), "Total")) +
  labs(title = paste0("Sowing Program — ", site_name, " (", target_decile, ")"),
       x = NULL, y = "Week", fill = "Zone") +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(face = "bold", size = 12))

print(p)

# --- Solver output -----------------------------------------------------
file_path <- "D:/work/RiskWise/early_sowing/Tool/Jackie_working/"
# --- Save the D1-3 results before switching decile --------------------------
# sowing_plan_d13 <- sowing_plan_dated
# result_d13 <- result
# objective_d13 <- result$objective_value
# write.csv(sowing_plan_dated, paste0(file_path, "sowing_plan_D1-3.csv"), row.names = FALSE)
# ggsave(paste0(file_path, "sowing_gantt_D1-3.png"), p, width = 9, height = 6, dpi = 300)


# # --- Save the D4-6 results before switching decile --------------------------
# sowing_plan_d46 <- sowing_plan_dated
# result_d46 <- result
# objective_d46 <- result$objective_value
# write.csv(sowing_plan_dated, paste0(file_path, "sowing_plan_D4-6.csv"), row.names = FALSE)
# ggsave(paste0(file_path, "sowing_gantt_D4-6.png"), p, width = 9, height = 6, dpi = 300)

# # --- Save the D7-9 results before switching decile --------------------------
# sowing_plan_d79 <- sowing_plan_dated
# result_d79 <- result
# objective_d79 <- result$objective_value
# write.csv(sowing_plan_dated, paste0(file_path, "sowing_plan_D7-9.csv"), row.names = FALSE)
# ggsave(paste0(file_path, "sowing_gantt_D7-9.png"), p, width = 9, height = 6, dpi = 300)
