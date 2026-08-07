# ===============================================================================
# DECILE COMPARISON — runs the full solve fresh for D1-3, D4-6, D7-9
# ===============================================================================
# Requires these to already exist in your environment (from your main script):
#   yield_long, active_crops, crops, zones, weeks,
#   n_crops, n_zones, n_weeks, capacity_vec, crop_targets_final, zone_ha,
#   red_zone_excluded_crop, active_calendar, crop_order, site_name, file_path
#
# For each decile, this rebuilds the yield table, builds a FRESH model,
# solves it, extracts the sowing plan, and saves a CSV + Gantt PNG — so
# there's no risk of accidentally reusing a stale result from a different
# decile (the bug that caused all three CSVs to come out identical before).
# ===============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)

required_objects <- c("yield_long", "active_crops", "crops", "zones", "weeks",
                      "n_crops", "n_zones", "n_weeks", "capacity_vec",
                      "crop_targets_final", "zone_ha", "red_zone_excluded_crop",
                      "active_calendar", "crop_order", "site_name", "file_path")

missing <- setdiff(required_objects, ls())
print(missing)


deciles_to_run <- c("D1-3", "D4-6", "D7-9")

# Store results from each decile for a final comparison
all_results <- list()

# --- Helper function: builds the Gantt chart for a given solved plan --------
build_gantt <- function(sowing_plan_dated, decile_label, crop_order, n_weeks, site_name) {
  
  n_crop_cols <- length(crop_order)
  
  weekly_totals <- sowing_plan_dated %>%
    group_by(week) %>%
    summarise(ha = sum(value), .groups = "drop") %>%
    mutate(x_pos = n_crop_cols + 1)
  
  crop_totals <- sowing_plan_dated %>%
    group_by(crop) %>%
    summarise(ha = sum(value), .groups = "drop") %>%
    mutate(crop = factor(crop, levels = crop_order),
           x_pos = as.numeric(crop),
           y_pos = n_weeks + 1)
  
  grand_total <- data.frame(
    x_pos = n_crop_cols + 1,
    y_pos = n_weeks + 1,
    ha = sum(sowing_plan_dated$value)
  )
  
  gantt_data <- sowing_plan_dated %>%
    group_by(crop, zone, week) %>%
    summarise(ha = sum(value), .groups = "drop") %>%
    mutate(crop = factor(crop, levels = crop_order),
           crop_num = as.numeric(crop)) %>%
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
              fill = "grey30", color = "white", linewidth = 0.4, width = 0.8, height = 0.8) +
    geom_text(data = weekly_totals, aes(x = x_pos, y = week, label = sprintf("%.0f", ha)),
              color = "white", fontface = "bold", size = 3) +
    geom_tile(data = crop_totals, aes(x = x_pos, y = y_pos),
              fill = "grey30", color = "white", linewidth = 0.4, width = 0.8, height = 0.8) +
    geom_text(data = crop_totals, aes(x = x_pos, y = y_pos, label = sprintf("%.0f", ha)),
              color = "white", fontface = "bold", size = 3) +
    geom_tile(data = grand_total, aes(x = x_pos, y = y_pos),
              fill = "grey10", color = "white", linewidth = 0.4, width = 0.8, height = 0.8) +
    geom_text(data = grand_total, aes(x = x_pos, y = y_pos, label = sprintf("%.0f", ha)),
              color = "white", fontface = "bold", size = 3) +
    scale_fill_manual(values = c(Green = "#2E7D32", Amber = "#F9A825", Red = "#C62828")) +
    scale_x_continuous(breaks = 1:(n_crop_cols + 1), labels = c(crop_order, "Total"),
                       position = "top", expand = expansion(add = 0.5)) +
    scale_y_reverse(breaks = 1:(n_weeks + 1), labels = c(as.character(1:n_weeks), "Total")) +
    labs(title = paste0("Sowing Program — ", site_name, " (", decile_label, ")"),
         x = NULL, y = "Week", fill = "Zone") +
    theme_minimal(base_size = 12) +
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(face = "bold", size = 12))
  
  return(p)
}


# ===============================================================================
# Main loop — runs the full solve fresh for each decile
# ===============================================================================
for (target_decile in deciles_to_run) {
  
  cat("\n========================================\n")
  cat("Solving for decile:", target_decile, "\n")
  cat("========================================\n")
  
  # --- Rebuild yield table for this decile -----------------------------------
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
  
  # --- Rebuild yield array + flattened lookup vector --------------------------
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
  
  na_count <- sum(is.na(yield_array))
  if (na_count > 0) {
    stop(paste("Missing", na_count, "yield lookups for decile", target_decile,
               "— check yield_matrix coverage before continuing."))
  }
  
  yield_vec <- c()
  for (ci in 1:n_crops) {
    for (zi in 1:n_zones) {
      for (wi in 1:n_weeks) {
        key <- paste(ci, zi, wi, sep = "_")
        yield_vec[key] <- yield_array[ci, zi, wi]
      }
    }
  }
  
  # --- Build a FRESH model for this decile -------------------------------------
  model <- MILPModel() %>%
    add_variable(ha[c, z, w], c = 1:n_crops, z = 1:n_zones, w = 1:n_weeks, type = "continuous", lb = 0) %>%
    add_variable(active[c, w], c = 1:n_crops, w = 1:n_weeks, type = "binary") %>%
    add_variable(start[c, w], c = 1:n_crops, w = 1:n_weeks, type = "continuous", lb = 0) %>%
    add_constraint(
      sum_expr(ha[c, z, w], z = 1:n_zones) <= capacity_vec[w] * active[c, w],
      c = 1:n_crops, w = 1:n_weeks
    ) %>%
    add_constraint(
      sum_expr(ha[c, z, w], c = 1:n_crops, z = 1:n_zones) <= capacity_vec[w],
      w = 1:n_weeks
    ) %>%
    add_constraint(
      start[c, 1] >= active[c, 1],
      c = 1:n_crops
    ) %>%
    add_constraint(
      start[c, w] >= active[c, w] - active[c, w - 1],
      c = 1:n_crops, w = 2:n_weeks
    ) %>%
    add_constraint(
      sum_expr(start[c, w], w = 1:n_weeks) <= 1,
      c = 1:n_crops
    ) %>%
    add_constraint(
      sum_expr(active[c, w], c = 1:n_crops) <= 2,
      w = 1:n_weeks
    ) %>%
    add_constraint(
      sum_expr(ha[c, z, w], z = 1:n_zones, w = 1:n_weeks) == crop_targets_final[c],
      c = 1:n_crops
    ) %>%
    add_constraint(
      sum_expr(ha[c, z, w], c = 1:n_crops, w = 1:n_weeks) <= zone_ha[z],
      z = 1:n_zones
    )
  
  if (!is.na(red_zone_excluded_crop)) {
    model <- model %>%
      add_constraint(
        ha[c, z, w] == 0,
        c = 1:n_crops, z = 1:n_zones, w = 1:n_weeks,
        zones[z] == "Red",
        crops[c] == red_zone_excluded_crop
      )
  }
  
  model <- model %>%
    set_objective(
      sum_expr(yield_vec[paste(c, z, w, sep = "_")] * ha[c, z, w],
               c = 1:n_crops, z = 1:n_zones, w = 1:n_weeks),
      sense = "max"
    )
  
  # --- Solve --------------------------------------------------------------
  result <- solve_model(model, with_ROI(solver = "glpk", verbose = FALSE))
  print(result)
  
  if (result$status != "success") {
    cat("WARNING:", target_decile, "did not solve successfully — skipping save.\n")
    next
  }
  
  # --- Extract the sowing plan ----------------------------------------------
  ha_solution <- get_solution(result, ha[c, z, w])
  ha_solution$crop <- crops[ha_solution$c]
  ha_solution$zone <- zones[ha_solution$z]
  ha_solution$week <- weeks[ha_solution$w]
  
  sowing_plan <- ha_solution[ha_solution$value > 0, c("crop", "zone", "week", "value")]
  sowing_plan <- sowing_plan[order(sowing_plan$crop, sowing_plan$week), ]
  
  sowing_plan_dated <- sowing_plan %>%
    left_join(active_calendar[, c("week", "date")], by = "week")
  
  # --- Save CSV -------------------------------------------------------------
  csv_path <- paste0(file_path, "sowing_plan_", target_decile, ".csv")
  write.csv(sowing_plan_dated, csv_path, row.names = FALSE)
  cat("Saved:", csv_path, "\n")
  
  # --- Build and save Gantt chart ---------------------------------------------
  p <- build_gantt(sowing_plan_dated, target_decile, crop_order, n_weeks, site_name)
  png_path <- paste0(file_path, "sowing_gantt_", target_decile, ".png")
  ggsave(png_path, p, width = 9, height = 6, dpi = 300)
  cat("Saved:", png_path, "\n")
  
  # --- Store for comparison ---------------------------------------------------
  all_results[[target_decile]] <- list(
    sowing_plan = sowing_plan_dated,
    objective = result$objective_value
  )
}


# ===============================================================================
# Final comparison across all three deciles
# ===============================================================================
cat("\n========================================\n")
cat("DECILE COMPARISON — total expected yield\n")
cat("========================================\n")
for (d in names(all_results)) {
  cat(d, ":", all_results[[d]]$objective, "\n")
}
