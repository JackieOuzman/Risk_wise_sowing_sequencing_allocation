# ===============================================================================
# SOWING SCHEDULE PLOTTING — reads saved results, builds charts
# ===============================================================================
# Standalone script — doesn't solve anything, just reads CSVs already saved
# by the main solver script and builds charts from them.
# ===============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)

# --- Which simulation and deciles to plot -----------------------------------
file_path <- "D:/work/RiskWise/early_sowing/Tool/Jackie_working/"
simulation_name <- "baseline_v1"
deciles_to_plot <- c("D1-3", "D4-6", "D7-9")

# --- Read in each decile's saved sowing plan ---------------------------------
plans <- list()

for (d in deciles_to_plot) {
  file <- paste0(file_path, simulation_name, "_sowing_plan_", d, ".csv")
  plans[[d]] <- read.csv(file, stringsAsFactors = FALSE)
  plans[[d]]$date <- as.Date(plans[[d]]$date)
  cat("Loaded", nrow(plans[[d]]), "rows for", d, "\n")
}

print(plans[["D4-6"]])

# --- Crop order (same priority as before) -----------------------------------
crop_priority <- c("Wheat", "Barley", "Canola")

build_gantt <- function(plan_data, decile_label, site_name) {
  
  crops_present <- unique(plan_data$crop)
  crop_order <- c(intersect(crop_priority, crops_present), setdiff(crops_present, crop_priority))
  n_crop_cols <- length(crop_order)
  n_weeks_plot <- max(plan_data$week)
  
  weekly_totals <- plan_data %>%
    group_by(week) %>%
    summarise(ha = sum(value), .groups = "drop") %>%
    mutate(x_pos = n_crop_cols + 1)
  
  crop_totals <- plan_data %>%
    group_by(crop) %>%
    summarise(ha = sum(value), .groups = "drop") %>%
    mutate(crop = factor(crop, levels = crop_order),
           x_pos = as.numeric(crop),
           y_pos = n_weeks_plot + 1)
  
  grand_total <- data.frame(
    x_pos = n_crop_cols + 1,
    y_pos = n_weeks_plot + 1,
    ha = sum(plan_data$value)
  )
  
  gantt_data <- plan_data %>%
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
    scale_y_reverse(breaks = 1:(n_weeks_plot + 1), labels = c(as.character(1:n_weeks_plot), "Total")) +
    labs(title = paste0("Sowing Program — ", site_name, " (", decile_label, ")"),
         x = NULL, y = "Week", fill = "Zone") +
    theme_minimal(base_size = 12) +
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(face = "bold", size = 12))
  
  return(p)
}

# --- Try it on one decile first -----------------------------------------
site_name <- "Lock, Eyre Peninsula"

p_d46 <- build_gantt(plans[["D4-6"]], "D4-6", site_name)
p_d79 <- build_gantt(plans[["D7-9"]], "D7-9", site_name)
p_d13 <- build_gantt(plans[["D1-3"]], "D1-3", site_name)

# install.packages("cowplot")
library(cowplot)

combined_plot <- plot_grid(
  p_d13, p_d46, p_d79,
  ncol = 1,
  labels = NULL
)

print(combined_plot)

ggsave(paste0(file_path, simulation_name, "_sowing_gantt_combined.png"),
       combined_plot, width = 9, height = 16, dpi = 300)
