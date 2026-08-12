# ===============================================================================
# SOWING SCHEDULE PRESENTATION v3 — one decile, no function, plain code
# ===============================================================================
library(dplyr)
library(tidyr)
library(ggplot2)

file_path <- "D:/work/RiskWise/early_sowing/Tool/Jackie_working/"
simulation_name <- "baseline_v1"
deciles <- c("D1-3", "D4-6", "D7-9")

# --- Load the run summary (inputs + outcomes for all three deciles) --------
run_summary <- read.csv(paste0(file_path, simulation_name, "_run_summary_20260812_1643.csv"),
                        stringsAsFactors = FALSE)

# --- Load each decile's sowing plan into one combined table -----------------
plans <- lapply(deciles, function(d) {
  df <- read.csv(paste0(file_path, simulation_name, "_sowing_plan_", d, ".csv"), stringsAsFactors = FALSE)
  df$decile <- d
  df$date <- as.Date(df$date)
  df
})
all_plans <- bind_rows(plans)

# --- Proportional segment widths within each week ---------------------------
all_plans <- all_plans %>%
  arrange(decile, crop, week) %>%
  group_by(decile, crop, week) %>%
  mutate(week_total = sum(value),
         seg_start = date + 10 * (cumsum(value) - value) / week_total,
         seg_end   = date + 10 * cumsum(value) / week_total,
         mid_date  = seg_start + (seg_end - seg_start) / 2) %>%
  ungroup()

# --- Full season axis range --------------------------------------------
axis_start <- as.Date(run_summary$program_start_date[1])
axis_end   <- axis_start + 110

# --- Crop order ----------------------------------------------------------
crop_priority <- c("Wheat", "Barley", "Canola")
crop_order <- c(intersect(crop_priority, unique(all_plans$crop)),
                setdiff(unique(all_plans$crop), crop_priority))



# ===============================================================================
# ONE PLOT — D1-3 only, equal-width zone segments (not proportional to ha)
# ===============================================================================

# ===============================================================================
# ONE PLOT — D1-3 only, equal-width zone segments (not proportional to ha)
# ===============================================================================
plot_data <- all_plans %>% filter(decile == "D1-3")

# --- One row per week, horizontal split for multi-zone weeks ---------------
plot_data <- plot_data %>%
  arrange(crop, week, zone) %>%
  group_by(crop, week) %>%
  mutate(n_seg = n(),
         seg_index = row_number(),
         crop_num = as.numeric(factor(crop, levels = crop_order)),
         x_start = crop_num - 0.35 + 0.7 * (seg_index - 1) / n_seg,
         x_end   = crop_num - 0.35 + 0.7 * seg_index / n_seg,
         x_mid   = (x_start + x_end) / 2) %>%
  ungroup()

# --- Uniform week spacing (decoupled from real day-gaps) --------------------
spacing_unit <- 4
half_height  <- 1.8  # bar half-height — keep in sync with the geom_rect below

plot_data$week_index <- as.numeric(round((plot_data$date - axis_start) / 10)) + 1
plot_data$y_pos <- -plot_data$week_index * spacing_unit

date_seq  <- seq(axis_start, axis_end, by = "10 days")
week_idx  <- 1:length(date_seq)
y_breaks  <- -week_idx * spacing_unit
y_labels  <- format(date_seq, "%d %b")
n_weeks_plot <- length(date_seq)
grid_boundaries <- -(seq(0.5, n_weeks_plot + 0.5, by = 1)) * spacing_unit


ggplot(plot_data) +
  geom_hline(yintercept = grid_boundaries, color = "grey85", linewidth = 0.3) +
  geom_rect(aes(xmin = x_start, xmax = x_end,
                ymin = y_pos - half_height, ymax = y_pos + half_height,
                fill = zone), color = "white", linewidth = 0.3) +
  geom_text(aes(x = x_mid, y = y_pos, label = value),
            size = 2.8, color = "white", fontface = "bold") +
  scale_x_continuous(breaks = 1:length(crop_order), labels = crop_order, position = "top") +
  scale_y_continuous(breaks = y_breaks, labels = y_labels) +
  scale_fill_manual(values = c(Green = "#2E7D32", Amber = "#F9A825", Red = "#C62828")) +
  labs(x = NULL, y = NULL, title = "D1-3") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none")



