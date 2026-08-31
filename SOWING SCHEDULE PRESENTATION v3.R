# ===============================================================================
# SOWING SCHEDULE PRESENTATION v3
# ===============================================================================

# ===============================================================================
# SECTION 1 — LIBRARIES
# ===============================================================================
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)
library(grid)
library(gridExtra)

# ===============================================================================
# SECTION 2 — LOAD DATA
# ===============================================================================
#file_path <- "D:/work/RiskWise/early_sowing/Tool/Jackie_working/"
getwd()
file_path <- "C:/Users/ouz001/working_from_home_post_Sep2022/Risk_wise_sowing_sequencing_allocation/files/"
simulation_name <- "baseline_v1"    # <- change this each time you run a new scenario
#simulation_name <- "MOCK_Yldsv1"    # <- change this each time you run a new scenario
deciles <- c("D1-3", "D4-6", "D7-9")

# --- Load the run summary (inputs + outcomes for all three deciles) --------
run_summary <- read.csv(paste0(file_path, simulation_name, "_run_summary.csv"),
                        stringsAsFactors = FALSE)

# --- Load each decile's sowing plan into one combined table -----------------
plans <- lapply(deciles, function(d) {
  df <- read.csv(paste0(file_path, simulation_name, "_sowing_plan_", d, ".csv"), stringsAsFactors = FALSE)
  df$decile <- d
  df$date <- as.Date(df$date)
  df
})
all_plans <- bind_rows(plans)

# ===============================================================================
# SECTION 3 — PREP: SEGMENT WIDTHS, AXIS RANGE, CROP ORDER
# ===============================================================================

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
# SECTION 4 — DECILE PLOT FUNCTION
# ===============================================================================
build_decile_plot <- function(decile_label, all_plans, crop_order, axis_start, axis_end,
                              spacing_unit = 4, half_height = 1.8) {
  
  plot_data <- all_plans %>% filter(decile == decile_label)
  
  # --- One row per week, horizontal split for multi-zone weeks -------------
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
  
  # --- Uniform week spacing (decoupled from real day-gaps) -----------------
  plot_data$week_index <- as.numeric(round((plot_data$date - axis_start) / 10)) + 1
  plot_data$y_pos <- -plot_data$week_index * spacing_unit
  
  date_seq <- seq(axis_start, axis_end, by = "10 days")
  week_idx <- 1:length(date_seq)
  y_breaks <- -week_idx * spacing_unit
  y_labels <- format(date_seq, "%d %b")
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
    labs(x = NULL, y = NULL, title = decile_label) +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          legend.position = "none")
}

# ===============================================================================
# SECTION 5 — BUILD THE THREE PLOTS
# ===============================================================================
p_d13 <- build_decile_plot("D1-3", all_plans, crop_order, axis_start, axis_end)
p_d46 <- build_decile_plot("D4-6", all_plans, crop_order, axis_start, axis_end)
p_d79 <- build_decile_plot("D7-9", all_plans, crop_order, axis_start, axis_end)

print(p_d13)
print(p_d46)
print(p_d79)

# ===============================================================================
# SECTION 6 — HEADER DATA
# ===============================================================================
# --- All these are the same across every decile row, so just take row 1 ----
header_info <- run_summary[1, ]

sim_name          <- header_info$simulation_name
site              <- header_info$site_name
area_ha           <- header_info$cropping_area_ha
capacity_ha_day   <- header_info$daily_capacity_ha
start_date        <- header_info$program_start_date
red_excl_crop     <- header_info$red_zone_excluded_crop

zone_green <- header_info$zone_green_ha
zone_amber <- header_info$zone_amber_ha
zone_red   <- header_info$zone_red_ha

# --- Coloured zone box function -----------------------------------------
build_zone_boxes <- function(header_info) {
  
  zones <- data.frame(
    label = c("Green", "Amber", "Red"),
    ha = c(header_info$zone_green_ha, header_info$zone_amber_ha, header_info$zone_red_ha),
    fill = c("#EAF3DE", "#FAEEDA", "#FCEBEB"),
    text_col = c("#173404", "#412402", "#501313")
  )
  
  zone_grobs <- lapply(1:3, function(i) {
    box  <- rectGrob(gp = gpar(fill = zones$fill[i], col = NA))
    label <- textGrob(paste0(zones$label[i], "   ", zones$ha[i], " ha"),
                      gp = gpar(fontsize = 10, col = zones$text_col[i]))
    grobTree(box, label)
  })
  
  plot_grid(plotlist = zone_grobs, ncol = 3)
}

# ===============================================================================
# SECTION 7 — HEADER BUILD FUNCTION
# ===============================================================================
build_header <- function(header_info) {
  
  title_text <- textGrob(paste0(header_info$simulation_name, " — sowing program report"),
                         x = 0, hjust = 0, gp = gpar(fontsize = 15, fontface = "bold"))
  subtitle_text <- textGrob(header_info$site_name,
                            x = 0, hjust = 0, gp = gpar(fontsize = 11, col = "grey40"))
  
  inputs_text <- textGrob(
    paste0("Cropping area: ", header_info$cropping_area_ha, " ha    |    ",
           "Sowing capacity: ", header_info$daily_capacity_ha, " ha/day    |    ",
           "Program start: ", header_info$program_start_date, "    |    ",
           "Red zone excluded: ", header_info$red_zone_excluded_crop),
    x = 0, hjust = 0, gp = gpar(fontsize = 10)
  )
  
  plot_grid(title_text, subtitle_text, inputs_text, build_zone_boxes(header_info),
            ncol = 1, rel_heights = c(1.3, 0.8, 0.8, 1.2))
}

header_panel <- build_header(header_info)
print(header_panel)

# ===============================================================================
# SECTION 8 — COMBINE INTO FINAL REPORT
# ===============================================================================
# --- Build a "sown vs capacity" table for each decile --------------------
build_capacity_table <- function(decile_label, all_plans, header_info) {
  
  weekly_sown <- all_plans %>%
    filter(decile == decile_label) %>%
    group_by(date) %>%
    summarise(sown = sum(value), .groups = "drop") %>%
    filter(sown > 0) %>%
    arrange(date)
  
  capacity_ha <- header_info$daily_capacity_ha * 10   # fixed 10-day window, same assumption used throughout this script
  
  cap_df <- data.frame(
    Date = format(weekly_sown$date, "%d %b"),
    `Sown / Capacity` = paste0(weekly_sown$sown, "ha / ", capacity_ha, "ha")
  )
  names(cap_df) <- c("", "")
  
  tableGrob(cap_df, rows = NULL,
            theme = ttheme_minimal(base_size = 9, padding = unit(c(4, 2), "mm")))
}



# --- Build a "what did we sow" table for each decile --------------------
build_sown_table <- function(decile_label, run_summary, crop_order) {
  row <- run_summary[run_summary$decile == decile_label, ]
  sown_df <- data.frame(
    Crop = crop_order,
    `Ha sown` = as.numeric(row[1, crop_order])
  )
  names(sown_df) <- c("", "")
  tableGrob(sown_df, rows = NULL,
            theme = ttheme_minimal(base_size = 9,
                                   padding = unit(c(4, 2), "mm")))
}


# --- Build a yield label for each decile ------------------------------
build_yield_label <- function(decile_label, run_summary) {
  yield_val <- run_summary$objective_expected_yield[run_summary$decile == decile_label]
  textGrob(paste0("Expected yield: ", format(round(yield_val, 0), big.mark = ","), " t"),
           gp = gpar(fontsize = 11, fontface = "bold"))
}

# --- Build each row of the report, one section at a time --------------------
plots_row <- plot_grid(p_d13, p_d46, p_d79, ncol = 3)

sown_header <- textGrob("Area sown (ha)", x = 0, hjust = 0, gp = gpar(fontsize = 11, fontface = "bold"))
sown_row <- plot_grid(
  build_sown_table("D1-3", run_summary, crop_order),
  build_sown_table("D4-6", run_summary, crop_order),
  build_sown_table("D7-9", run_summary, crop_order),
  ncol = 3
)

capacity_header <- textGrob("Crop area sown / capacity to sow", x = 0, hjust = 0, gp = gpar(fontsize = 11, fontface = "bold"))
capacity_row <- plot_grid(
  build_capacity_table("D1-3", all_plans, header_info),
  build_capacity_table("D4-6", all_plans, header_info),
  build_capacity_table("D7-9", all_plans, header_info),
  ncol = 3
)

yield_row <- plot_grid(
  build_yield_label("D1-3", run_summary),
  build_yield_label("D4-6", run_summary),
  build_yield_label("D7-9", run_summary),
  ncol = 3
)

# --- Combine header + all rows into the final report -------------------
final_report <- plot_grid(
  header_panel,
  plots_row,
  sown_header,
  sown_row,
  capacity_header,
  capacity_row,
  yield_row,
  ncol = 1,
  rel_heights = c(0.3, 1, 0.08, 0.25, 0.08, 0.4, 0.08)
)
print(final_report)
ggsave(paste0(file_path, simulation_name, "_report.png"), final_report,
       width = 16, height = 9, dpi = 150)


ggsave(paste0(file_path, simulation_name, "_report.png"), final_report,
       width = 16, height = 9, dpi = 150)
