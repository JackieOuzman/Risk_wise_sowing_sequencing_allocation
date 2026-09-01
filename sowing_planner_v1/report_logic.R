# ===============================================================================
# REPORT LOGIC — builds the composite sowing program report from a model result
# ===============================================================================
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)
library(grid)
library(gridExtra)

build_sowing_report <- function(result) {
  
  run_summary <- result$run_summary
  grain_prices <- result$grain_price_table
  variable_costs <- result$variable_cost_table
  
  all_plans <- bind_rows(lapply(names(result$all_results), function(d) {
    df <- result$all_results[[d]]$sowing_plan
    df$decile <- d
    df
  }))
  
  deciles <- names(result$all_results)
  
  # ===========================================================================
  # PREP: SEGMENT WIDTHS, AXIS RANGE, CROP ORDER
  # ===========================================================================
  all_plans <- all_plans %>%
    arrange(decile, crop, week) %>%
    group_by(decile, crop, week) %>%
    mutate(week_total = sum(value),
           seg_start = date + 10 * (cumsum(value) - value) / week_total,
           seg_end   = date + 10 * cumsum(value) / week_total,
           mid_date  = seg_start + (seg_end - seg_start) / 2) %>%
    ungroup()
  
  axis_start <- as.Date(run_summary$program_start_date[1])
  axis_end   <- axis_start + 110
  
  crop_priority <- c("Wheat", "Barley", "Canola")
  crop_order <- c(intersect(crop_priority, unique(all_plans$crop)),
                  setdiff(unique(all_plans$crop), crop_priority))
  
  # ===========================================================================
  # DECILE PLOT FUNCTION
  # ===========================================================================
  build_decile_plot <- function(decile_label, all_plans, crop_order, axis_start, axis_end,
                                spacing_unit = 4, half_height = 1.8) {
    
    plot_data <- all_plans %>% filter(decile == decile_label)
    
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
      geom_text(aes(x = x_mid, y = y_pos, label = round(value)),
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
  
  p_d13 <- build_decile_plot("D1-3", all_plans, crop_order, axis_start, axis_end)
  p_d46 <- build_decile_plot("D4-6", all_plans, crop_order, axis_start, axis_end)
  p_d79 <- build_decile_plot("D7-9", all_plans, crop_order, axis_start, axis_end)
  
  # ===========================================================================
  # HEADER
  # ===========================================================================
  header_info <- run_summary[1, ]
  
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
  
  build_header <- function(header_info) {
    title_text <- textGrob(paste0(header_info$simulation_name, " — sowing program report"),
                           x = 0, hjust = 0, gp = gpar(fontsize = 15, fontface = "bold"))
    subtitle_text <- textGrob(header_info$site_name,
                              x = 0, hjust = 0, gp = gpar(fontsize = 11, col = "grey40"))
    
    optimise_label <- if (header_info$optimise_for == "gm") "Gross Margin" else "Yield"
    
    inputs_text <- textGrob(
      paste0("Cropping area: ", header_info$cropping_area_ha, " ha    |    ",
             "Sowing capacity: ", header_info$daily_capacity_ha, " ha/day    |    ",
             "Program start: ", header_info$program_start_date, "    |    ",
             "Red zone excluded: ", header_info$red_zone_excluded_crop, "    |    ",
             "Optimising for: ", optimise_label, "    |    ",
             "Price scenario: ", header_info$price_scenario),
      x = 0, hjust = 0, gp = gpar(fontsize = 10)
    )
    
    plot_grid(title_text, subtitle_text, inputs_text, build_zone_boxes(header_info),
              ncol = 1, rel_heights = c(1.3, 0.8, 0.8, 1.2))
  }
  
  header_panel <- build_header(header_info)
  
  # ===========================================================================
  # TABLES AND LABELS
  # ===========================================================================
  build_price_cost_table <- function(grain_prices, variable_costs, price_scenario, crop_order) {
    combined <- grain_prices %>%
      select(crop, price = all_of(price_scenario)) %>%
      left_join(variable_costs, by = "crop")
    
    transposed <- data.frame(
      rbind(combined$price, combined$`D1-3`, combined$`D4-6`, combined$`D7-9`)
    )
    colnames(transposed) <- combined$crop
    transposed <- cbind(Metric = c("Price ($/t)", "VC D1-3 ($/ha)", "VC D4-6 ($/ha)", "VC D7-9 ($/ha)"),
                        transposed)
    names(transposed)[1] <- ""
    
    fontface_vec <- c("plain", ifelse(combined$crop %in% crop_order, "bold", "plain"))
    
    tableGrob(transposed, rows = NULL,
              theme = ttheme_minimal(base_size = 9, padding = unit(c(4, 2), "mm"),
                                     colhead = list(fg_params = list(fontface = fontface_vec))))
  }
  
  build_capacity_table <- function(decile_label, all_plans, header_info) {
    weekly_sown <- all_plans %>%
      filter(decile == decile_label) %>%
      group_by(date) %>%
      summarise(sown = sum(value), .groups = "drop") %>%
      filter(sown > 0) %>%
      arrange(date)
    
    capacity_ha <- header_info$daily_capacity_ha * 10
    
    cap_df <- data.frame(
      Date = format(weekly_sown$date, "%d %b"),
      `Sown / Capacity` = paste0(weekly_sown$sown, "ha / ", capacity_ha, "ha")
    )
    names(cap_df) <- c("", "")
    
    tableGrob(cap_df, rows = NULL,
              theme = ttheme_minimal(base_size = 9, padding = unit(c(4, 2), "mm")))
  }
  
  build_sown_table <- function(decile_label, run_summary, crop_order) {
    row <- run_summary[run_summary$decile == decile_label, ]
    sown_df <- data.frame(
      Crop = crop_order,
      `Ha sown` = as.numeric(row[1, crop_order])
    )
    names(sown_df) <- c("", "")
    tableGrob(sown_df, rows = NULL,
              theme = ttheme_minimal(base_size = 9, padding = unit(c(4, 2), "mm")))
  }
  
  build_yield_label <- function(decile_label, run_summary) {
    yield_val <- run_summary$expected_yield_t[run_summary$decile == decile_label]
    textGrob(paste0("Expected yield: ", format(round(yield_val, 0), big.mark = ","), " t"),
             gp = gpar(fontsize = 11, fontface = "bold"))
  }
  
  build_gm_label <- function(decile_label, run_summary) {
    gm_val <- run_summary$expected_gm_dollars[run_summary$decile == decile_label]
    textGrob(paste0("Expected GM: $", format(round(gm_val, 0), big.mark = ",")),
             gp = gpar(fontsize = 11, fontface = "bold"))
  }
  
  # ===========================================================================
  # ASSEMBLE THE FINAL REPORT
  # ===========================================================================
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
  
  gm_row <- plot_grid(
    build_gm_label("D1-3", run_summary),
    build_gm_label("D4-6", run_summary),
    build_gm_label("D7-9", run_summary),
    ncol = 3
  )
  
  price_cost_panel <- build_price_cost_table(grain_prices, variable_costs, header_info$price_scenario, crop_order)
  
  final_report <- plot_grid(
    header_panel,
    price_cost_panel,
    plots_row,
    sown_header,
    sown_row,
    capacity_header,
    capacity_row,
    yield_row,
    gm_row,
    ncol = 1,
    rel_heights = c(0.3, 0.35, 1, 0.08, 0.25, 0.08, 0.4, 0.08, 0.08)
  )
  
  return(final_report)
}