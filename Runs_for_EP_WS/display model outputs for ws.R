library(tidyverse)

# Folder holding all the report bundle zips
bundle_dir <- "C:/Users/ouz001/working_from_home_post_Sep2022/Risk_wise_sowing_sequencing_allocation/Notes_outputs_ws_EP_Sep"

# One row per zip: filename, a clean label for plotting, and which part of
# the workshop narrative it belongs to
scenario_manifest <- tribble(
  ~zip_file,                                                ~scenario_label,        ~group,          ~optimise_for,
  "baseline_opt_GM_report_bundle.zip",                       "Baseline (150)",       "capacity",      "gm",
  "baseline_opt_yld_report_bundle.zip",                      "Baseline (150)",       "capacity",      "yield",
  "Sc2a_SowingCapacity_opt_GM_report_bundle.zip",            "High (250)",           "capacity",      "gm",
  "Sc2a_SowingCapacity_opt_Yld_report_bundle.zip",           "High (250)",           "capacity",      "yield",
  "Sc2b_SowingCapacity_low_opt_GM_report_bundle.zip",        "Low (50)",             "capacity",      "gm",
  "Sc2b_SowingCapacity_low_opt_Yld_report_bundle.zip",       "Low (50)",             "capacity",      "yield",
  
  "baseline_opt_GM_report_bundle.zip",                       "Baseline (Canola)",    "crop_mix",      "gm",
  "baseline_opt_yld_report_bundle.zip",                      "Baseline (Canola)",    "crop_mix",      "yield",
  "Sc3a_SwapCanola_Lentils_opt_GM_report_bundle (1).zip",    "Swap (Lentils)",       "crop_mix",      "gm",
  "Sc3a_SwapCanola_Lentils_opt_Yld_report_bundle (1).zip",   "Swap (Lentils)",       "crop_mix",      "yield",
  
  "baseline_opt_GM_report_bundle.zip",                       "Early (8 Apr)",        "timing",        "gm",
  "baseline_opt_yld_report_bundle.zip",                      "Early (8 Apr)",        "timing",        "yield",
  "Sc4a_Mid_start_opt_GM_report_bundle.zip",                 "Mid (18 May)",         "timing",        "gm",
  "Sc4a_Mid_start_opt_Yld_report_bundle.zip",                "Mid (18 May)",         "timing",        "yield",
  "Sc4b_late_OptGM_report_bundle.zip",                       "Late (27 Jun)",        "timing",        "gm",
  "Sc4b_late_OptYld_report_bundle.zip",                      "Late (27 Jun)",        "timing",        "yield",
  
  "Sc1b_Exclusion_W_L_opt_GM_report_bundle.zip",             "No Early wheat",       "new_crop",      "gm",
  "Sc1b_Exclusion_W_L_opt_yld_report_bundle.zip",            "No Early wheat",       "new_crop",      "yield",
  "Sc5a_Early_Wheat_OptGM_report_bundle.zip",                "+ Early wheat",        "new_crop",      "gm",
  "Sc5a_Early_Wheat_OptYld_report_bundle.zip",               "+ Early wheat",        "new_crop",      "yield",
  
  
)



scenario_manifest <- scenario_manifest %>% 
  filter(group != "new_crop") %>%  # drop the old Sc1b-based new_crop rows
  bind_rows(scenario_manifest_new_crop)

# Re-run to pick up the new bundle
unique_bundles <- unique(scenario_manifest$zip_file)
run_data <- map_dfr(unique_bundles, read_run_summary)
plot_data <- scenario_manifest %>%
  left_join(run_data, by = c("zip_file", "optimise_for")) %>%
  mutate(decile = factor(decile, levels = c("D1-3", "D4-6", "D7-9")))

# Where to unzip each bundle to (one subfolder per zip)
unzip_dir <- file.path(bundle_dir, "unzipped")
dir.create(unzip_dir, showWarnings = FALSE)

read_run_summary <- function(zip_file) {
  zip_path <- file.path(bundle_dir, zip_file)
  dest <- file.path(unzip_dir, tools::file_path_sans_ext(basename(zip_file)))
  
  if (!dir.exists(dest)) {
    unzip(zip_path, exdir = dest)
  }
  
  summary_file <- list.files(dest, pattern = "run_summary\\.csv$", full.names = TRUE)
  
  if (length(summary_file) != 1) {
    stop("Expected exactly one run_summary.csv in ", zip_file, " — found ", length(summary_file))
  }
  
  read_csv(summary_file, show_col_types = FALSE) %>%
    select(decile, optimise_for, expected_yield_t, expected_gm_dollars) %>%
    mutate(zip_file = zip_file)
}

# Unzip + read each UNIQUE bundle once
unique_bundles <- unique(scenario_manifest$zip_file)
run_data <- map_dfr(unique_bundles, read_run_summary)


##############################################################

plot_data <- scenario_manifest %>%
  left_join(run_data, by = c("zip_file", "optimise_for")) %>%
  mutate(decile = factor(decile, levels = c("D1-3", "D4-6", "D7-9")))

# Sanity check: this should return zero rows — flags any manifest entry
# that didn't find a matching result (wrong filename, wrong optimise_for, etc.)
plot_data %>% filter(is.na(expected_gm_dollars))

################################################################################
# capacity_data
###############################################################################

capacity_data <- plot_data %>%
  filter(group == "capacity", optimise_for == "gm") %>%
  mutate(capacity_ha_day = factor(scenario_label, 
                                  levels = c("Low (50)", "Baseline (150)", "High (250)"),
                                  labels = c("50", "150", "250")))

ggplot(capacity_data, aes(x = capacity_ha_day, y = expected_gm_dollars, fill = capacity_ha_day)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = scales::dollar(round(expected_gm_dollars, -3), scale = 1e-6, suffix = "M")),
            vjust = -0.5, size = 4.0) +
  facet_wrap(~ decile, nrow = 1) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  scale_fill_manual(values = c("50" = "#B8D9E8", "150" = "#3E8FC4", "250" = "#00304D")) +
  labs(#title = "Sowing capacity: diminishing returns above 150 ha/day",
    #subtitle = "Expected Gross Margin by season type",
    x = "Sowing capacity (ha/day)", y = "Expected GM ($M)") +
  theme_minimal(base_size = 16) +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text = element_text(face = "bold"),
        axis.title.x = element_text(margin = margin(t = 10)))



ggsave(
  filename = file.path(bundle_dir, "plot_01_capacity.png"),
  width = 10, height = 5.5, dpi = 300, bg = "white"
)


capacity_data <- capacity_data %>%
  mutate(capacity_story = case_when(
    capacity_ha_day == "50"  ~ "50 ha/day \u2014 backup disc seeder",
    capacity_ha_day == "150" ~ "150 ha/day \u2014 current air seeder",
    capacity_ha_day == "250" ~ "250 ha/day \u2014 wider bar or longer day"
  ),
  capacity_story = factor(capacity_story, levels = c(
    "50 ha/day \u2014 backup disc seeder",
    "150 ha/day \u2014 current air seeder",
    "250 ha/day \u2014 wider bar or longer day"
  )))

ggplot(capacity_data, aes(x = capacity_ha_day, y = expected_gm_dollars, fill = capacity_story)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = scales::dollar(round(expected_gm_dollars, -3), scale = 1e-6, suffix = "M")),
            vjust = -0.5, size = 4.0) +
  facet_wrap(~ decile, nrow = 1) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  scale_fill_manual(values = c("50 ha/day \u2014 backup disc seeder" = "#B8D9E8",
                               "150 ha/day \u2014 current air seeder" = "#3E8FC4",
                               "250 ha/day \u2014 wider bar or longer day" = "#00304D")) +
  labs(x = "Sowing capacity (ha/day)", y = "Expected GM ($M)", fill = NULL)+ 
       #caption = "A $320k machine upgrade (150 \u2192 250 ha/day)\nbuys almost nothing in Gross Margin") +
  theme_minimal(base_size = 16) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = rel(0.75)),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text = element_text(face = "bold"),
        axis.title.x = element_text(margin = margin(t = 10)),
        plot.caption = element_text(hjust = 0.5, size = 14, face = "bold",
                                    color = "#00304D", margin = margin(t = 15)))
ggsave(
  filename = file.path(bundle_dir, "plot_01_capacity_annoated.png"),
  width = 10, height = 5.5, dpi = 300, bg = "white"
)


###############################################################################
#### Crop mix plot (Canola to Lentils swap)
###############################################################################

crop_mix_data <- plot_data %>%
  filter(group == "crop_mix", optimise_for == "gm") %>%
  mutate(scenario_label = factor(scenario_label, 
                                 levels = c("Baseline (Canola)", "Swap (Lentils)")))

ggplot(crop_mix_data, aes(x = scenario_label, y = expected_gm_dollars, fill = scenario_label)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = scales::dollar(round(expected_gm_dollars, -3), scale = 1e-6, suffix = "M")),
            vjust = -0.5, size = 4.0) +
  facet_wrap(~ decile, nrow = 1) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  scale_fill_manual(values = c("Baseline (Canola)" = "#3E8FC4", "Swap (Lentils)" = "#00304D")) +
  labs(x = NULL, y = "Expected GM ($M)", fill = NULL)+
      # caption = "600 ha Canola \u2192 600 ha Lentils: costs GM in an average/poor season,\nsmall gain in a great one \u2014 driven by lentils' lower cost of production") +
  theme_minimal(base_size = 16) +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = rel(0.9)),
        strip.text = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0.5, size = 13, face = "bold",
                                    color = "#00304D", margin = margin(t = 15)))

ggsave(
  filename = file.path(bundle_dir, "plot_02_crop_mix.png"),
  width = 10, height = 5.5, dpi = 300, bg = "white"
)

#################################################################################
## Table for preso on prices and yld for lentils and canola
#################################################################################
library(gridExtra)
library(grid)

tbl_data <- data.frame(
  Decile = c("D1-3","D1-3","D1-3","D4-6","D4-6","D4-6","D7-9","D7-9","D7-9"),
  Zone = c("Green","Amber","Red","Green","Amber","Red","Green","Amber","Red"),
  `Lentils $/ha` = c("1.33\u00d7650-241=624","1.06\u00d7650-241=448","0.53\u00d7650-241=104",
                     "1.73\u00d7650-267=858","1.38\u00d7650-267=630","0.69\u00d7650-267=182",
                     "2.10\u00d7650-312=1053","1.68\u00d7650-312=780","0.84\u00d7650-312=234"),
  `Canola $/ha` = c("1.68\u00d7700-324=852","1.35\u00d7700-324=621","1.15\u00d7700-324=481",
                    "2.09\u00d7700-418=1045","1.67\u00d7700-418=751","1.42\u00d7700-418=576",
                    "2.14\u00d7700-469=1029","1.68\u00d7700-469=728","1.45\u00d7700-469=546"),
  `Yield gap` = c("+0.35","+0.29","+0.62","+0.36","+0.29","+0.73","+0.04","0.00 (tied)","+0.61"),
  Winner = c("Canola +228","Canola +173","Canola +378","Canola +188","Canola +121",
             "Canola +395","Lentils +24","Lentils +52","Canola +312"),
  check.names = FALSE
)

winner_is_lentils <- grepl("Lentils", tbl_data$Winner)

# Colour only the Winner column — everything else stays plain
fill_matrix <- matrix("white", nrow = nrow(tbl_data), ncol = ncol(tbl_data))
fill_matrix[, ncol(tbl_data)] <- ifelse(winner_is_lentils, "#FAEEDA", "#E6F1FB")

text_matrix <- matrix("black", nrow = nrow(tbl_data), ncol = ncol(tbl_data))
text_matrix[, ncol(tbl_data)] <- ifelse(winner_is_lentils, "#633806", "#0C447C")

tt <- ttheme_default(
  core = list(
    bg_params = list(fill = fill_matrix, col = "grey85"),
    fg_params = list(col = text_matrix, fontsize = 15)
  ),
  colhead = list(
    fg_params = list(col = "white", fontface = "bold", fontsize = 15),
    bg_params = list(fill = "#00304D")
  )
)

g <- tableGrob(tbl_data, rows = NULL, theme = tt)

png(file.path(bundle_dir, "table_crop_mix_margin.png"), width = 13, height = 6, units = "in", res = 300)
grid.draw(g)
dev.off()


###############################################################################
###timing
###############################################################################
# Option A — same bar template as capacity/crop-mix, for consistency:


timing_data <- plot_data %>%
  filter(group == "timing", optimise_for == "gm") %>%
  mutate(scenario_label = factor(scenario_label, 
                                 levels = c("Early (8 Apr)", "Mid (18 May)", "Late (27 Jun)")))

ggplot(timing_data, aes(x = scenario_label, y = expected_gm_dollars, fill = scenario_label)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = scales::dollar(round(expected_gm_dollars, -3), scale = 1e-6, suffix = "M")),
            vjust = -0.5, size = 4.0) +
  facet_wrap(~ decile, nrow = 1) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  scale_fill_manual(values = c("Early (8 Apr)" = "#B8D9E8", "Mid (18 May)" = "#3E8FC4", "Late (27 Jun)" = "#00304D")) +
  labs(x = NULL, y = "Expected GM ($M)", fill = NULL)+
       #caption = "Starting late costs 59-70% of GM \u2014 by far the biggest lever we've tested") +
  theme_minimal(base_size = 16) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = rel(0.75)),
        panel.grid.major.x = element_blank(), panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(), axis.text.y = element_blank(),
        axis.ticks.y = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        strip.text = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0.5, size = 13, face = "bold", color = "#00304D", margin = margin(t = 15)))

ggsave(file.path(bundle_dir, "plot_03a_timing_bar.png"), width = 10, height = 5.5, dpi = 300, bg = "white")

# Option B

ggplot(timing_data, aes(x = scenario_label, y = expected_gm_dollars, group = decile, color = decile)) +
  geom_line(linewidth = 1.4) +
  geom_point(size = 3.5) +
  geom_text(aes(label = scales::dollar(round(expected_gm_dollars, -3), scale = 1e-6, suffix = "M")),
            vjust = -1.3, size = 3.8, show.legend = FALSE) +
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.2))) +
  scale_color_manual(values = c("D1-3" = "#B8D9E8", "D4-6" = "#3E8FC4", "D7-9" = "#00304D")) +
  labs(x = "Program start date", y = "Expected GM ($M)", color = "Season type") +
  theme_minimal(base_size = 16) +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank())

ggsave(file.path(bundle_dir, "plot_03b_timing_line.png"), width = 10, height = 5.5, dpi = 300, bg = "white")

### Explain the drivers with sowing plan?
### optin A - not great 


library(gridExtra)
library(grid)

sowing_data <- data.frame(
  Scenario = c(rep("Baseline (8 Apr)", 6), rep("Late start (27 Jun)", 6)),
  Crop = c("Barley","Canola","Lentils","Wheat","Wheat","Wheat",
           "Barley","Canola","Canola","Lentils","Lentils","Wheat"),
  Zone = c("Red","Green","Amber","Green","Green","Amber",
           "Amber","Amber","Red","Green","Red","Green"),
  Week = c("Wk4 (8 May)","Wk3 (28 Apr)","Wk5 (18 May)","Wk2 (18 Apr)","Wk3 (28 Apr)","Wk3 (28 Apr)",
           "Wk11 (17 Jul)","Wk9 (27 Jun)","Wk9 (27 Jun)","Wk9 (27 Jun)","Wk9 (27 Jun)","Wk10 (7 Jul)"),
  Ha = c(600,600,600,300,600,300, 600,300,300,300,300,1200),
  `Yield (t/ha)` = c(1.94,1.72,1.16,2.62,2.63,1.92, 1.30,0.44,0.37,0.74,0.53,1.74),
  check.names = FALSE
)

sowing_data$Tonnes <- round(sowing_data$Ha * sowing_data$`Yield (t/ha)`, 0)

# Highlight the rows where a crop got split across two zones in the same week
# (Canola and Lentils in the Late scenario — the "forced squeeze" rows)
split_rows <- sowing_data$Scenario == "Late start (27 Jun)" & sowing_data$Crop %in% c("Canola", "Lentils")

fill_matrix <- matrix("white", nrow = nrow(sowing_data), ncol = ncol(sowing_data))
fill_matrix[split_rows, ] <- "#FAEEDA"

text_matrix <- matrix("black", nrow = nrow(sowing_data), ncol = ncol(sowing_data))
text_matrix[split_rows, ] <- "#633806"

tt <- ttheme_default(
  core = list(
    bg_params = list(fill = fill_matrix, col = "grey85"),
    fg_params = list(col = text_matrix, fontsize = 14)
  ),
  colhead = list(
    fg_params = list(col = "white", fontface = "bold", fontsize = 14),
    bg_params = list(fill = "#00304D")
  )
)

g <- tableGrob(sowing_data, rows = NULL, theme = tt)

png(file.path(bundle_dir, "table_timing_sowing_plan.png"), width = 13, height = 6.5, units = "in", res = 300)
grid.draw(g)
dev.off()



### optin b - reformat of report
library(tidyverse)

week_dates <- tibble(
  week = 1:11,
  date_label = c("08 Apr","18 Apr","28 Apr","08 May","18 May","28 May",
                 "07 Jun","17 Jun","27 Jun","07 Jul","17 Jul")
)

read_plan <- function(zip_folder, scenario_name) {
  file <- list.files(file.path(bundle_dir, "unzipped", zip_folder), 
                     pattern = "sowing_plan_D1-3\\.csv$", full.names = TRUE)
  read_csv(file, show_col_types = FALSE) %>%
    mutate(scenario = scenario_name)
}

gantt_data <- bind_rows(
  read_plan("baseline_opt_GM_report_bundle", "Early start (8 Apr)"),
  read_plan("Sc4a_Mid_start_opt_GM_report_bundle", "Mid start (18 May)"),
  read_plan("Sc4b_late_OptGM_report_bundle", "Late start (27 Jun)")
) %>%
  mutate(scenario = factor(scenario, levels = c("Early start (8 Apr)", "Mid start (18 May)", "Late start (27 Jun)")),
         crop = factor(crop, levels = c("Lentils", "Canola", "Barley", "Wheat")),
         zone = factor(zone, levels = c("Green", "Amber", "Red")),
         crop_num = as.numeric(crop)) %>%
  group_by(scenario, crop, week) %>%
  mutate(n_segments = n(),
         seg_index = row_number(),
         seg_width = 1 / n_segments,
         xmin = week + (seg_index - 1) * seg_width,
         xmax = xmin + seg_width) %>%
  ungroup()

ggplot(gantt_data) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = crop_num - 0.4, ymax = crop_num + 0.4, fill = zone),
            color = "white", linewidth = 1) +
  geom_text(aes(x = (xmin + xmax) / 2, y = crop_num, label = value),
            size = 4.5, fontface = "bold", color = "white") +
  facet_wrap(~ scenario, ncol = 1) +
  scale_x_continuous(breaks = week_dates$week, labels = week_dates$date_label,
                     limits = c(1, 12), expand = c(0, 0)) +
  scale_y_continuous(breaks = 1:4, labels = levels(gantt_data$crop), limits = c(0.5, 4.5)) +
  scale_fill_manual(values = c("Green" = "#639922", "Amber" = "#FFC107", "Red" = "#A32D2D")) +
  labs(x = NULL, y = NULL, fill = "Zone") +
  theme_minimal(base_size = 18) +
  theme(panel.grid.major.x = element_line(color = "grey85", linewidth = 0.4),
        panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(face = "bold", size = 18),
        legend.position = "bottom")

ggsave(file.path(bundle_dir, "table_timing_gantt_compact.png"), width = 13, height = 9, dpi = 300, bg = "white")


###############################################################################
## Early wheat

scenario_manifest_new_crop <- tribble(
  ~zip_file,                                                ~scenario_label,     ~group,      ~optimise_for,
  "baseline_opt_GM_report_bundle.zip",                       "Baseline (Wheat)",  "new_crop",  "gm",
  "baseline_opt_yld_report_bundle.zip",                      "Baseline (Wheat)",  "new_crop",  "yield",
  "Sc5b_Early_Wheat_NoEx_OptGM_report_bundle.zip",           "+ Early wheat",     "new_crop",  "gm",
  "Sc5b_Early_Wheat_NoEx_OptYld_report_bundle.zip",          "+ Early wheat",     "new_crop",  "yield"
)

scenario_manifest <- scenario_manifest %>% 
  filter(group != "new_crop") %>%  # drop the old Sc1b-based new_crop rows
  bind_rows(scenario_manifest_new_crop)

# Re-run to pick up the new bundle
unique_bundles <- unique(scenario_manifest$zip_file)
run_data <- map_dfr(unique_bundles, read_run_summary)
plot_data <- scenario_manifest %>%
  left_join(run_data, by = c("zip_file", "optimise_for")) %>%
  mutate(decile = factor(decile, levels = c("D1-3", "D4-6", "D7-9")))

# Sanity check - should be zero rows
plot_data %>% filter(is.na(expected_gm_dollars))

new_crop_data <- plot_data %>%
  filter(group == "new_crop", optimise_for == "gm") %>%
  mutate(scenario_label = factor(scenario_label, levels = c("Baseline (Wheat)", "+ Early wheat")))

ggplot(new_crop_data, aes(x = scenario_label, y = expected_gm_dollars, fill = scenario_label)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = scales::dollar(round(expected_gm_dollars, -3), scale = 1e-6, suffix = "M")),
            vjust = -0.5, size = 4.5) +
  facet_wrap(~ decile, nrow = 1) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  scale_fill_manual(values = c("Baseline (Wheat)" = "#3E8FC4", "+ Early wheat" = "#00304D")) +
  labs(x = NULL, y = "Expected GM ($M)", fill = NULL) +
  theme_minimal(base_size = 18) +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(), panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(), axis.text.y = element_blank(),
        axis.ticks.y = element_blank(), axis.text.x = element_text(size = rel(0.85)),
        strip.text = element_text(face = "bold"))

ggsave(file.path(bundle_dir, "plot_04_new_crop.png"), width = 10, height = 5.5, dpi = 300, bg = "white")


gain_data <- new_crop_data %>%
  select(decile, scenario_label, expected_gm_dollars) %>%
  pivot_wider(names_from = scenario_label, values_from = expected_gm_dollars) %>%
  mutate(gain_dollars = `+ Early wheat` - `Baseline (Wheat)`,
         gain_pct = gain_dollars / `Baseline (Wheat)` * 100)

ggplot(gain_data, aes(x = decile, y = gain_dollars, fill = decile)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0("+$", scales::comma(round(gain_dollars, -2)), "\n(", 
                               sprintf("%.1f", gain_pct), "%)")),
            vjust = -0.3, size = 5, fontface = "bold") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
  scale_fill_manual(values = c("D1-3" = "#B8D9E8", "D4-6" = "#3E8FC4", "D7-9" = "#00304D")) +
  labs(x = NULL, y = "GM gain from adding Early wheat ($)") +
  theme_minimal(base_size = 18) +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(), panel.grid.minor.y = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank())

ggsave(file.path(bundle_dir, "plot_04_new_crop_gain.png"), width = 9, height = 5.5, dpi = 300, bg = "white")

### But why?

read_plan_new_crop <- function(zip_folder, scenario_name) {
  file <- list.files(file.path(bundle_dir, "unzipped", zip_folder), 
                     pattern = "sowing_plan_D1-3\\.csv$", full.names = TRUE)
  read_csv(file, show_col_types = FALSE) %>%
    mutate(scenario = scenario_name)
}

gantt_new_crop <- bind_rows(
  read_plan_new_crop("baseline_opt_GM_report_bundle", "Baseline (Wheat only)"),
  read_plan_new_crop("Sc5b_Early_Wheat_NoEx_OptGM_report_bundle", "+ Early wheat")
) %>%
  mutate(scenario = factor(scenario, levels = c("Baseline (Wheat only)", "+ Early wheat")),
         crop = factor(crop, levels = c("Lentils", "Canola", "Barley", "Early wheat", "Wheat")),
         zone = factor(zone, levels = c("Green", "Amber", "Red")),
         crop_num = as.numeric(crop)) %>%
  group_by(scenario, crop, week) %>%
  mutate(n_segments = n(),
         seg_index = row_number(),
         seg_width = 1 / n_segments,
         xmin = week + (seg_index - 1) * seg_width,
         xmax = xmin + seg_width) %>%
  ungroup()

ggplot(gantt_new_crop) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = crop_num - 0.4, ymax = crop_num + 0.4, fill = zone),
            color = "white", linewidth = 1) +
  geom_text(aes(x = (xmin + xmax) / 2, y = crop_num, label = value),
            size = 4.5, fontface = "bold", color = "white") +
  facet_wrap(~ scenario, ncol = 1) +
  scale_x_continuous(breaks = week_dates$week, labels = week_dates$date_label,
                     limits = c(1, 12), expand = c(0, 0)) +
  scale_y_continuous(breaks = 1:5, labels = levels(gantt_new_crop$crop), limits = c(0.5, 5.5)) +
  scale_fill_manual(values = c("Green" = "#639922", "Amber" = "#FFC107", "Red" = "#A32D2D")) +
  labs(x = NULL, y = NULL, fill = "Zone") +
  theme_minimal(base_size = 18) +
  theme(panel.grid.major.x = element_line(color = "grey85", linewidth = 0.4),
        panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(face = "bold", size = 18),
        legend.position = "bottom")

ggsave(file.path(bundle_dir, "table_new_crop_gantt.png"), width = 13, height = 7, dpi = 300, bg = "white")

### supportng yld 
library(tidyverse)
library(readxl)

yield_file <- "D:/work/RiskWise/early_sowing/Tool/sowing_planner_v1/EP_yld_long_format.xlsx"

yield_long <- read_excel(yield_file, sheet = "Yield data long format")



heatmap_data <- heatmap_data %>%
  mutate(row_label = factor(row_label, levels = rev(row_order)))

heatmap_data_early <- heatmap_data %>%
  filter(`week of sowing program window` <= 5)

ggplot(heatmap_data_early, aes(x = week_label, y = row_label, fill = yield_t_per_ha)) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(aes(label = sprintf("%.1f", yield_t_per_ha)), size = 4.5, fontface = "bold") +
  facet_wrap(~ crop, ncol = 2) +
  scale_fill_distiller(palette = "RdYlGn", direction = 1, name = "Yield (t/ha)") +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 16) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(face = "bold", size = 18),
        panel.grid = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(1.5, "cm"))

ggsave(file.path(bundle_dir, "plot_wheat_comparison_heatmap2.png"), width = 10, height = 7, dpi = 300, bg = "white")

ggsave(file.path(bundle_dir, "plot_wheat_comparison_heatmap.png"), width = 14, height = 7, dpi = 300, bg = "white")

wheat_compare_data <- tibble(
  Decile = c("D1-3", "D4-6", "D7-9"),
  Wheat = c(2.6306, 3.4284, 4.1406),
  `Early wheat` = c(1.9745, 2.6590, 3.9195)
) %>%
  pivot_longer(-Decile, names_to = "crop", values_to = "yield") %>%
  mutate(Decile = factor(Decile, levels = c("D7-9", "D4-6", "D1-3")),
         crop = factor(crop, levels = c("Wheat", "Early wheat")))



ggplot(wheat_compare_data, aes(x = crop, y = Decile, fill = yield)) +
  geom_tile(color = "white", linewidth = 1.5) +
  geom_text(aes(label = sprintf("%.2f t/ha", yield)), size = 6, fontface = "bold") +
  scale_fill_distiller(palette = "RdYlGn", direction = 1, name = "Yield (t/ha)") +
  scale_x_discrete(position = "top") +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 18) +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(face = "bold", size = 20),
        axis.text.y = element_text(face = "bold"),
        legend.position = "none")

ggsave(file.path(bundle_dir, "plot_wheat_comparison_compact.png"), width = 7, height = 6, dpi = 300, bg = "white")


###############################################################################
### Excluding crops 

scenario_manifest_redzone <- tribble(
  ~zip_file,                                                ~scenario_label,   ~group,     ~optimise_for,
  "baseline_opt_GM_report_bundle.zip",                       "No exclusion",    "redzone",  "gm",
  "Sc1b_Exclusion_W_L_opt_GM_report_bundle.zip",             "Wheat+Lentils excluded", "redzone", "gm"
)

scenario_manifest <- scenario_manifest %>% 
  filter(group != "redzone") %>%
  bind_rows(scenario_manifest_redzone)

unique_bundles <- unique(scenario_manifest$zip_file)
run_data <- map_dfr(unique_bundles, read_run_summary)
plot_data <- scenario_manifest %>%
  left_join(run_data, by = c("zip_file", "optimise_for")) %>%
  mutate(decile = factor(decile, levels = c("D1-3", "D4-6", "D7-9")))

plot_data %>% filter(is.na(expected_gm_dollars))  # should be empty

redzone_data <- plot_data %>%
  filter(group == "redzone") %>%
  mutate(scenario_label = factor(scenario_label, levels = c("No exclusion", "Wheat+Lentils excluded")))

ggplot(redzone_data, aes(x = scenario_label, y = expected_gm_dollars, fill = scenario_label)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = scales::dollar(round(expected_gm_dollars, -3), scale = 1e-6, suffix = "M")),
            vjust = -0.5, size = 4.5) +
  facet_wrap(~ decile, nrow = 1) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  scale_fill_manual(values = c("No exclusion" = "#3E8FC4", "Wheat+Lentils excluded" = "#00304D")) +
  labs(x = NULL, y = "Expected GM ($M)", fill = NULL) +
  theme_minimal(base_size = 18) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = rel(0.7)),
        panel.grid.major.x = element_blank(), panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(), axis.text.y = element_blank(),
        axis.ticks.y = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        strip.text = element_text(face = "bold"))

ggsave(file.path(bundle_dir, "plot_05_redzone_exclusion.png"), width = 10, height = 5.5, dpi = 300, bg = "white")

### WHY

gantt_redzone <- read_plan("baseline_opt_GM_report_bundle", "Baseline (no exclusion)") %>%
  mutate(crop = factor(crop, levels = c("Lentils", "Canola", "Barley", "Wheat")),
         zone = factor(zone, levels = c("Green", "Amber", "Red")),
         crop_num = as.numeric(crop)) %>%
  group_by(crop, week) %>%
  mutate(n_segments = n(),
         seg_index = row_number(),
         seg_width = 1 / n_segments,
         xmin = week + (seg_index - 1) * seg_width,
         xmax = xmin + seg_width) %>%
  ungroup()

ggplot(gantt_redzone) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = crop_num - 0.4, ymax = crop_num + 0.4, fill = zone),
            color = "white", linewidth = 1) +
  geom_text(aes(x = (xmin + xmax) / 2, y = crop_num, label = value),
            size = 5, fontface = "bold", color = "white") +
  scale_x_continuous(breaks = week_dates$week, labels = week_dates$date_label,
                     limits = c(1, 12), expand = c(0, 0)) +
  scale_y_continuous(breaks = 1:4, labels = levels(gantt_redzone$crop), limits = c(0.5, 4.5)) +
  scale_fill_manual(values = c("Green" = "#639922", "Amber" = "#FFC107", "Red" = "#A32D2D")) +
  labs(x = NULL, y = NULL, fill = "Zone") +
  theme_minimal(base_size = 18) +
  theme(panel.grid.major.x = element_line(color = "grey85", linewidth = 0.4),
        panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

ggsave(file.path(bundle_dir, "plot_05_redzone_gantt.png"), width = 11, height = 5, dpi = 300, bg = "white")
