# =============================================================================
# Sowing optimiser: yield inputs, no-frost stage
# Script 08: Plot expected yield by 7-day sowing window
# Author: Jackie Ouzman        Started: 2026-10-27
#
# PURPOSE
#   Plots the window yield table from script 07: one panel per season type,
#   a dot for each year (average of the sowing days in that window), a line for the
#   average across years, grey bands where no yield exists in the model,
#   and the number of years behind each average along the bottom.
#
# INPUTS  Yield_by_sowing_window_<rule>.csv and Yield_by_year_and_window_<rule>.csv (script 07)
#
# NOTE    The saved files do not record the out-of-range rule (script 06), so
#         out_of_range_label below is only a label for the plot. Keep it in step with script 06.
#         To overlay two rules, run 06 and 07 for each rule with the same out_of_range_rule, then list both in plot_rules.
# =============================================================================

library(tidyverse)

# ---- 0. Settings ------------------------------------------------------------
out_dir            <- "D:/work/RiskWise/early_sowing/Tool/Jackie_working/yield inputs proposed workflow"
#plot_rules         <- c("Light soil")        # e.g. c("Fixed 15 mm", "Light soil") to overlay two rules
plot_rules         <- c("Fixed 15 mm", "Light soil")        # e.g. c("Fixed 15 mm", "Light soil") to overlay two rules
out_of_range_label <- "exclude"              # label only: must match script 06
crop_to_plot       <- "Wheat"
window_start       <- as.Date("2026-03-01")  # must match script 07
window_days        <- 7

rule_cols <- c("Fixed 15 mm" = "#9ecae1", "Heavy soil" = "#c6dbef",
               "Medium soil" = "#5598e7", "Light soil" = "#184f95")

# text for the plot subtitle (must match scripts 04, 05 and 02)
dd_target         <- 120                    # degree-days from germination to emergence (script 05)
base_temp         <- 0                      # base temperature, C (script 05)
yield_table_range <- "20 Mar to 10 Jun"     # emergence dates covered by Kenton's yield table (script 02)
rule_text <- c("Fixed 15 mm"  = "15 mm of rain in 5 days",
               "Heavy soil"   = "heavy-soil trigger (25 down to 15 mm in 5 days, by sowing date)",
               "Medium soil"  = "medium-soil trigger (18 down to 10 mm in 5 days, by sowing date)",
               "Light soil"   = "light-soil trigger (12 down to 6 mm in 5 days, by sowing date)")

# ---- 1. Read the tables for each rule ----------------------------------------
read_rule <- function(rule) {
  slug <- str_replace_all(rule, " ", "_")
  list(wt = read_csv(file.path(out_dir, paste0("Yield_by_sowing_window_", slug, ".csv")), show_col_types = FALSE) %>%
         mutate(rule = rule),
       yw = read_csv(file.path(out_dir, paste0("Yield_by_year_and_window_", slug, ".csv")), show_col_types = FALSE) %>%
         rename(season_type = yield_table) %>% mutate(rule = rule))
}
res <- map(plot_rules, read_rule)

add_dates <- function(d) d %>%
  mutate(window_from = window_start + (window_no - 1L) * window_days,
         window_mid  = window_from + (window_days - 1) / 2,
         rule        = factor(rule, levels = plot_rules))

window_tbl <- map_dfr(res, "wt") %>% filter(crop == crop_to_plot) %>% add_dates()
year_tbl   <- map_dfr(res, "yw") %>% filter(crop == crop_to_plot) %>% add_dates()

# panel titles with the number of years of each season type
season_labs <- window_tbl %>% group_by(season_type) %>% summarise(n = max(n_years), .groups = "drop") %>%
  mutate(lab = paste0(season_type, "  (", n, " years)"))
season_labs <- set_names(season_labs$lab, season_labs$season_type)

# windows with no yield in any rule (grey band)
no_yield <- window_tbl %>% group_by(season_type, window_from) %>%
  summarise(all_na = all(is.na(yield_mean)), .groups = "drop") %>% filter(all_na)
no_yield_lab <- no_yield %>% group_by(season_type) %>%
  summarise(x = min(window_from) + (max(window_from) + window_days - min(window_from)) / 2, .groups = "drop")

# ---- 2. Plot -----------------------------------------------------------------
single_rule <- length(plot_rules) == 1

year_dots <- if (single_rule) geom_point(data = year_tbl, aes(window_mid, yield_year_mean, colour = rule),
                                         alpha = 0.45, size = 1.7, na.rm = TRUE) else NULL
n_labels  <- if (single_rule) geom_text(data = window_tbl, aes(window_mid, y = 0, label = n_years_with_yield),
                                        vjust = -0.6, size = 2.6, colour = "grey45", na.rm = TRUE) else NULL

p_window <- ggplot() +
  geom_rect(data = no_yield, aes(xmin = window_from, xmax = window_from + window_days, ymin = -Inf, ymax = Inf),
            fill = "grey92") +
  geom_text(data = no_yield_lab, aes(x, y = Inf, label = "No yield in Kenton's table\n(emerges after 10 Jun)"),
            vjust = 1.3, size = 2.8, colour = "grey35") +
  year_dots +
  geom_line(data = window_tbl, aes(window_mid, yield_mean, colour = rule, group = rule, linetype = rule), linewidth = 1, na.rm = TRUE) +
  scale_linetype_manual(values = c("Fixed 15 mm" = "dashed", "Heavy soil" = "dotdash", "Medium soil" = "dotted", "Light soil" = "solid"),
                        name = "Germination rule") +
  geom_point(data = window_tbl, aes(window_mid, yield_mean, colour = rule), size = 1.4, na.rm = TRUE) +
  n_labels +
  facet_wrap(~ season_type, ncol = 1, labeller = labeller(season_type = season_labs)) +
  scale_colour_manual(values = rule_cols, name = "Germination rule") +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%d %b") +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.08))) +
  
  labs(title = paste0("Expected ", tolower(crop_to_plot), " yield by sowing date (Kenton model, Shotgun)"),
       subtitle = paste0("Sowing date  >  germination (rain)  >  emergence (heat)  >  yield\n",
                         "Germination: first day with ", paste(rule_text[plot_rules], collapse = "; "), ", counting rain from the sowing day\n",
                         "Emergence: ", dd_target, " degree-days after germination (daily mean temperature, base ", base_temp, " C)\n",
                         "Yield: read from Kenton's table at the emergence date (", yield_table_range, "); no yield outside it (", out_of_range_label, ")"),
       x = "Sowing date (first day of each 7-day week)", y = "Yield (t/ha)",
       caption = paste0("Line = average across the years of that season type. Dots = individual years (average of the sowing days in that week).\n",
                        "Small numbers = years behind each average. Each year uses the yield table for its own season type (water supply).\n",
                        "Lock, 2016-2025 (2022 is D10, left out).")) +
  
  theme_minimal(base_size = 10) +
  theme(panel.grid.minor = element_blank(),
        legend.position = if (single_rule) "none" else "top",
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.caption = element_text(hjust = 0, size = 7.5),
        plot.subtitle = element_text(size = 8.5, lineheight = 1.15),
        strip.text = element_text(face = "bold", hjust = 0))
p_window

ggsave(file.path(out_dir, paste0("Yield_by_sowing_window_plot_", paste(str_replace_all(plot_rules, " ", "_"), collapse = "_and_"), ".png")),
       p_window, width = 22, height = 18, units = "cm", dpi = 300)


bind_rows(
  read_csv(file.path(out_dir, "Yield_by_year_and_window_Fixed_15_mm.csv"), show_col_types = FALSE) %>% mutate(rule = "Fixed 15 mm"),
  read_csv(file.path(out_dir, "Yield_by_year_and_window_Light_soil.csv"),  show_col_types = FALSE) %>% mutate(rule = "Light soil")) %>%
  filter(yield_table == "D4-6", window_no == 3) %>%
  select(rule, year, window_label, median_emergence, yield_year_mean) %>%
  arrange(year, rule)
