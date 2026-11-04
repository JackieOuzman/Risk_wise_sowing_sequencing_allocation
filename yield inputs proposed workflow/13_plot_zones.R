# =============================================================================
# Sowing optimiser: yield inputs, Amber and Red zones
# Script 13: Weekly yield by sowing window, Green, Amber and Red together
# Author: Jackie Ouzman        Started: 2026-11-04
#
# PURPOSE   One plot for a chosen germination rule: each crop (row) and season type (column),
#           with one line per zone.
# INPUTS    Yield_by_sowing_window_<rule>[_Amber|_Red].csv   (script 07, run by script 12)
# OUTPUT    Yield_by_sowing_window_zones_<rule>.png
# =============================================================================

library(tidyverse)

out_dir   <- "D:/work/RiskWise/early_sowing/Tool/Jackie_working/yield inputs proposed workflow"
plot_rule <- "Fixed 15 mm"          # "Fixed 15 mm", "Heavy soil", "Medium soil" or "Light soil"

rule_slug  <- str_replace_all(plot_rule, " ", "_")
zone_cols  <- c("Green" = "#2e7d32", "Amber" = "#f9a825", "Red" = "#c62828")
crop_order <- c("Barley", "Wheat", "Mowhawk", "Canola", "Beans", "Lupins", "Lentils", "Peas")

read_zone <- function(z) {
  suffix <- if (z == "Green") "" else paste0("_", z)
  read_csv(file.path(out_dir, paste0("Yield_by_sowing_window_", rule_slug, suffix, ".csv")), show_col_types = FALSE) %>%
    mutate(zone = z)
}

zw <- map_dfr(names(zone_cols), read_zone) %>%
  mutate(window_mid = as.Date("2026-03-01") + (window_no - 1) * 7 + 3,
         zone = factor(zone, levels = names(zone_cols)),
         crop = factor(crop, levels = crop_order))

p_zones <- ggplot(zw, aes(window_mid, yield_mean, colour = zone)) +
  geom_line(linewidth = 0.8, na.rm = TRUE) +
  geom_point(size = 1, na.rm = TRUE) +
  facet_grid(crop ~ season_type, scales = "free_y") +
  scale_colour_manual(values = zone_cols, name = "Zone") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(limits = c(0, NA), breaks = scales::pretty_breaks(n = 3)) +
  labs(title = paste0("Expected yield by sowing date, Green, Amber and Red zones (", plot_rule, ")"),
       subtitle = "Each point = one 7-day sowing week, averaged across the years of that season type.\nAmber and Red = Green x Ed's zone ratios (script 11).",
       x = "Sowing date (middle of each 7-day week)", y = "Yield (t/ha)") +
  theme_minimal(base_size = 9) +
  theme(panel.grid.minor = element_blank(), legend.position = "top",
        strip.text = element_text(face = "bold"))
p_zones

ggsave(file.path(out_dir, paste0("Yield_by_sowing_window_zones_", rule_slug, ".png")), p_zones,
       width = 22, height = 36, units = "cm", dpi = 300)
