# =============================================================================
# Sowing optimiser: yield inputs, Amber and Red zones
# Script 10: Zone ratios from Ed's seed data tab
# Author: Jackie Ouzman        Started: 2026-11-04
#
# PURPOSE
#   Reads Ed's Green, Amber and Red yield blocks (seed data tab, one block per crop)
#   and works out Amber/Green and Red/Green for each crop, season type and date.
#   Script 11 applies these ratios to our no-frost (Green) yield tables.
#
# INPUT   Ed's workbook, "seed data" tab
# OUTPUT  Zone_ratios_from_Ed.csv, Zone_ratios_from_Ed.png (check plot)
#
# CALLS MADE
#   Amber/Green is capped at 1 (Amber never higher than Green).
#   Red/Green is never higher than Amber/Green.
#   Dates outside Ed's 8 Apr to 17 Jul are handled in script 11 (first / last ratio held).
# =============================================================================

library(tidyverse)
library(readxl)

# ---- 0. Settings ------------------------------------------------------------
out_dir          <- "D:/work/RiskWise/early_sowing/Tool/Jackie_working/yield inputs proposed workflow"
ed_file          <- "D:/work/RiskWise/early_sowing/Tool/From Therese/Eds base model_DrysowingNieldsHome26baseprogramplayall deciles.xls"
placeholder_year <- 2026                                # dates are on a placeholder year, as elsewhere
season_types     <- c("D1-3", "D4-6", "D7-9")

# ---- 1. Read the seed data tab as text, so rows and columns can be picked by position ----
raw <- read_excel(ed_file, sheet = "seed data", col_names = FALSE, col_types = "text")

# One block = one crop. Dates run down every second row (11 dates); D1-3, D4-6, D7-9 are three columns.
# first_row = the row of the 8th April line. Columns: E to G = no frost; green_cols etc. = the zone blocks.
get_zone_block <- function(crop, first_row, green_cols, amber_cols, red_cols) {
  rows <- first_row + 2 * (0:10)
  map_dfr(seq_along(season_types), function(i) {
    tibble(crop        = crop,
           season_type = season_types[i],
           date_text   = raw[[1]][rows],
           no_frost    = as.numeric(raw[[4 + i]][rows]),          # columns E, F, G
           green       = as.numeric(raw[[green_cols[i]]][rows]),
           amber       = as.numeric(raw[[amber_cols[i]]][rows]),
           red         = as.numeric(raw[[red_cols[i]]][rows]))
  })
}

# Wheat and Mowhawk share one block (columns P to AL); the other crops have Green P:R, Amber T:V, Red X:Z
zones_raw <- bind_rows(
  get_zone_block("Wheat",   8,   16:18, 24:26, 32:34),
  get_zone_block("Mowhawk", 8,   20:22, 28:30, 36:38),
  get_zone_block("Barley",  41,  16:18, 20:22, 24:26),
  get_zone_block("Canola",  73,  16:18, 20:22, 24:26),
  get_zone_block("Lentils", 106, 16:18, 20:22, 24:26),
  get_zone_block("Beans",   139, 16:18, 20:22, 24:26),
  get_zone_block("Lupins",  171, 16:18, 20:22, 24:26),
  get_zone_block("Peas",    204, 16:18, 20:22, 24:26))

# ---- 2. Dates and ratios -------------------------------------------------------
zones <- zones_raw %>%
  mutate(ed_date = as.Date(paste(str_remove(date_text, "(?<=\\d)(st|nd|rd|th)"), placeholder_year),
                           format = "%d %B %Y"),
         amber_green_raw = amber / green,
         red_amber_raw   = red / amber,
         amber_green     = pmin(amber_green_raw, 1),               # call: cap at 1
         red_green       = pmin(red / green, amber_green)) %>%     # call: Red never above Amber
  select(crop, season_type, ed_date, no_frost, green, amber, red,
         amber_green_raw, amber_green, red_amber_raw, red_green)

# ---- 3. Checks ------------------------------------------------------------------
zones %>% count(crop)                       # expect 33 rows each (11 dates x 3 season types)
sum(is.na(zones$ed_date))                   # expect 0

# Canola D4-6 on 8 April: expect no_frost 2.2, green 1.34, amber 1.14, red 0.97
zones %>% filter(crop == "Canola", season_type == "D4-6") %>% slice_min(ed_date, n = 1) %>% print(width = Inf)

# Do the ranges match Table 1 in the doc?
zones %>% group_by(crop) %>%
  summarise(amber_green_min = round(min(amber_green_raw), 2), amber_green_max = round(max(amber_green_raw), 2),
            n_capped = sum(amber_green_raw > 1),
            red_amber_min = round(min(red_amber_raw), 2), red_amber_max = round(max(red_amber_raw), 2),
            .groups = "drop") %>% print(n = Inf)

# ---- 4. Save and plot ---------------------------------------------------------------
write_csv(zones, file.path(out_dir, "Zone_ratios_from_Ed.csv"))

p_ratios <- zones %>%
  pivot_longer(c(amber_green, red_green), names_to = "ratio", values_to = "value") %>%
  mutate(ratio = recode(ratio, amber_green = "Amber / Green", red_green = "Red / Green")) %>%
  ggplot(aes(ed_date, value, colour = season_type, linetype = ratio)) +
  geom_line() + geom_point(size = 1) +
  facet_wrap(~ crop, nrow = 2) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_colour_manual(values = c("D1-3" = "#9ecae1", "D4-6" = "#4292c6", "D7-9" = "#08519c"), name = "Season type") +
  labs(title = "Zone ratios from Ed's seed data (after the cap)", x = "Emergence date", y = "Ratio to Green", linetype = NULL) +
  theme_minimal(base_size = 10) + theme(panel.grid.minor = element_blank(), legend.position = "top")
p_ratios
ggsave(file.path(out_dir, "Zone_ratios_from_Ed.png"), p_ratios, width = 26, height = 14, units = "cm", dpi = 300)
