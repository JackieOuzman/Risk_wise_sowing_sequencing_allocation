# =============================================================================
# Sowing optimiser: yield inputs, Amber and Red zones
# Script 11: Amber and Red yield-by-emergence tables
# Author: Jackie Ouzman        Started: 2026-11-04
#
# PURPOSE
#   Multiplies our extended Green (no-frost) yield-by-emergence tables by Ed's zone ratios
#   (script 10): Amber/Green and Red/Green, by crop, season type and emergence date.
#   Between Ed's dates the ratio is a straight line; before 8 Apr and after 17 Jul the first /
#   last ratio is held (approx rule = 2).
#   Script 06 then reads the new files (one per zone), exactly as it reads the Green file.
#
# INPUTS  Yield_by_emergence_date_extended_all_crops.csv   (scripts 02b and 02d)
#         Zone_ratios_from_Ed.csv                           (script 10)
# OUTPUT  Yield_by_emergence_date_extended_all_crops_Amber.csv
#         Yield_by_emergence_date_extended_all_crops_Red.csv
#         Yield_by_emergence_date_zones.png                 (check plot)
#
# ASSUMPTIONS [calls made]
#   Shotgun uses Ed's Wheat ratios, Bigfoot his Barley ratios, Mowhawk his Mowhawk ratios.
#   Kenton's Neo is not used (no ratios).
# =============================================================================

library(tidyverse)

# ---- 0. Settings ------------------------------------------------------------
out_dir    <- "D:/work/RiskWise/early_sowing/Tool/Jackie_working/yield inputs proposed workflow"
yield_file <- "Yield_by_emergence_date_extended_all_crops.csv"
ratio_file <- "Zone_ratios_from_Ed.csv"

# which of Ed's crops supplies the ratios for each crop or cultivar in our yield file
crop_map <- tribble(
  ~crop_or_cultivar, ~ed_crop,
  "Shotgun", "Wheat",
  "Mowhawk", "Mowhawk",
  "Bigfoot", "Barley",
  "Canola",  "Canola",
  "Beans",   "Beans",
  "Lupins",  "Lupins",
  "Lentils", "Lentils",
  "Peas",    "Peas")

# ---- 1. Read the inputs -------------------------------------------------------
green <- read_csv(file.path(out_dir, yield_file), show_col_types = FALSE) %>%
  mutate(emergence_date = as.Date(emergence_date)) %>%
  filter((str_detect(source, "^Kenton") & crop_or_cultivar %in% c("Shotgun", "Mowhawk", "Bigfoot")) |
           (str_detect(source, "^Ed") & crop_or_cultivar %in% c("Canola", "Beans", "Lupins", "Lentils", "Peas"))) %>%
  inner_join(crop_map, by = "crop_or_cultivar")
green %>% count(source, crop_or_cultivar, season_type)       # expect 8 crops x 3 season types = 24 rows

ratios <- read_csv(file.path(out_dir, ratio_file), show_col_types = FALSE) %>%
  mutate(ed_date = as.Date(ed_date))

# ---- 2. Apply the ratios --------------------------------------------------------
make_zone <- function(zone_name, ratio_col) {
  green %>%
    group_by(ed_crop, season_type, source, crop_or_cultivar) %>%
    group_modify(function(d, key) {
      r <- ratios %>% filter(crop == key$ed_crop, season_type == key$season_type) %>% arrange(ed_date)
      d %>% mutate(zone_ratio = approx(as.numeric(r$ed_date), r[[ratio_col]],
                                       xout = as.numeric(emergence_date), rule = 2)$y)    # rule = 2: hold the end ratios
    }) %>%
    ungroup() %>%
    mutate(zone = zone_name,
           yield_green_t_ha = yield_t_ha,
           yield_t_ha       = yield_green_t_ha * zone_ratio)
}
amber <- make_zone("Amber", "amber_green")
red   <- make_zone("Red",   "red_green")

# ---- 3. Checks ----------------------------------------------------------------
nrow(green); nrow(amber); nrow(red)                          # the three should be equal
sum(is.na(amber$yield_t_ha)) + sum(is.na(red$yield_t_ha))    # expect 0
sum(amber$yield_t_ha > amber$yield_green_t_ha + 1e-9)        # expect 0 (Amber never above Green)
sum(red$yield_t_ha > amber$yield_t_ha + 1e-9)                # expect 0 (Red never above Amber)

# Shotgun D4-6: the ratio on 1 Mar should equal the 8 Apr ratio, and 31 Aug the 17 Jul ratio (held)
bind_rows(amber, red) %>%
  filter(crop_or_cultivar == "Shotgun", season_type == "D4-6",
         emergence_date %in% as.Date(c("2026-03-01", "2026-04-08", "2026-04-10", "2026-06-15", "2026-07-15", "2026-08-31"))) %>%
  select(zone, emergence_date, point_type, yield_green_t_ha, zone_ratio, yield_t_ha) %>%
  arrange(zone, emergence_date) %>% print(n = Inf)

# ---- 4. Save ------------------------------------------------------------------
write_csv(amber, file.path(out_dir, "Yield_by_emergence_date_extended_all_crops_Amber.csv"))
write_csv(red,   file.path(out_dir, "Yield_by_emergence_date_extended_all_crops_Red.csv"))

# ---- 5. Plot: Green, Amber and Red together (D4-6) -------------------------------
zone_cols <- c("Green" = "#2e7d32", "Amber" = "#f9a825", "Red" = "#c62828")

p_zones <- bind_rows(green %>% mutate(zone = "Green", zone_ratio = 1, yield_green_t_ha = yield_t_ha), amber, red) %>%
  filter(season_type == "D4-6") %>%
  mutate(zone = factor(zone, levels = names(zone_cols)),
         crop_or_cultivar = factor(crop_or_cultivar,       # changed: panel order
                                   levels = c("Bigfoot", "Shotgun", "Mowhawk", "Canola", "Beans", "Lupins", "Lentils", "Peas"))) %>%
  ggplot(aes(emergence_date, yield_t_ha, colour = zone)) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~ crop_or_cultivar, nrow = 2) +
  scale_colour_manual(values = zone_cols, name = "Zone") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(title = "Yield by emergence date, Green, Amber and Red zones (D4-6)",
       subtitle = "Green = our no-frost tables. Amber and Red = Green x Ed's ratios\n(first / last ratio held outside 8 Apr to 17 Jul).",
       x = "Emergence date", y = "Yield (t/ha)",
       caption = paste0("Shotgun Red rises again from mid-June because Ed's Red/Green ratio jumps (D4-6: 0.37 on 7 Jun to 0.62 on 27 Jun):\n",
                        "late emergence escapes more of the frost loss. Late-sown Red wheat can therefore show a second, smaller peak.")) +
  theme_minimal(base_size = 10) + theme(panel.grid.minor = element_blank(), legend.position = "top")
p_zones
ggsave(file.path(out_dir, "Yield_by_emergence_date_zones.png"), p_zones, width = 26, height = 14, units = "cm", dpi = 300)
