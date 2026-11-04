# =============================================================================
# Sowing optimiser: yield inputs, no-frost stage
# Script 02d: Extend Ed's non-cereal yield-by-emergence-date tables
# Author: Jackie Ouzman        Started: 2026-11-04
#
# PURPOSE
#   Ed's non-cereal curves (beans, canola, lentils, lupins, peas) cover emergence dates 8 Apr to 17 Jul.
#   Extends them to 1 Mar to 31 Aug (5-day steps) so that no sowing week has an empty yield, then
#   joins them to Kenton's extended tables (script 02b) in one file for script 06.
#     - before the first date: straight line through the first 2 table points (never below 0)
#     - after the last date: the last table value is held
#   No uplift (the wheat uplift applies to Kenton's Shotgun and Mowhawk only).
#
# INPUT   Yield_by_emergence_date_Kenton_and_Ed.csv        (script 02)
#         Yield_by_emergence_date_extended.csv             (script 02b: Kenton extended + uplift)
# OUTPUT  Yield_by_emergence_date_extended_Ed.csv          (Ed's extended tables)
#         Yield_by_emergence_date_extended_all_crops.csv   (Kenton + Ed, read by script 06)
#         Yield_by_emergence_date_extended_Ed.png          (check plot)
#
# ASSUMPTIONS [to confirm with Ed / Therese]
#   Ed's date column is the emergence date (to confirm with Ed).
#   Ed's yield is held flat after 17 Jul. Before 8 Apr it keeps rising or falling at the rate of his first two dates.
#   Beans and lupins are identical to canola, and peas identical to lentils, in Ed's file.
# =============================================================================

library(tidyverse)

# ---- 0. Settings ------------------------------------------------------------
out_dir  <- "D:/work/RiskWise/early_sowing/Tool/Jackie_working/yield inputs proposed workflow"
in_file  <- "Yield_by_emergence_date_Kenton_and_Ed.csv"
kenton_extended_file <- "Yield_by_emergence_date_extended.csv"
out_file_ed  <- "Yield_by_emergence_date_extended_Ed.csv"
out_file_all <- "Yield_by_emergence_date_extended_all_crops.csv"

extend_pattern   <- "^Ed"
extend_from      <- as.Date("2026-03-01")     # emergence dates sit on a placeholder year, 2026
extend_to        <- as.Date("2026-08-31")
extend_step_days <- 5

# ---- 1. Read Ed's tables ------------------------------------------------------
yields_ed <- read_csv(file.path(out_dir, in_file), show_col_types = FALSE) %>%
  filter(str_detect(source, extend_pattern)) %>%
  mutate(emergence_date = as.Date(emergence_date),
         point_type     = "table")
yields_ed %>% count(source, crop_or_cultivar, point_type)       # check: 5 crops x 33 rows

# ---- 2. Extend each Ed curve ---------------------------------------------------
# One curve = one crop and season type. Returns only the NEW rows.
extend_one_ed <- function(curve, key) {
  curve   <- arrange(curve, emergence_date)
  first_d <- min(curve$emergence_date)
  last_d  <- max(curve$emergence_date)
  
  # before the first date: straight line through the first 2 table points
  x2    <- as.numeric(head(curve$emergence_date, 2))
  y2    <- head(curve$yield_t_ha, 2)
  slope <- (y2[2] - y2[1]) / (x2[2] - x2[1])
  
  before <- if (extend_from < first_d) seq(extend_from, first_d - 1, by = extend_step_days) else as.Date(character())
  after  <- c(if (last_d + extend_step_days <= extend_to) seq(last_d + extend_step_days, extend_to, by = extend_step_days),
              extend_to)
  after  <- unique(after[after > last_d])
  
  bind_rows(
    tibble(emergence_date = before,
           yield_t_ha     = pmax(0, y2[1] + slope * (as.numeric(before) - x2[1])),
           date_note      = "extended: straight line through the first 2 table dates"),
    tibble(emergence_date = after,
           yield_t_ha     = last(curve$yield_t_ha),
           date_note      = "extended: last table value held")
  ) %>%
    mutate(point_type = "extended")
}

extended_ed <- yields_ed %>%
  group_by(source, crop_or_cultivar, season_type, yp_input, date_type, frost) %>%
  group_modify(extend_one_ed) %>%
  ungroup()

# ---- 3. Combine (no uplift for Ed's crops) --------------------------------------
combined_ed <- bind_rows(yields_ed, extended_ed) %>%
  mutate(yield_uplift      = 1,
         yield_t_ha_before = yield_t_ha) %>%
  arrange(source, crop_or_cultivar, season_type, emergence_date)

# ---- 4. Checks ------------------------------------------------------------------
combined_ed %>% count(crop_or_cultivar, point_type) %>% print(n = Inf)
combined_ed %>% filter(yield_t_ha < 0) %>% nrow()                              # expect 0
combined_ed %>% filter(crop_or_cultivar == "Canola", season_type == "D1-3") %>%
  select(emergence_date, point_type, yield_t_ha, date_note) %>% print(n = Inf)  # eyeball the joins at 8 Apr and 17 Jul

# ---- 5. Save Ed's file, and join with Kenton's extended table for script 06 ----------
write_csv(combined_ed, file.path(out_dir, out_file_ed))

kenton_ext <- read_csv(file.path(out_dir, kenton_extended_file), show_col_types = FALSE) %>%
  mutate(emergence_date = as.Date(emergence_date)) %>%
  filter(str_detect(source, "^Kenton"))               # drops the un-extended Ed rows that 02b carries

all_crops <- bind_rows(kenton_ext, combined_ed) %>%
  arrange(source, crop_or_cultivar, season_type, emergence_date)

all_crops %>% count(source, crop_or_cultivar, point_type) %>% print(n = Inf)   # 9 crops and cultivars
write_csv(all_crops, file.path(out_dir, out_file_all))

# ---- 6. Plot: Ed's curves, table (filled dots) and extended (open dots) ----------------
type_colours <- c("D1-3" = "#9ecae1", "D4-6" = "#4292c6", "D7-9" = "#08519c")

p_ed <- combined_ed %>%
  filter(crop_or_cultivar %in% c("Canola", "Lentils")) %>%       # beans and lupins = canola, peas = lentils
  ggplot(aes(emergence_date, yield_t_ha, colour = season_type)) +
  geom_line(aes(group = season_type), linewidth = 0.7) +
  geom_point(aes(shape = point_type), size = 1.8) +
  scale_colour_manual(values = type_colours, name = "Season type") +
  scale_shape_manual(values = c(table = 16, extended = 1), name = NULL) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  facet_wrap(~ crop_or_cultivar, nrow = 1) +
  labs(title = "Ed's yield tables by emergence date, extended",
       subtitle = "Filled dots = Ed's table. Open dots = extended (straight line before 8 Apr, last value held after 17 Jul).",
       x = "Emergence date", y = "Yield (t/ha)") +
  theme_minimal(base_size = 10) +
  theme(panel.grid.minor = element_blank(), legend.position = "top",
        strip.text = element_text(face = "bold", hjust = 0))
p_ed

ggsave(file.path(out_dir, "Yield_by_emergence_date_extended_Ed.png"), p_ed,
       width = 20, height = 10, units = "cm", dpi = 300)