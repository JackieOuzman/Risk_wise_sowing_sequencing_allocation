# =============================================================================
# Sowing optimiser: yield inputs, no-frost stage
# Script 07: Yield by 7-day sowing window (the table the optimiser needs)
# Author: Jackie Ouzman        Started: 2026-10-27
#
# PURPOSE
#   Turns the daily lookup table (script 06) into one yield per
#   crop x season type x 7-day sowing window.
#   Step A: for each YEAR and window, average the daily yields (the days in the window).
#   Step B: for each season type and window, average across the YEARS of that type
#           (each year counts equally). Min and max of the daily yields are kept too.
#
# INPUT   Yield_lookup_table_<rule>.csv  (script 06, same use_rule)
# OUTPUT  Yield_by_sowing_window_<rule>.csv        (crop x season type x window)
#         Yield_by_year_and_window_<rule>.csv      (the year-level values behind each mean)
#
# ASSUMPTIONS [to confirm]
#   Windows are 7 days, week 1 starting on window_start; windows are the same every year.
#   A partial window at the end (fewer than 7 days) is dropped unless keep_partial_window is TRUE.
#   The yield for a window is the average of the daily yields in it (a grower spreads sowing across the week).
# =============================================================================

library(tidyverse)

# ---- 0. Settings ------------------------------------------------------------
out_dir     <- "D:/work/RiskWise/early_sowing/Tool/Jackie_working/yield inputs proposed workflow"
# changed: run from script 12, the rule and zone come from its loop; run on its own, change the two values after "else"
use_rule    <- if (exists("driver_rule")) driver_rule else "Fixed 15 mm"
zone        <- if (exists("driver_zone")) driver_zone else "Green"      # "Green", "Amber" or "Red"
zone_suffix <- if (zone == "Green") "" else paste0("_", zone)           # Green keeps the file names we already have

window_start        <- as.Date("2026-03-01")   # week 1 starts here (placeholder year 2026, as in the yield lookup)
window_days         <- 7
keep_partial_window <- FALSE          # TRUE keeps the short final window

rule_slug <- paste0(str_replace_all(use_rule, " ", "_"), zone_suffix)     # changed: Amber and Red files end in _Amber or _Red

# ---- 1. Read the daily lookup table and give each sowing day a window --------
lookup <- read_csv(file.path(out_dir, paste0("Yield_lookup_table_", rule_slug, ".csv")), show_col_types = FALSE)

lookup <- lookup %>%
  mutate(sow_lookup_date = as.Date(paste0("2026-", format(sow_date, "%m-%d"))),
         window_no       = as.integer(sow_lookup_date - window_start) %/% window_days + 1L,
         window_from     = window_start + (window_no - 1L) * window_days,
         window_to       = window_from + window_days - 1L,
         window_label    = paste(format(window_from, "%d %b"), "to", format(window_to, "%d %b")))

# the windows and how many sowing days each has (should be 7, with a short last one)
windows <- lookup %>% distinct(window_no, window_label, sow_lookup_date) %>%
  count(window_no, window_label, name = "n_days")
windows %>% print(n = Inf)

if (!keep_partial_window) {
  full_windows <- windows %>% filter(n_days == window_days) %>% pull(window_no)
  lookup <- lookup %>% filter(window_no %in% full_windows)
}

# ---- 2. Step A: average within each year and window --------------------------
safe_mean <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)

year_window <- lookup %>%
  group_by(crop, yield_table, year, window_no, window_label) %>%
  summarise(n_days               = n(),
            n_days_with_yield    = sum(!is.na(yield_used)),
            share_days_in_table  = round(mean(yield_status == "in table"), 2),   # 1 = every day inside Kenton's own dates (20 Mar to 10 Jun)
            share_days_extended  = round(mean(yield_status == "extended"), 2),   # changed: share of days on the extended curve (script 02b)
            yield_year_mean      = safe_mean(yield_used),                        # table + extended curve
            yield_year_mean_in_table = safe_mean(if_else(yield_status == "in table", yield_t_ha, NA_real_)),   # changed: only days inside Kenton's own dates
            median_emergence     = median(emergence_date, na.rm = TRUE),
            .groups = "drop")

# ---- 3. Step B: average across the years of each season type ------------------
daily_range <- lookup %>%
  group_by(crop, yield_table, window_no) %>%
  summarise(yield_min_daily = if (all(is.na(yield_used))) NA_real_ else min(yield_used, na.rm = TRUE),
            yield_max_daily = if (all(is.na(yield_used))) NA_real_ else max(yield_used, na.rm = TRUE),
            .groups = "drop")

window_table <- year_window %>%
  group_by(crop, yield_table, window_no, window_label) %>%
  summarise(n_years               = n_distinct(year),                      # years of this season type
            n_years_with_yield    = sum(!is.na(yield_year_mean)),          # years that contribute to the mean
            n_years_in_table      = sum(!is.na(yield_year_mean_in_table)), # years with any day inside Kenton's own dates   # changed
            share_extended        = round(mean(share_days_extended), 2),   # changed: average share of the week's days on the extended curve (0 to 1)
            yield_mean            = safe_mean(yield_year_mean),
            yield_mean_in_table   = safe_mean(yield_year_mean_in_table),
            yield_min_year        = if (all(is.na(yield_year_mean))) NA_real_ else min(yield_year_mean, na.rm = TRUE),
            yield_max_year        = if (all(is.na(yield_year_mean))) NA_real_ else max(yield_year_mean, na.rm = TRUE),
            .groups = "drop") %>%
  left_join(daily_range, by = c("crop", "yield_table", "window_no")) %>%
  mutate(across(starts_with("yield_") & where(is.numeric), ~ round(.x, 2))) %>%
  rename(season_type = yield_table) %>%
  arrange(crop, season_type, window_no)

# ---- 4. Checks ----------------------------------------------------------------
# 4a. the window table for one season type (spot-check against the daily table)
window_table %>% filter(season_type == "D1-3") %>% print(n = Inf, width = Inf)

# 4b. a hand check: average the 2017 daily yields for window 1 and compare
lookup %>% filter(year == 2017, window_no == 1) %>% summarise(n = n(), mean_daily_yield = mean(yield_used, na.rm = TRUE))
year_window %>% filter(year == 2017, window_no == 1) %>% select(year, window_label, n_days, yield_year_mean)   # should be the same number

# 4c. windows per season type (D1-3, D4-6 and D7-9 should all be there now, 17 windows each)   # changed
window_table %>% count(season_type)

# ---- 5. Save -----------------------------------------------------------------
window_table <- mutate(window_table, zone = zone, .before = 1)     # changed: each file says which zone it is
year_window  <- mutate(year_window,  zone = zone, .before = 1)
write_csv(window_table, file.path(out_dir, paste0("Yield_by_sowing_window_", rule_slug, ".csv")))
write_csv(year_window,  file.path(out_dir, paste0("Yield_by_year_and_window_", rule_slug, ".csv")))


