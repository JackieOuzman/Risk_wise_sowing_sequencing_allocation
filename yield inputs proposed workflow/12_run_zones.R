# =============================================================================
# Sowing optimiser: yield inputs, no-frost and zones
# Script 12: Run scripts 06 and 07 for every zone and germination rule
# Author: Jackie Ouzman        Started: 2026-11-04
#
# PURPOSE
#   Runs script 06 (daily yield lookup) and script 07 (yield by 7-day sowing window) in turn
#   for each zone (Green, Amber, Red) and each of the four germination rules, so the
#   weekly yield tables for the optimiser are all built in one go.
#   Scripts 06 and 07 pick up the zone and the rule from driver_zone and driver_rule below.
#   Run on their own, they use the values set in their own Settings.
#
# INPUTS  Scripts 06 and 07, and the files they read:
#           Yield_by_emergence_date_extended_all_crops.csv         (Green, script 02d)
#           Yield_by_emergence_date_extended_all_crops_Amber.csv   (script 11)
#           Yield_by_emergence_date_extended_all_crops_Red.csv     (script 11)
#           Sowing_to_emergence_dates_Lock_1997_2025.csv           (script 05)
# OUTPUT  For each zone and rule (Green files have no zone ending):
#           Yield_lookup_table_<rule>[_Amber|_Red].csv
#           Yield_by_sowing_window_<rule>[_Amber|_Red].csv         (the tables the optimiser needs)
#           Yield_by_year_and_window_<rule>[_Amber|_Red].csv
#
# NOTE    Scripts 06 and 07 also save plots and print checks each run. Check script_dir and the two
#         script names below before running.
# =============================================================================

# Script 12: run scripts 06 and 07 for each zone and each germination rule
script_dir   <- "C:/Users/ouz001/working_from_home_post_Sep2022/Risk_wise_sowing_sequencing_allocation/yield inputs proposed workflow"   # changed: the folder that holds scripts 06 and 07
script_06    <- "06_yield_lookup_wheat.R"
script_07    <- "07_yield_by_sowing_window.R"
zones_to_run <- c("Green", "Amber", "Red")
rules_to_run <- c("Fixed 15 mm", "Heavy soil", "Medium soil", "Light soil")

for (driver_zone in zones_to_run) {
  for (driver_rule in rules_to_run) {
    message("Running ", driver_zone, ", ", driver_rule)
    source(file.path(script_dir, script_06), local = new.env())
    source(file.path(script_dir, script_07), local = new.env())
  }
}
rm(driver_zone, driver_rule)

out_dir <- "D:/work/RiskWise/early_sowing/Tool/Jackie_working/yield inputs proposed workflow"

bind_rows(
  read_csv(file.path(out_dir, "Yield_lookup_table_Fixed_15_mm_Amber.csv"), show_col_types = FALSE),
  read_csv(file.path(out_dir, "Yield_lookup_table_Fixed_15_mm_Red.csv"),   show_col_types = FALSE)) %>%
  count(zone, crop, yield_status) %>% print(n = Inf)
zw <- bind_rows(
  read_csv(file.path(out_dir, "Yield_by_sowing_window_Fixed_15_mm.csv"),       show_col_types = FALSE),
  read_csv(file.path(out_dir, "Yield_by_sowing_window_Fixed_15_mm_Amber.csv"), show_col_types = FALSE),
  read_csv(file.path(out_dir, "Yield_by_sowing_window_Fixed_15_mm_Red.csv"),   show_col_types = FALSE)) %>%
  select(zone, crop, season_type, window_no, window_label, yield_mean) %>%
  pivot_wider(names_from = zone, values_from = yield_mean)

# expect 0, 0, 0
zw %>% summarise(amber_above_green = sum(Amber > Green + 0.005, na.rm = TRUE),
                 red_above_amber   = sum(Red > Amber + 0.005, na.rm = TRUE),
                 rows_with_na      = sum(is.na(Green) | is.na(Amber) | is.na(Red)))

# D4-6, five weeks across the season
zw %>% filter(season_type == "D4-6", window_no %in% c(1, 5, 9, 13, 17)) %>%
  arrange(crop, window_no) %>% print(n = Inf)
