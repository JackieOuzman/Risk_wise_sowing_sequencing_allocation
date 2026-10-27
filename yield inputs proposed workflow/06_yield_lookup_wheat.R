# =============================================================================
# Sowing optimiser: yield inputs, no-frost stage
# Script 06: Yield lookup table (wheat first) and yield by sowing date
# Author: Jackie Ouzman        Started: 2026-10-27
#
# PURPOSE
#   Builds the lookup table that links each sowing date to a yield:
#     year -> sowing date -> germination date -> emergence date -> yield
#   The yield is read from the yield table (script 02) for the crop and for the
#   YEAR'S OWN season type (D1-3, D4-6, D7-9, from script 01), at the emergence date.
#   Then averages over the years to give the expected yield for each sowing date and
#   season type (step 7).
#
# INPUTS (all in out_dir)
#   Sowing_to_emergence_dates_Lock_2016_2025.csv   (script 05)
#   Yield_by_emergence_date_Kenton_and_Ed.csv      (script 02)
#   Lock_18046_water_supply_season_types.csv       (script 01)
#
# RULE SWITCHES (these are choices still to be agreed with Kenton / Therese)
#   use_rule             which germination rule to run (from script 04), e.g. "Fixed 15 mm"
#   out_of_range_rule    emergence before the first / after the last date of the yield table:
#                        "hold_last" = use the yield at the nearest end of the table
#                        "zero"      = yield 0
#                        "exclude"   = no yield (left out of the average)
#   no_emergence_rule    sowings that never germinate or emerge: "exclude" or "zero"
#   years_for_each_type  "own" = each year uses its own season type's table (agreed for now)
#                        "all" = every year is run through every table
#
# ASSUMPTIONS [to confirm]
#   Wheat (regular) = Kenton's Shotgun. Between table dates the yield is interpolated in a straight line.
#   Years with no matching table (e.g. D10 years such as 2022) are left out under "own".
# =============================================================================

library(tidyverse)

# ---- 0. Settings ------------------------------------------------------------
out_dir     <- "D:/work/RiskWise/early_sowing/Tool/Jackie_working/yield inputs proposed workflow"
emerg_file  <- "Sowing_to_emergence_dates_Lock_2016_2025.csv"
yield_file  <- "Yield_by_emergence_date_Kenton_and_Ed.csv"
season_file <- "Lock_18046_water_supply_season_types.csv"

use_rule            <- "Fixed 15 mm"     # change to "Light soil", "Medium soil" etc. to relax the rule
#use_rule            <- "Light soil"      # change to "Light soil", "Medium soil" etc. to relax the rule
#out_of_range_rule   <- "hold_last"       # "hold_last", "zero" or "exclude"
out_of_range_rule   <- "exclude"         # "hold_last", "zero" or "exclude"
no_emergence_rule   <- "exclude"         # "exclude" or "zero"
years_for_each_type <- "own"             # "own" or "all"

# must match script 05
dd_target <- 120
base_temp <- 0

season_types <- c("D1-3", "D4-6", "D7-9")

# one row per crop. To add a crop, add a row (Ed's crops use source_pattern "^Ed").
crop_settings <- tribble(
  ~crop,   ~cultivar, ~source_pattern, ~yield_source,           ~decile_basis,
  "Wheat", "Shotgun", "^Kenton",       "Kenton model, Shotgun", "Rainfall (water supply)"
)
# e.g. Barley:  "Barley", "Bigfoot", "^Kenton", "Kenton model, Bigfoot", "Rainfall (water supply)"
# e.g. Canola:  "Canola", "Canola",   "^Ed",    "Ed seed data, cols E-G (no frost)", "Rainfall (water supply), stand-in for Ed's yield deciles"

rule_slug <- str_replace_all(use_rule, " ", "_")

# ---- 1. Read the inputs ------------------------------------------------------
emerg <- read_csv(file.path(out_dir, emerg_file), show_col_types = FALSE) %>%
  filter(rule == use_rule)
stopifnot(nrow(emerg) > 0)
nrow(emerg)                                       # 1230 for the 1 Mar-1 Jul window (10 years x 123 days)
sum(is.na(emerg$emergence_date))                  # sowings with no emergence under this rule

yields <- read_csv(file.path(out_dir, yield_file), show_col_types = FALSE)
yields %>% count(source, crop_or_cultivar)        # check the crop is in the yield file

season_raw <- read_csv(file.path(out_dir, season_file), show_col_types = FALSE)
stopifnot(all(c("season_year", "season_type") %in% names(season_raw)))      # if this fails, check the column names
year_types <- season_raw %>% select(year = season_year, year_season_type = season_type)

# the season type of each year we run
year_types %>% filter(year %in% unique(emerg$year)) %>% arrange(year)
year_types %>% filter(year %in% unique(emerg$year)) %>% count(year_season_type)   # D7-9 should be missing

# ---- 2. Yield lookup for one crop and one season type's table -----------------
lookup_one <- function(emerg_tbl, cs, st_label) {
  curve <- yields %>%
    filter(str_detect(source, cs$source_pattern), crop_or_cultivar == cs$cultivar, season_type == st_label) %>%
    mutate(x = as.numeric(emergence_date)) %>%
    arrange(x)
  stopifnot(nrow(curve) > 1)
  x        <- as.numeric(emerg_tbl$emergence_lookup_date)               # emergence date, placeholder year 2026
  y_interp <- approx(curve$x, curve$yield_t_ha, xout = x, rule = 1)$y   # straight line between table dates, NA outside
  first_y  <- curve$yield_t_ha[1]
  last_y   <- curve$yield_t_ha[nrow(curve)]
  status <- case_when(is.na(x)         ~ "no emergence",
                      x < min(curve$x) ~ "before table",
                      x > max(curve$x) ~ "after table",
                      TRUE             ~ "in table")
  yield_used <- case_when(
    status == "in table"     ~ y_interp,
    status == "no emergence" ~ if (no_emergence_rule == "zero") 0 else NA_real_,
    status == "before table" ~ switch(out_of_range_rule, hold_last = first_y, zero = 0, exclude = NA_real_),
    status == "after table"  ~ switch(out_of_range_rule, hold_last = last_y,  zero = 0, exclude = NA_real_))
  emerg_tbl %>%
    mutate(yield_table        = st_label,
           yield_emerg_window = paste(format(as.Date(min(curve$x)), "%d %b"), "to", format(as.Date(max(curve$x)), "%d %b")),
           yield_t_ha         = y_interp,        # strictly from the table, NA outside its dates
           yield_status       = status,
           yield_used         = yield_used)      # after the out-of-range and no-emergence rules
}

# ---- 3. Build the lookup table: every crop x every season type's table --------
sowing_window_text <- paste(format(min(emerg$sow_date), "%d %b"), "to", format(max(emerg$sow_date), "%d %b"))

lookup_all <- pmap_dfr(crop_settings, function(crop, cultivar, source_pattern, yield_source, decile_basis) {
  cs <- list(cultivar = cultivar, source_pattern = source_pattern)
  map_dfr(season_types, ~ lookup_one(emerg, cs, .x)) %>%
    mutate(crop = crop, yield_source = yield_source, decile_basis = decile_basis)
}) %>%
  left_join(year_types, by = "year")

# own season type only (or all years through every table)
lookup_table <- (if (years_for_each_type == "own") lookup_all %>% filter(yield_table == year_season_type) else lookup_all) %>%
  mutate(sowing_window   = sowing_window_text,
         germination_rule = paste0(trigger_mm, " mm in 5 days", if_else(use_rule == "Fixed 15 mm", "", paste0(" (", use_rule, ")"))),
         emergence_rule   = paste0(dd_target, " degree-days (base ", base_temp, " C)")) %>%
  
  select(crop, year, year_season_type, decile_basis, sowing_window, sow_date,
         germination_rule, germination_date, days_sow_to_germ, emergence_rule, emergence_date,
         yield_source, yield_emerg_window, yield_table, yield_status, yield_t_ha, yield_used)

# years left out because they have no matching table (e.g. D10)
setdiff(unique(emerg$year), unique(lookup_table$year))

# ---- 4. Checks ----------------------------------------------------------------
# 4a. How many rows get a yield from the table, and how many fall outside it?
lookup_table %>% count(crop, yield_table, yield_status) %>% print(n = Inf)

# 4b. The mock-table rows we talked about: 2016, sown 1 Apr and 15 May
lookup_table %>%
  filter(year == 2016, format(sow_date, "%m-%d") %in% c("04-01", "05-15")) %>%
  print(width = Inf)

# 4c. How long does seed sit in the ground before germinating? (one row per year)**
  lookup_table %>%
    group_by(year) %>%
    summarise(shortest_wait_days = min(days_sow_to_germ, na.rm = TRUE),
                              longest_wait_days  = max(days_sow_to_germ, na.rm = TRUE),
                              sowings_never_germinating = sum(is.na(germination_date)),
                              .groups = "drop") %>%
    print(n = Inf)
# ---- 5. Average over the years: yield by sowing date and season type (step 7) -
yield_by_sowing_date <- lookup_table %>%
  mutate(sow_md = format(sow_date, "%m-%d")) %>%
  group_by(crop, yield_table, sow_md) %>%
  summarise(n_years            = n_distinct(year),                  # years of this season type
            n_years_in_table   = sum(yield_status == "in table"),   # of those, years inside the table's dates
            n_years_with_yield = sum(!is.na(yield_used)),           # years that contribute to the average
            mean_yield_in_table = mean(yield_t_ha, na.rm = TRUE),   # average using only in-table years
            mean_yield_used     = mean(yield_used, na.rm = TRUE),   # average after the rules above
            .groups = "drop") %>%
  complete(crop, yield_table = season_types, sow_md = unique(emerg$sow_md),
           fill = list(n_years = 0L, n_years_in_table = 0L, n_years_with_yield = 0L)) %>%
  mutate(across(starts_with("mean_"), ~ if_else(is.nan(.x), NA_real_, round(.x, 2)))) %>%
  arrange(crop, yield_table, sow_md)

# one line per season type on the 1st and 15th of each month
yield_by_sowing_date %>%
  filter(sow_md %in% c("03-01", "03-15", "04-01", "04-15", "05-01", "05-15", "06-01", "06-15", "07-01")) %>%
  select(crop, yield_table, sow_md, n_years, n_years_in_table, mean_yield_in_table, mean_yield_used) %>%
  print(n = Inf)

# ---- 6. Save -----------------------------------------------------------------
write_csv(lookup_table,          file.path(out_dir, paste0("Yield_lookup_table_", rule_slug, ".csv")))
write_csv(yield_by_sowing_date,  file.path(out_dir, paste0("Yield_by_sowing_date_", rule_slug, ".csv")))

# ---- 7. Plots ----------------------------------------------------------------
type_colours <- c("D1-3" = "#9ecae1", "D4-6" = "#4292c6", "D7-9" = "#08519c")

plot_y <- yield_by_sowing_date %>%
  mutate(sow_lookup_date = as.Date(paste0("2026-", sow_md)),
         yield_table = factor(yield_table, levels = season_types))

# 7a. Expected yield by sowing date. Solid = after the rules, dashed = only years inside the table.
# If the two lines are far apart, the out-of-range rule is driving the answer.
plot_y_long <- plot_y %>%
  pivot_longer(c(mean_yield_used, mean_yield_in_table), names_to = "measure", values_to = "yield") %>%
  mutate(measure = recode(measure, mean_yield_used = "After the out-of-range rule",
                          mean_yield_in_table = "Only years inside the yield table"))

p_yield <- ggplot(plot_y_long, aes(sow_lookup_date, yield, colour = yield_table, linetype = measure)) +
  geom_line(linewidth = 0.9, na.rm = TRUE) +
  scale_colour_manual(values = type_colours, name = "Season type") +
  scale_linetype_manual(values = c("After the out-of-range rule" = "solid", "Only years inside the yield table" = "dashed"), name = NULL) +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%d %b") +
  labs(title = paste0("Expected yield by sowing date: ", unique(plot_y$crop)[1], ", ", use_rule),
       subtitle = paste0("Each year uses its own season type's yield table, averaged over the years (2016-2025). Out-of-range rule: ",
                         out_of_range_rule, ". No-emergence rule: ", no_emergence_rule, "."),
       x = "Sowing date", y = "Yield (t/ha)",
       caption = "Gaps = no year of that season type has a yield for that sowing date.") +
  theme_minimal(base_size = 10) +
  theme(panel.grid.minor = element_blank(), legend.position = "top",
        axis.text.x = element_text(angle = 45, hjust = 1), plot.caption = element_text(hjust = 0))
p_yield

# 7b. How many years contribute: years of each season type, and years inside the yield table
plot_n <- plot_y %>%
  pivot_longer(c(n_years, n_years_in_table), names_to = "measure", values_to = "n") %>%
  mutate(measure = recode(measure, n_years = "Years of this season type",
                          n_years_in_table = "of which inside the yield table"))

p_n <- ggplot(plot_n, aes(sow_lookup_date, n, colour = yield_table, linetype = measure)) +
  geom_line(linewidth = 0.9) +
  scale_colour_manual(values = type_colours, name = "Season type") +
  scale_linetype_manual(values = c("Years of this season type" = "solid", "of which inside the yield table" = "dashed"), name = NULL) +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%d %b") +
  labs(title = paste0("Years behind each yield: ", unique(plot_y$crop)[1], ", ", use_rule),
       x = "Sowing date", y = "Number of years") +
  theme_minimal(base_size = 10) +
  theme(panel.grid.minor = element_blank(), legend.position = "top",
        axis.text.x = element_text(angle = 45, hjust = 1))
p_n

ggsave(file.path(out_dir, paste0("Yield_by_sowing_date_", rule_slug, ".png")), p_yield, width = 26, height = 11, units = "cm", dpi = 300)
ggsave(file.path(out_dir, paste0("Years_behind_yield_", rule_slug, ".png")),   p_n,     width = 26, height = 9,  units = "cm", dpi = 300)
