# =============================================================================
# Sowing optimiser: yield inputs, no-frost stage
# Script 04: Sowing date -> germination date -> emergence date (Lock, 2016-2025)
# Author: Jackie Ouzman        Started: 2026-10-23
#
# PURPOSE
#   Work backwards from the yield tables (indexed by EMERGENCE date) to the
#   SOWING date. For each year and each candidate sowing date, find:
#     germination date = first day on or after sowing when the rain over the last
#                        5 days reaches the trigger (mm)
#     emergence date   = germination date + 120 degree-days
#   The emergence date is what we look up in the yield tables (step 6).
#
# RAIN TRIGGER RULES (run side by side so they can be workshopped)
#   "Fixed 15 mm"  the single 15 mm rule from the plan
#   Heavy / Medium / Light soil: Kenton's table, trigger falls through autumn
#     Sowing date     Heavy  Medium  Light
#     1 April          25     18      12
#     15 April         20     15      10
#     1 May            15     10       8
#     15 May onwards   15     10       6
#   * Kenton: the germination rule will vary with soil and sowing date. It will
#     give some early germination events and maybe too many late breaks.
#
# ASSUMPTIONS [to confirm with Kenton / Therese]
#   A1 The 5-day window is the 5 days ENDING on the day tested (day d-4 to d).
#   A2 Rain only counts from the sowing day onwards (rain before sowing is ignored),
#      and the sowing day's rain counts.
#   A3 Germination can happen on the sowing day itself if that day alone meets the trigger.
#   A4 Soil table is a step function: a sowing date uses the latest row on or before it
#      (sowing before 1 April uses the 1 April row).
#   A5 Degree-days = max(0, (maxt + mint)/2 - base_temp), base 0 C, added up from the
#      day AFTER germination until 120 is reached.
#   A6 If the trigger is not met within 120 days of sowing, no germination (NA).
#   A7 Years 2016-2025 (2026 excluded) [to confirm].
#   A8 Sowing dates every 5 days from 1 Apr to 30 Jun [to confirm against the optimiser's dates].
# =============================================================================

library(tidyverse)

# ---- 0. Settings ------------------------------------------------------------
met_path <- "D:/work/RiskWise/early_sowing/Tool/Jackie_working/yield inputs proposed workflow/LOCK_18046.sim"
out_dir  <- "D:/work/RiskWise/early_sowing/Tool/Jackie_working/yield inputs proposed workflow"

years    <- 2016:2025                                                  # A7
sow_grid <- seq(as.Date("2026-04-01"), as.Date("2026-06-30"), by = "5 days")   # A8 (2026 = placeholder year)
sow_md   <- format(sow_grid, "%m-%d")

germ_window_days    <- 5      # A1
max_days_to_germ    <- 120    # A6
dd_target           <- 120    # degree-days from germination to emergence
base_temp           <- 0      # A5
max_days_to_emerge  <- 90     # safety limit when adding up degree-days

rule_names <- c("Fixed 15 mm", "Heavy soil", "Medium soil", "Light soil")
main_rule  <- "Fixed 15 mm"   # rule used for the yield lookup in step 6

# Kenton's trigger table (mm of rain over 5 days)
soil_triggers <- tribble(
  ~from_md, ~`Heavy soil`, ~`Medium soil`, ~`Light soil`,
  "04-01",  25,            18,             12,
  "04-15",  20,            15,             10,
  "05-01",  15,            10,             8,
  "05-15",  15,            10,             6
)

# trigger (mm) for a sowing date and rule                                   # A4
get_trigger <- function(sow_date, rule) {
  if (rule == "Fixed 15 mm") return(15)
  md  <- format(sow_date, "%m-%d")
  row <- max(1, sum(soil_triggers$from_md <= md))
  soil_triggers[[rule]][row]
}

# ---- 1. Read the Lock met file -----------------------------------------------
met_lines <- read_lines(met_path)
hdr       <- which(str_detect(met_lines, "^year"))[1]        # column names line
met_names <- str_split(str_squish(met_lines[hdr]), " ")[[1]]
met_raw   <- read_table(I(met_lines[(hdr + 2):length(met_lines)]),   # skip the units line
                        col_names = met_names, show_col_types = FALSE)

met <- met_raw %>%
  mutate(date = as.Date(paste0(year, "-01-01")) + day - 1) %>%
  select(date, maxt, mint, rain)

range(met$date)                                   # 1901-01-01 to 2026-10-21 (approx)
met %>% summarise(across(c(maxt, mint, rain), ~ sum(is.na(.x))))     # expect 0 missing

# ---- 2. Germination and emergence functions ----------------------------------
find_germination <- function(sow_date, trigger_mm) {
  d <- met %>% filter(date >= sow_date, date <= sow_date + max_days_to_germ)
  cs  <- c(0, cumsum(d$rain))                     # cumulative rain, starting at the sowing day (A2)
  idx <- seq_len(nrow(d))
  win <- cs[idx + 1] - cs[pmax(idx - germ_window_days, 0) + 1]   # rain in the last 5 days (A1)
  hit <- which(win >= trigger_mm)[1]              # first day the trigger is met (A3)
  as.numeric(d$date[hit])                         # NA if never met (A6)
}

find_emergence <- function(germ_num) {
  if (is.na(germ_num)) return(NA_real_)
  germ_date <- as.Date(germ_num, origin = "1970-01-01")
  d  <- met %>% filter(date > germ_date, date <= germ_date + max_days_to_emerge)   # A5
  tt <- pmax(0, (d$maxt + d$mint) / 2 - base_temp)
  hit <- which(cumsum(tt) >= dd_target)[1]
  as.numeric(d$date[hit])
}

# ---- 3. Run every rule x year x sowing date ----------------------------------
results <- expand_grid(rule = rule_names, year = years, sow_md = sow_md) %>%
  mutate(sow_date   = as.Date(paste0(year, "-", sow_md)),
         trigger_mm = map2_dbl(sow_date, rule, get_trigger),
         germ_num   = map2_dbl(sow_date, trigger_mm, find_germination),
         emerg_num  = map_dbl(germ_num, find_emergence),
         germination_date = as.Date(germ_num,  origin = "1970-01-01"),
         emergence_date   = as.Date(emerg_num, origin = "1970-01-01"),
         days_sow_to_germ    = as.numeric(germination_date - sow_date),
         days_germ_to_emerge = as.numeric(emergence_date - germination_date),
         days_sow_to_emerge  = as.numeric(emergence_date - sow_date),
         # same calendar day in the placeholder year 2026, used for the yield lookup (step 6)
         emergence_lookup_date = as.Date(paste0("2026-", format(emergence_date, "%m-%d")))) %>%
  select(rule, year, sow_date, sow_md, trigger_mm, germination_date, emergence_date,
         days_sow_to_germ, days_germ_to_emerge, days_sow_to_emerge, emergence_lookup_date)

nrow(results)                                     # 4 rules x 10 years x 19 sowing dates = 760
results %>% filter(rule == main_rule, year == 2016) %>% print(n = Inf)    # eyeball one year

# ---- 4. Checks and summaries --------------------------------------------------
# 4a. Does it look sensible? Autumn emergence should be roughly 1-2 weeks after germination
results %>%
  group_by(rule) %>%
  summarise(no_germination   = sum(is.na(germination_date)),
            median_days_germ_to_emerge = median(days_germ_to_emerge, na.rm = TRUE),
            min_days_germ_to_emerge    = min(days_germ_to_emerge, na.rm = TRUE),
            max_days_germ_to_emerge    = max(days_germ_to_emerge, na.rm = TRUE))

# 4b. For each rule and sowing date: how long until germination, and how often is there a late break?
by_sow <- results %>%
  group_by(rule, sow_md) %>%
  summarise(n_years         = n(),
            n_no_germ       = sum(is.na(germination_date)),
            median_days_to_germ = median(days_sow_to_germ, na.rm = TRUE),
            max_days_to_germ    = max(days_sow_to_germ, na.rm = TRUE),
            median_days_sow_to_emerge = median(days_sow_to_emerge, na.rm = TRUE),
            .groups = "drop")
by_sow %>% filter(rule == main_rule) %>% print(n = Inf)

# 4c. Median days to germination under each rule (wide), to compare rules at a glance
by_sow %>%
  select(rule, sow_md, median_days_to_germ) %>%
  pivot_wider(names_from = rule, values_from = median_days_to_germ) %>%
  print(n = Inf)

# ---- 5. Save -----------------------------------------------------------------
write_csv(results, file.path(out_dir, "Sowing_to_emergence_dates_Lock_2016_2025.csv"))
write_csv(by_sow,  file.path(out_dir, "Sowing_to_emergence_summary_by_sowing_date.csv"))

# ---- 6. Plot: if I sow on this date, when does the crop emerge? --------------
plot_dates <- results %>%
  mutate(sow_lookup_date = as.Date(paste0("2026-", sow_md)),
         rule = factor(rule, levels = rule_names))

plot_median <- plot_dates %>%
  group_by(rule, sow_lookup_date) %>%
  summarise(emerge_median = median(emergence_lookup_date, na.rm = TRUE), .groups = "drop")

p_emerge <- ggplot(plot_dates, aes(sow_lookup_date, emergence_lookup_date)) +
  geom_abline(slope = 1, intercept = 0, colour = "grey70", linetype = "dotted") +
  geom_line(aes(group = year), colour = "#8fb4e8", linewidth = 0.5, alpha = 0.8) +
  geom_line(data = plot_median, aes(y = emerge_median), colour = "#184f95", linewidth = 1.2) +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%d %b") +
  scale_y_date(date_breaks = "2 weeks", date_labels = "%d %b") +
  facet_wrap(~ rule, nrow = 1) +
  labs(title = "Emergence date for each sowing date, Lock 2016-2025",
       subtitle = "Each light line is one year, dark line is the median. Dotted line = emergence on the day of sowing. Gaps = no germination within 120 days.",
       x = "Sowing date", y = "Emergence date",
       caption = paste0("Germination = rain over the last 5 days reaches the trigger (mm); emergence = germination + ",
                        dd_target, " degree-days (base ", base_temp, " C).")) +
  theme_minimal(base_size = 10) +
  theme(panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold", hjust = 0),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.caption = element_text(hjust = 0))
p_emerge

ggsave(file.path(out_dir, "Sowing_to_emergence_dates.png"), p_emerge,
       width = 26, height = 10, units = "cm", dpi = 300)