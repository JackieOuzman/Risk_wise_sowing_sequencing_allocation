# =============================================================================
# Sowing optimiser: yield inputs, no-frost stage
# Script 01: Lock met file -> water supply per season -> season types
# Author: Jackie Ouzman        Started: 2026-10-22
#
# PURPOSE
#   Step 1-3 of the proposed workflow (Plan for Oct 2026 Sowing optimiser yield inputs):
#   - Calculate water supply for each season from the Lock met file
#   - Group seasons into season types D1-3, D4-6, D7-9
#   - Convert water supply to a yield environment (YP, t/ha) for cell B2 of
#     Kenton's model (Model sheet)
#
# INPUT
#   LOCK_18046.sim  (SILO 'BoM only', APSIM format, Lock station 018046,
#   extracted 22 Oct 2026, data to 21 Oct 2026)
#
# DEFINITIONS
#   Summer          1 Nov to 31 Mar (Nov-Dec counted toward the NEXT season_year)
#   Growing season  1 Apr to 14 Oct
#   Water supply    0.3 * summer rain + growing season rain - 60 mm
#   Yield potential water supply * 0.026  (t/ha)
#   Decile          water supply (rainfall-based) decile, NOT a yield decile
#   Period          last 30 seasons, 1997-2026 (long-term average)
#   Last 11 seasons (2016-2026) are used later for emergence dates (step 5)
#
# OUTPUT
#   Lock_18046_water_supply_season_types.csv  (saved next to the met file)
# =============================================================================


# ---- 1. Read the Lock met file ----
library(tidyverse)
library(lubridate)


## Read the  met file and check it
met_path <- "D:/work/RiskWise/early_sowing/Tool/Jackie_working/yield inputs proposed workflow/LOCK_18046.sim"

lines      <- readLines(met_path)
header_row <- which(str_detect(lines, "^year\\s"))[1]   # line holding the column names

met <- read_table(
  met_path,
  skip       = header_row + 1,                 # skip the names line and the units line
  col_names  = str_split(str_squish(lines[header_row]), " ")[[1]],
  col_types  = cols(.default = col_double(), code = col_character())
) %>%
  mutate(date = as.Date(paste0(year, "-01-01")) + day - 1)

# ---- 2. Checks ----
range(met$date)                                  # should be 1900-01-01 to 2026-10-21
sum(is.na(met$rain))                             # should be 0

met %>%
  count(year) %>%
  filter(n < 365)                                # only 1900?? and 2026 should show up

met %>%
  group_by(year) %>%
  summarise(annual_rain = sum(rain)) %>%
  filter(year >= 2016) %>%
  print(n = Inf)                                 # Lock should be roughly 300-450 mm in most years




# ---- 3. Water supply per year and season types ----


season_rain <- met %>%
  mutate(
    md = month(date) * 100 + mday(date),         # 1 Apr = 401, 14 Oct = 1014
    period = case_when(
      md >= 1101 | md <= 331 ~ "summer",         # 1 Nov - 31 Mar
      md >= 401 & md <= 1014 ~ "gs",             # 1 Apr - 14 Oct
      TRUE ~ "other"),
    season_year = if_else(md >= 1101, year + 1, year)   # Nov-Dec belongs to the NEXT season
  ) %>%
  filter(period != "other") %>%
  group_by(season_year, period) %>%
  summarise(rain = sum(rain, na.rm = TRUE), n_days = n(), .groups = "drop") %>%
  pivot_wider(names_from = period, values_from = c(rain, n_days))

# keep complete seasons only (drops 1900, which has no Nov-Dec 1899, and 2026, which is unfinished)
water_supply <- season_rain %>%
  filter(n_days_gs == 197, n_days_summer >= 151,
         season_year >= 1997) %>%                       # last 30 seasons: 1997-2026
  mutate(
    summer_period          = paste0("1 Nov ", season_year - 1, " to 31 Mar ", season_year),
    gs_period              = paste0("1 Apr to 14 Oct ", season_year),
    long_term_period       = "last 30 seasons, 1997-2026",
    water_supply            = 0.3 * rain_summer + rain_gs - 60,     # water supply, mm
    water_supply_calulation       = "0.3*summer_rain + GS rain - 60",
    yield_potential         = water_supply * 0.026,                           # yield environment, t/ha
    yield_potential_calulation         = "water_supply * 0.026",
    decile     = ntile(water_supply, 10),
    decile_basis = "water supply decile (rainfall-based: 0.3*summer + GS rain - 60), NOT a yield decile",
    season_type = case_when(
      decile <= 3 ~ "D1-3",
      decile <= 6 ~ "D4-6",
      decile <= 9 ~ "D7-9",
      TRUE        ~ "D10 (not used)")
  )

# save the year-by-year table next to the met file
write_csv(water_supply,
          file.path(dirname(met_path), "Lock_18046_water_supply_season_types.csv"))

# checks
range(water_supply$season_year)                  # expect 1901 to 2025
count(water_supply, season_type)                 # about 37 / 37 / 37 / 12-13

water_supply %>%                                 # one value per season type, for cell B2
  group_by(season_type) %>%
  summarise(n = n(), mean_water_supply = mean(water_supply), 
            median_water_supply = median(water_supply),
            mean_yield_potential = mean(yield_potential), 
            median_yield_potential = median(yield_potential))

water_supply %>%                                 # where the last 10 years fall
  filter(season_year >= 2016) %>%
  select(season_year, rain_summer, 
         rain_gs, 
         water_supply, 
         yield_potential, 
         season_type) %>%
  print(n = Inf)
water_supply %>%
  filter(season_type == "D7-9") %>%
  select(season_year, water_supply, yield_potential)
