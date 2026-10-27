# =============================================================================
# Sowing optimiser: yield inputs, no-frost stage
# Script 05: GERMINATION date -> EMERGENCE date (Lock, 2016-2025)
# Author: Jackie Ouzman        Started: 2026-10-27
#
# PURPOSE
#   Reads the germination dates from script 04 and adds the emergence date:
#     emergence date = germination date + 120 degree-days
#   The emergence date is what we look up in the yield tables (step 6).
#   Also works out the SOWING WINDOW for each yield table: which sowing dates
#   give an emergence date inside Kenton's (20 Mar-10 Jun) and Ed's (8 Apr-17 Jul) range.
#
# INPUT
#   Sowing_to_germination_dates_Lock_2016_2025.csv   (saved by script 04)
#   LOCK_18046.sim                                   (daily max and min temperature)
#
# ASSUMPTIONS [to confirm with Kenton / Therese]
#   A5 Degree-days = max(0, (maxt + mint)/2 - base_temp), base 0 C, added up from the
#      day AFTER germination until 120 is reached.
#   A6 No germination (NA) in script 04 means no emergence date either.
#   A9 If 120 degree-days are not reached within 90 days of germination, emergence is NA.
# =============================================================================

library(tidyverse)

# ---- 0. Settings ------------------------------------------------------------
met_path <- "D:/work/RiskWise/early_sowing/Tool/Jackie_working/yield inputs proposed workflow/LOCK_18046.sim"
out_dir  <- "D:/work/RiskWise/early_sowing/Tool/Jackie_working/yield inputs proposed workflow"

dd_target          <- 120    # degree-days from germination to emergence
base_temp          <- 0      # A5
max_days_to_emerge <- 90     # A9

rule_names <- c("Fixed 15 mm", "Heavy soil", "Medium soil", "Light soil")
main_rule  <- "Fixed 15 mm"  # rule used for the yield lookup in step 6

# ---- 1. Read the germination dates (script 04) --------------------------------
germination <- read_csv(file.path(out_dir, "Sowing_to_germination_dates_Lock_2016_2025.csv"),
                        show_col_types = FALSE)
nrow(germination)                                 # 5560 = 4 rules x 10 years x 139 sowing dates
sum(is.na(germination$germination_date))          # sowings with no germination

# ---- 2. Read the Lock met file (temperature only is needed here) -------------
met_lines <- read_lines(met_path)
hdr       <- which(str_detect(met_lines, "^year"))[1]
met_names <- str_split(str_squish(met_lines[hdr]), " ")[[1]]
met_raw   <- read_table(I(met_lines[(hdr + 2):length(met_lines)]),
                        col_names = met_names, show_col_types = FALSE)

met <- met_raw %>%
  mutate(date = as.Date(paste0(year, "-01-01")) + day - 1) %>%
  select(date, maxt, mint) %>%
  filter(date >= as.Date(paste0(min(germination$year), "-01-01")))   # years only, not trimmed to the window
range(met$date)
met %>% summarise(across(c(maxt, mint), ~ sum(is.na(.x))))          # expect 0 missing

# ---- 3. Emergence function ----------------------------------------------------
find_emergence <- function(germ_date) {
  if (is.na(germ_date)) return(NA_real_)                             # A6
  d  <- met %>% filter(date > germ_date, date <= germ_date + max_days_to_emerge)   # A5
  tt <- pmax(0, (d$maxt + d$mint) / 2 - base_temp)
  hit <- which(cumsum(tt) >= dd_target)[1]
  as.numeric(d$date[hit])                                            # NA if not reached (A9)
}

# ---- 4. Add the emergence date to every row ----------------------------------
results <- germination %>%
  mutate(emerg_num      = map_dbl(germination_date, find_emergence),
         emergence_date = as.Date(emerg_num, origin = "1970-01-01"),
         days_germ_to_emerge = as.numeric(emergence_date - germination_date),
         days_sow_to_emerge  = as.numeric(emergence_date - sow_date),
         # same calendar day in the placeholder year 2026, used for the yield lookup (step 6)
         emergence_lookup_date = as.Date(paste0("2026-", format(emergence_date, "%m-%d")))) %>%
  select(-emerg_num)

nrow(results)
results %>% filter(rule == main_rule, year == 2016,
                   sow_md %in% c("04-01", "04-15", "05-01", "05-15", "06-01")) %>%
  select(rule, sow_date, trigger_mm, germination_date, emergence_date,
         days_sow_to_germ, days_germ_to_emerge, days_sow_to_emerge)

# ---- 5. Checks and summaries --------------------------------------------------
# 5a. Autumn emergence should be roughly 1-2 weeks after germination
results %>%
  group_by(rule) %>%
  summarise(no_emergence = sum(is.na(emergence_date)),
            median_days_germ_to_emerge = median(days_germ_to_emerge, na.rm = TRUE),
            min_days_germ_to_emerge    = min(days_germ_to_emerge, na.rm = TRUE),
            max_days_germ_to_emerge    = max(days_germ_to_emerge, na.rm = TRUE))

# 5b. Days from germination to emergence by month of germination (it should get longer as it cools)
results %>%
  filter(rule == main_rule, !is.na(emergence_date)) %>%
  group_by(germination_month = format(germination_date, "%m %b")) %>%
  summarise(n = n(), median_days = median(days_germ_to_emerge),
            min_days = min(days_germ_to_emerge), max_days = max(days_germ_to_emerge))

# 5c. Per rule and sowing date: median days from sowing to emergence
emerge_by_sow <- results %>%
  group_by(rule, sow_md) %>%
  summarise(n_years        = n(),
            n_no_emergence = sum(is.na(emergence_date)),
            median_days_sow_to_emerge = median(days_sow_to_emerge, na.rm = TRUE),
            max_days_sow_to_emerge    = max(days_sow_to_emerge, na.rm = TRUE),
            .groups = "drop")

emerge_by_sow %>%
  filter(sow_md %in% c("03-01", "03-15", "04-01", "04-15", "05-01", "05-15", "06-01", "06-15", "07-01", "07-15")) %>%
  select(rule, sow_md, median_days_sow_to_emerge) %>%
  pivot_wider(names_from = rule, values_from = median_days_sow_to_emerge)

# 5d. SOWING WINDOW for each yield table: which sowing dates emerge inside its date range?
# "any year"  = sowing dates that emerge in range in at least one of the 10 years
# "all years" = sowing dates that emerge in range in every one of the 10 years
emerge_ranges <- tribble(~model,          ~from_md, ~to_md,
                         "Kenton",        "03-20",  "06-10",
                         "Ed",            "04-08",  "07-17",
                         "Both together", "03-20",  "07-17") %>%
  mutate(from = as.Date(paste0("2026-", from_md)),
         to   = as.Date(paste0("2026-", to_md)))

window_by_sow <- results %>%
  select(rule, year, sow_md, emergence_lookup_date) %>%
  crossing(emerge_ranges %>% select(model, from, to)) %>%
  mutate(in_range = !is.na(emergence_lookup_date) &
           emergence_lookup_date >= from & emergence_lookup_date <= to) %>%
  group_by(rule, model, sow_md) %>%
  summarise(n_years = n(), years_in_range = sum(in_range), .groups = "drop")

md_label <- function(x) {                                   # "04-15" -> "15 Apr", NA stays NA
  out <- rep(NA_character_, length(x))
  ok  <- !is.na(x)
  out[ok] <- format(as.Date(paste0("2026-", x[ok])), "%d %b")
  out
}
window_label <- function(from, to)
  if_else(is.na(from) | is.na(to), "none (no sowing date)", paste(md_label(from), "to", md_label(to)))

sowing_window <- window_by_sow %>%
  group_by(rule, model) %>%
  summarise(sow_from_any_year  = first(sow_md[years_in_range > 0]),
            sow_to_any_year    = last(sow_md[years_in_range > 0]),
            sow_from_all_years = first(sow_md[years_in_range == n_years]),
            sow_to_all_years   = last(sow_md[years_in_range == n_years]),
            .groups = "drop") %>%
  left_join(emerge_ranges %>% select(model, from_md, to_md), by = "model") %>%
  mutate(emergence_range = paste(md_label(from_md), "to", md_label(to_md))) %>%
  transmute(rule, model, emergence_range,
            sowing_any_year  = window_label(sow_from_any_year, sow_to_any_year),
            sowing_all_years = window_label(sow_from_all_years, sow_to_all_years),
            sow_from_any_year, sow_to_any_year) %>%
  arrange(match(rule, rule_names), match(model, emerge_ranges$model))

sowing_window %>% filter(rule == main_rule) %>% select(-rule, -sow_from_any_year, -sow_to_any_year)
sowing_window %>% select(-sow_from_any_year, -sow_to_any_year) %>% print(n = Inf)    # all four rules

# 5e. FOR TRIMMING THE WINDOW: in how many of the 10 years does each sowing date emerge inside
# each yield table's emergence range? (fortnightly, main rule). Trim to the dates where
# at least trim_min_years of the 10 years land in range (suggested: 5).
trim_min_years <- 5
window_by_sow %>%
  filter(rule == main_rule,
         sow_md %in% c("02-01", "02-15", "03-01", "03-15", "04-01", "04-15", "05-01", "05-15",
                       "06-01", "06-15", "07-01", "07-15", "08-01", "08-15", "08-31")) %>%
  select(model, sow_md, years_in_range) %>%
  pivot_wider(names_from = model, values_from = years_in_range)

# the sowing dates that meet the threshold, by model
window_by_sow %>%
  filter(rule == main_rule, years_in_range >= trim_min_years) %>%
  group_by(model) %>%
  summarise(first_sowing_date = md_label(first(sow_md)),
            last_sowing_date  = md_label(last(sow_md)),
            n_dates           = n())

# ---- 6. Save -----------------------------------------------------------------
write_csv(results,       file.path(out_dir, "Sowing_to_emergence_dates_Lock_2016_2025.csv"))
write_csv(emerge_by_sow, file.path(out_dir, "Sowing_to_emergence_summary_by_sowing_date.csv"))
write_csv(sowing_window %>% select(-sow_from_any_year, -sow_to_any_year),
          file.path(out_dir, "Sowing_window_for_each_yield_table.csv"))

# ---- 7. Plot: if I sow on this date, when does the crop emerge? --------------
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
       subtitle = "Each light line is one year, dark line is the median. Dotted line = emergence on the day of sowing. Gaps = no germination or emergence.",
       x = "Sowing date", y = "Emergence date",
       caption = paste0("Germination from script 04; emergence = germination + ", dd_target,
                        " degree-days (base ", base_temp, " C).")) +
  theme_minimal(base_size = 10) +
  theme(panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold", hjust = 0),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.caption = element_text(hjust = 0))
p_emerge

ggsave(file.path(out_dir, "Sowing_to_emergence_dates.png"), p_emerge,
       width = 26, height = 10, units = "cm", dpi = 300)



results %>%
  filter(rule == main_rule, !is.na(emergence_date)) %>%
  group_by(year) %>%
  filter(emergence_date == min(emergence_date)) %>%
  summarise(earliest_emergence = first(emergence_date),
            germination_date   = first(germination_date),
            latest_sowing_that_gives_it = max(sow_date), .groups = "drop")
