# =============================================================================
# Sowing optimiser: yield inputs, no-frost stage
# Script 05: GERMINATION date -> EMERGENCE date (Lock, 2016-2025)
# Author: Jackie Ouzman        Started: 2026-10-23
#
# PURPOSE
#   Reads the germination dates from script 04 and adds the emergence date:
#     emergence date = germination date + 120 degree-days
#   The emergence date is what we look up in the yield tables (step 6).
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
nrow(germination)                                 # 4800
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
  select(rule, year, sow_date, sow_md, trigger_mm, germination_date, emergence_date,
         days_sow_to_germ, days_germ_to_emerge, days_sow_to_emerge, emergence_lookup_date)

nrow(results)
results %>% filter(rule == main_rule, year == 2016,
                   sow_md %in% c("04-01", "04-15", "05-01", "05-15", "06-01"))

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
  filter(sow_md %in% c("03-20", "04-01", "04-15", "05-01", "05-15", "06-01", "06-15", "07-01", "07-15")) %>%
  select(rule, sow_md, median_days_sow_to_emerge) %>%
  pivot_wider(names_from = rule, values_from = median_days_sow_to_emerge)

# ---- 6. Save -----------------------------------------------------------------
write_csv(results,       file.path(out_dir, "Sowing_to_emergence_dates_Lock_2016_2025.csv"))
write_csv(emerge_by_sow, file.path(out_dir, "Sowing_to_emergence_summary_by_sowing_date.csv"))

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