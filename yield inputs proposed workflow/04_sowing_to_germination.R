# =============================================================================
# Sowing optimiser: yield inputs, no-frost stage
# Script 04: Sowing date -> GERMINATION date (Lock, 2016-2025)
# Author: Jackie Ouzman        Started: 2026-10-23
#
# PURPOSE
#   For every year and every candidate sowing date, find the germination date:
#   the first day on or after sowing when the rain over the last 5 days (a rolling
#   TOTAL, not an average) reaches the trigger (mm). Saved for script 05
#   (germination -> emergence).
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
#      and the sowing day's rain counts. (Columns rain_5d_to_sow_day and
#      trigger_if_pre_sowing_rain_counted show what would change if it did count.)
#   A3 Germination can happen on the sowing day itself if that day alone meets the trigger.
#   A4 Soil table is a step function: a sowing date uses the latest row on or before it
#      (sowing before 1 April uses the 1 April row).
#   A6 seed waits in the ground for as long as it takes; NA only if no germination event occurs before the end of the weather record. 
#      A real seed-survival limit is still to confirm with Kenton.
#   A7 Years 2016-2025 (2026 excluded) [to confirm].
#   A8 Sowing dates every day from 1 Mar to 1 Jul: a deliberately broad starting window, to be
#      trimmed from the results of script 05 (section 5e) [to confirm].
# =============================================================================

library(tidyverse)

# ---- 0. Settings ------------------------------------------------------------
met_path <- "D:/work/RiskWise/early_sowing/Tool/Jackie_working/yield inputs proposed workflow/LOCK_18046.sim"
out_dir  <- "D:/work/RiskWise/early_sowing/Tool/Jackie_working/yield inputs proposed workflow"

years <- 2016:2025                                                  # A7

# A8: candidate SOWING dates, every day (2026 is only a placeholder year).
# Deliberately BROAD to start with. The window is a guess: a crop sown early just waits for the
# same break, so early dates add identical rows. Script 05 (section 5e) shows in how many years each
# sowing date emerges inside the yield tables, and we trim the window from that.
sow_window_start <- as.Date("2026-03-01")
sow_window_end   <- as.Date("2026-07-01")
sow_grid <- seq(sow_window_start, sow_window_end, by = "1 day")
sow_md   <- format(sow_grid, "%m-%d")

germ_window_days <- 5      # A1
max_days_to_germ <- 365   # effectively no limit: seed waits for the break (only the end of the met data stops it). Real seed-survival limit still to confirm with Kenton**

rule_names <- c("Fixed 15 mm", "Heavy soil", "Medium soil", "Light soil")

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

# ---- 1. Read the Lock met file (rain only is needed here) --------------------
met_lines <- read_lines(met_path)
hdr       <- which(str_detect(met_lines, "^year"))[1]        # column names line
met_names <- str_split(str_squish(met_lines[hdr]), " ")[[1]]
met_raw   <- read_table(I(met_lines[(hdr + 2):length(met_lines)]),   # skip the units line
                        col_names = met_names, show_col_types = FALSE)

met <- met_raw %>%
  mutate(date = as.Date(paste0(year, "-01-01")) + day - 1) %>%
  select(date, rain)

# keep only the years we run. NOT trimmed to the sowing window: the germination
# search runs up to 120 days past a late sowing date
met <- met %>% filter(date >= as.Date(paste0(min(years), "-01-01")))
range(met$date)                                   # 2016-01-01 to 2026-10-21 (approx)
sum(is.na(met$rain))                              # expect 0 missing

# ---- 2. Germination functions -------------------------------------------------
find_germination <- function(sow_date, trigger_mm) {
  d <- met %>% filter(date >= sow_date, date <= sow_date + max_days_to_germ)
  cs  <- c(0, cumsum(d$rain))                     # cumulative rain, starting at the sowing day (A2)
  idx <- seq_len(nrow(d))
  win <- cs[idx + 1] - cs[pmax(idx - germ_window_days, 0) + 1]   # rain total over the last 5 days (A1)
  hit <- which(win >= trigger_mm)[1]              # first day the trigger is met (A3)
  as.numeric(d$date[hit])                         # NA if never met (A6)
}

# ---- 2b. Trackability: how does the 5-day rain build up to the trigger? ------
# Daily trace for ONE sowing date. rain_last_5d = rain TOTAL over the last 5 days,
# counting only days from the sowing day onwards (A1, A2). Germination is the first
# day rain_last_5d reaches the trigger.
trace_germination <- function(sow_date, trigger, stop_after_germ = TRUE) {
  out <- met %>%
    filter(date >= sow_date, date <= sow_date + max_days_to_germ) %>%
    mutate(day_no       = row_number() - 1,                       # 0 = sowing day
           cum_rain     = cumsum(rain),
           rain_last_5d = cum_rain - lag(cum_rain, germ_window_days, default = 0),
           trigger_mm   = trigger,
           trigger_met  = rain_last_5d >= trigger_mm,
           germination_day = row_number() %in% which(trigger_met)[1]) %>%
    select(date, day_no, rain, rain_last_5d, trigger_mm, trigger_met, germination_day)
  hit <- which(out$germination_day)[1]
  if (stop_after_germ && !is.na(hit)) out <- out[seq_len(min(hit + 2, nrow(out))), ]  # show 2 days past
  out
}

# total rain in the 5 days that triggered germination (only days from sowing onwards)
rain_5d_total <- function(sow_date, germ_date) {
  if (is.na(germ_date)) return(NA_real_)
  met %>%
    filter(date >= max(sow_date, germ_date - (germ_window_days - 1)), date <= germ_date) %>%
    summarise(total = sum(rain)) %>%
    pull(total)
}

# daily rain with the 5-day rolling TOTAL ending on each date, INCLUDING days before sowing.
# Used only to show what the sowing day looked like and for the plots; the germination
# search itself ignores rain before sowing (A2).
met_roll <- met %>%
  arrange(date) %>%
  mutate(cum_rain    = cumsum(rain),
         rain_5d_all = cum_rain - lag(cum_rain, germ_window_days, default = 0)) %>%
  select(date, rain, rain_5d_all)

# ---- 3. Run every rule x year x sowing date ----------------------------------
germination <- expand_grid(rule = rule_names, year = years, sow_md = sow_md) %>%
  mutate(sow_date   = as.Date(paste0(year, "-", sow_md)),
         trigger_mm = map2_dbl(sow_date, rule, get_trigger),
         germ_num   = map2_dbl(sow_date, trigger_mm, find_germination),
         germination_date = as.Date(germ_num, origin = "1970-01-01"),
         days_sow_to_germ = as.numeric(germination_date - sow_date),
         rain_5d_at_germ  = map2_dbl(sow_date, germination_date, rain_5d_total)) %>%   # rain total that met the trigger
  left_join(met %>% select(germination_date = date, rain_on_germ_day = rain),         # rain on the germination day
            by = "germination_date") %>%
  left_join(met_roll %>% select(sow_date = date, rain_on_sow_day = rain,              # the sowing day itself
                                rain_5d_to_sow_day = rain_5d_all),
            by = "sow_date") %>%
  mutate(triggers_on_sow_day = rain_on_sow_day >= trigger_mm,                         # A2: only the sowing day's rain counts
         trigger_if_pre_sowing_rain_counted = rain_5d_to_sow_day >= trigger_mm) %>%   # for information only
  select(rule, year, sow_date, sow_md, trigger_mm,
         rain_on_sow_day, rain_5d_to_sow_day, triggers_on_sow_day, trigger_if_pre_sowing_rain_counted,
         germination_date, rain_on_germ_day, rain_5d_at_germ, days_sow_to_germ)

nrow(germination)                                 # 4 rules x 10 years x 139 sowing dates = 5560
germination %>% filter(rain_5d_at_germ < trigger_mm)     # check: should return no rows
germination %>% filter(triggers_on_sow_day != (days_sow_to_germ == 0))   # check: should return no rows

# how often would counting pre-sowing rain change the answer (A2)?
germination %>%
  count(rule, triggers_on_sow_day, trigger_if_pre_sowing_rain_counted)

germination %>% filter(rule == "Fixed 15 mm", year == 2016, sow_md %in% c("04-01", "04-15", "05-01", "05-15", "06-01"))

# daily trace: 2016, sown 1 April, 15 mm trigger. View() it to scroll through.
trace_germination(as.Date("2016-04-01"), 15) %>% print(n = Inf)

# the same year sown 15 May: a later sowing that may germinate sooner after sowing
trace_germination(as.Date("2016-05-15"), 15) %>% print(n = Inf)

# ---- 4. Checks and summaries --------------------------------------------------
# 4a. Per rule: how many sowings never germinate, and how long does it take?
germination %>%
  group_by(rule) %>%
  summarise(n                = n(),
            no_germination   = sum(is.na(germination_date)),
            median_days_to_germ = median(days_sow_to_germ, na.rm = TRUE),
            max_days_to_germ    = max(days_sow_to_germ, na.rm = TRUE))

# 4b. Per rule and sowing date: how long until germination, and how often a late break?
germ_by_sow <- germination %>%
  group_by(rule, sow_md) %>%
  summarise(n_years             = n(),
            n_no_germ           = sum(is.na(germination_date)),
            median_days_to_germ = median(days_sow_to_germ, na.rm = TRUE),
            max_days_to_germ    = max(days_sow_to_germ, na.rm = TRUE),
            .groups = "drop")

# one line per rule on the 1st and 15th of each month, to compare rules at a glance
germ_by_sow %>%
  filter(sow_md %in% c("03-01", "03-15", "04-01", "04-15", "05-01", "05-15", "06-01", "06-15", "07-01", "07-15")) %>%
  select(rule, sow_md, median_days_to_germ) %>%
  pivot_wider(names_from = rule, values_from = median_days_to_germ)

# ---- 5. Save (script 05 reads the first file) ---------------------------------
write_csv(germination, file.path(out_dir, "Sowing_to_germination_dates_Lock_2016_2025.csv"))
write_csv(germ_by_sow, file.path(out_dir, "Sowing_to_germination_summary_by_sowing_date.csv"))

# ---- 6. Simple illustration: one rule, one year -------------------------------
# Daily rain, the 5-day rolling rain total and the trigger, with the germination date
# for a few example sowing dates. Edit the settings to look at other cases.
ill_rule <- "Fixed 15 mm"
ill_year <- 2016
ill_sow  <- c("03-20", "04-15", "05-01", "05-15", "06-15", "07-01")   # example sowing dates (month-day)

# x axis: the sowing window (1 Mar-1 Jul) plus a week either side
ill_start <- as.Date(paste0(ill_year, "-03-13"))
ill_end   <- as.Date(paste0(ill_year, "-07-24"))

ill_germ <- germination %>%
  filter(rule == ill_rule, year == ill_year, sow_md %in% ill_sow) %>%
  arrange(sow_date) %>%
  mutate(row_y = -4 * row_number(),             # height of each sowing-to-germination bar, below the rain
         label = paste0("Sown ", format(sow_date, "%d %b"), ": ",
                        if_else(is.na(germination_date), "no germination",
                                paste0("germinates ", format(germination_date, "%d %b"),
                                       " (", days_sow_to_germ, " days)"))))

ill_trigger <- ill_germ$trigger_mm[1]
ill_daily   <- met_roll %>% filter(date >= ill_start, date <= ill_end)

p_ill <- ggplot() +
  geom_col(data = ill_daily, aes(date, rain), fill = "#5598e7", width = 0.9) +
  geom_line(data = ill_daily, aes(date, rain_5d_all), colour = "#184f95", linewidth = 0.9) +
  geom_hline(yintercept = ill_trigger, colour = "#c77c00", linetype = "dashed", linewidth = 0.8) +
  annotate("text", x = ill_end, y = ill_trigger, hjust = 1, vjust = -0.6, size = 3,
           colour = "#c77c00", label = paste0("Trigger: ", ill_trigger, " mm in 5 days")) +
  geom_vline(data = ill_germ %>% filter(!is.na(germination_date)) %>% distinct(germination_date),
             aes(xintercept = germination_date), colour = "#c77c00", linetype = "dotted") +
  geom_segment(data = ill_germ, aes(x = sow_date, xend = germination_date, y = row_y, yend = row_y),
               colour = "#1d6b24", linewidth = 1.2) +
  geom_point(data = ill_germ, aes(sow_date, row_y), shape = 25, size = 3,
             fill = "#1d6b24", colour = "#1d6b24") +
  geom_point(data = ill_germ %>% filter(!is.na(germination_date)), aes(germination_date, row_y),
             shape = 8, size = 3, colour = "#1d6b24") +
  geom_text(data = ill_germ, aes(x = ill_start, y = row_y, label = label),     # labels in a column at the left
            hjust = 0, vjust = -1.1, size = 3, colour = "#1d6b24") +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%d %b") +
  scale_y_continuous(breaks = seq(0, ceiling(max(ill_daily$rain_5d_all) / 10) * 10, 10),
                     limits = c(-4 * (nrow(ill_germ) + 1), NA)) +
  coord_cartesian(xlim = c(ill_start, ill_end)) +     # a bar for a late sowing runs off the right edge if it germinates after 24 Jul
  labs(title = paste0("How germination is triggered: ", ill_rule, ", Lock ", ill_year),
       subtitle = paste0("Bars = daily rain (mm). Dark line = rain total over the last 5 days. Dashed line = trigger.\n",
                         "Green = sowing (triangle) to germination (star): the first day on or after sowing that the 5-day total reaches the trigger."),
       x = NULL, y = "Rain (mm)",
       caption = paste0("Rain before the sowing date does not count (A2), so the line can sit above the trigger before a sowing date without germinating it.\n",
                        "The 5-day total shown here includes days before sowing; the germination search counts only days from sowing.")) +
  theme_minimal(base_size = 10) +
  theme(panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.subtitle = element_text(lineheight = 1.1),
        plot.caption = element_text(hjust = 0, lineheight = 1.1))
p_ill

ggsave(file.path(out_dir, paste0("Germination_trigger_illustration_", ill_year, ".png")), p_ill,
       width = 28, height = 14, units = "cm", dpi = 300)

# ---- 7. Rain, rolling total and germination events: every year ----------------
# Between the one-year illustration and the all-years summary. Faint bars = daily rain,
# dark blue line = 5-day rolling total, grey dashed line = trigger, dots = germination
# events (days on which at least one sowing date in the window germinates). Bigger dot =
# more sowing dates germinate on that day; only the bigger events are labelled with a date.
plot_rule      <- "Fixed 15 mm"   # one trigger line only, so keep the fixed rule here
plot_years     <- 2016:2020       # 5 years at a time (wider panels), then 2021:2025. Use 2016 alone for one large panel
ev_start       <- "03-01"         # calendar window shown (month-day)
ev_end         <- "08-31"
ev_show_labels <- TRUE            # FALSE hides the event dates
ev_label_min   <- 5               # only label events that germinate at least this many sowing dates
ev_ncol        <- 1               # panels per row (1 = wide panels)

ev_trigger <- germination %>% filter(rule == plot_rule) %>% pull(trigger_mm) %>% unique()
stopifnot(length(ev_trigger) == 1)          # the soil rules change with sowing date, so no single line

# daily rain and rolling total, put on a common 2026 placeholder year so years sit on the same axis
ev_daily <- met_roll %>%
  mutate(year = as.integer(format(date, "%Y")), md = format(date, "%m-%d")) %>%
  filter(year %in% plot_years, md >= ev_start, md <= ev_end) %>%
  mutate(plot_date = as.Date(paste0("2026-", md)))

# germination events: each distinct germination date, and how many sowing dates germinate on it
ev_all <- germination %>%
  filter(rule == plot_rule, year %in% plot_years, !is.na(germination_date)) %>%
  count(year, germination_date, name = "n_sowing_days") %>%
  mutate(md        = format(germination_date, "%m-%d"),
         plot_date = as.Date(paste0("2026-", md)),
         label     = format(germination_date, "%d %b"))
ev_events <- ev_all %>%
  filter(md >= ev_start, md <= ev_end) %>%
  left_join(met_roll %>% select(germination_date = date, total_5d = rain_5d_all),   # height of the event dot
            by = "germination_date")
message(nrow(ev_all) - nrow(ev_events), " germination events fall outside the plotted window")

ev_one      <- length(plot_years) == 1
ev_trig_lab <- tibble(year = plot_years[1], plot_date = as.Date("2026-03-02"),
                      label = paste0("Trigger: ", ev_trigger, " mm in 5 days"))

p_events <- ggplot() +
  geom_col(data = ev_daily, aes(plot_date, rain), fill = "#5598e7", alpha = 0.75, width = 0.9) +
  geom_line(data = ev_daily, aes(plot_date, rain_5d_all), colour = "#184f95", linewidth = 0.5) +
  geom_hline(yintercept = ev_trigger, colour = "grey55", linetype = "dashed", linewidth = 0.6) +
  geom_text(data = ev_trig_lab, aes(plot_date, ev_trigger, label = label),
            hjust = 0, vjust = -0.6, size = 2.8, colour = "grey40") +
  geom_segment(data = ev_events, aes(x = plot_date, xend = plot_date, y = 0, yend = total_5d),
               colour = "grey25", linewidth = 0.35) +                       # thin line up to the rolling total
  geom_point(data = ev_events, aes(plot_date, total_5d, size = n_sowing_days), colour = "grey25") +
  scale_size_area(max_size = 4, breaks = c(1, 10, 30, 60), name = "Sowing dates\ngerminating") +
  (if (ev_show_labels)
    geom_text(data = ev_events %>% filter(n_sowing_days >= ev_label_min),
              aes(plot_date, total_5d, label = label),
              angle = 90, hjust = -0.25, vjust = 0.4, size = 2.6, colour = "grey25")
   else NULL) +
  scale_x_date(date_breaks = if (ev_one) "2 weeks" else "1 month",
               date_labels = if (ev_one) "%d %b" else "%b") +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.3))) +
  coord_cartesian(xlim = as.Date(c(paste0("2026-", ev_start), paste0("2026-", ev_end)))) +
  facet_wrap(~ year, ncol = ev_ncol) +
  labs(title = paste0("Rain and germination events, ", plot_rule, ", Lock"),
       subtitle = paste0("Faint bars = daily rain (mm). Dark blue line = rain total over the last 5 days. Grey dashed line = trigger.\n",
                         "Dots = germination events: a day when at least one sowing date (1 Feb-31 Aug) germinates. Bigger dot = more sowing dates germinate then.\n",
                         "Dates are shown for events that germinate ", ev_label_min, " or more sowing dates."),
       x = NULL, y = "Rain (mm)",
       caption = paste0("One event can germinate many sowing dates: every sowing before it waits for it.\n",
                        "Rain before sowing does not count (A2), and the rolling total shown includes days before sowing, ",
                        "so the line can be above the trigger without a germination event.")) +
  theme_minimal(base_size = 10) +
  theme(panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold", hjust = 0),
        plot.subtitle = element_text(lineheight = 1.1),
        plot.caption = element_text(hjust = 0, lineheight = 1.1))
p_events

ev_height <- if (ev_one) 11 else 4 + 4.5 * ceiling(length(plot_years) / ev_ncol)
ggsave(file.path(out_dir, paste0("Germination_events_", str_replace_all(plot_rule, " ", "_"), ".png")), p_events,
       width = 28, height = ev_height, units = "cm", dpi = 300)

# ---- 8. Plot: all years, one rule ---------------------------------------------
# Same idea as the illustration above, for every sowing date and every year.
# Reading a panel: pick a sowing date on the bottom axis, go up to the line, read across
# to the date it germinates. Dotted line = germinates on the day of sowing.
# Flat stretches = a run of sowing dates that all wait for the same rain event (the break).
sum_rule <- "Fixed 15 mm"          # change to "Heavy soil", "Medium soil" or "Light soil" to compare

plot_germ <- germination %>%
  filter(rule == sum_rule) %>%
  mutate(sow_lookup_date  = as.Date(paste0("2026-", sow_md)),            # 2026 is only a placeholder year
         germ_lookup_date = as.Date(paste0("2026-", format(germination_date, "%m-%d"))))

p_germ <- ggplot(plot_germ, aes(sow_lookup_date, germ_lookup_date)) +
  geom_abline(slope = 1, intercept = 0, colour = "grey60", linetype = "dotted") +
  geom_line(colour = "#184f95", linewidth = 0.8) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_date(date_breaks = "1 month", date_labels = "%b") +
  facet_wrap(~ year, nrow = 2) +
  labs(title = paste0("Sowing date and germination date, ", sum_rule, ", Lock 2016-2025"),
       subtitle = paste0("One panel per year. Sown on (bottom) and germinates on (side). Dotted line = germinates on the day of sowing.\n",
                         "Flat stretches = sowing dates that all wait for the same rain event. Gaps = no germination before the end of the weather record."),
       x = "Sown on", y = "Germinates on",
       caption = "Germination = first day on or after sowing when the rain total over the last 5 days reaches the trigger (mm).") +
  theme_minimal(base_size = 10) +
  theme(panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold", hjust = 0),
        plot.subtitle = element_text(lineheight = 1.1),
        plot.caption = element_text(hjust = 0))
p_germ

ggsave(file.path(out_dir, paste0("Sowing_to_germination_dates_", str_replace_all(sum_rule, " ", "_"), ".png")), p_germ,
       width = 26, height = 12, units = "cm", dpi = 300)
