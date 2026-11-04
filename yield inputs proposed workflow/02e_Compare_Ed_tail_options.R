# =============================================================================
# Sowing optimiser: yield inputs, no-frost stage
# Script 02e: Compare ways of extending Ed's yield curves at both ends
# Author: Jackie Ouzman        Started: 2026-11-04
#
# PURPOSE
#   Ed's table covers emergence 8 Apr to 17 Jul. Options for the two ends, out to 1 Mar and 31 Aug:
#     Tail (after 17 Jul):
#       A. hold the last value
#       B. straight line from the last value to zero on 31 Aug
#       C. fall at a set % of Ed's peak per day (Kenton's late slope is 0.61 to 0.82 % per day), never below 0
#       D. broken stick (two straight lines, best breakpoint), joined to the last table point, never below 0
#     Early end (before 8 Apr):
#       straight line through the first 2 table points (script 02d)
#       D. broken stick, joined to the first table point, never below 0
#
# INPUT   Yield_by_emergence_date_Kenton_and_Ed.csv   (script 02)
# OUTPUT  Yield_Ed_extension_options_compared.png
# =============================================================================

library(tidyverse)

# ---- 0. Settings ------------------------------------------------------------
out_dir  <- "D:/work/RiskWise/early_sowing/Tool/Jackie_working/yield inputs proposed workflow"
in_file  <- "Yield_by_emergence_date_Kenton_and_Ed.csv"

extend_from     <- as.Date("2026-03-01")
tail_end        <- as.Date("2026-08-31")
step_days       <- 5
late_slope_pct  <- 0.7                    # option C: % of Ed's peak lost per day
plot_crops      <- c("Canola", "Lentils") # beans and lupins = canola, peas = lentils

opt_A <- "A. Hold the last value"
opt_B <- "B. Straight line to zero on 31 Aug"
opt_C <- paste0("C. Fall ", late_slope_pct, "% of peak per day")
opt_D <- "D. Broken stick"
opt_E <- "Early end: straight line, first 2 points (02d)"

# ---- 1. Read Ed's tables --------------------------------------------------------
yields_ed <- read_csv(file.path(out_dir, in_file), show_col_types = FALSE) %>%
  filter(str_detect(source, "^Ed"), crop_or_cultivar %in% plot_crops) %>%
  mutate(emergence_date = as.Date(emergence_date))

# ---- 2. Broken stick: two straight lines joined at a breakpoint, best breakpoint by day -----
stick_design <- function(x, b) tibble(lo = pmin(x - b, 0), hi = pmax(x - b, 0))

fit_stick <- function(x, y) {
  n    <- length(x)
  grid <- seq(x[3], x[n - 2], by = 1)                      # breakpoint kept away from the ends
  rss  <- map_dbl(grid, ~ sum(resid(lm(y ~ lo + hi, data = bind_cols(tibble(y = y), stick_design(x, .x))))^2))
  b    <- grid[which.min(rss)]
  list(b = b, model = lm(y ~ lo + hi, data = bind_cols(tibble(y = y), stick_design(x, b))))
}
predict_stick <- function(st, x_new) as.numeric(predict(st$model, newdata = stick_design(x_new, st$b)))

# ---- 3. Build the options for each curve ------------------------------------------------------
# One curve = one crop and season type. Every option starts at the table point it joins, so lines join up.
make_ext <- function(curve, key) {
  curve   <- arrange(curve, emergence_date)
  x       <- as.numeric(curve$emergence_date)
  y       <- curve$yield_t_ha
  n       <- length(x)
  first_d <- min(curve$emergence_date)
  last_d  <- max(curve$emergence_date)
  first_y <- y[1]
  last_y  <- y[n]
  peak    <- max(y)
  st      <- fit_stick(x, y)
  
  d_tail  <- sort(unique(c(last_d,
                           if (last_d + step_days <= tail_end) seq(last_d + step_days, tail_end, by = step_days),
                           tail_end)))
  d_early <- sort(unique(c(seq(extend_from, first_d - 1, by = step_days), first_d)))
  
  days_after   <- as.numeric(d_tail - last_d)
  days_in_tail <- as.numeric(tail_end - last_d)
  slope_early  <- (y[2] - y[1]) / (x[2] - x[1])
  
  bind_rows(
    tibble(end = "tail",  option = opt_A, emergence_date = d_tail,
           yield_t_ha = last_y),
    tibble(end = "tail",  option = opt_B, emergence_date = d_tail,
           yield_t_ha = last_y * (1 - days_after / days_in_tail)),
    tibble(end = "tail",  option = opt_C, emergence_date = d_tail,
           yield_t_ha = pmax(0, last_y - peak * late_slope_pct / 100 * days_after)),
    tibble(end = "tail",  option = opt_D, emergence_date = d_tail,
           yield_t_ha = pmax(0, last_y + predict_stick(st, as.numeric(d_tail)) - predict_stick(st, x[n]))),
    tibble(end = "early", option = opt_E, emergence_date = d_early,
           yield_t_ha = pmax(0, first_y + slope_early * (as.numeric(d_early) - x[1]))),
    tibble(end = "early", option = opt_D, emergence_date = d_early,
           yield_t_ha = pmax(0, first_y + predict_stick(st, as.numeric(d_early)) - predict_stick(st, x[1])))
  ) %>%
    mutate(break_date = as.Date(st$b))
}

ext <- yields_ed %>%
  group_by(crop_or_cultivar, season_type) %>%
  group_modify(make_ext) %>%
  ungroup()

# ---- 4. Checks ----------------------------------------------------------------------------------
# where the broken stick puts its breakpoint
ext %>% distinct(crop_or_cultivar, season_type, break_date) %>% print(n = Inf)

# yield at 15 Aug and 31 Aug under each tail option
ext %>% filter(end == "tail", emergence_date %in% as.Date(c("2026-08-15", "2026-08-31"))) %>%
  mutate(emergence_date = format(emergence_date, "%d %b")) %>%
  select(crop_or_cultivar, season_type, emergence_date, option, yield_t_ha) %>%
  pivot_wider(names_from = option, values_from = yield_t_ha) %>%
  arrange(crop_or_cultivar, emergence_date, season_type) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2))) %>% print(n = Inf, width = Inf)

# yield at 1 Mar and 1 Apr under each early option
ext %>% filter(end == "early", emergence_date %in% as.Date(c("2026-03-01", "2026-04-01"))) %>%
  mutate(emergence_date = format(emergence_date, "%d %b")) %>%
  select(crop_or_cultivar, season_type, emergence_date, option, yield_t_ha) %>%
  pivot_wider(names_from = option, values_from = yield_t_ha) %>%
  arrange(crop_or_cultivar, emergence_date, season_type) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2))) %>% print(n = Inf, width = Inf)

# ---- 5. Plot: the whole curve, 1 Mar to 31 Aug -----------------------------------------------------
option_colours <- setNames(c("#7f7f7f", "#d95f02", "#7570b3", "#1b9e77", "#a6761d"),
                           c(opt_A, opt_B, opt_C, opt_D, opt_E))

p_ext <- ggplot() +
  geom_line(data = yields_ed, aes(emergence_date, yield_t_ha, group = interaction(crop_or_cultivar, season_type)),
            colour = "black", linewidth = 0.7) +
  geom_point(data = yields_ed, aes(emergence_date, yield_t_ha), colour = "black", size = 1.5) +
  geom_line(data = ext, aes(emergence_date, yield_t_ha, colour = option, group = interaction(option, end)), linewidth = 0.9) +
  geom_point(data = ext, aes(emergence_date, yield_t_ha, colour = option), size = 1.2, shape = 1) +
  scale_colour_manual(values = option_colours, name = NULL, breaks = names(option_colours)) +
  scale_x_date(limits = c(extend_from, tail_end), date_breaks = "1 month", date_labels = "%b") +
  facet_grid(crop_or_cultivar ~ season_type, switch = "y") +
  labs(title = "Ed's yield by emergence date: ways to extend the curve at both ends",
       subtitle = "Black = Ed's table (8 Apr to 17 Jul). Coloured = options for 1 Mar to 8 Apr and 17 Jul to 31 Aug.",
       x = "Emergence date", y = "Yield (t/ha)") +
  guides(colour = guide_legend(nrow = 3)) +
  theme_minimal(base_size = 10) +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom",
        strip.placement = "outside", strip.text = element_text(face = "bold"))
p_ext

ggsave(file.path(out_dir, "Yield_Ed_extension_options_compared.png"), p_ext,
       width = 26, height = 14, units = "cm", dpi = 300)
