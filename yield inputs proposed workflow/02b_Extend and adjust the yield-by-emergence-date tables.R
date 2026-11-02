# =============================================================================
# Sowing optimiser: yield inputs, no-frost stage
# Script 02b: Extend and adjust the yield-by-emergence-date tables
# Author: Jackie Ouzman        Started: 2026-11-02
#
# PURPOSE
#   Takes the yield tables from script 02 (yield by EMERGENCE date for each crop or cultivar and
#   season type) and
#     #     1. extends Kenton's curves to more emergence dates (1 Mar to 31 Aug) with a broken stick: rise and fall in      # changed
#        two straight lines fitted to the table (extend_method; "line" = straight line from the last 3 dates).       # changed
#        Never below 0, and shifted to join Kenton's table. Ed's curves are left as they are.                         # changed
#        Therese's 30 Oct 2026 figures were 10% for D1-6 and 20% for D7-9, reduced so wheat stays at or      # changed
#        below barley). Barley is left as it is.                                                              # changed
#   Script 06 then reads the new file and looks up yield at each emergence date, with no
#   extrapolation or uplift of its own.
#
# INPUT   Yield_by_emergence_date_Kenton_and_Ed.csv      (script 02)
# OUTPUT  Yield_by_emergence_date_extended.csv           (read by script 06)
#         Yield_by_emergence_date_extended.png           (check plot)
#
# ASSUMPTIONS [to confirm with Kenton / Therese]
#   The shape of Kenton's curve continues in a straight line beyond its first / last dates.
#   Kenton's curve is one shape times a peak, so the line scales with the season type's height.
# =============================================================================

library(tidyverse)

# ---- 0. Settings ------------------------------------------------------------
out_dir  <- "D:/work/RiskWise/early_sowing/Tool/Jackie_working/yield inputs proposed workflow"
in_file  <- "Yield_by_emergence_date_Kenton_and_Ed.csv"
out_file <- "Yield_by_emergence_date_extended.csv"

extend_pattern   <- "^Kenton"                 # sources whose curves are extended
extend_from      <- as.Date("2026-03-01")     # emergence dates are on a placeholder year, 2026
extend_to        <- as.Date("2026-08-31")
extend_step_days <- 5                         # spacing of the new dates
extrap_n_points  <- 3                         # the straight line is fitted to the first / last 3 table dates
extend_method    <- "broken_stick"            # changed: "broken_stick" (chosen) or "line" (straight line, for comparison)
plot_cultivars   <- c("Bigfoot", "Shotgun", "Mowhawk")   # changed: the cultivars shown in the plot and slope table

uplift_pattern   <- "^Kenton"
uplift_cultivars <- c("Shotgun", "Mowhawk")   # the wheat columns that get the uplift
wheat_uplift <- tribble(~season_type, ~yield_uplift,      # changed: moderated to a flat 5% (Therese's 10% / 10% / 20% was 1.10 / 1.10 / 1.20)
                        "D1-3", 1.05,
                        "D4-6", 1.05,
                        "D7-9", 1.05)

# ---- 1. Read the tables -------------------------------------------------------
yields <- read_csv(file.path(out_dir, in_file), show_col_types = FALSE) %>%
  mutate(emergence_date = as.Date(emergence_date),
         point_type     = "table")
yields %>% count(source, crop_or_cultivar, point_type)

# ---- 2. Extend each Kenton curve ---------------------------------------------
# One curve = one source, cultivar and season type. Returns only the NEW rows.
extend_one <- function(curve, key) {
  curve   <- arrange(curve, emergence_date)
  x       <- as.numeric(curve$emergence_date)
  first_d <- min(curve$emergence_date)
  last_d  <- max(curve$emergence_date)
  
  before <- if (extend_from < first_d) seq(extend_from, first_d - 1, by = extend_step_days) else as.Date(character())
  after  <- c(if (last_d + extend_step_days <= extend_to) seq(last_d + extend_step_days, extend_to, by = extend_step_days),
              extend_to)
  after  <- unique(after[after > last_d])
  
  fit_first <- lm(yield_t_ha ~ x, data = tibble(x = head(x, extrap_n_points), yield_t_ha = head(curve$yield_t_ha, extrap_n_points)))
  fit_last  <- lm(yield_t_ha ~ x, data = tibble(x = tail(x, extrap_n_points), yield_t_ha = tail(curve$yield_t_ha, extrap_n_points)))
  
  # changed from here: broken stick = rise then fall in two straight lines, joined at the best breakpoint
  curve_xy  <- tibble(x = x, yield_t_ha = curve$yield_t_ha)
  rss_at    <- function(bp) sum(resid(lm(yield_t_ha ~ x + pmax(0, x - bp), data = curve_xy))^2)
  bps       <- seq(min(x) + 7, max(x) - 7, by = 1)                  # candidate breakpoints, one day apart
  bp        <- bps[which.min(map_dbl(bps, rss_at))]                 # the breakpoint with the smallest error
  fit_stick <- lm(yield_t_ha ~ x + pmax(0, x - bp), data = curve_xy)
  
  predict_ext <- function(xx) {                                     # the chosen method, at dates xx
    if (extend_method == "line") {
      if_else(xx <= min(x), as.numeric(predict(fit_first, newdata = tibble(x = xx))),
              as.numeric(predict(fit_last,  newdata = tibble(x = xx))))
    } else {
      as.numeric(predict(fit_stick, newdata = tibble(x = xx)))
    }
  }
  shift_first <- curve$yield_t_ha[1]       - predict_ext(min(x))    # so the extension starts at Kenton's first table value
  shift_last  <- tail(curve$yield_t_ha, 1) - predict_ext(max(x))    # and at his last
  
  tibble(emergence_date = c(before, after)) %>%
    mutate(x_new      = as.numeric(emergence_date),
           y_ext      = predict_ext(x_new),
           yield_t_ha = pmax(0, if_else(emergence_date < first_d, y_ext + shift_first, y_ext + shift_last)),
           date_note  = paste0("extended: ", extend_method, " method, shifted to join Kenton's table"),
           point_type = "extended") %>%
    select(emergence_date, yield_t_ha, date_note, point_type)
}

extended <- yields %>%
  filter(str_detect(source, extend_pattern)) %>%
  group_by(source, crop_or_cultivar, season_type, yp_input, date_type, frost) %>%
  group_modify(extend_one) %>%
  ungroup()

# ---- 3. Combine, then lift the Kenton wheat yields -----------------------------
combined <- bind_rows(yields, extended) %>%
  left_join(wheat_uplift, by = "season_type") %>%
  mutate(yield_uplift      = if_else(str_detect(source, uplift_pattern) & crop_or_cultivar %in% uplift_cultivars,
                                     coalesce(yield_uplift, 1), 1),
         yield_t_ha_before = yield_t_ha,
         yield_t_ha        = yield_t_ha_before * yield_uplift) %>%
  arrange(source, crop_or_cultivar, season_type, emergence_date)

# ---- 4. Checks ----------------------------------------------------------------
combined %>% count(source, crop_or_cultivar, point_type) %>% print(n = Inf)       # extended rows only for Kenton
combined %>% filter(crop_or_cultivar == "Shotgun", season_type == "D4-6") %>%
  select(emergence_date, point_type, yield_t_ha_before, yield_uplift, yield_t_ha) %>% print(n = Inf)
combined %>% filter(yield_t_ha < 0) %>% nrow()                                      # expect 0
combined %>% group_by(source, crop_or_cultivar, season_type) %>%
  summarise(uplift = first(yield_uplift), peak_before = max(yield_t_ha_before), peak_after = max(yield_t_ha),
            .groups = "drop") %>% print(n = Inf)                                    # uplift only on Shotgun and Mowhawk

# ---- 4b. Slope of the straight line used for the extension ----------------------
# Slope (t/ha per day) of the line fitted to the first / last extrap_n_points table dates, on Kenton's original yields.
# The uplift scales the line, so slope after the uplift = slope x uplift.
fit_slope <- function(d, which) {
  d <- arrange(d, emergence_date)
  d <- if (which == "first") head(d, extrap_n_points) else tail(d, extrap_n_points)
  unname(coef(lm(yield_t_ha ~ as.numeric(emergence_date), data = d))[2])
}

slope_table <- yields %>%
  filter(str_detect(source, extend_pattern), crop_or_cultivar %in% plot_cultivars) %>%
  group_by(crop_or_cultivar, season_type) %>%
  group_modify(~ tibble(peak_before          = max(.x$yield_t_ha),
                        slope_start_per_day  = fit_slope(.x, "first"),
                        slope_end_per_day    = fit_slope(.x, "last"))) %>%
  ungroup() %>%
  left_join(wheat_uplift, by = "season_type") %>%
  mutate(yield_uplift             = if_else(crop_or_cultivar %in% uplift_cultivars, coalesce(yield_uplift, 1), 1),
         slope_end_after_uplift   = slope_end_per_day * yield_uplift,           # t/ha per day, after the uplift
         slope_end_per_week       = slope_end_after_uplift * 7,                 # t/ha per week, after the uplift
         slope_end_pct_peak_day   = 100 * slope_end_per_day / peak_before) %>%   # % of the season type's peak lost per day
  mutate(across(where(is.numeric), ~ round(.x, 4)))
slope_table %>% print(n = Inf, width = Inf)
write_csv(slope_table, file.path(out_dir, "Yield_extension_slopes.csv"))

# check: how do the wheat peaks compare with Bigfoot after the uplift? (1 = the same)
combined %>%
  filter(crop_or_cultivar %in% plot_cultivars) %>%
  group_by(crop_or_cultivar, season_type) %>%
  summarise(peak = max(yield_t_ha), .groups = "drop") %>%
  pivot_wider(names_from = crop_or_cultivar, values_from = peak) %>%
  mutate(Shotgun_vs_Bigfoot = round(Shotgun / Bigfoot, 2),
         Mowhawk_vs_Bigfoot = round(Mowhawk / Bigfoot, 2))

# ---- 5. Save ------------------------------------------------------------------
write_csv(combined, file.path(out_dir, out_file))

# ---- 6. Plot: the Kenton curves, table (filled dots) and extended (open dots) -------
type_colours <- c("D1-3" = "#9ecae1", "D4-6" = "#4292c6", "D7-9" = "#08519c")

uplift_text <- if (n_distinct(wheat_uplift$yield_uplift) == 1) {                                   # changed: label follows the uplift table
  paste0("+", round((wheat_uplift$yield_uplift[1] - 1) * 100), "%")
} else {
  paste0("+", round((wheat_uplift$yield_uplift - 1) * 100), "% ", wheat_uplift$season_type, collapse = ", ")
}
cultivar_labels <- c(Bigfoot = "Bigfoot (+0%)",
                     Shotgun = paste0("Shotgun (", uplift_text, ")"),
                     Mowhawk = paste0("Mowhawk (", uplift_text, ")"))
p_ext <- combined %>%
  filter(str_detect(source, "^Kenton"), crop_or_cultivar %in% plot_cultivars) %>%
  mutate(crop_or_cultivar = factor(crop_or_cultivar, levels = plot_cultivars,
                                   labels = unname(cultivar_labels[plot_cultivars]))) %>%
  ggplot(aes(emergence_date, yield_t_ha, colour = season_type)) +
  geom_line(aes(group = season_type), linewidth = 0.7) +
  geom_point(aes(shape = point_type), size = 1.8) +
  scale_colour_manual(values = type_colours, name = "Season type") +
  scale_shape_manual(values = c(table = 16, extended = 1), name = NULL) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  facet_wrap(~ crop_or_cultivar, nrow = 1) +
  labs(title = "Kenton yield tables by emergence date, extended",
       subtitle = paste0("Filled dots = Kenton's table. Open dots = extended with a ",                                       # changed
                         if (extend_method == "line") "straight line (last 3 table dates)" else "broken stick (rise and fall in two straight lines)",
                         ", never below 0. Shotgun and Mowhawk include the wheat uplift."),
       x = "Emergence date", y = "Yield (t/ha)") +
  theme_minimal(base_size = 10) +
  theme(panel.grid.minor = element_blank(), legend.position = "top",
        strip.text = element_text(face = "bold", hjust = 0))
p_ext

ggsave(file.path(out_dir, "Yield_by_emergence_date_extended.png"), p_ext,
       width = 28, height = 10, units = "cm", dpi = 300)

