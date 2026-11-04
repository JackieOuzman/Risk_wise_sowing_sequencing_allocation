library(tidyverse)

out_dir <- "D:/work/RiskWise/early_sowing/Tool/Jackie_working/yield inputs proposed workflow/"

ext <- read_csv(paste0(out_dir, "Yield_by_emergence_date_extended.csv"), show_col_types = FALSE)

# Ed's wheat and barley (saved by script 02 with the folder name stuck on the front of the file name)
ed <- read_csv("D:/work/RiskWise/early_sowing/Tool/Jackie_working/yield inputs proposed workflowEd_wheat_barley_all_long.csv",
               show_col_types = FALSE)

# Kenton: uplifted yields (Shotgun +5%, Bigfoot unchanged), table and extended points
kenton_y <- ext %>%
  filter(str_detect(crop_or_cultivar, regex("Shotgun|Bigfoot", ignore_case = TRUE))) %>%
  transmute(crop_group = if_else(str_detect(crop_or_cultivar, regex("Bigfoot", ignore_case = TRUE)),
                                 "Barley", "Wheat"),
            series = if_else(crop_group == "Barley", "Kenton Bigfoot (barley)", "Kenton Shotgun +5% (wheat)"),
            season_type,
            emergence_date,
            point_type,
            yield = yield_t_ha)                       # after uplift

# Ed: wheat and barley
ed_y <- ed %>%
  filter(crop %in% c("Wheat", "Barley")) %>%
  transmute(crop_group = crop,
            series = if_else(crop == "Barley", "Ed Barley", "Ed Wheat"),
            season_type,
            emergence_date = as.Date(paste0("2026-", md)),
            point_type = "Ed",
            yield)

series_levels <- c("Kenton Bigfoot (barley)", "Ed Barley",
                   "Kenton Shotgun +5% (wheat)", "Ed Wheat")

yield_cmp <- bind_rows(kenton_y, ed_y) %>%
  mutate(crop_group = factor(crop_group, levels = c("Barley", "Wheat")),
         series     = factor(series, levels = series_levels))

series_colours <- c("Kenton Bigfoot (barley)"     = "#006d2c",   # dark green
                    "Ed Barley"                    = "#74c476",   # light green
                    "Kenton Shotgun +5% (wheat)"   = "#08306b",   # dark blue
                    "Ed Wheat"                     = "#6baed6")   # light blue

p_yield <- ggplot(yield_cmp, aes(emergence_date, yield, colour = series)) +
  geom_line(linewidth = 0.9) +
  geom_point(aes(shape = point_type), size = 1.8, fill = "white") +
  facet_grid(crop_group ~ season_type, switch = "y") +
  scale_colour_manual(values = series_colours) +
  scale_shape_manual(values = c(table = 16, extended = 21, Ed = 16),
                     labels = c(table = "Kenton table", extended = "Kenton extended (hollow)", Ed = "Ed")) +
  scale_x_date(limits = as.Date(c("2026-03-01", "2026-08-31")),
               date_breaks = "3 weeks", date_labels = "%d %b") +
  labs(x = "Emergence date", y = "Yield (t/ha)", colour = NULL, shape = NULL,
       title = "Yield by emergence date: Kenton (with 5% wheat uplift and extension) against Ed",
       subtitle = "Kenton's season types are rainfall; Ed's are yield deciles.") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.placement = "outside",
        legend.position = "bottom", legend.box = "vertical")

p_yield
ggsave(paste0(out_dir, "Yield_t_ha_Kenton_extended_vs_Ed_by_season_type.png"), p_yield,
       width = 26, height = 15, units = "cm", dpi = 200)

ed_dates <- sort(unique(ed_y$emergence_date))

# Kenton interpolated to Ed's dates
kenton_at_ed <- kenton_y %>%
  group_by(crop_group, season_type) %>%
  arrange(emergence_date) %>%
  reframe(ed_date = ed_dates,
          kenton_yield = approx(emergence_date, yield, xout = ed_dates, ties = mean)$y)

# Ed minus Kenton
ed_y %>%
  select(crop_group, season_type, ed_date = emergence_date, ed_yield = yield) %>%
  left_join(kenton_at_ed, by = c("crop_group", "season_type", "ed_date")) %>%
  mutate(ed_minus_kenton = round(ed_yield - kenton_yield, 2),
         ed_pct = round(100 * (ed_yield / kenton_yield - 1))) %>%
  arrange(crop_group, season_type, ed_date) %>%
  print(n = 70)
