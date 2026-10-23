# =============================================================================
# Sowing optimiser: yield inputs, no-frost stage
# Script 03: Do Ed's and Kenton's yields agree?
# Author: Jackie Ouzman        Started: 2026-10-23
#
# WHY
#   Ed's non-cereals and Kenton's cereals are being put in one yield table, but
#   they come from two different models with two different decile definitions:
#     Kenton season types = RAINFALL defined (water supply at Lock, last 30 seasons)
#     Ed season types     = YIELD defined (confirmed by Ed)
#   Ed's file also has Wheat and Barley, so we can compare his cereals with
#   Kenton's cereals on the same emergence dates (like for like) and see how far
#   apart the two sources are before we blend them.
#
# INPUTS
#   Yield_by_emergence_date_Kenton_and_Ed.csv   (saved by script 02)
#   Ed's xls, 'seed data' tab (wheat and barley blocks, no-frost columns E-G)
#
# HOW DATES ARE MATCHED
#   Kenton's 14 dates (20 Mar-10 Jun) are put onto Ed's 11 dates (8 Apr-17 Jul)
#   by straight-line interpolation. Only dates where both have a value are used,
#   so the comparison covers the overlap (8 Apr to 10 Jun).
#
# ASSUMPTION [to confirm]: which cultivar matches which Ed crop
#   Ed Wheat  <-> Kenton Shotgun, Mowhawk
#   Ed Barley <-> Kenton Bigfoot, Neo
# =============================================================================

library(tidyverse)
library(readxl)

# ---- 0. Settings ------------------------------------------------------------
ed_path <- "D:/work/RiskWise/early_sowing/Tool/From Therese/Eds base model_DrysowingNieldsHome26baseprogramplayall deciles.xls"
out_dir <- "D:/work/RiskWise/early_sowing/Tool/Jackie_working/yield inputs proposed workflow"

season_types <- c("D1-3", "D4-6", "D7-9")

# month-day text -> real date (2026 is only a placeholder year)
md_to_date <- function(md) as.Date(paste0("2026-", md))

pairs <- tribble(~ed_crop, ~kenton_crop,
                 "Wheat",  "Shotgun",
                 "Barley", "Bigfoot")

# ---- 1. Read the saved combined file (from script 02) ------------------------
saved <- read_csv(file.path(out_dir, "Yield_by_emergence_date_Kenton_and_Ed.csv"),
                  show_col_types = FALSE)

kenton <- saved %>%
  filter(str_detect(source, "^Kenton")) %>%
  transmute(crop = crop_or_cultivar, season_type, date = emergence_date, yield = yield_t_ha)

ed_noncereals <- saved %>%
  filter(str_detect(source, "^Ed")) %>%
  transmute(crop = crop_or_cultivar, season_type, date = emergence_date, yield = yield_t_ha)

# ---- 2. Read Ed's wheat and barley (not in the saved file) -------------------
raw <- read_excel(ed_path, sheet = "seed data", col_names = FALSE)

parse_md <- function(x) {                      # "8th April" -> "04-08"
  day   <- as.integer(str_extract(x, "^[0-9]+"))
  month <- match(str_extract(x, "[A-Za-z]+$"), month.name)
  sprintf("%02d-%02d", month, day)
}

get_block <- function(crop) {                  # same reader as script 02
  start <- which(raw[[1]] == crop)[1]
  slice(raw, (start + 1):min(start + 30, nrow(raw))) %>%
    select(1, 3, 5, 6, 7) %>%
    set_names(c("date_label", "prob", "D1-3", "D4-6", "D7-9")) %>%
    filter(str_detect(date_label, "^[0-9]+(st|nd|rd|th) [A-Za-z]+$")) %>%
    mutate(crop = crop, across(c(prob, `D1-3`, `D4-6`, `D7-9`), as.numeric))
}

ed_cereals <- map_dfr(c("Wheat", "Barley"), get_block) %>%
  mutate(date = md_to_date(parse_md(date_label))) %>%
  select(crop, date, `D1-3`, `D4-6`, `D7-9`) %>%
  pivot_longer(all_of(season_types), names_to = "season_type", values_to = "yield")

ed_all <- bind_rows(ed_cereals, ed_noncereals)

# checks
ed_all %>% group_by(crop) %>% summarise(n_dates = n_distinct(date), first = min(date), last = max(date))
kenton %>% group_by(crop) %>% summarise(n_dates = n_distinct(date), first = min(date), last = max(date))

# ---- 3. Put Kenton's curves onto Ed's dates ----------------------------------
ed_dates <- sort(unique(ed_all$date))

kenton_on_ed <- kenton %>%
  group_by(crop, season_type) %>%
  reframe(yield = approx(as.numeric(date), yield, xout = as.numeric(ed_dates))$y,  # yield first
          date  = ed_dates)                                                       # then date
# NA = Ed date outside Kenton's 20 Mar-10 Jun range

overlap_dates <- kenton_on_ed %>% filter(!is.na(yield)) %>% distinct(date) %>% pull(date)
overlap_dates          # the dates used for every comparison below

# ---- 4. Mean yield by crop and season type over the overlap -------------------
overlap_long <- bind_rows(ed_all %>% mutate(source_short = "Ed"),
                          kenton_on_ed %>% mutate(source_short = "Kenton")) %>%
  filter(date %in% overlap_dates) %>%
  group_by(source_short, crop, season_type) %>%
  summarise(mean_yield = mean(yield), .groups = "drop")

overlap_means <- overlap_long %>%
  mutate(mean_yield = round(mean_yield, 2)) %>%
  pivot_wider(names_from = season_type, values_from = mean_yield) %>%
  arrange(source_short, crop)
overlap_means

# ---- 5. Like for like: Ed's cereals minus Kenton's cereals --------------------
ed_vs_kenton <- pairs %>%
  inner_join(ed_cereals %>% select(ed_crop = crop, date, season_type, yield_ed = yield),
             by = "ed_crop", relationship = "many-to-many") %>%
  inner_join(kenton_on_ed %>% select(kenton_crop = crop, date, season_type, yield_kenton = yield),
             by = c("kenton_crop", "date", "season_type")) %>%
  filter(date %in% overlap_dates) %>%
  group_by(ed_crop, kenton_crop, season_type) %>%
  summarise(n_dates     = n(),
            mean_ed     = round(mean(yield_ed), 2),
            mean_kenton = round(mean(yield_kenton), 2),
            mean_diff   = round(mean_ed - mean_kenton, 2),       # + means Ed is higher
            pct_diff    = round(100 * mean_diff / mean_kenton, 1),
            .groups = "drop") %>%
  arrange(ed_crop, kenton_crop, match(season_type, season_types))
ed_vs_kenton %>% print(n = Inf)

# ---- 6. Peak yield and the date it occurs (whole date range of each source) ----
peaks <- bind_rows(ed_all %>% mutate(source_short = "Ed"),
                   kenton %>% mutate(source_short = "Kenton")) %>%
  group_by(source_short, crop, season_type) %>%
  slice_max(yield, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  transmute(source_short, crop, season_type,
            peak_yield = round(yield, 2), peak_date = format(date, "%d %b")) %>%
  arrange(source_short, crop, match(season_type, season_types))
peaks %>% print(n = Inf)

# ---- 7. How far below wheat are the non-cereals? ------------------------------
# within Ed's file (Ed non-cereal / Ed wheat) and against Kenton's Shotgun (wheat)
ref_ed_wheat <- overlap_long %>%
  filter(source_short == "Ed", crop == "Wheat") %>%
  select(season_type, ed_wheat = mean_yield)
ref_kenton_wheat <- overlap_long %>%
  filter(source_short == "Kenton", crop == "Shotgun") %>%
  select(season_type, kenton_shotgun = mean_yield)

crop_ratios <- overlap_long %>%
  left_join(ref_ed_wheat, by = "season_type") %>%
  left_join(ref_kenton_wheat, by = "season_type") %>%
  mutate(mean_yield = round(mean_yield, 2),
         ratio_to_ed_wheat       = round(mean_yield / ed_wheat, 2),
         ratio_to_kenton_shotgun = round(mean_yield / kenton_shotgun, 2)) %>%
  select(source_short, crop, season_type, mean_yield, ratio_to_ed_wheat, ratio_to_kenton_shotgun) %>%
  arrange(source_short, crop, match(season_type, season_types))
crop_ratios %>% print(n = Inf)

# ---- 8. Save -----------------------------------------------------------------
write_csv(overlap_means, file.path(out_dir, "Check_mean_yield_by_crop_overlap_dates.csv"))
write_csv(ed_vs_kenton,  file.path(out_dir, "Check_Ed_vs_Kenton_cereals.csv"))
write_csv(peaks,         file.path(out_dir, "Check_peak_yield_by_crop.csv"))
write_csv(crop_ratios,   file.path(out_dir, "Check_crop_ratios.csv"))

# ---- 9. Plot: Ed's cereals vs Kenton's cereals, each on its own dates ----------
cultivar_colours <- c(Neo = "#4fb55b", Bigfoot = "#1d6b24", Shotgun = "#184f95", Mowhawk = "#5598e7")

plot_pairs <- bind_rows(
  pairs %>%
    inner_join(kenton, by = c("kenton_crop" = "crop"), relationship = "many-to-many") %>%
    transmute(group = ed_crop, line = kenton_crop, source_short = "Kenton", season_type, date, yield),
  ed_cereals %>%
    transmute(group = crop, line = paste("Ed", crop), source_short = "Ed", season_type, date, yield)
) %>%
  mutate(season_type = factor(season_type, levels = season_types),
         group = factor(group, levels = c("Wheat", "Barley")))

p_check <- ggplot(plot_pairs, aes(date, yield, colour = line, linetype = source_short, shape = line)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  scale_colour_manual(values = c(cultivar_colours, "Ed Wheat" = "#444444", "Ed Barley" = "#444444"), name = NULL) +
  scale_shape_manual(values = c(Neo = 16, Bigfoot = 17, Shotgun = 15, Mowhawk = 18,
                                "Ed Wheat" = 4, "Ed Barley" = 4), name = NULL) +
  scale_linetype_manual(values = c(Kenton = "solid", Ed = "dashed"), guide = "none") +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%d %b") +
  facet_grid(group ~ season_type) +
  labs(title = "Ed's and Kenton's cereal yields by emergence date",
       subtitle = paste0("Season types are defined differently: Kenton = RAINFALL defined (water supply at Lock, ",
                         "last 30 seasons); Ed = YIELD defined (Ed's model).\nSolid = Kenton, dashed grey = Ed."),
       x = "Emergence date", y = "Yield (t/ha)",
       caption = "Ed Wheat is compared with Kenton's Shotgun and Mowhawk; Ed Barley with Bigfoot and Neo [pairing to confirm].") +
  theme_minimal(base_size = 10) +
  theme(panel.grid.minor = element_blank(),
        legend.position = "top",
        strip.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.subtitle = element_text(lineheight = 1.1),
        plot.caption = element_text(hjust = 0))
p_check

ggsave(file.path(out_dir, "Check_Ed_vs_Kenton_cereals.png"), p_check,
       width = 24, height = 12, units = "cm", dpi = 300)


# ---- 10. Visualise the comparison (conventional wheat and barley only) ---------
# wheat = blues, barley = greens; DARK = Kenton, LIGHT = Ed
crop_fills <- c("Kenton Shotgun (wheat)"  = "#184f95",
                "Ed Wheat"                = "#5598e7",
                "Kenton Bigfoot (barley)" = "#1d6b24",
                "Ed Barley"               = "#4fb55b")

plot_simple <- ed_vs_kenton %>%
  filter(kenton_crop %in% c("Shotgun", "Bigfoot")) %>%          # conventional varieties only
  mutate(season_type = factor(season_type, levels = season_types),
         crop_group  = factor(ed_crop, levels = c("Wheat", "Barley")),
         kenton_key  = paste0("Kenton ", kenton_crop, " (", tolower(ed_crop), ")"),
         ed_key      = paste("Ed", ed_crop)) %>%
  select(crop_group, season_type, pct_diff, kenton_key, ed_key,
         Kenton = mean_kenton, Ed = mean_ed) %>%
  pivot_longer(c(Kenton, Ed), names_to = "source_short", values_to = "mean_yield") %>%
  mutate(fill_key     = if_else(source_short == "Kenton", kenton_key, ed_key),
         source_short = factor(source_short, levels = c("Kenton", "Ed")),
         fill_key     = factor(fill_key, levels = names(crop_fills)))

# the % difference, written above each pair of bars
plot_annot <- plot_simple %>%
  group_by(crop_group, season_type) %>%
  summarise(top = max(mean_yield), pct_diff = first(pct_diff), .groups = "drop")

p_simple <- ggplot(plot_simple, aes(crop_group, mean_yield, fill = fill_key, group = source_short)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  geom_text(aes(label = sprintf("%.2f", mean_yield)),
            position = position_dodge(width = 0.8), vjust = -0.4, size = 2.8) +
  geom_text(data = plot_annot, inherit.aes = FALSE,
            aes(crop_group, top, label = sprintf("Ed %+.0f%%", pct_diff)),
            vjust = -2, size = 2.8, fontface = "bold", colour = "grey30") +
  scale_fill_manual(values = crop_fills, name = NULL) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  facet_wrap(~ season_type, nrow = 1) +
  guides(fill = guide_legend(nrow = 1)) +
  labs(title = "Ed's and Kenton's cereal yields, mean over the same emergence dates",
       subtitle = paste0("Season types are defined differently: Kenton = RAINFALL defined (water supply at Lock, last 30 seasons);\n",
                         "Ed = YIELD defined (Ed's model). Dark = Kenton, light = Ed.\n",
                         "Emergence dates ", format(min(overlap_dates), "%d %b"), " to ",
                         format(max(overlap_dates), "%d %b"),
                         " only. Bold label = Ed's yield relative to Kenton's."),
       x = NULL, y = "Mean yield (t/ha)",
       caption = "Wheat compared with Kenton's Shotgun, barley with Bigfoot [pairing to confirm].") +
  theme_minimal(base_size = 10) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "top",
        strip.text = element_text(face = "bold", hjust = 0),
        plot.subtitle = element_text(lineheight = 1.1),
        plot.caption = element_text(hjust = 0))
p_simple

ggsave(file.path(out_dir, "Check_Ed_vs_Kenton_cereals_bar.png"), p_simple,
       width = 24, height = 11, units = "cm", dpi = 300)
# ---- 11. Same comparison on fixed emergence dates -----------------------------
# No date is in both models (Kenton: irregular dates; Ed: every 10 days from 8 Apr),
# so we use Ed's dates that sit closest to a Kenton date. Kenton's yield on those
# dates is interpolated from his neighbouring dates (a few days either side).
# To try other dates, pick from overlap_dates (printed in section 3).
fixed_dates <- as.Date(c("2026-04-18", "2026-05-08", "2026-05-28"))
stopifnot(all(fixed_dates %in% overlap_dates))

fixed_data <- pairs %>%
  filter(kenton_crop %in% c("Shotgun", "Bigfoot")) %>%
  inner_join(ed_cereals %>% select(ed_crop = crop, date, season_type, yield_ed = yield),
             by = "ed_crop", relationship = "many-to-many") %>%
  inner_join(kenton_on_ed %>% select(kenton_crop = crop, date, season_type, yield_kenton = yield),
             by = c("kenton_crop", "date", "season_type")) %>%
  filter(date %in% fixed_dates) %>%
  mutate(diff     = round(yield_ed - yield_kenton, 2),                       # + means Ed is higher
         pct_diff = round(100 * (yield_ed - yield_kenton) / yield_kenton, 1)) %>%
  arrange(date, ed_crop, match(season_type, season_types))
fixed_data %>% print(n = Inf)

write_csv(fixed_data, file.path(out_dir, "Check_Ed_vs_Kenton_cereals_fixed_dates.csv"))

date_levels <- format(sort(fixed_dates), "%d %b")

plot_fixed <- fixed_data %>%
  mutate(season_type = factor(season_type, levels = season_types),
         date_label  = factor(format(date, "%d %b"), levels = date_levels),
         crop_group  = factor(ed_crop, levels = c("Wheat", "Barley")),
         kenton_key  = paste0("Kenton ", kenton_crop, " (", tolower(ed_crop), ")"),
         ed_key      = paste("Ed", ed_crop)) %>%
  select(crop_group, season_type, date_label, pct_diff, kenton_key, ed_key,
         Kenton = yield_kenton, Ed = yield_ed) %>%
  pivot_longer(c(Kenton, Ed), names_to = "source_short", values_to = "yield") %>%
  mutate(fill_key     = if_else(source_short == "Kenton", kenton_key, ed_key),
         source_short = factor(source_short, levels = c("Kenton", "Ed")),
         fill_key     = factor(fill_key, levels = names(crop_fills)))

fixed_annot <- plot_fixed %>%
  group_by(crop_group, season_type, date_label) %>%
  summarise(top = max(yield), pct_diff = first(pct_diff), .groups = "drop")

p_fixed <- ggplot(plot_fixed, aes(crop_group, yield, fill = fill_key, group = source_short)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  geom_text(aes(label = sprintf("%.2f", yield)),
            position = position_dodge(width = 0.8), vjust = -0.4, size = 2.6) +
  geom_text(data = fixed_annot, inherit.aes = FALSE,
            aes(crop_group, top, label = sprintf("Ed %+.0f%%", pct_diff)),
            vjust = -2, size = 2.6, fontface = "bold", colour = "grey30") +
  scale_fill_manual(values = crop_fills, name = NULL) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
  facet_grid(season_type ~ date_label) +
  guides(fill = guide_legend(nrow = 1)) +
  labs(title = "Ed's and Kenton's cereal yields on fixed emergence dates",
       subtitle = paste0("Season types are defined differently: Kenton = RAINFALL defined (water supply at Lock, last 30 seasons);\n",
                         "Ed = YIELD defined (Ed's model). Dark = Kenton, light = Ed. Bold label = Ed's yield relative to Kenton's."),
       x = NULL, y = "Yield (t/ha)",
       caption = paste0("Columns = emergence date. The two models have no dates in common, so Kenton's yield on each date is interpolated\n",
                        "from his neighbouring dates (a few days either side). Wheat compared with Shotgun, barley with Bigfoot [pairing to confirm]."))  +
  theme_minimal(base_size = 10) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "top",
        strip.text = element_text(face = "bold"),
        plot.subtitle = element_text(lineheight = 1.1),
        plot.caption = element_text(hjust = 0, lineheight = 1.1))
p_fixed

ggsave(file.path(out_dir, "Check_Ed_vs_Kenton_cereals_fixed_dates.png"), p_fixed,
       width = 24, height = 18, units = "cm", dpi = 300)
