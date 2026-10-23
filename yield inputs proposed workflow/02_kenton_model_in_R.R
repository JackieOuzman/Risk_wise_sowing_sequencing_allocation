# =============================================================================
# Sowing optimiser: yield inputs, no-frost stage
# Script 02: Yield by emergence date for Kenton's cereals (model rebuilt in R)
#            and Ed's non-cereals, combined in one traceable table
# Author: Jackie Ouzman        Started: 2026-10-23
#
# PURPOSE
#   1. Recreate yield_environment_linear_regression_model_KPV2.xlsx so every input
#      is visible, labelled by source, and the three season type tables
#      (D1-3, D4-6, D7-9) can be reproduced without Excel.
#   2. Read Ed's no-frost non-cereal yields (seed data tab, columns E-G).
#   3. Combine both into one table indexed by EMERGENCE DATE and save it.
#
# DATE TERMS
#   Sowing date      the day the seed goes in the ground (our choice)
#   Germination date first day after sowing with >= 15 mm rain over 5 days
#   Emergence date   germination date + 120 degree-days
#   Both yield sources are indexed by emergence date (the crop establishment
#   event), NOT the sowing date. Ed's file labels this column "sowing date"
#   [to confirm with Ed]. Kenton's sheet says emergence in the title and sowing
#   in the notes [to confirm with Kenton].
#
# HOW KENTON'S MODEL WORKS
#   yield on a date = cultivar peak for the yield environment  x  % of peak for that date
#   peak = slope * YP + intercept           (tab: Yield environment regression)
#   % of peak = curve / cultivar peak       (tabs: Base sowing curve, Sowing response factors)
#
# NOTES FOR KENTON [to confirm]: see the flags marked FLAG in this script
# =============================================================================

library(tidyverse)
library(readxl)

cultivars <- c("Neo", "Bigfoot", "Shotgun", "Mowhawk")

# ---- 0. Settings ------------------------------------------------------------
# Yield environments from script 01: mean YP of each season type, last 30 seasons
yp_inputs <- c("D1-3" = 4.16, "D4-6" = 5.40, "D7-9" = 6.48)

# FLAG 1: the workbook divides Mowhawk by 4.3 but its bolded peak is 4.35.
# Keep 4.3 to match the workbook; change to 4.35 once Kenton confirms.
mowhawk_divisor <- 4.3

# FLAG 2: five response factors are typed by hand in the workbook.
# TRUE = use them (matches the workbook); FALSE = use the calculated value.
use_hand_typed_factors <- TRUE

# paths (change if yours are elsewhere)
ed_path <- "D:/work/RiskWise/early_sowing/Tool/From Therese/Eds base model_DrysowingNieldsHome26baseprogramplayall deciles.xls"
out_dir <- "D:/work/RiskWise/early_sowing/Tool/Jackie_working/yield inputs proposed workflow"

# month-day text ("04-10") -> real date; 2026 is only a placeholder year
# (real dates stop Excel misreading "04-10" as 4 October)
md_to_date <- function(md) as.Date(paste0("2026-", md))

date_type_label <- "emergence date (crop establishment event, NOT the sowing date)"


# =============================================================================
# PART A: KENTON'S MODEL (cereals)
# =============================================================================

# ---- 1. Regression inputs ('Yield environment regression' tab, rows 3-11) ----
reg_inputs <- tibble(
  water_supply  = c(50, 100, 120, 240, 280, 300, 340, 400, 500),   # 30% summer + GSR - 60
  neo           = c(1, 2.1, 2.5, 5.1, 5.8, 6.2, 7.1, 8.3, 10.4),   # typed
  mowhawk       = c(0.8, 1.8, 1.9, 4, 4.6, 4.9, 5.7, 6.9, 9),      # typed
  shotgun_typed = c(0.9, 1.9, 2.3, 4.5, 5.0, NA, NA, NA, NA)       # typed for 5 driest only
) %>%
  mutate(
    yp      = water_supply * 0.026,                       # 0.026 is built into Kenton's regression
    bigfoot = 0.8011 * neo + 0.5709,                      # FLAG 3: all calculated from Neo
    shotgun = if_else(is.na(shotgun_typed),
                      0.8787 * neo - 0.19,                # FLAG 3: top 4 calculated from Neo
                      shotgun_typed),
    neo_80pct_of_yp = 0.8 * yp,                           # FLAG 4: Neo is about 80% of YP
    neo_minus_80pct = neo - neo_80pct_of_yp
  )

# check: how close is Neo to 80% of YP? (8 of 9 should be within 0.05)
reg_inputs %>% select(water_supply, yp, neo, neo_80pct_of_yp, neo_minus_80pct)

# long format with the source of every value
reg_long <- reg_inputs %>%
  select(water_supply, yp, shotgun_typed,
         Neo = neo, Bigfoot = bigfoot, Shotgun = shotgun, Mowhawk = mowhawk) %>%
  pivot_longer(all_of(cultivars), names_to = "cultivar", values_to = "yield") %>%
  mutate(source = case_when(
    cultivar == "Neo"                              ~ "typed (about 0.8 x YP)",
    cultivar == "Mowhawk"                          ~ "typed",
    cultivar == "Shotgun" & !is.na(shotgun_typed)  ~ "typed",
    cultivar == "Shotgun"                          ~ "calculated from Neo: 0.8787*Neo - 0.19",
    cultivar == "Bigfoot"                          ~ "calculated from Neo: 0.8011*Neo + 0.5709"
  )) %>%
  select(-shotgun_typed)

reg_long %>% count(cultivar, source)       # 23 typed, 13 calculated in total

# ---- 2. Cultivar peak = slope * YP + intercept ------------------------------
regression <- reg_long %>%
  group_by(cultivar) %>%
  summarise(
    slope     = unname(coef(lm(yield ~ yp))[2]),
    intercept = unname(coef(lm(yield ~ yp))[1]),
    r2        = summary(lm(yield ~ yp))$r.squared,   # FLAG 5: partly circular, not validation
    n_typed   = sum(str_detect(source, "^typed")),
    .groups   = "drop"
  ) %>%
  arrange(match(cultivar, cultivars))
regression


# ---- 3. Base sowing curve ('Base sowing curve' tab) -------------------------
# Typed in the workbook for a 4-5 t/ha environment. No sites, years or trials given.
base_curve <- tibble(
  md      = c("03-20","04-01","04-10","04-15","04-20","04-25","05-01",
              "05-05","05-10","05-15","05-20","05-25","06-01","06-10"),
  Neo     = c(2.8, 3.85, 4.45, 4.82, 5.05, 5.2, 5.15, 5, 4.8, 4.55, 4.3, 4.02, 3.65, 3.1),
  Bigfoot = c(1.9, 3, 3.7, 4.15, 4.45, 4.6, 4.7, 4.78, 4.8, 4.65, 4.5, 4.32, 4.1, 3.85),
  Shotgun = c(1.45, 2.4, 2.9, 3.45, 4, 4.35, 4.5, 4.6, 4.58, 4.45, 4.3, 4.1, 3.9, 3.55),
  Mowhawk = c(3, 3.5, 4, 4.28, 4.35, 4.3, 4.2, 4.12, 4.05, 3.92, 3.75, 3.48, 3.15, 2.9)
)

# the peaks Kenton bolded in the workbook
bolded_peak <- c(Neo = 5.2, Bigfoot = 4.8, Shotgun = 4.6, Mowhawk = 4.35)
base_curve %>% summarise(across(all_of(cultivars), max))   # should equal bolded_peak

# ---- 4. Response factors ('Sowing response factors' tab) --------------------
peak_divisor <- c(Neo = 5.2, Bigfoot = 4.8, Shotgun = 4.6, Mowhawk = mowhawk_divisor)

hand_typed_factors <- tribble(
  ~md,     ~cultivar, ~factor_typed,
  "04-10", "Shotgun", 0.66,
  "04-25", "Bigfoot", 0.96,
  "04-25", "Shotgun", 0.95,
  "05-01", "Bigfoot", 0.98,
  "05-01", "Shotgun", 0.99
)

factors <- base_curve %>%
  pivot_longer(all_of(cultivars), names_to = "cultivar", values_to = "base_yield") %>%
  mutate(divisor           = unname(peak_divisor[cultivar]),
         factor_calculated = base_yield / divisor) %>%
  left_join(hand_typed_factors, by = c("md", "cultivar")) %>%
  mutate(
    use_typed     = use_hand_typed_factors & !is.na(factor_typed),
    factor_used   = if_else(use_typed, factor_typed, factor_calculated),
    factor_source = if_else(use_typed, "typed in workbook", "calculated")
  ) %>%
  select(-use_typed)

factors %>% filter(factor_source == "typed in workbook")   # the five hand-typed values
factors %>% filter(factor_used > 1)                        # FLAG 1: Mowhawk 20 Apr = 1.012

# ---- 5. The model: yield by date for a yield environment ---------------------
yield_table <- function(yp_input) {
  peaks <- regression %>% transmute(cultivar, peak = slope * yp_input + intercept)
  factors %>%
    left_join(peaks, by = "cultivar") %>%
    transmute(yp_input = yp_input, md, cultivar, peak,
              factor = factor_used, yield = peak * factor_used)
}

# ---- 6. Check against the workbook (B2 = 5) ----------------------------------
expected_yp5 <- tribble(
  ~md,     ~cultivar,  ~expected,
  "03-20", "Neo",      2.1560245,
  "04-25", "Neo",      4.0040456,
  "06-10", "Neo",      2.3870272,
  "03-20", "Bigfoot",  1.4956724,
  "04-25", "Bigfoot",  3.6273993,
  "06-10", "Bigfoot",  3.0307047,
  "03-20", "Shotgun",  1.1010339,
  "04-25", "Shotgun",  3.3182883,
  "06-10", "Shotgun",  2.6956347,
  "03-20", "Mowhawk",  2.2450796,
  "04-25", "Mowhawk",  3.2179474,
  "06-10", "Mowhawk",  2.1702436
)

check <- yield_table(5) %>%
  inner_join(expected_yp5, by = c("md", "cultivar")) %>%
  mutate(diff = yield - expected)
max(abs(check$diff))        # should be below 0.0001 (needs mowhawk_divisor = 4.3)

# ---- 7. The three season type tables -----------------------------------------
kenton_tables <- imap_dfr(yp_inputs, ~ yield_table(.x) %>% mutate(season_type = .y, .before = 1))

kenton_tables_wide <- kenton_tables %>%
  select(season_type, yp_input, md, cultivar, yield) %>%
  pivot_wider(names_from = cultivar, values_from = yield)

kenton_tables_wide %>%
  mutate(across(all_of(cultivars), ~ round(.x, 2))) %>%
  print(n = Inf)


# =============================================================================
# PART B: ED'S NON-CEREALS (seed data tab, columns E-G, no frost)
# =============================================================================

# ---- 8. Read Ed's file -------------------------------------------------------
ed_crops <- c("Canola", "Lentils", "Beans", "Lupins", "Peas")   # wheat and barley come from Kenton

raw <- read_excel(ed_path, sheet = "seed data", col_names = FALSE)

# "8th April" -> "04-08"
parse_md <- function(x) {
  day   <- as.integer(str_extract(x, "^[0-9]+"))
  month <- match(str_extract(x, "[A-Za-z]+$"), month.name)
  sprintf("%02d-%02d", month, day)
}

# one crop block: date (col A), probability (col C), no-frost yields (cols E-G)
get_block <- function(crop) {
  start <- which(raw[[1]] == crop)[1]
  slice(raw, (start + 1):min(start + 30, nrow(raw))) %>%
    select(1, 3, 5, 6, 7) %>%
    set_names(c("date_label", "prob", "D1-3", "D4-6", "D7-9")) %>%
    filter(str_detect(date_label, "^[0-9]+(st|nd|rd|th) [A-Za-z]+$")) %>%
    mutate(crop = crop, across(c(prob, `D1-3`, `D4-6`, `D7-9`), as.numeric))
}

ed_blocks <- map_dfr(ed_crops, get_block) %>%
  mutate(md = parse_md(date_label))

# checks
ed_blocks %>% group_by(crop) %>% summarise(n_dates = n(), prob_sum = sum(prob))   # 11 dates, prob 1.00
ed_blocks %>% filter(md == "04-08")   # Canola 1.7/2.2/2.8, Lentils 1.12/1.6/2.0

ed_yields <- ed_blocks %>%
  select(crop, md, `D1-3`, `D4-6`, `D7-9`) %>%
  pivot_longer(c(`D1-3`, `D4-6`, `D7-9`), names_to = "season_type", values_to = "yield_no_frost")

# which crops share exactly the same curve? (expect Canola = Beans = Lupins; Lentils = Peas)
ed_yields %>%
  group_by(crop) %>%
  summarise(curve = paste(round(yield_no_frost, 3), collapse = ","), .groups = "drop") %>%
  group_by(curve) %>%
  summarise(crops = paste(crop, collapse = " = "), .groups = "drop") %>%
  pull(crops)


# =============================================================================
# PART C: COMBINE, THEN SAVE
# =============================================================================

# ---- 9. One table: Kenton + Ed, by emergence date ----------------------------
kenton_est <- kenton_tables %>%
  transmute(source = "Kenton model (rebuilt in R, script 02)",
            crop_or_cultivar = cultivar, season_type, yp_input, md,
            yield_t_ha = yield, frost = "no frost",
            date_note = "Kenton's sheet title says emergence date; notes say sowing date [to confirm]")

ed_est <- ed_yields %>%
  transmute(source = "Ed seed data tab, cols E-G",
            crop_or_cultivar = crop, season_type, yp_input = NA_real_, md,
            yield_t_ha = yield_no_frost, frost = "no frost",
            date_note = "labelled sowing date in Ed's file; treated as emergence date [to confirm]")

establishment_yields <- bind_rows(kenton_est, ed_est) %>%
  mutate(emergence_date = md_to_date(md),
         date_type      = date_type_label) %>%
  select(source, crop_or_cultivar, season_type, yp_input, emergence_date, date_type,
         yield_t_ha, frost, date_note) %>%
  arrange(source, crop_or_cultivar, season_type, emergence_date)

nrow(establishment_yields)                      # 333 = Kenton 168 + Ed 165
establishment_yields %>% count(source, crop_or_cultivar)

# Kenton detail (peak and factor behind each yield), with emergence_date in place of md
kenton_detail <- kenton_tables %>%
  mutate(emergence_date = md_to_date(md), date_type = date_type_label) %>%
  select(season_type, yp_input, cultivar, emergence_date, date_type, peak, factor, yield)

factors_detail <- factors %>%
  mutate(emergence_date = md_to_date(md), date_type = date_type_label) %>%
  select(cultivar, emergence_date, date_type, base_yield, divisor,
         factor_calculated, factor_typed, factor_used, factor_source)

# ---- 10. Save outputs --------------------------------------------------------
write_csv(establishment_yields, file.path(out_dir, "Yield_by_emergence_date_Kenton_and_Ed.csv"))
write_csv(kenton_detail,        file.path(out_dir, "Kenton_model_yield_tables_long.csv"))
write_csv(regression,           file.path(out_dir, "Kenton_model_regression_parameters.csv"))
write_csv(factors_detail,       file.path(out_dir, "Kenton_model_response_factors.csv"))


# =============================================================================
# PART D: LOOK AT THE RESULTS
# =============================================================================

# ---- 11. One yield environment, dates down, cultivars across -----------------
yp_single <- 4        # yield environment, t/ha (change this to try another)

table_single <- yield_table(yp_single) %>%
  arrange(md) %>%
  mutate(emergence_date = md_to_date(md),
         yield          = round(yield, 2)) %>%
  select(emergence_date, cultivar, yield) %>%
  pivot_wider(names_from = cultivar, values_from = yield)
table_single

# ---- 12. Do Kenton's and Ed's yields agree? -----------------------------------
# Ed's file also has Wheat and Barley blocks (same layout), so we can compare his
# cereals with Kenton's cereals. NOTE: the season types are defined differently
# (Kenton = rainfall; Ed = basis to confirm), so differences may come from that.

ed_all_long <- map_dfr(c("Wheat", "Barley", ed_crops), get_block) %>%
  mutate(md = parse_md(date_label)) %>%
  select(crop, md, `D1-3`, `D4-6`, `D7-9`) %>%
  pivot_longer(c(`D1-3`, `D4-6`, `D7-9`), names_to = "season_type", values_to = "yield") %>%
  mutate(source_short = "Ed")

# Kenton's curves put onto Ed's dates (straight-line interpolation; NA outside 20 Mar-10 Jun)
ed_md <- sort(unique(ed_all_long$md))
kenton_on_ed <- kenton_tables %>%
  mutate(d = as.numeric(md_to_date(md))) %>%
  group_by(crop = cultivar, season_type) %>%
  reframe(md    = ed_md,
          yield = approx(d, yield, xout = as.numeric(md_to_date(ed_md)))$y) %>%
  mutate(source_short = "Kenton")

# only use dates where both sources have a value (Ed's 8 Apr to Kenton's 10 Jun)
overlap_md <- kenton_on_ed %>% filter(!is.na(yield)) %>% distinct(md) %>% pull(md)
overlap_md

# 12a. Mean yield (t/ha) over the overlapping dates, every crop, by season type
overlap_means <- bind_rows(ed_all_long, kenton_on_ed) %>%
  filter(md %in% overlap_md) %>%
  group_by(source_short, crop, season_type) %>%
  summarise(mean_yield = round(mean(yield), 2), .groups = "drop") %>%
  pivot_wider(names_from = season_type, values_from = mean_yield) %>%
  arrange(source_short, crop)
overlap_means

# 12b. Like-for-like: Ed's wheat/barley minus Kenton's wheat/barley, same dates
pairs <- tribble(~ed_crop, ~kenton_crop,
                 "Wheat",  "Shotgun",
                 "Wheat",  "Mowhawk",
                 "Barley", "Bigfoot",
                 "Barley", "Neo")

ed_vs_kenton <- pairs %>%
  inner_join(ed_all_long %>% select(ed_crop = crop, md, season_type, yield_ed = yield),
             by = "ed_crop", relationship = "many-to-many") %>%
  inner_join(kenton_on_ed %>% select(kenton_crop = crop, md, season_type, yield_kenton = yield),
             by = c("kenton_crop", "md", "season_type")) %>%
  filter(md %in% overlap_md) %>%
  group_by(ed_crop, kenton_crop, season_type) %>%
  summarise(n_dates     = n(),
            mean_ed     = round(mean(yield_ed), 2),
            mean_kenton = round(mean(yield_kenton), 2),
            mean_diff   = round(mean_ed - mean_kenton, 2),     # + means Ed is higher
            pct_diff    = round(100 * mean_diff / mean_kenton, 1),
            .groups = "drop")
ed_vs_kenton %>% print(n = Inf)

# 12c. Save the comparison
write_csv(overlap_means, file.path(out_dir, "Yield_comparison_mean_by_crop.csv"))
write_csv(ed_vs_kenton,  file.path(out_dir, "Yield_comparison_Ed_vs_Kenton_cereals.csv"))


# ---- 13. Plots for the working doc -------------------------------------------
# Ed has only 2 distinct non-cereal curves (Canola = Beans = Lupins; Lentils = Peas),
# so one crop from each group stands in for all five. Edit plot_crops to change.
plot_crops <- c("Bigfoot", "Shotgun", "Mowhawk",   # Kenton cereals (Neo dropped)
                "Canola", "Lentils")               # Ed non-cereals

crop_colours <- c(Bigfoot = "#1d6b24",    # barley, dark green
                  Shotgun = "#184f95",    # wheat, dark blue
                  Mowhawk = "#5598e7",    # wheat, light blue
                  Canola  = "#c77c00",    # amber
                  Lentils = "#7a4fb5")    # purple
crop_shapes  <- c(Bigfoot = 17, Shotgun = 15, Mowhawk = 18, Canola = 16, Lentils = 3)
ed_colour    <- "#c77c00"                 # box and bars that mark Ed's data

plot_data <- establishment_yields %>%
  filter(crop_or_cultivar %in% plot_crops) %>%
  mutate(crop         = factor(crop_or_cultivar, levels = plot_crops),
         yield        = yield_t_ha,
         date_label   = format(emergence_date, "%d %b"),
         season_type  = factor(season_type, levels = c("D1-3", "D4-6", "D7-9")),
         source_short = if_else(str_detect(source, "^Kenton"), "Kenton", "Ed"))

# date range of each crop, used for the panel labels, the box and the bars
date_ranges <- plot_data %>%
  group_by(crop, source_short) %>%
  summarise(from = min(emergence_date), to = max(emergence_date),
            n_dates = n_distinct(emergence_date), .groups = "drop") %>%
  arrange(crop) %>%
  mutate(crop_label = paste0(crop, "   |   ", source_short, " emergence dates: ",
                             format(from, "%d %b"), " to ", format(to, "%d %b")))

plot_data <- plot_data %>%
  left_join(date_ranges %>% select(crop, crop_label), by = "crop") %>%
  mutate(crop_label = factor(crop_label, levels = date_ranges$crop_label))

plot_subtitle <- paste0(
  "Season types (D1-3, D4-6, D7-9) are defined differently in the two sources:\n",
  "Kenton cereals = RAINFALL defined (water supply at Lock, last 30 seasons, YP = ",
  paste(names(yp_inputs), yp_inputs, sep = " ", collapse = ", "), " t/ha).\n",
  "Ed non-cereals = deciles from Ed's model; basis to confirm with Ed (may be YIELD defined).")

plot_caption <- paste0(
  "Cereals: Kenton's model rebuilt in R. Non-cereals: Ed's seed data tab, no-frost columns only.\n",
  "Not all of Ed's yields are shown: Beans and Lupins are identical to Canola, Peas identical to Lentils; ",
  "Ed's wheat, barley and frost columns, and Kenton's Neo, are not plotted.\n",
  "The two sources cover different emergence date ranges.")

# 13a. Heat map: one panel per crop, season types down, dates across.
# Ed's panels are boxed. Each panel shows its own dates.
date_order <- plot_data %>% distinct(emergence_date, date_label) %>% arrange(emergence_date)

heat_data <- plot_data %>%
  mutate(date_label  = factor(date_label, levels = unique(date_order$date_label)),
         season_type = factor(season_type, levels = rev(levels(season_type))))   # D1-3 at the top

ed_box <- date_ranges %>%
  filter(source_short == "Ed") %>%
  mutate(crop_label = factor(crop_label, levels = levels(heat_data$crop_label)))

p_heat <- ggplot(heat_data, aes(date_label, season_type, fill = yield)) +
  geom_tile(colour = "white", linewidth = 0.6) +
  geom_text(aes(label = sprintf("%.1f", yield), colour = yield > 0.55 * max(yield)),
            size = 2.6, show.legend = FALSE) +
  geom_rect(data = ed_box, inherit.aes = FALSE,
            aes(xmin = 0.5, xmax = n_dates + 0.5, ymin = 0.5, ymax = 3.5),
            fill = NA, colour = ed_colour, linewidth = 1.3) +
  scale_colour_manual(values = c("TRUE" = "white", "FALSE" = "#0b0b0b")) +
  scale_fill_gradient(low = "#cde2fb", high = "#0d366b", name = "Yield (t/ha)") +
  facet_wrap(~ crop_label, ncol = 1, scales = "free_x") +
  labs(title = "Expected yield by emergence date and season type",
       subtitle = plot_subtitle,
       x = "Emergence date", y = NULL,
       caption = paste0("Orange box = Ed's data.\n", plot_caption)) +
  theme_minimal(base_size = 10) +
  theme(panel.grid = element_blank(),
        strip.text = element_text(face = "bold", hjust = 0),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.subtitle = element_text(lineheight = 1.1),
        plot.caption = element_text(hjust = 0, lineheight = 1.1))
p_heat

# 13b. Line plot: one panel per season type, a line per crop (no box).
# solid = Kenton cereals, dashed = Ed non-cereals.
# The bars under the lines show the date range each source covers.
range_bars <- date_ranges %>%
  group_by(source_short) %>%
  summarise(from = min(from), to = max(to), .groups = "drop") %>%
  mutate(y = if_else(source_short == "Kenton", -0.3, -0.9),
         label = paste0(source_short, " dates: ", format(from, "%d %b"), " to ", format(to, "%d %b")),
         season_type = factor("D1-3", levels = levels(plot_data$season_type)))   # label in first panel only

p_lines <- ggplot(plot_data, aes(emergence_date, yield, colour = crop, shape = crop,
                                 linetype = source_short)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  geom_segment(data = range_bars, inherit.aes = FALSE,
               aes(x = from, xend = to, y = y, yend = y),
               linewidth = 2, colour = "grey60") +
  geom_text(data = range_bars, inherit.aes = FALSE,
            aes(x = from, y = y + 0.28, label = label),
            hjust = 0, size = 2.6, colour = "grey30") +
  scale_colour_manual(values = crop_colours, name = NULL) +
  scale_shape_manual(values = crop_shapes, name = NULL) +
  scale_linetype_manual(values = c(Kenton = "solid", Ed = "dashed"), guide = "none") +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%d %b") +
  scale_y_continuous(breaks = 0:6, limits = c(-1.2, NA)) +
  facet_wrap(~ season_type, nrow = 1) +
  labs(title = "Expected yield by emergence date",
       subtitle = plot_subtitle,
       x = "Emergence date", y = "Yield (t/ha)",
       caption = paste0("Solid lines = Kenton, dashed lines = Ed.\n", plot_caption)) +
  theme_minimal(base_size = 10) +
  theme(panel.grid.minor = element_blank(),
        legend.position = "top",
        strip.text = element_text(face = "bold", hjust = 0),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.subtitle = element_text(lineheight = 1.1),
        plot.caption = element_text(hjust = 0, lineheight = 1.1))
p_lines

ggsave(file.path(out_dir, "Yield_heatmap_cereals_and_noncereals.png"),  p_heat,  width = 24, height = 22, units = "cm", dpi = 300)
ggsave(file.path(out_dir, "Yield_lineplot_cereals_and_noncereals.png"), p_lines, width = 24, height = 13, units = "cm", dpi = 300)
