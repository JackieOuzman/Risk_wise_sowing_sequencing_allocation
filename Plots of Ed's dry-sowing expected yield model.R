# -----------------------------------------------------------------------------
# Plots of Ed's dry-sowing expected yield model (rebuilt in Excel)
#
# Purpose: illustrate how expected yields are produced, for the summary doc
#   Plot 1: Yield potentials by sowing date, No Frost vs With Frost (All Wheat)
#   Plot 2: Yield potential vs expected yield (All Wheat, Green zone),
#           showing the delayed-emergence effect of dry sowing
#   Plot 3: Green, Amber and Red expected yields, All Wheat vs Barley,
#           showing the effect of each crop's Red factor
#
# Model reminder: Green uses the No Frost potentials, Amber uses the With Frost
#   potentials, and Red = Amber expected yield x the crop's Red factor.
#
# Data: 'Plots yield potentials etc' tab, B47:R80, of
#   checking yield inputs 24_09_2026.xlsx
# -----------------------------------------------------------------------------

library(readxl)
library(tidyverse)

file_path <- "D:/work/RiskWise/early_sowing/Tool/Yield_model_inputs_check_etc/checking yield inputs 24_09_2026.xlsx"

#Read and tidy the data
# ---- Read ----
raw <- read_excel(file_path,
                  sheet = "Plots yield potentials etc",
                  range = "B47:R80")

# ---- Tidy to long format ----
# keep sowing dates in their calendar order (8th April ... 17th July)
sowing_dates <- unique(raw$Date)

yield_long <- raw |>
  pivot_longer(-c(Decile, Date), names_to = "series", values_to = "yield") |>
  mutate(series = str_squish(series)) |>               # drop trailing spaces in headers
  extract(series,
          into  = c("crop", "zone", "measure"),
          regex = "^(.*) (Green|Amber|Red)\\. (.*)$") |>  # e.g. "All wheat" "Green" "Yield Potential"
  mutate(crop = recode(crop, "All wheat" = "All Wheat"),
         zone = factor(zone, levels = c("Green", "Amber", "Red")),
         Date = factor(Date, levels = sowing_dates))

# check: 33 rows x 15 series = 495 rows, no NAs in crop/zone/measure
nrow(yield_long)
count(yield_long, crop, zone, measure)

#Shared plot theme

theme_plots <- theme_minimal(base_size = 11) +
  theme(axis.text.x     = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        strip.text      = element_text(face = "bold"))

#Plot 1: yield potentials, No Frost vs With Frost (All Wheat)
# Green column holds the No Frost potentials, Amber holds the With Frost potentials
p1 <- yield_long |>
  filter(crop == "All Wheat", measure == "Yield Potential",
         zone %in% c("Green", "Amber")) |>
  mutate(potential = if_else(zone == "Green", "No Frost", "With Frost")) |>
  ggplot(aes(Date, yield, colour = potential, group = potential)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ Decile) +
  scale_colour_manual(values = c("No Frost" = "#3A8DDE", "With Frost" = "#0B2545")) +
  labs(title  = "All Wheat yield potentials by sowing date",
       x = "Sowing date", y = "Yield potential (t/ha)", colour = NULL) +
  theme_plots
p1


#Plot 2: yield potential vs expected yield (All Wheat, Green)

# gap between the lines = cost of the crop possibly emerging later than sown
p2 <- yield_long |>
  filter(crop == "All Wheat", zone == "Green") |>
  ggplot(aes(Date, yield, colour = measure, group = measure)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ Decile) +
  scale_colour_manual(values = c("Yield Potential" = "#3A8DDE", "Expected yield" = "#0B2545")) +
  labs(title = "All Wheat, Green zone: yield potential vs expected yield",
       x = "Sowing date", y = "Yield (t/ha)", colour = NULL) +
  theme_plots
p2

#Plot 3: Green, Amber and Red expected yields, All Wheat vs Barley
# Green identical (same No Frost potentials); Amber differs slightly (Barley's
# With Frost potentials are higher); Red differs a lot (Barley x 0.8 flat,
# All Wheat x 0.26 rising to 1)
p3 <- yield_long |>
  filter(crop %in% c("All Wheat", "Barley"), measure == "Expected yield") |>
  ggplot(aes(Date, yield, colour = crop, group = crop)) +
  geom_line() +
  geom_point() +
  facet_grid(zone ~ Decile) +
  scale_colour_manual(values = c("All Wheat" = "#3A8DDE", "Barley" = "#0B2545")) +
  labs(title = "Expected yield by zone: All Wheat vs Barley",
       x = "Sowing date", y = "Expected yield (t/ha)", colour = NULL) +
  theme_plots
p3

#################################################################################
#Read Ed's yield potentials

# ---- Ed's yield potentials (All Wheat, all crops) ----
# 'Ed yield potential' tab: Crop, Date, Zone, Decile, Yield Potential, Sow order
# Green = No Frost potentials, Amber = With Frost potentials

ed_pot <- read_excel(file_path, sheet = "Ed yield potential", range = "B2:G464") |>
  set_names(c("crop", "date", "zone", "decile", "yield_potential", "sow_order")) |>
  mutate(
    # "8th April" -> 2026-04-08, so dates plot on a true time axis
    sow_date  = as.Date(paste(str_remove(date, "(?<=\\d)(st|nd|rd|th)"), "2026"),
                        format = "%d %B %Y"),
    potential = if_else(zone == "Green", "No Frost", "With Frost")
  )

# check: 7 crops x 66 rows = 462, no NA dates
nrow(ed_pot)
sum(is.na(ed_pot$sow_date))

#Implied Mowhawk potentials

# ---- Implied Mowhawk potentials ----
# Ed has no Mowhawk potentials. His Mowhawk Green = All Wheat Green EXPECTED
# yield x factor. Here we apply the same factors to the All Wheat POTENTIALS
# instead, to see what his factors imply at the potential step.
mowhawk_factors <- tibble(
  sow_order      = 1:11,
  mowhawk_factor = c(1, 1, 0.95, 0.95, 0.9, 0.9, 0.9, 0.85, 0.85, 0.85, 0.8)
)

wheat_mowhawk_pot <- ed_pot |>
  filter(crop == "All Wheat", potential == "No Frost") |>
  left_join(mowhawk_factors, by = "sow_order") |>
  transmute(decile, sow_date,
            `All Wheat`          = yield_potential,
            `Mowhawk (implied)`  = yield_potential * mowhawk_factor) |>
  pivot_longer(-c(decile, sow_date), names_to = "variety", values_to = "yield_potential")


#Plot: Ed's All Wheat vs implied Mowhawk potentials (No Frost)

p_pot <- ggplot(wheat_mowhawk_pot,
                aes(sow_date, yield_potential, colour = variety, linetype = variety)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.5) +
  facet_wrap(~ decile) +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%d %b") +
  scale_colour_manual(values = c("All Wheat" = "#0B2545", "Mowhawk (implied)" = "#7CB342")) +
  scale_linetype_manual(values = c("All Wheat" = "dotted", "Mowhawk (implied)" = "solid")) +
  coord_cartesian(ylim = c(0, NA)) +
  labs(title    = "Ed's yield potentials (No Frost): All Wheat vs implied Mowhawk",
       subtitle = "Mowhawk = Ed's Green Mowhawk factor x All Wheat potential",
       x = "Emergence date", y = "Yield potential (t/ha)",
       colour = NULL, linetype = NULL) +
  theme_plots
p_pot


#################################################################################
#################################################################################
#################################################################################
# Kenton's model (YP 5 = D7-9)

# ---- Kenton's yield environment x emergence date model ----
# 'Model' tab A14:E28: emergence date + yield potential for 4 cultivars,
# calculated at the yield environment in B2 (saved at YP = 5 t/ha, which
# matches the D7-9 column of the EP/KP sheet). No frost in this model.
# YP = water-limited yield potential (yield environment), not a wheat type.
kp_model_path <- "D:/work/RiskWise/early_sowing/Tool/From Therese/yield_environment_linear_regression_model_KPV2.xlsx"

kp_model <- read_excel(kp_model_path, sheet = "Model", range = "A14:E28") |>
  rename(emergence_date = Date) |>
  mutate(emergence_date = as.Date(emergence_date)) |>
  select(emergence_date, `Shotgun/Tomahawk Wheat`, Mowhawk) |>
  pivot_longer(-emergence_date, names_to = "variety", values_to = "yield_potential") |>
  mutate(variety = recode(variety,
                          "Shotgun/Tomahawk Wheat" = "All Wheat / Tomahawk",
                          "Mowhawk"                = "Early wheat (Mowhawk)"),
         source  = "Kenton's model (YP 5)")

# check: 14 dates x 2 varieties = 28 rows; Tomahawk peak ~3.49 on 5 May
kp_model |> group_by(variety) |> slice_max(yield_potential)

#################################################################################
# Ed's potentials (Green, D7-9)
# Ed's All Wheat No Frost potentials, and implied Mowhawk
# (Ed's Green Mowhawk factor x All Wheat potential)
ed_d79 <- ed_pot |>
  filter(crop == "All Wheat", potential == "No Frost", decile == "D7-9") |>
  left_join(mowhawk_factors, by = "sow_order") |>
  transmute(emergence_date          = sow_date,
            `All Wheat / Tomahawk`  = yield_potential,
            `Early wheat (Mowhawk)` = yield_potential * mowhawk_factor) |>
  pivot_longer(-emergence_date, names_to = "variety", values_to = "yield_potential") |>
  mutate(source = "Ed's model (Mowhawk implied)")

pot_d79 <- bind_rows(ed_d79, kp_model)

#################################################################################
# Legend grouping: one label per line, so the legend groups by model
pot_d79 <- pot_d79 |>
  mutate(series = case_when(
    source == "Ed's model (Mowhawk implied)" & variety == "All Wheat / Tomahawk"  ~ "Ed: All Wheat",
    source == "Ed's model (Mowhawk implied)" & variety == "Early wheat (Mowhawk)" ~ "Ed: Mowhawk (implied)",
    source == "Kenton's model (YP 5)"        & variety == "All Wheat / Tomahawk"  ~ "Kenton: Shotgun/Tomahawk (spring)",
    source == "Kenton's model (YP 5)"        & variety == "Early wheat (Mowhawk)" ~ "Kenton: Mowhawk (winter)"),
    series = factor(series, levels = c("Ed: All Wheat",
                                       "Ed: Mowhawk (implied)",
                                       "Kenton: Shotgun/Tomahawk (spring)",
                                       "Kenton: Mowhawk (winter)")))

# check: 4 series, no NAs (Ed 11 rows each, Kenton 14 rows each)
count(pot_d79, series)

#################################################################################
# Plot: yield potentials, Green zone, D7-9
series_colours <- c("Ed: All Wheat"                     = "#0B2545",
                    "Ed: Mowhawk (implied)"             = "#43A047",
                    "Kenton: Shotgun/Tomahawk (spring)" = "#0B2545",
                    "Kenton: Mowhawk (winter)"          = "#43A047")

series_lines   <- c("Ed: All Wheat"                     = "solid",
                    "Ed: Mowhawk (implied)"             = "solid",
                    "Kenton: Shotgun/Tomahawk (spring)" = "22",
                    "Kenton: Mowhawk (winter)"          = "22")

p_pot_d79 <- ggplot(pot_d79,
                    aes(emergence_date, yield_potential,
                        colour = series, linetype = series)) +
  geom_line(linewidth = 1.6) +
  geom_point(size = 2.8) +
  scale_x_date(date_breaks = "1 week", date_labels = "%d %b") +
  scale_colour_manual(values = series_colours) +
  scale_linetype_manual(values = series_lines) +
  coord_cartesian(ylim = c(0, NA)) +
  labs(title    = "Wheat yield potentials, Green zone (No Frost), D7-9",
       subtitle = "Kenton's model run at a yield environment (YP) of 5 t/ha",
       x = "Emergence date", y = "Yield potential (t/ha)",
       colour = NULL, linetype = NULL) +
  guides(colour   = guide_legend(ncol = 2, byrow = FALSE),
         linetype = guide_legend(ncol = 2, byrow = FALSE)) +
  theme_plots +
  theme(legend.text      = element_text(size = 11),
        legend.key.width = unit(2.5, "cm"))
p_pot_d79

#################################################################################
# Kenton's model at any yield environment (YP)
# Yield potential = cultivar peak (slope x YP + intercept) x sowing response factor
# Both parts read from Kenton's file, so we can run it at any YP, not just the saved YP 5

kp_coef <- read_excel(kp_model_path, sheet = "Yield environment regression", range = "A15:C19") |>
  set_names(c("cultivar", "slope", "intercept"))

kp_shape <- read_excel(kp_model_path, sheet = "Sowing response factors", range = "A1:E15") |>
  rename(emergence_date = Date) |>
  mutate(emergence_date = as.Date(emergence_date)) |>
  pivot_longer(-emergence_date, names_to = "cultivar", values_to = "response_factor")

# which yield environment to use for each decile
# Option A (EP/KP sheet): 3, 4, 5.  Option B (match Ed's levels): 4.3, 5.9, 7.3
decile_yp <- tibble(decile = c("D1-3", "D4-6", "D7-9"),
                    YP     = c(3, 4, 5))

kp_deciles <- kp_shape |>
  filter(cultivar %in% c("Shotgun/Tomahawk Wheat", "MowHawk")) |>
  left_join(kp_coef, by = "cultivar") |>
  cross_join(decile_yp) |>
  mutate(yield_potential = (slope * YP + intercept) * response_factor,
         series = if_else(cultivar == "MowHawk",
                          "Kenton: Mowhawk (winter)",
                          "Kenton: Shotgun/Tomahawk (spring)")) |>
  select(decile, YP, emergence_date, series, yield_potential)

# check: D7-9 (YP 5) should match the saved Model tab (Tomahawk peak ~3.49)
kp_deciles |> filter(decile == "D7-9") |> group_by(series) |> slice_max(yield_potential)

#################################################################################
# Ed's All Wheat No Frost potentials + implied Mowhawk, all deciles
ed_all_dec <- ed_pot |>
  filter(crop == "All Wheat", potential == "No Frost") |>
  left_join(mowhawk_factors, by = "sow_order") |>
  transmute(decile,
            emergence_date          = sow_date,
            `Ed: All Wheat`         = yield_potential,
            `Ed: Mowhawk (implied)` = yield_potential * mowhawk_factor) |>
  pivot_longer(c(`Ed: All Wheat`, `Ed: Mowhawk (implied)`),
               names_to = "series", values_to = "yield_potential")

pot_all_dec <- bind_rows(ed_all_dec, kp_deciles) |>
  left_join(decile_yp |> rename(YP_kp = YP), by = "decile") |>
  mutate(series      = factor(series, levels = names(series_colours)),
         decile_lab  = paste0(decile, " (Kenton YP ", YP_kp, " t/ha)"))

# check: 4 series per decile
count(pot_all_dec, decile, series)
#################################################################################
# Plot: yield potentials, Green zone, all deciles
p_pot_all_dec <- ggplot(pot_all_dec,
                        aes(emergence_date, yield_potential,
                            colour = series, linetype = series)) +
  geom_line(linewidth = 1.3) +
  geom_point(size = 2) +
  facet_wrap(~ decile_lab, ncol = 3) +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%d %b") +
  scale_colour_manual(values = series_colours) +
  scale_linetype_manual(values = series_lines) +
  coord_cartesian(ylim = c(0, NA)) +
  labs(title = "Wheat yield potentials, Green zone (No Frost), by decile",
       x = "Emergence date", y = "Yield potential (t/ha)",
       colour = NULL, linetype = NULL) +
  guides(colour   = guide_legend(ncol = 2, byrow = FALSE),
         linetype = guide_legend(ncol = 2, byrow = FALSE)) +
  theme_plots +
  theme(legend.text      = element_text(size = 11),
        legend.key.width = unit(2.5, "cm"))
p_pot_all_dec


out_dir <- "D:/work/RiskWise/early_sowing/Tool/Yield_model_inputs_check_etc"
ggsave(file.path(out_dir, "wheat_yield_potentials_by_decile.png"),
       p_pot_all_dec, width = 14, height = 6, dpi = 300)
