# ===============================================================================
# MODEL CHECK — comparing baseline vs mock yield runs across deciles
# ===============================================================================
# ===============================================================================
# SECTION 1 — LIBRARIES
# ===============================================================================
library(dplyr)
library(readr)
library(purrr)
library(ggplot2)

# ===============================================================================
# SECTION 2 — LOAD AND COMBINE ALL RUN SUMMARIES
# ===============================================================================
check_path <- "D:/work/RiskWise/early_sowing/Tool/Jackie_working/model check/"
yld_table_path <- "D:/work/RiskWise/early_sowing/Tool/Jackie_working/"


summary_files <- list.files(check_path, pattern = "_run_summary\\.csv$", full.names = TRUE)
print(summary_files)   # sanity check — should list all 6 files

all_summaries <- summary_files %>%
  map_df(read_csv, show_col_types = FALSE)

print(all_summaries)
# ===============================================================================
# SECTION 3 — PLOT: EXPECTED YIELD BY DECILE AND SCENARIO
# ===============================================================================


all_summaries <- all_summaries %>%
  mutate(scenario = if_else(grepl("MOCK", scenario), "Mock (adjusted yields)", "Baseline"),
         decile = factor(decile, levels = c("D1-3", "D4-6", "D7-9")))

ggplot(all_summaries, aes(x = decile, y = objective_expected_yield, fill = scenario)) +
  geom_col(position = "dodge", width = 0.6) +
  geom_text(aes(label = round(objective_expected_yield, 0)),
            position = position_dodge(width = 0.6), vjust = -0.4, size = 3.5) +
  scale_fill_manual(values = c("Baseline" = "#4C72B0", "Mock (adjusted yields)" = "#DD8452")) +
  labs(title = "Expected yield by decile: baseline vs mock",
       x = "Decile", y = "Expected yield (t)", fill = "Scenario") +
  theme_minimal(base_size = 12)

# ===============================================================================
# SECTION 4 — DIRECTLY COMPARE THE TWO YIELD INPUT FILES
# ===============================================================================
baseline_yield <- read_excel(paste0(yld_table_path,
                                    "EP_yld_long_format.xlsx"),
                             sheet = "Yield data long format")

mock_yield <- read_excel(paste0(yld_table_path,
                                "EP_yld_long_format_MOCK.xlsx"),
                         sheet = "Yield data long format")

comparison <- baseline_yield %>%
  rename(yield_baseline = yield_t_per_ha) %>%
  left_join(
    mock_yield %>% select(crop, frost_zone, decile_band, `week of sowing program window`, yield_t_per_ha) %>%
      rename(yield_mock = yield_t_per_ha),
    by = c("crop", "frost_zone", "decile_band", "week of sowing program window")
  ) %>%
  mutate(diff = yield_mock - yield_baseline,
         changed = abs(diff) > 0.0001)

cat("Total rows:", nrow(comparison), "\n")
cat("Rows that actually differ between baseline and mock:", sum(comparison$changed, na.rm = TRUE), "\n\n")

cat("Which crops have any changed rows:\n")
print(comparison %>% filter(changed) %>% count(crop))


# ===============================================================================
# SECTION 5 — COMPARE SOWING SCHEDULES ACROSS ALL 6 RUNS
# ===============================================================================

weekly_files <- list.files(check_path, pattern = "_weekly_table\\.csv$", full.names = TRUE)
print(weekly_files)   # sanity check — should list all 6

# Pull scenario + decile straight out of each filename, then stack
weekly_all <- weekly_files %>%
  set_names(basename(.)) %>%
  map_df(read_csv, show_col_types = FALSE, .id = "source_file") %>%
  mutate(
    scenario = if_else(grepl("MOCK", source_file), "Mock", "Baseline"),
    decile = case_when(
      grepl("D1-3", source_file) ~ "D1-3",
      grepl("D4-6", source_file) ~ "D4-6",
      grepl("D7-9", source_file) ~ "D7-9"
    )
  ) %>%
  select(scenario, decile, week, date, everything(), -source_file)

print(weekly_all)


# --- Direct comparison: does the schedule differ, decile by decile? --------
for (d in c("D1-3", "D4-6", "D7-9")) {
  base_d <- weekly_all %>% filter(scenario == "Baseline", decile == d) %>% select(-scenario, -decile)
  mock_d <- weekly_all %>% filter(scenario == "Mock", decile == d) %>% select(-scenario, -decile)
  
  cat("\n===", d, "===\n")
  cat("Identical schedule (Baseline vs Mock)?", identical(base_d, mock_d), "\n")
}


# ===============================================================================
# SECTION 6 — COMPARE SCHEDULES ACROSS DECILES (WITHIN EACH SCENARIO)
# ===============================================================================
for (s in c("Baseline", "Mock")) {
  d13 <- weekly_all %>% filter(scenario == s, decile == "D1-3") %>% select(-scenario, -decile)
  d46 <- weekly_all %>% filter(scenario == s, decile == "D4-6") %>% select(-scenario, -decile)
  d79 <- weekly_all %>% filter(scenario == s, decile == "D7-9") %>% select(-scenario, -decile)
  
  cat("\n===", s, "===\n")
  cat("D1-3 identical to D4-6?", identical(d13, d46), "\n")
  cat("D4-6 identical to D7-9?", identical(d46, d79), "\n")
  cat("D1-3 identical to D7-9?", identical(d13, d79), "\n")
}


# ===============================================================================
# SECTION 7 — MANUAL YIELD CALCULATION FOR THE FIXED SOWING PLAN
# ===============================================================================
# Confirmed identical across every scenario/decile — hard-coded from the saved
# weekly tables so we can multiply it directly against each yield file,
# bypassing ompr entirely.



sowing_plan_fixed <- tribble(
  ~crop,    ~zone,    ~week, ~ha,
  "Wheat",  "Amber",  1,     50,
  "Wheat",  "Green",  1,     100,
  "Wheat",  "Green",  2,     100,
  "Wheat",  "Green",  3,     150,
  "Beans",  "Green",  4,     100,
  "Barley", "Red",    4,     50,
  "Barley", "Amber",  5,     150,
  "Barley", "Red",    6,     50,
  "Lupins", "Red",    6,     100,
  "Barley", "Green",  7,     150
)

cat("Total ha in fixed plan:", sum(sowing_plan_fixed$ha), "\n")   # should print 1000

# --- Load both yield files fresh --------------------------------------------
baseline_yield <- read_excel(paste0(yld_table_path,
                                    "EP_yld_long_format.xlsx"),
                             sheet = "Yield data long format")
mock_yield <- read_excel(paste0(yld_table_path,
                                "EP_yld_long_format_MOCK.xlsx"),
                         sheet = "Yield data long format")

# --- For each yield table x each decile, join the fixed plan and sum -------
manual_check <- expand_grid(
  scenario = c("Baseline", "Mock"),
  decile = c("D1-3", "D4-6", "D7-9")
) %>%
  rowwise() %>%
  mutate(
    total_yield = {
      yld_tbl <- if (scenario == "Baseline") baseline_yield else mock_yield
      plan_with_yield <- sowing_plan_fixed %>%
        left_join(
          yld_tbl %>% filter(decile_band == decile) %>%
            select(crop, frost_zone, `week of sowing program window`, yield_t_per_ha),
          by = c("crop" = "crop", "zone" = "frost_zone", "week" = "week of sowing program window")
        )
      sum(plan_with_yield$ha * plan_with_yield$yield_t_per_ha)
    }
  ) %>%
  ungroup()

print(manual_check)


# ===============================================================================
# SECTION 8 — PLOT: SOLVER-REPORTED vs MANUALLY CALCULATED (cleaned up)
# ===============================================================================
comparison_plot_data <- all_summaries %>%
  select(scenario, decile, objective_expected_yield) %>%
  rename(yield = objective_expected_yield) %>%
  mutate(scenario = if_else(grepl("Mock", scenario), "Mock", "Baseline"),
         source = "Solver-reported") %>%
  bind_rows(
    manual_check %>%
      rename(yield = total_yield) %>%
      mutate(source = "Manually calculated")
  ) %>%
  mutate(series = paste(scenario, "-", source),
         series = factor(series, levels = c(
           "Baseline - Solver-reported", "Baseline - Manually calculated",
           "Mock - Solver-reported", "Mock - Manually calculated"
         )))

ggplot(comparison_plot_data, aes(x = decile, y = yield, fill = series)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7, color = "grey30", linewidth = 0.3) +
  geom_text(aes(label = round(yield, 0)),
            position = position_dodge(width = 0.8), vjust = -0.4, size = 3.2) +
  scale_fill_manual(values = c(
    "Baseline - Solver-reported"      = "#4C72B0",
    "Baseline - Manually calculated"  = "#A9C0E0",
    "Mock - Solver-reported"          = "#DD8452",
    "Mock - Manually calculated"      = "#F2C4A0"
  )) +
  labs(title = "Expected yield: solver-reported vs manually calculated",
       subtitle = "Same fixed sowing plan in every case — only the yield table changes",
       x = "Decile", y = "Expected yield (t)", fill = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.text = element_text(size = 9))
