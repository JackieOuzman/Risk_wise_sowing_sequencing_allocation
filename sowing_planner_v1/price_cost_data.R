# ===============================================================================
# GRAIN PRICES AND VARIABLE COSTS — loaded from external file (no longer hardcoded)
# ===============================================================================
library(dplyr)
library(tidyr)
library(readr)

price_cost_file <- "input_commondity_variable_cost_long.csv"

price_cost_long <- read_csv(price_cost_file, show_col_types = FALSE)

grain_price_table <- price_cost_long %>%
  filter(varible_type == "commondity_price") %>%
  select(crop, band, value) %>%
  pivot_wider(names_from = band, values_from = value)

variable_cost_table <- price_cost_long %>%
  filter(varible_type == "variable_cost") %>%
  select(crop, band, value) %>%
  pivot_wider(names_from = band, values_from = value)