# ===============================================================================
# SOLVER LOGIC — callable version of the sowing schedule MILP model
# ===============================================================================
library(readxl)
library(dplyr)
library(tidyr)
library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)
library(rlang)

run_sowing_model <- function(params) {
  
  # --- Grain prices and variable costs (fixed, not user-editable yet) ------
  grain_price_table <- tribble(
    ~crop,     ~Low, ~Average, ~High,
    "Wheat",    200,  315,      400,
    "Barley",   200,  285,      380,
    "Canola",   500,  700,      1100,
    "Lentils",  500,  650,      1000,
    "Beans",    400,  500,      700,
    "Lupins",   200,  400,      600,
    "Peas",     300,  400,      800
  )
  
  variable_cost_table <- tribble(
    ~crop,     ~`D1-3`, ~`D4-6`, ~`D7-9`,
    "Wheat",    284,     392,     473,
    "Barley",   230,     339,     402,
    "Canola",   324,     418,     469,
    "Lentils",  241,     267,     312,
    "Beans",    222,     238,     259,
    "Lupins",   194,     221,     265,
    "Peas",     184,     199,     214
  )
  
  grain_price <- setNames(grain_price_table[[params$price_scenario]], grain_price_table$crop)
  
  # --- Everything else from here on will use params$... instead of hard-coded values
  
  cat("Function reached this point successfully.\n")
  cat("Site:", params$site_name, "\n")
  cat("Optimising for:", params$optimise_for, "\n")
  cat("Grain prices for this scenario:\n")
  print(grain_price)
  
  return(list(status = "placeholder - real solve not yet wired in"))
}
