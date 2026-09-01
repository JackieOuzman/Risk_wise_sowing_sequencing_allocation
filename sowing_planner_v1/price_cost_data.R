# ===============================================================================
# GRAIN PRICES AND VARIABLE COSTS — shared reference data
# ===============================================================================
library(dplyr)

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