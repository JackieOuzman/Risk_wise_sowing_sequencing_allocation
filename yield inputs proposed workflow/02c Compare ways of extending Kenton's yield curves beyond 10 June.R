# =============================================================================
# Sowing optimiser: yield inputs, no-frost stage
# Script 02c: Compare ways of extending Kenton's yield curves beyond 10 June
# Author: Jackie Ouzman        Started: 2026-11-02
#
# PURPOSE
#   Fits three simple curves to Kenton's yield-by-emergence-date table and extends each to
#   31 Aug, so the choice of method can be compared with the table:
#     Straight line  = line fitted to the last 3 table dates (the method in script 02b)
#     Quadratic      = parabola fitted to the whole table
#     Broken stick   = rise then fall in two straight lines, joined at the best breakpoint
#   Uses Kenton's ORIGINAL yields (before any uplift), so only the shapes are compared.
#   Each extension is shifted to start at Kenton's last table value (splice = TRUE), so there is no jump.
# =============================================================================

library(tidyverse)

# ---- 0. Settings ------------------------------------------------------------
out_dir  <- "D:/work/RiskWise/early_sowing/Tool/Jackie_working/yield inputs proposed workflow"
in_file  <- "Yield_by_emergence_date_Kenton_and_Ed.csv"

extend_to       <- as.Date("2026-08-31")      # placeholder year 2026, as in the yield tables
extrap_n_points <- 3                          # straight line: fitted to the last 3 table dates
splice          <- TRUE                       # shift each extension to start at the last table value
plot_cultivars  <- c("Bigfoot", "Shotgun", "Mowhawk")

# ---- 1. Kenton's table --------------------------------------------------------
curves <- read_csv(file.path(out_dir, in_file), show_col_types = FALSE) %>%
  filter(str_detect(source, "^Kenton"), crop_or_cultivar %in% plot_cultivars) %>%
  mutate(emergence_date = as.Date(emergence_date),
         x              = as.numeric(emergence_date)) %>%
  select(crop_or_cultivar, season_type, emergence_date, x, yield_t_ha)
curves %>% count(crop_or_cultivar, season_type)

# ---- 2. The three curves: each takes a table (d) and dates (xout) and returns yields ---
pred_line <- function(d, xout) {
  fit <- lm(yield_t_ha ~ x, data = tail(d, extrap_n_points))
  as.numeric(predict(fit, newdata = tibble(x = xout)))
}

pred_quad <- function(d, xout) {
  x_mid <- mean(d$x)
  d     <- mutate(d, x0 = x - x_mid, x0sq = x0^2)
  fit   <- lm(yield_t_ha ~ x0 + x0sq, data = d)
  as.numeric(predict(fit, newdata = tibble(x0 = xout - x_mid, x0sq = (xout - x_mid)^2)))
}

pred_stick <- function(d, xout) {
  rss_at <- function(bp) sum(resid(lm(yield_t_ha ~ x + pmax(0, x - bp), data = d))^2)
  bps    <- seq(min(d$x) + 7, max(d$x) - 7, by = 1)           # candidate breakpoints, one day apart
  bp     <- bps[which.min(map_dbl(bps, rss_at))]              # the breakpoint with the smallest error
  fit    <- lm(yield_t_ha ~ x + pmax(0, x - bp), data = d)
  as.numeric(predict(fit, newdata = tibble(x = xout)))
}

methods <- list("Straight line" = pred_line, "Quadratic" = pred_quad, "Broken stick" = pred_stick)

# ---- 3. Fit every curve and extend it -------------------------------------------
curves_ext <- curves %>%
  group_by(crop_or_cultivar, season_type) %>%
  group_modify(function(d, key) {
    d      <- arrange(d, x)
    last_x <- max(d$x)
    all_x  <- seq(min(d$x), as.numeric(extend_to), by = 1)
    imap_dfr(methods, function(f, nm) {
      fit   <- f(d, all_x)
      shift <- if (splice) tail(d$yield_t_ha, 1) - f(d, last_x) else 0
      tibble(method = nm, x = all_x,
             yield_t_ha = pmax(0, if_else(all_x <= last_x, fit, fit + shift)),
             part = if_else(all_x <= last_x, "fitted to the table", "extension"))
    })
  }) %>%
  ungroup() %>%
  filter(!(method == "Straight line" & part == "fitted to the table")) %>%     # the line is only meaningful beyond the table
  mutate(emergence_date = as.Date(x, origin = "1970-01-01"),
         method = factor(method, levels = names(methods)))

# ---- 4. Checks ----------------------------------------------------------------
# 4a. Yield on three dates beyond the table, by method (Kenton's original yields, before any uplift)
curves_ext %>%
  filter(part == "extension", emergence_date %in% as.Date(c("2026-06-30", "2026-07-15", "2026-08-31"))) %>%
  mutate(date = format(emergence_date, "%d %b"), yield_t_ha = round(yield_t_ha, 2)) %>%
  select(crop_or_cultivar, season_type, date, method, yield_t_ha) %>%
  pivot_wider(names_from = method, values_from = yield_t_ha) %>%
  arrange(crop_or_cultivar, season_type, match(date, c("30 Jun", "15 Jul", "31 Aug"))) %>%
  print(n = Inf)

# 4b. How closely does each fitted curve follow Kenton's own table points? (RMSE, t/ha; smaller = closer)
curves_ext %>%
  filter(part == "fitted to the table") %>%
  inner_join(curves %>% select(crop_or_cultivar, season_type, x, observed = yield_t_ha),
             by = c("crop_or_cultivar", "season_type", "x")) %>%
  group_by(crop_or_cultivar, season_type, method) %>%
  summarise(rmse = round(sqrt(mean((yield_t_ha - observed)^2)), 3), .groups = "drop") %>%
  pivot_wider(names_from = method, values_from = rmse) %>%
  print(n = Inf)

# ---- 5. Plot: Kenton's table (black dots) and the three curves --------------------
p_cmp <- ggplot() +
  geom_line(data = curves_ext, aes(emergence_date, yield_t_ha, colour = method, linetype = part), linewidth = 0.7) +
  geom_point(data = curves, aes(emergence_date, yield_t_ha), size = 1.5, colour = "black") +
  scale_colour_manual(values = c("Straight line" = "#c77c00", "Quadratic" = "#1d6b24", "Broken stick" = "#8b2f97"), name = "Method") +
  scale_linetype_manual(values = c("fitted to the table" = "dotted", "extension" = "solid"), name = NULL) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  facet_grid(season_type ~ crop_or_cultivar) +
  labs(title = "Ways of extending Kenton's yield curves beyond 10 June",
       subtitle = paste0("Black dots = Kenton's table (original yields, before any uplift). Solid = extension to ", format(extend_to, "%d %b"),
                         ". Dotted = the fitted curve over the table.\nEach extension is shifted to start at Kenton's last table value."),
       x = "Emergence date", y = "Yield (t/ha)") +
  theme_minimal(base_size = 10) +
  theme(panel.grid.minor = element_blank(), legend.position = "top",
        strip.text = element_text(face = "bold", hjust = 0))
p_cmp

ggsave(file.path(out_dir, "Yield_extension_methods_compared.png"), p_cmp,
       width = 28, height = 20, units = "cm", dpi = 300)
