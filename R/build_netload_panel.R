library(readr); library(dplyr); library(lubridate)

# --- Load side: actual and day-ahead forecast, 2021-2025 ---
load_act <- read_csv("data-raw/sld_fcst_actual.csv", show_col_types = FALSE)
load_dam <- read_csv("data-raw/sld_fcst_dam.csv",    show_col_types = FALSE)

pick3 <- function(d) d |>
  filter(TAC_AREA_NAME == "CA ISO-TAC") |>
  select(INTERVALSTARTTIME_GMT, OPR_DT, OPR_HR, MW) |>
  distinct(INTERVALSTARTTIME_GMT, .keep_all = TRUE)

full <- pick3(load_act) |> rename(actual = MW) |>
  inner_join(pick3(load_dam) |> select(INTERVALSTARTTIME_GMT, forecast = MW),
             by = "INTERVALSTARTTIME_GMT") |>
  mutate(err = forecast - actual, pct = err / actual * 100) |>
  arrange(INTERVALSTARTTIME_GMT)

stopifnot(nrow(full) == 43818)
write_csv(full, "data/caiso_load_hourly.csv")

# --- Renewables side: actual generation, 2023-05 onward ---
# Wind/ZP26 excluded: CAISO began publishing it 2024-02-08 (mean 25.4 MW,
# max 94 MW). Including it would create a step change mid-sample.
ren_act <- read_csv("data-raw/sld_ren_fcst_actual.csv", show_col_types = FALSE)

ren_hourly <- ren_act |>
  filter(!(RENEWABLE_TYPE == "Wind" & TRADING_HUB == "ZP26")) |>
  distinct(INTERVALSTARTTIME_GMT, RENEWABLE_TYPE, TRADING_HUB, .keep_all = TRUE) |>
  group_by(INTERVALSTARTTIME_GMT, OPR_DT, OPR_HR) |>
  summarise(ren_mw = sum(MW), n_series = n(), .groups = "drop") |>
  mutate(ren_mw = if_else(n_series == 5, ren_mw, NA_real_)) |>
  arrange(INTERVALSTARTTIME_GMT)

# --- Net load ---
net <- full |>
  inner_join(ren_hourly |> select(INTERVALSTARTTIME_GMT, ren_mw),
             by = "INTERVALSTARTTIME_GMT") |>
  filter(!is.na(ren_mw)) |>
  mutate(net_actual = actual - ren_mw) |>
  arrange(INTERVALSTARTTIME_GMT)

stopifnot(nrow(net) == 23375)
write_csv(net, "data/caiso_netload_hourly.csv")