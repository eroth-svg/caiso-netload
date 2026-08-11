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

# =====================================================================
# BENCHMARK: CAISO's own day-ahead net load forecast
#   net_forecast = (day-ahead load forecast) - (day-ahead renewables forecast)
# This is the number every model in the paper is measured against.
# =====================================================================

# ZP26 wind is excluded here exactly as on the actual side (Sec 6.1).
# The symmetry is mandatory, not cosmetic: differencing a 6-series
# forecast against a 5-series actual would bias the benchmark by the
# full ZP26 wind amount from 2024-02-08 onward.
ren_dam <- read_csv("data-raw/sld_ren_fcst_dam.csv", show_col_types = FALSE)

ren_dam_hourly <- ren_dam |>
  filter(!(RENEWABLE_TYPE == "Wind" & TRADING_HUB == "ZP26")) |>
  distinct(INTERVALSTARTTIME_GMT, RENEWABLE_TYPE, TRADING_HUB, .keep_all = TRUE) |>
  group_by(INTERVALSTARTTIME_GMT) |>
  summarise(ren_dam = sum(MW), n_series = n(), .groups = "drop") |>
  mutate(ren_dam = if_else(n_series == 5L, ren_dam, NA_real_)) |>
  select(INTERVALSTARTTIME_GMT, ren_dam) |>
  arrange(INTERVALSTARTTIME_GMT)

# err sign convention matches `full`: (forecast - actual).
# Negative bias => CAISO forecasts below what actually happened.
net2 <- net |>
  inner_join(ren_dam_hourly, by = "INTERVALSTARTTIME_GMT") |>
  filter(!is.na(ren_dam)) |>
  mutate(
    net_forecast = forecast - ren_dam,
    net_err      = net_forecast - net_actual,
    net_pct      = net_err / net_actual * 100
  ) |>
  arrange(INTERVALSTARTTIME_GMT)

# --- Metrics ---------------------------------------------------------
# MAE and RMSE are primary. MAPE is reported for continuity with the
# gross-load results but is unstable here: the denominator gets small at
# midday (Sec 8.2). The two normalised variants have stable denominators.
benchmark_overall <- net2 |>
  summarise(
    n         = n(),
    bias      = mean(net_err),
    MAE       = mean(abs(net_err)),
    RMSE      = sqrt(mean(net_err^2)),
    MAPE      = mean(abs(net_err / net_actual)) * 100,
    nMAE_mean = mean(abs(net_err)) / mean(net_actual) * 100,
    nMAE_peak = mean(abs(net_err)) / max(net_actual)  * 100
  )

benchmark_hourly <- net2 |>
  group_by(OPR_HR) |>
  summarise(
    n        = n(),
    mean_net = mean(net_actual),
    bias     = mean(net_err),
    MAE      = mean(abs(net_err)),
    RMSE     = sqrt(mean(net_err^2)),
    MAPE     = mean(abs(net_err / net_actual)) * 100,
    .groups  = "drop"
  ) |>
  arrange(OPR_HR)

if (!dir.exists("output")) dir.create("output")
write_csv(net2,              "data/caiso_netload_benchmark_hourly.csv")
write_csv(benchmark_overall, "output/benchmark_overall.csv")
write_csv(benchmark_hourly,  "output/benchmark_by_hour.csv")

# --- Guards ----------------------------------------------------------
# Informative rather than bare integers: a failure should say what broke.
if (nrow(net2) != 23375) {
  stop(sprintf(
    "net2 has %d rows, expected 23375 (lost %d to the DAM renewables join)",
    nrow(net2), 23375 - nrow(net2)))
}
if (!isTRUE(all.equal(benchmark_overall$MAE,  2077.21, tolerance = 1e-4))) {
  stop(sprintf("MAE drifted: got %.2f, expected 2077.21",  benchmark_overall$MAE))
}
if (!isTRUE(all.equal(benchmark_overall$RMSE, 3243.86, tolerance = 1e-4))) {
  stop(sprintf("RMSE drifted: got %.2f, expected 3243.86", benchmark_overall$RMSE))
}