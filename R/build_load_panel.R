library(readr); library(dplyr); library(lubridate)

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