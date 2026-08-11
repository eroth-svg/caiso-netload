library(httr2); library(dplyr); library(purrr); library(tibble); library(lubridate)

# --- NOAA CDO API wrapper. Token lives in .Renviron, not in this file. ---
noaa_get <- function(endpoint, params = list()) {
  request("https://www.ncei.noaa.gov/cdo-web/api/v2") |>
    req_url_path_append(endpoint) |>
    req_url_query(!!!params) |>
    req_headers(token = Sys.getenv("NOAA_TOKEN")) |>
    req_perform() |>
    resp_body_json()
}

# One station, one datatype, one year (API caps at 1000 records per request).
fetch_noaa_year <- function(station, datatype, year) {
  r <- noaa_get("data", list(
    datasetid  = "GHCND",
    stationid  = station,
    datatypeid = datatype,
    startdate  = paste0(year, "-01-01"),
    enddate    = paste0(year, "-12-31"),
    units      = "metric",
    limit      = 1000
  ))
  if (length(r$results) == 0) return(NULL)
  tibble(
    station  = map_chr(r$results, "station"),
    date     = as.Date(substr(map_chr(r$results, "date"), 1, 10)),
    datatype = map_chr(r$results, "datatype"),
    value    = map_dbl(r$results, "value")
  )
}

# Loop over every station x datatype x year combination.
fetch_noaa_range <- function(stations, datatypes, years, pause = 0.5) {
  grid <- expand.grid(station = stations, datatype = datatypes, year = years,
                      stringsAsFactors = FALSE)
  out <- vector("list", nrow(grid))
  for (i in seq_len(nrow(grid))) {
    g <- grid[i, ]
    message(sprintf("[%d/%d] %s %s %d", i, nrow(grid), g$station, g$datatype, g$year))
    out[[i]] <- tryCatch(
      fetch_noaa_year(g$station, g$datatype, g$year),
      error = function(e) {
        warning(sprintf("FAILED %s %s %d: %s", g$station, g$datatype, g$year,
                        conditionMessage(e)))
        NULL
      }
    )
    Sys.sleep(pause)
  }
  bind_rows(out) |> distinct()
}

# --- Station selection and population weights ---
# Seven airport stations covering CAISO's major load centers.
# NOTE: weights are approximate population shares, NOT a documented CAISO
# load allocation. Consider re-deriving from TAC-area load shares.
stations <- c("GHCND:USW00023174",  # LAX
              "GHCND:USW00023188",  # San Diego Intl
              "GHCND:USW00023234",  # SFO
              "GHCND:USW00023232",  # Sacramento Exec
              "GHCND:USW00093193",  # Fresno Yosemite
              "GHCND:USW00023293",  # San Jose
              "GHCND:USW00003171")  # Riverside Muni

# =====================================================================
# Station weights derived from observed CAISO TAC-area load share.
#
# Rule (stated in full so every number is traceable):
#   1. Load share per TAC area over the modelling window
#      2023-05-01 -> 2025-12-31, from SLD_FCST/ACTUAL.
#   2. VEA-TAC dropped: Nevada, outside the CA weather footprint,
#      0.273% of load. Remaining shares renormalised.
#   3. MWD-TAC folded into SCE-TAC: Southern California water pumping,
#      same climate zone, 0.58% of load.
#   4. Each TAC's share split EVENLY across its stations.
#      No population data used anywhere.
#
# Known limitation: TAC load share drifts across the sample. PGE falls
# 0.454 -> 0.430 and SCE rises 0.451 -> 0.473 from 2021 to 2025,
# monotonic from 2022. A single fixed vector is a window average.
# =====================================================================

station_tac <- tibble::tribble(
  ~station,             ~region,             ~tac,
  "GHCND:USW00023234",  "Bay Area",          "PGE-TAC",
  "GHCND:USW00023293",  "South Bay",         "PGE-TAC",
  "GHCND:USW00023232",  "Sacramento Valley", "PGE-TAC",
  "GHCND:USW00093193",  "Central Valley",    "PGE-TAC",
  "GHCND:USW00023174",  "LA Basin",          "SCE-TAC",
  "GHCND:USW00003171",  "Inland Empire",     "SCE-TAC",
  "GHCND:USW00023188",  "San Diego",         "SDGE-TAC"
)

build_weights <- function(win_start = as.POSIXct("2023-05-01 07:00:00", tz = "UTC"),
                          win_end   = as.POSIXct("2026-01-01 08:00:00", tz = "UTC")) {
  
  # fetch_noaa.R has no reason to have loaded the CAISO load file, so read
  # it here if it isn't already in the session.
  if (!exists("load_act", envir = globalenv())) {
    load_act <- readr::read_csv("data-raw/sld_fcst_actual.csv",
                                show_col_types = FALSE)
  } else {
    load_act <- get("load_act", envir = globalenv())
  }
  
  tac_share <- load_act |>
    dplyr::filter(TAC_AREA_NAME %in% c("PGE-TAC","SCE-TAC","SDGE-TAC","MWD-TAC"),
                  INTERVALSTARTTIME_GMT >= win_start,
                  INTERVALSTARTTIME_GMT <  win_end) |>
    dplyr::distinct(INTERVALSTARTTIME_GMT, TAC_AREA_NAME, .keep_all = TRUE) |>
    dplyr::mutate(tac = dplyr::if_else(TAC_AREA_NAME == "MWD-TAC",
                                       "SCE-TAC", TAC_AREA_NAME)) |>
    dplyr::group_by(tac) |>
    dplyr::summarise(mwh = sum(MW), .groups = "drop") |>
    dplyr::mutate(tac_share = mwh / sum(mwh))
  
  w <- station_tac |>
    dplyr::left_join(tac_share, by = "tac") |>
    dplyr::group_by(tac) |>
    dplyr::mutate(w = tac_share / dplyr::n()) |>
    dplyr::ungroup() |>
    dplyr::select(station, region, tac, w)
  
  stopifnot(!any(is.na(w$w)))
  stopifnot(isTRUE(all.equal(sum(w$w), 1, tolerance = 1e-10)))
  w
}

weights <- build_weights()

build_weather_daily <- function(wx) {
  wx |>
    tidyr::pivot_wider(names_from = datatype, values_from = value) |>
    mutate(tavg = (TMAX + TMIN) / 2) |>
    # Drop station-days missing either reading; sum(w) renormalises over
    # whichever stations are present.
    filter(!is.na(TMAX), !is.na(TMIN)) |>
    inner_join(weights, by = "station") |>
    group_by(date) |>
    summarise(tavg_w = sum(tavg * w) / sum(w),
              tmax_w = sum(TMAX * w) / sum(w),
              tmin_w = sum(TMIN * w) / sum(w),
              n_stn  = n(), .groups = "drop") |>
    # Degree days, base 18.3 C = 65 F
    mutate(cdd = pmax(tavg_w - 18.3, 0),
           hdd = pmax(18.3 - tavg_w, 0))
}