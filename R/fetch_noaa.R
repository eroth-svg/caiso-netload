library(httr2); library(dplyr); library(purrr); library(tibble); library(lubridate)

noaa_get <- function(endpoint, params = list()) {
  request("https://www.ncei.noaa.gov/cdo-web/api/v2") |>
    req_url_path_append(endpoint) |>
    req_url_query(!!!params) |>
    req_headers(token = Sys.getenv("NOAA_TOKEN")) |>
    req_perform() |>
    resp_body_json()
}

# One station, one datatype, one year at a time (API caps at 1000 records).
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

fetch_noaa_range <- function(stations, datatypes, years, pause = 0.5) {
  grid <- expand.grid(station = stations, datatype = datatypes, year = years,
                      stringsAsFactors = FALSE)
  out <- vector("list", nrow(grid))
  for (i in seq_len(nrow(grid))) {
    g <- grid[i, ]
    message(sprintf("[%d/%d] %s %s %d", i, nrow(grid), g$station, g$datatype, g$year))
    out[[i]] <- tryCatch(
      fetch_noaa_year(g$station, g$datatype, g$year),
      error = function(e) { warning(sprintf("FAILED %s %s %d: %s", g$station, g$datatype, g$year, conditionMessage(e))); NULL }
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

weights <- tibble(
  station = stations,
  w = c(0.30, 0.10, 0.15, 0.12, 0.10, 0.13, 0.10)
)

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