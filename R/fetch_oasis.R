library(httr2)
library(readr)
library(dplyr)
library(lubridate)

# Downloads one OASIS report. OASIS returns a ZIP containing one CSV.
fetch_oasis <- function(queryname, start, end, extra = list(), version = 1) {  
  # OASIS requires UTC timestamps formatted like 20250415T07:00-0000
  fmt <- function(x) format(with_tz(x, "UTC"), "%Y%m%dT%H:%M-0000")
  
  req <- request("https://oasis.caiso.com/oasisapi/SingleZip") |>
    req_url_query(
      resultformat  = 6,
      queryname     = queryname,
      version       = version,
      startdatetime = fmt(start),
      enddatetime   = fmt(end),
      !!!extra
    ) |>
    req_user_agent("ucsb-netload-research")
  
  tmp <- tempfile(fileext = ".zip")
  req_perform(req, path = tmp)
  
  exdir <- tempfile()
  dir.create(exdir)
  unzip(tmp, exdir = exdir)
  
  csv <- list.files(exdir, pattern = "\\.csv$", full.names = TRUE)
  if (length(csv) == 0) {
    stop("No CSV returned. OASIS probably errored - check the date range.")
  }
  read_csv(csv[1], show_col_types = FALSE)
}

# Fetch a long date range in <=30 day chunks, since OASIS caps most reports at 31 days.
fetch_range <- function(queryname, from, to, extra = list(), pause = 10) {
  starts <- seq(from, to, by = "30 days")
  out <- vector("list", length(starts))
  
  for (i in seq_along(starts)) {
    s <- starts[i]
    e <- min(s + days(30), to)
    message(sprintf("[%d/%d] %s -> %s", i, length(starts), s, e))
    
    out[[i]] <- tryCatch(
      fetch_oasis(queryname, s, e, extra),
      error = function(err) {
        warning(sprintf("FAILED %s to %s: %s", s, e, conditionMessage(err)))
        NULL
      }
    )
    Sys.sleep(pause)
  }
  
  bind_rows(out) |> distinct()
}
