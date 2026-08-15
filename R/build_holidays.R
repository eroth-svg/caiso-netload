.nth_wday <- function(y, m, wd, n) {
  d1 <- as.Date(sprintf("%04d-%02d-01", y, m))
  d1 + ((wd - as.POSIXlt(d1)$wday) %% 7) + 7 * (n - 1)
}

.last_wday <- function(y, m, wd) {
  d <- seq(as.Date(sprintf("%04d-%02d-01", y, m)), by = "month", length.out = 2)[2] - 1
  d - ((as.POSIXlt(d)$wday - wd) %% 7)
}

.observed <- function(d) {
  w <- as.POSIXlt(d)$wday
  d + ifelse(w == 6L, -1, ifelse(w == 0L, 1, 0))
}

# --- the holiday table ------------------------------------------------------
build_holidays <- function(years) {
  MON <- 1L; THU <- 4L
  
  parts <- lapply(years, function(y) {
    thx <- .nth_wday(y, 11, THU, 4)             # Thanksgiving
    
    fed <- data.frame(
      date = as.Date(c(
        .observed(as.Date(sprintf("%04d-01-01", y))),
        .nth_wday(y,  1, MON, 3),
        .nth_wday(y,  2, MON, 3),
        .last_wday(y, 5, MON),
        .observed(as.Date(sprintf("%04d-06-19", y))),
        .observed(as.Date(sprintf("%04d-07-04", y))),
        .nth_wday(y,  9, MON, 1),
        .nth_wday(y, 10, MON, 2),
        .observed(as.Date(sprintf("%04d-11-11", y))),
        thx,
        .observed(as.Date(sprintf("%04d-12-25", y)))
      ), origin = "1970-01-01"),
      holiday = c("new_year", "mlk", "presidents", "memorial", "juneteenth",
                  "independence", "labor", "columbus", "veterans",
                  "thanksgiving", "christmas"),
      hol_class = c("major", "minor", "minor", "major", "minor",
                    "major", "major", "minor", "minor",
                    "major", "major"),
      federal = TRUE,
      stringsAsFactors = FALSE
    )
    

    extra <- data.frame(
      date = as.Date(c(thx + 1,
                       as.Date(sprintf("%04d-12-24", y)),
                       as.Date(sprintf("%04d-12-31", y)),
                       as.Date(sprintf("%04d-03-31", y)))),
      holiday   = c("black_friday", "christmas_eve", "new_year_eve", "cesar_chavez"),
      hol_class = c("minor", "minor", "minor", "minor"),
      federal   = FALSE,
      stringsAsFactors = FALSE
    )
    
    rbind(fed, extra)
  })
  
  h <- do.call(rbind, parts)
  

  prio <- (!h$federal) * 2L + (h$hol_class == "minor") * 1L
  h <- h[order(h$date, prio), ]
  h <- h[!duplicated(h$date), ]
  h <- h[order(h$date), ]
  rownames(h) <- NULL
  
  stopifnot(!any(duplicated(h$date)))
  h
}

# --- panel-side flags -------------------------------------------------------
holiday_features <- function(dates, hol) {
  dates <- as.Date(dates)
  wd    <- as.POSIXlt(dates)$wday
  

  hkey  <- as.integer(hol$date)
  dkey  <- as.integer(dates)
  i     <- match(dkey, hkey)
  
  is_hol <- !is.na(i)
  cls    <- ifelse(is_hol, as.character(hol$hol_class[i]), "none")
  cls[is.na(cls)] <- "none"
  
  is_weekend <- wd %in% c(0L, 6L)
  off        <- is_hol | is_weekend          # "non-working day"
  
  # Bridge day: a working weekday whose neighbors on both sides are off.
  prev_off <- (dkey - 1L) %in% hkey | as.POSIXlt(dates - 1)$wday %in% c(0L, 6L)
  next_off <- (dkey + 1L) %in% hkey | as.POSIXlt(dates + 1)$wday %in% c(0L, 6L)
  bridge   <- !off & prev_off & next_off
  
  data.frame(
    OPR_DT       = dates,
    dow          = wd,
    is_weekend   = is_weekend,
    is_holiday   = is_hol,
    holiday      = ifelse(is_hol, hol$holiday[i], NA_character_),
    hol_class    = factor(cls, levels = c("none", "minor", "major")),
    is_bridge    = bridge,
    is_offday    = off | bridge,
    stringsAsFactors = FALSE
  )
}