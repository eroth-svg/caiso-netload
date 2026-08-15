source("R/build_holidays.R")

build_model_panel <- function(
    bench_path   = "data/caiso_netload_benchmark_hourly.csv",
    weather_path = "data/weather_daily.csv",
    out_path     = "data/caiso_model_panel.csv"
) {
  b <- read.csv(bench_path, stringsAsFactors = FALSE)
  w <- read.csv(weather_path, stringsAsFactors = FALSE)
  
  b$ts     <- as.POSIXct(b$INTERVALSTARTTIME_GMT, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  b$OPR_DT <- as.Date(b$OPR_DT)
  w$date   <- as.Date(w$date)
  
  stopifnot(!any(is.na(b$ts)), !any(is.na(b$OPR_DT)), !any(is.na(w$date)))
  b <- b[order(b$ts), ]
  
  wcols <- c("tavg_w", "tmax_w", "tmin_w", "cdd", "hdd")
  wkey  <- as.integer(w$date)
  
  i0 <- match(as.integer(b$OPR_DT),      wkey)
  i1 <- match(as.integer(b$OPR_DT) - 1L, wkey)
  stopifnot(!any(is.na(i0)), !any(is.na(i1)))
  
  d0 <- w[i0, wcols]; names(d0) <- paste0(c("tavg", "tmax", "tmin", "cdd", "hdd"), "_d0")
  d1 <- w[i1, wcols]; names(d1) <- paste0(c("tavg", "tmax", "tmin", "cdd", "hdd"), "_lag1")
  
  hol <- build_holidays(seq(as.numeric(format(min(b$OPR_DT), "%Y")) - 1,
                            as.numeric(format(max(b$OPR_DT), "%Y")) + 1))
  hf  <- holiday_features(b$OPR_DT, hol)
  
  p <- data.frame(
    ts           = b$ts,
    OPR_DT       = b$OPR_DT,
    OPR_HR       = b$OPR_HR,
    net_actual   = b$net_actual,
    net_forecast = b$net_forecast,
    net_err      = b$net_err,
    load_actual  = b$actual,
    load_dam     = b$forecast,
    ren_actual   = b$ren_mw,
    ren_dam      = b$ren_dam,
    dow          = factor(hf$dow, levels = 0:6,
                          labels = c("Sun","Mon","Tue","Wed","Thu","Fri","Sat")),
    month        = factor(as.integer(format(b$OPR_DT, "%m")), levels = 1:12),
    doy          = as.integer(format(b$OPR_DT, "%j")),
    is_weekend   = hf$is_weekend,
    is_holiday   = hf$is_holiday,
    hol_class    = hf$hol_class,
    is_bridge    = hf$is_bridge,
    is_offday    = hf$is_offday,
    stringsAsFactors = FALSE
  )
  
  p <- cbind(p, d1, d0)
  rownames(p) <- NULL
  
  stopifnot(nrow(p) == nrow(b))
  stopifnot(!anyNA(p))
  stopifnot(!any(duplicated(as.numeric(p$ts))))
  stopifnot(all(diff(as.numeric(p$ts)) > 0))
  
  out <- p
  out$ts <- format(p$ts, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  stopifnot(all(nchar(out$ts) == 20))
  write.csv(out, out_path, row.names = FALSE)
  
  chk <- read_model_panel(out_path)
  stopifnot(nrow(chk) == nrow(p))
  stopifnot(identical(as.numeric(chk$ts), as.numeric(p$ts)))
  
  p
}

read_model_panel <- function(path = "data/caiso_model_panel.csv") {
  q <- read.csv(path, stringsAsFactors = FALSE)
  q$ts     <- as.POSIXct(q$ts, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  q$OPR_DT <- as.Date(q$OPR_DT)
  q$dow    <- factor(q$dow, levels = c("Sun","Mon","Tue","Wed","Thu","Fri","Sat"))
  q$month  <- factor(q$month, levels = 1:12)
  q$hol_class <- factor(q$hol_class, levels = c("none","minor","major"))
  stopifnot(!anyNA(q$ts), !anyNA(q$OPR_DT))
  q
}