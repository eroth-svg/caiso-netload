source("R/build_features.R")

DAM_CLOSE_LOCAL_HOUR <- 10
PUBLICATION_LAG_H    <- 2

add_lag_features <- function(p, lags = c(48, 72, 96, 120, 144, 168, 336)) {
  key <- as.numeric(p$ts)
  stopifnot(!any(duplicated(key)), all(diff(key) > 0))
  
  cut_local <- as.POSIXct(paste0(format(p$OPR_DT - 1), " ",
                                 sprintf("%02d:00:00", DAM_CLOSE_LOCAL_HOUR)),
                          tz = "America/Los_Angeles")
  p$cutoff_ts <- as.POSIXct(as.numeric(cut_local) - PUBLICATION_LAG_H * 3600,
                            origin = "1970-01-01", tz = "UTC")
  
  for (k in lags) {
    src <- match(key - k * 3600, key)
    p[[paste0("net_lag", k)]] <- p$net_actual[src]
    stopifnot(all(key - k * 3600 <= as.numeric(p$cutoff_ts)))
  }
  
  wk <- c(48, 72, 96, 120, 144, 168)
  m  <- as.matrix(p[, paste0("net_lag", wk)])
  p$net_lag_mean6 <- rowMeans(m)
  
  p$lag_complete <- !is.na(p$net_lag48) & !is.na(p$net_lag168) &
    !is.na(p$net_lag336) & !is.na(p$net_lag_mean6)
  p
}