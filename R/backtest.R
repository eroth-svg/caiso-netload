source("R/build_lags.R")

metrics <- function(pred, actual) {
  e <- pred - actual
  c(n         = length(e),
    bias      = mean(e),
    MAE       = mean(abs(e)),
    RMSE      = sqrt(mean(e^2)),
    MAPE      = 100 * mean(abs(e / actual)),
    nMAE_mean = 100 * mean(abs(e)) / mean(actual),
    nMAE_peak = 100 * mean(abs(e)) / max(actual))
}

make_folds <- function(p, first_test = as.Date("2025-01-01")) {
  stopifnot(!is.unsorted(as.numeric(p$ts)))
  months <- sort(unique(format(p$OPR_DT[p$OPR_DT >= first_test], "%Y-%m")))
  lapply(months, function(m) {
    test  <- which(format(p$OPR_DT, "%Y-%m") == m)
    train <- which(as.numeric(p$ts) < min(as.numeric(p$cutoff_ts[test])))
    stopifnot(length(test) > 0, length(train) > 0,
              max(as.numeric(p$ts[train])) < min(as.numeric(p$cutoff_ts[test])))
    list(month = m, train = train, test = test)
  })
}

run_backtest <- function(p, folds, fit_predict, require_complete = TRUE) {
  out <- lapply(folds, function(f) {
    tr <- p[f$train, ]; te <- p[f$test, ]
    if (require_complete) { tr <- tr[tr$lag_complete, ]; te <- te[te$lag_complete, ] }
    data.frame(ts = te$ts, month = f$month, actual = te$net_actual,
               caiso = te$net_forecast, pred = fit_predict(tr, te))
  })
  r <- do.call(rbind, out)
  stopifnot(!anyNA(r$pred))
  r
}

compare <- function(r) {
  rbind(model = metrics(r$pred, r$actual),
        caiso = metrics(r$caiso, r$actual))
}

fit_seasonal_naive <- function(tr, te) te$net_lag168

fit_hour_bias <- function(tr, te) {
  b <- tapply(tr$net_forecast - tr$net_actual, tr$OPR_HR, mean)
  adj <- b[as.character(te$OPR_HR)]
  adj[is.na(adj)] <- 0
  te$net_forecast - as.numeric(adj)
}