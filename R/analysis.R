source("R/model_gbm.R")

dm_test <- function(e1, e2, h = 24) {
  d <- abs(e1) - abs(e2)
  n <- length(d)
  dbar <- mean(d)
  dc <- d - dbar
  lrv <- sum(dc^2) / n
  L <- min(h, n - 1)
  if (L >= 1) for (k in 1:L) {
    gk <- sum(dc[(k + 1):n] * dc[1:(n - k)]) / n
    lrv <- lrv + 2 * (1 - k / (L + 1)) * gk
  }
  stat <- dbar / sqrt(max(lrv, .Machine$double.eps) / n)
  c(mean_diff = dbar, DM = stat, p = 2 * pnorm(-abs(stat)))
}

ablate <- function(p, folds, seed = 1) {
  drops <- list(
    full        = character(0),
    no_weather  = c("cdd_lag1", "hdd_lag1"),
    no_lags     = c("net_lag48", "net_lag168", "net_lag336", "net_lag_mean6"),
    no_caiso    = "net_forecast",
    no_calendar = c("dow_n", "mon_n", "hol_n", "is_bridge"),
    no_trend    = "tnum"
  )
  base <- names(gbm_features(p[1, ]))
  out <- lapply(names(drops), function(v) {
    keep <- setdiff(base, drops[[v]])
    fit <- function(tr, te) {
      set.seed(seed)
      m <- xgboost::xgb.train(
        params = list(objective = "reg:squarederror", max_depth = 6, eta = 0.05,
                      subsample = 0.8, colsample_bytree = 0.8, min_child_weight = 5),
        data = xgboost::xgb.DMatrix(as.matrix(gbm_features(tr)[, keep, drop = FALSE]),
                                    label = tr$net_actual),
        nrounds = 400, verbose = 0)
      as.numeric(predict(m, as.matrix(gbm_features(te)[, keep, drop = FALSE])))
    }
    r <- run_backtest(p, folds, fit)
    c(MAE = mean(abs(r$pred - r$actual)), RMSE = sqrt(mean((r$pred - r$actual)^2)))
  })
  m <- do.call(rbind, out)
  rownames(m) <- names(drops)
  round(cbind(m, cost_MAE = m[, "MAE"] - m["full", "MAE"]), 1)
}

ablate_ols <- function(p, folds) {
  drops <- list(full = character(0), no_weather = c("cdd_lag1", "hdd_lag1"),
                no_lags = c("net_lag48", "net_lag168", "net_lag336", "net_lag_mean6"),
                no_caiso = "net_forecast",
                no_calendar = c("dow", "month", "hol_class", "is_bridge"),
                no_trend = "tnum")
  terms_all <- c("hr", "dow", "month", "hol_class", "is_bridge", "cdd_lag1", "hdd_lag1",
                 "net_lag48", "net_lag168", "net_lag336", "net_lag_mean6")
  out <- lapply(names(drops), function(v) {
    tt <- setdiff(terms_all, drops[[v]])
    inter <- character(0)
    if (!"net_forecast" %in% drops[[v]]) inter <- c(inter, "hr:net_forecast")
    if (!"tnum" %in% drops[[v]]) inter <- c(inter, "hr:tnum")
    fml <- stats::as.formula(paste("~", paste(c(tt, inter), collapse = " + ")))
    fit <- function(tr, te) {
      prep <- function(d) {
        d$hr <- factor(pmin(d$OPR_HR, 24), levels = 1:24)
        d$tnum <- as.numeric(difftime(d$ts, as.POSIXct("2023-05-01", tz = "UTC"), units = "days")) / 365.25
        stats::model.matrix(fml, d)
      }
      Xtr <- prep(tr); Xte <- prep(te)
      k <- intersect(colnames(Xtr), colnames(Xte))
      as.numeric(Xte[, k, drop = FALSE] %*% qr.solve(Xtr[, k, drop = FALSE], tr$net_actual))
    }
    r <- run_backtest(p, folds, fit)
    c(MAE = mean(abs(r$pred - r$actual)), RMSE = sqrt(mean((r$pred - r$actual)^2)))
  })
  m <- do.call(rbind, out); rownames(m) <- names(drops)
  round(cbind(m, cost_MAE = m[, "MAE"] - m["full", "MAE"]), 1)
}

by_hour <- function(r, panel) {
  hr <- panel$OPR_HR[match(as.numeric(r$ts), as.numeric(panel$ts))]
  agg <- function(e) tapply(abs(e), hr, mean)
  data.frame(OPR_HR = as.integer(names(agg(r$pred - r$actual))),
             n      = as.integer(table(hr)),
             caiso  = round(as.numeric(agg(r$caiso - r$actual)), 1),
             model  = round(as.numeric(agg(r$pred - r$actual)), 1),
             pct    = round(100 * (as.numeric(agg(r$pred - r$actual)) /
                                     as.numeric(agg(r$caiso - r$actual)) - 1), 1))
}

extreme_perf <- function(r, q = 0.95) {
  hi <- r$actual >= quantile(r$actual, q)
  rbind(all  = c(n = nrow(r), caiso = mean(abs(r$caiso - r$actual)),
                 model = mean(abs(r$pred - r$actual))),
        peak = c(n = sum(hi), caiso = mean(abs(r$caiso[hi] - r$actual[hi])),
                 model = mean(abs(r$pred[hi] - r$actual[hi]))))
}

ramp_perf <- function(r, k = 3) {
  key <- as.numeric(r$ts)
  src <- match(key - k * 3600, key)
  ok <- !is.na(src)
  ra <- r$actual[ok] - r$actual[src[ok]]
  rc <- r$caiso[ok]  - r$caiso[src[ok]]
  rm <- r$pred[ok]   - r$pred[src[ok]]
  hi <- abs(ra) >= quantile(abs(ra), 0.95)
  rbind(all      = c(n = sum(ok), caiso = mean(abs(rc - ra)), model = mean(abs(rm - ra))),
        steepest = c(n = sum(hi), caiso = mean(abs(rc[hi] - ra[hi])),
                     model = mean(abs(rm[hi] - ra[hi]))))
}