source("R/model_ols.R")
library(xgboost)

gbm_features <- function(d) {
  data.frame(
    OPR_HR        = as.numeric(d$OPR_HR),
    tnum          = as.numeric(difftime(d$ts, as.POSIXct("2023-05-01", tz = "UTC"), units = "days")) / 365.25,
    dow_n         = as.integer(d$dow),
    mon_n         = as.integer(d$month),
    hol_n         = as.integer(d$hol_class),
    is_bridge     = as.integer(d$is_bridge),
    cdd_lag1      = d$cdd_lag1,
    hdd_lag1      = d$hdd_lag1,
    net_lag48     = d$net_lag48,
    net_lag168    = d$net_lag168,
    net_lag336    = d$net_lag336,
    net_lag_mean6 = d$net_lag_mean6,
    net_forecast  = d$net_forecast
  )
}

make_gbm_fit <- function(nrounds = 400, max_depth = 6, eta = 0.05, seed = 1) {
  function(tr, te) {
    set.seed(seed)
    dtr <- xgb.DMatrix(as.matrix(gbm_features(tr)), label = tr$net_actual)
    m <- xgb.train(
      params = list(objective = "reg:squarederror", max_depth = max_depth, eta = eta,
                    subsample = 0.8, colsample_bytree = 0.8, min_child_weight = 5),
      data = dtr, nrounds = nrounds, verbose = 0)
    as.numeric(predict(m, as.matrix(gbm_features(te))))
  }
}