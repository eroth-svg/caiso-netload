source("R/figures.R")

run_all <- function(outdir = "output") {
  dir.create(outdir, showWarnings = FALSE)
  t0 <- Sys.time()
  
  message("[1/6] building panel")
  build_model_panel()
  p <- add_lag_features(read_model_panel())
  f <- make_folds(p)
  stopifnot(nrow(p) == 23375, sum(p$lag_complete) == 22701, length(f) == 12)
  
  message("[2/6] running ladder")
  rungs <- list(
    "Seasonal naive"          = fit_seasonal_naive,
    "Hour-of-day bias corr."  = fit_hour_bias,
    "OLS (no CAISO input)"    = make_ols_fit(FALSE, TRUE,  "qr"),
    "OLS + CAISO x hour"      = make_ols_fit(TRUE,  FALSE, "qr"),
    "OLS + CAISO + trend"     = make_ols_fit(TRUE,  TRUE,  "qr"),
    "Gradient boosting"       = make_gbm_fit()
  )
  runs <- lapply(rungs, function(fn) run_backtest(p, f, fn))
  lad <- do.call(rbind, lapply(runs, function(r) metrics(r$pred, r$actual)))
  lad <- rbind("CAISO day-ahead" = metrics(runs[[1]]$caiso, runs[[1]]$actual), lad)
  lad <- data.frame(label = rownames(lad), lad, row.names = NULL)
  lad$vs_caiso_pct <- round(100 * (lad$MAE / lad$MAE[1] - 1), 1)
  write.csv(lad, file.path(outdir, "ladder.csv"), row.names = FALSE)
  
  message("[3/6] significance and diagnostics")
  best <- runs[["Gradient boosting"]]
  ols  <- runs[["OLS + CAISO + trend"]]
  hb   <- runs[["Hour-of-day bias corr."]]
  dm <- rbind(
    "OLS vs CAISO"    = dm_test(ols$caiso - ols$actual,  ols$pred  - ols$actual),
    "GBM vs CAISO"    = dm_test(best$caiso - best$actual, best$pred - best$actual),
    "OLS vs hourbias" = dm_test(hb$pred   - hb$actual,   ols$pred  - ols$actual),
    "GBM vs OLS"      = dm_test(ols$pred  - ols$actual,  best$pred - best$actual))
  write.csv(data.frame(comparison = rownames(dm), dm, row.names = NULL),
            file.path(outdir, "dm_tests.csv"), row.names = FALSE)
  
  bh <- by_hour(best, p)
  write.csv(bh, file.path(outdir, "by_hour.csv"), row.names = FALSE)
  ep <- extreme_perf(best); rp <- ramp_perf(best)
  write.csv(data.frame(subset = rownames(ep), ep, row.names = NULL),
            file.path(outdir, "extreme.csv"), row.names = FALSE)
  write.csv(data.frame(subset = rownames(rp), rp, row.names = NULL),
            file.path(outdir, "ramps.csv"), row.names = FALSE)
  ck <- check_ols_agreement(p, f)
  write.csv(data.frame(check = names(ck), value = as.numeric(ck)),
            file.path(outdir, "conditioning.csv"), row.names = FALSE)
  
  message("[4/6] ablation")
  ab <- ablate(p, f)
  write.csv(data.frame(variant = rownames(ab), ab, row.names = NULL),
            file.path(outdir, "ablation.csv"), row.names = FALSE)
  
  message("[5/6] sensitivity")
  build_model_panel(weather_path = "data/weather_daily_popweights.csv",
                    out_path     = "data/caiso_model_panel_popw.csv")
  pw <- add_lag_features(read_model_panel("data/caiso_model_panel_popw.csv"))
  fw <- make_folds(pw)
  p2 <- p; p2$cdd_lag1 <- p2$cdd_d0; p2$hdd_lag1 <- p2$hdd_d0
  sens <- rbind(
    "OLS, load weights"    = metrics(ols$pred, ols$actual),
    "OLS, pop weights"     = metrics(run_backtest(pw, fw, make_ols_fit(TRUE, TRUE, "qr"))$pred, ols$actual),
    "OLS, perfect weather" = metrics(run_backtest(p2, f,  make_ols_fit(TRUE, TRUE, "qr"))$pred, ols$actual),
    "GBM, load weights"    = metrics(best$pred, best$actual),
    "GBM, pop weights"     = metrics(run_backtest(pw, fw, make_gbm_fit())$pred, best$actual),
    "GBM, perfect weather" = metrics(run_backtest(p2, f,  make_gbm_fit())$pred, best$actual))
  write.csv(data.frame(variant = rownames(sens), sens, row.names = NULL),
            file.path(outdir, "sensitivity.csv"), row.names = FALSE)
  
  message("[6/6] figures")
  g <- read.csv("data/caiso_load_hourly.csv", stringsAsFactors = FALSE)
  fig_divergence(g)
  fig_ladder(lad[, c("label", "MAE")])
  fig_by_hour(bh)
  fig_peak_ramp(ep, rp)
  
  message(sprintf("done in %.1f min", as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  invisible(list(panel = p, folds = f, ladder = lad, dm = dm, by_hour = bh,
                 extreme = ep, ramps = rp, ablation = ab, sensitivity = sens))
}