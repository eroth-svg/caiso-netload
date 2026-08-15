source("R/backtest.R")

design <- function(d, use_caiso) {
  d$hr <- factor(pmin(d$OPR_HR, 24), levels = 1:24)
  f <- ~ hr + dow + month + hol_class + is_bridge +
    cdd_lag1 + hdd_lag1 +
    net_lag48 + net_lag168 + net_lag336 + net_lag_mean6
  if (use_caiso) f <- update(f, ~ . + hr:net_forecast)
  model.matrix(f, d)
}

ols_normal <- function(X, y) {
  XtX <- t(X) %*% X
  Xty <- t(X) %*% y
  as.numeric(solve(XtX) %*% Xty)
}

ols_qr <- function(X, y) as.numeric(qr.solve(X, y))

make_ols_fit <- function(use_caiso = TRUE, method = c("qr", "normal")) {
  method <- match.arg(method)
  function(tr, te) {
    Xtr <- design(tr, use_caiso)
    Xte <- design(te, use_caiso)
    keep <- intersect(colnames(Xtr), colnames(Xte))
    Xtr <- Xtr[, keep, drop = FALSE]
    Xte <- Xte[, keep, drop = FALSE]
    b <- if (method == "normal") ols_normal(Xtr, tr$net_actual) else ols_qr(Xtr, tr$net_actual)
    as.numeric(Xte %*% b)
  }
}

check_ols_agreement <- function(p, folds, use_caiso = TRUE, fold = 1) {
  tr <- p[folds[[fold]]$train, ]
  tr <- tr[tr$lag_complete, ]
  X  <- design(tr, use_caiso)
  y  <- tr$net_actual
  q  <- qr(X)
  bn <- ols_normal(X, y)
  bq <- ols_qr(X, y)
  bl <- as.numeric(coef(lm.fit(X, y)))
  c(rank        = q$rank,
    ncol        = ncol(X),
    kappa_X     = kappa(X, exact = TRUE),
    kappa_XtX   = kappa(crossprod(X), exact = TRUE),
    max_abs_nq  = max(abs(bn - bq)),
    max_abs_qlm = max(abs(bq - bl)),
    max_rel_nq  = max(abs((bn - bq) / bq)))
}