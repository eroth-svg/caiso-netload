source("R/analysis.R")

fig_divergence <- function(g, path = "output/fig1_divergence.pdf") {
  g$OPR_DT <- as.Date(g$OPR_DT)
  ym <- format(g$OPR_DT, "%Y-%m")
  mid <- tapply(g$err[g$OPR_HR %in% 10:14], ym[g$OPR_HR %in% 10:14], mean)
  ngt <- tapply(g$err[g$OPR_HR %in% c(1:6, 21:24)], ym[g$OPR_HR %in% c(1:6, 21:24)], mean)
  x <- seq_along(mid)
  pdf(path, width = 6.5, height = 3.6); par(mar = c(4, 4.2, 1, 1), cex = 0.85)
  plot(x, mid, type = "l", lwd = 2, col = "#B22222", ylim = range(c(mid, ngt, 0)),
       xaxt = "n", xlab = "", ylab = "Mean forecast error (MW)")
  lines(x, ngt, lwd = 2, col = "#4682B4")
  abline(h = 0, lty = 3, col = "grey50")
  at <- which(substr(names(mid), 6, 7) == "01")
  axis(1, at = at, labels = substr(names(mid)[at], 1, 4))
  fit <- lm(log(-mid) ~ x); lines(x, -exp(predict(fit)), lty = 2, lwd = 1.5)
  legend("bottomleft", bty = "n",
         legend = c("Midday (hours 10-14)", "Night (hours 1-6, 21-24)",
                    sprintf("Exponential fit: %.0f%%/yr", 100 * (exp(coef(fit)[2] * 12) - 1))),
         col = c("#B22222", "#4682B4", "black"), lty = c(1, 1, 2), lwd = c(2, 2, 1.5))
  dev.off(); invisible(path)
}

fig_ladder <- function(res, path = "output/fig2_ladder.pdf") {
  o <- order(-res$MAE)
  pdf(path, width = 6.5, height = 3.8); par(mar = c(4, 11, 1, 3.5), cex = 0.85)
  b <- barplot(res$MAE[o], horiz = TRUE, names.arg = res$label[o], las = 1,
               col = ifelse(res$label[o] == "CAISO day-ahead", "grey60", "#4682B4"),
               xlab = "MAE (MW)", xlim = c(0, max(res$MAE) * 1.18), border = NA)
  text(res$MAE[o] + max(res$MAE) * 0.015, b, sprintf("%.0f", res$MAE[o]), adj = 0, cex = 0.8)
  dev.off(); invisible(path)
}

fig_by_hour <- function(bh, path = "output/fig3_by_hour.pdf") {
  bh <- bh[bh$OPR_HR <= 24, ]
  pdf(path, width = 6.5, height = 3.6); par(mar = c(4, 4.2, 1, 1), cex = 0.85)
  plot(bh$OPR_HR, bh$caiso, type = "b", pch = 16, lwd = 2, col = "grey40",
       ylim = c(0, max(bh$caiso) * 1.05), xlab = "Operating hour (OPR_HR)",
       ylab = "MAE (MW)")
  lines(bh$OPR_HR, bh$model, type = "b", pch = 17, lwd = 2, col = "#B22222")
  legend("topright", bty = "n", legend = c("CAISO day-ahead", "Gradient boosting"),
         col = c("grey40", "#B22222"), pch = c(16, 17), lwd = 2)
  dev.off(); invisible(path)
}

fig_peak_ramp <- function(ep, rp, path = "output/fig4_peak_ramp.pdf") {
  m <- rbind(ep[, c("caiso", "model")], rp[, c("caiso", "model")])
  rownames(m) <- c("All\nhours", "Top 5%\nnet load", "All\nramps", "Steepest 5%\nramps")
  pdf(path, width = 6.5, height = 3.4); par(mar = c(3.2, 4.2, 1, 1), cex = 0.85, mgp = c(2.6, 1.4, 0))
  b <- barplot(t(m), beside = TRUE, col = c("grey60", "#B22222"), border = NA,
               ylab = "MAE (MW)", ylim = c(0, max(m) * 1.15))
  text(b, t(m) + max(m) * 0.02, sprintf("%.0f", t(m)), cex = 0.7)
  legend("topleft", bty = "n", legend = c("CAISO", "Model"),
         fill = c("grey60", "#B22222"), border = NA)
  dev.off(); invisible(path)
}

latex_table <- function(df, digits = 1) {
  num <- sapply(df, is.numeric)
  df[num] <- lapply(df[num], function(z) formatC(z, format = "f", digits = digits, big.mark = ","))
  cat(paste(colnames(df), collapse = " & "), "\\\\\n\\midrule\n")
  invisible(apply(df, 1, function(r) cat(paste(r, collapse = " & "), "\\\\\n")))
}