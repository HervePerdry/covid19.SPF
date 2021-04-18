
plotRoll <- function(col, data, n = 7, ...) {
  y <- data[, col, drop = FALSE]
  plot(data$jour, y[,1], ylab = names(y)[1], xlab = "date", type = "o", ...)
  lines(data$jour, rolling.mean(y[,1], n), col = "red", lwd = 2)
}
