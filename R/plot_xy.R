#' Matches names and plots scattrplot of two variables
#'
#' @param x names x vector
#' @param y named y vector
#' @param fit add fit
#'
#' @return
#' @export
#'
#'
plot_xy <- function(x, y, fit = F) {

  # check input for names
  if (length(names(x)) == 0 || length(names(y)) == 0) stop("Arguments don't have names.")

  names <- intersect(names(x), names(y))

  if (fit) model <- lm(y ~ x)

  plot(x[names], y[names], main = ifelse(fit, paste0("R2 = ", summary(model)$r.squared), ""),
       xlab = deparse(substitute(x)),
       ylab = deparse(substitute(x)),
       pch = 16)

  if (fit) abline(lm(y ~ x), col = "red")

}
