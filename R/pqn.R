#' Probabilistic quotient normalization
#'
#' @param data data to be normalized
#' @param data2 (optional) data from which normalization factors can be calculated
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
#'
pqn <- function(data, data2) {

  #
  if (!hasArg(data2)) {
    data2 <- data
  }

  # Check observations of data and data2
  if (any(rownames(data) != rownames(data2))) stop("Datasets contain different observations.")


  #
  factors <- matrix(NA, nrow = nrow(data2), ncol = nrow(data2))
  rownames(factors) <- colnames(factors)
  rownames(factors) <- rownames(data2)

  #
  for(i in 1:nrow(data2)) {
    factors[i, ] <- data2 %>%
      factorize_matrix(i) %>%
      apply(1, FUN = function(x) {median(x, na.rm = T)})
  }

  #
  return(data / apply(factors, 2, geomean))


}
