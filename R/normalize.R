#' Normalizes and returns data
#'
#' @param data data
#' @param method method to use
#'
#' @return
#' @export
#'
#'
normalize <- function(data, method = "pqn") {

  #
  if (method == "none") return(data)

  #
  else if (method == "pqn") return(pqn(data))

  #
  else stop("Normalization method not found.")

}
