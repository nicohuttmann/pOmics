#' Calculates similarity from correlation
#'
#' @param data correlation matrix
#' @param method similarity function ("absolute", "preserve", "none")
#'
#' @return
#' @export
#'
#'
similarity <- function(data, method = "none") {

  #
  if (!method %in% c("absolute", "preserve", "none"))
    stop("Similarity function not known.")

  #
  if (method == "absolute") data <- abs(data)

  if (method == "preserve") data <- (1 + data) / 2

  #
  return(data)

}
