#' Calculates coefficient of variation
#'
#' @param data data matrix
#'
#' @return
#' @export
#'
#'
cv_m <- function(data) {

  #
  cvs <- apply(X = data,
               MARGIN = 2,
               FUN = cv)

  # Return
  return(cvs)

}
