#' Combines logical vectors using and
#'
#' @param a logical vector 1
#' @param b logical vector 2
#'
#' @return
#' @export
#'
#'
`%and%` <- function(a, b) {

  # if not same length stop
  if(length(a) != length(b)) stop("Arguments are not the same lenght.")

  c <- c()
  for(i in seq_along(a)) {
    c <- c(c, a[i] && b[i])
  }
  c
}
