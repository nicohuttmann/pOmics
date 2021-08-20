#' Matches vector entries to list (stackoverflow question 11002391)
#'
#' @param vector vector from which to match
#' @param list list vector entries should be matched to
#'
#' @return
#' @export
#'
#'
match_v2l <- function(vector, list) {

  g <- rep(seq_along(list), sapply(list, length))

  return(g[match(vector, unlist(list))])

}
