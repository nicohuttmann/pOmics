#' Transforms list to group vector for functional enrichment
#'
#' @param x list of proein ids
#'
#' @return
#' @export
#'
#'
list2group_vector <- function(x) {

  #
  return(unlist(topGO::inverseList(x)))

}
