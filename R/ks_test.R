#' Calculates p-value from Fisher's exact test
#'
#' @param protein.scores numeric vector of all proteins scores
#' @param annotated.proteins proteins in annotated group
#'
#' @return
#' @export
#'
ks_test <- function(protein.scores, annotated.proteins) {

  # Load library
  require(topGO)


  return(
    topGO::runTest(
      new("classicScore",
          testStatistic = topGO::GOKSTest,
          name = "ks",
          allMembers = names(protein.scores),
          score = protein.scores,
          groupMembers = intersect(annotated.proteins, names(protein.scores)))
    )
  )

}
