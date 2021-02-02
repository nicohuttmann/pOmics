#' Calculates p-value from Fisher's exact test
#'
#' @param proteins logical vector of all proteins
#' @param annotated.proteins proteins in annotated group
#'
#' @return
#' @export
#'
#'
fisher_test <- function(proteins, annotated.proteins) {

  # Load topGO
  require(topGO)

  #
  return(
    topGO::runTest(
      new("classicCount",
          testStatistic = topGO::GOFisherTest,
          name = "fisher",
          allMembers = names(proteins),
          groupMembers = intersect(annotated.proteins, names(proteins)),
          sigMembers = names(proteins)[proteins])
    )
  )

}
