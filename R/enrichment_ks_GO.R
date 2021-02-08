#' Performs gene set enrichment on numeric protein vector with specified Ontology
#'
#' @param proteins numeric score vector
#' @param ontology GO ontology
#' @param algorithm algorithm to use ("classic", "elim", "weight", "weight01")
#'
#' @return
#' @export
#'
#'
enrichment_ks_GO <- function(proteins, ontology, algorithm) {

  # Get and update GO object
  GOdata <- get_GOdata(proteins = proteins, ontology = ontology)

  #
  ks <- topGO::runTest(GOdata, algorithm = algorithm, statistic = "ks")


  results <- topGO::GenTable(GOdata,
                             p.value = ks,
                             orderBy = "ks",
                             topNodes = ks@geneData["SigTerms"])

  # Return
  return(results)

}
