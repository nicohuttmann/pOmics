#' Performs gene set enrichment on numeric protein vector with specified Ontology
#'
#' @param proteins numeric score vector
#' @param ontology GO ontology
#'
#' @return
#' @export
#'
#'
enrichment_ks_GO <- function(proteins, ontology) {

  # Get and update GO object
  GOdata <- get_GOdata(proteins = proteins, ontology = ontology)

  #
  ks <- topGO::runTest(GOdata, algorithm = "classic", statistic = "ks")


  results <- topGO::GenTable(GOdata,
                             p.value = ks,
                             orderBy = "ks",
                             topNodes = ks@geneData["SigTerms"])

  # Return
  return(results)

}
