#' Performs overexpression enrichment on logical protein vector with specified GO ontology
#'
#' @param proteins character vector
#' @param ontology GO ontology
#' @param algorithm algorithm to use ("classic", "elim", "weight", "weight01")
#'
#' @return
#' @export
#'
#'
enrichment_fisher_GO <- function(proteins, ontology, algorithm) {

  # Get and update GO object
  GOdata <- get_GOdata(proteins = proteins, ontology = ontology)

  #
  fisher <- topGO::runTest(GOdata, algorithm = algorithm, statistic = "fisher")


  results <- topGO::GenTable(GOdata,
                             p.value = fisher,
                             orderBy = algorithm,
                             topNodes = fisher@geneData["SigTerms"])

  # Return
  return(results)

}
