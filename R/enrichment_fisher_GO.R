#' Performs overexpression enrichment on logical protein vector with specified GO ontology
#'
#' @param proteins character vector
#' @param ontology GO ontology
#'
#' @return
#' @export
#'
#'
enrichment_fisher_GO <- function(proteins, ontology) {

  # Get and update GO object
  GOdata <- get_GOdata(proteins = proteins, ontology = ontology)

  #
  fisher <- topGO::runTestrunTest(GOdata, algorithm = "classic", statistic = "fisher")


  results <- topGO::runTestGenTable(GOdata,
                                    p.value = fisher,
                                    orderBy = "classic",
                                    topNodes = fisher@geneData["SigTerms"])

  # Return
  return(results)

}
