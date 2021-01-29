#' Performs overexpression enrichment on logical protein vector with specified GO ontology
#'
#' @param proteins character vector
#' @param ontology GO ontology
#'
#' @return
#' @export
#'
<<<<<<< HEAD
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

=======
#' 
enrichment_fisher_GO <- function(proteins, ontology) {
  
  # Get and update GO object
  GOdata <- get_GOdata(proteins = proteins, ontology = ontology)
  
  # 
  fisher <- runTest(GOdata, algorithm = "classic", statistic = "fisher")
  
  
  results <- GenTable(GOdata,
                      p.value = fisher,
                      orderBy = "classic",
                      topNodes = fisher@geneData["SigTerms"])
  
  # Return
  return(results)
  
>>>>>>> 8038c7547053747d8c5b58ae7c7fccdb642c2412
}
