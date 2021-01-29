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
<<<<<<< HEAD

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

=======
  
  # Get and update GO object
  GOdata <- get_GOdata(proteins = proteins, ontology = ontology)
  
  # 
  ks <- runTest(GOdata, algorithm = "classic", statistic = "ks")
  
  
  results <- GenTable(GOdata,
                      p.value = ks,
                      orderBy = "ks",
                      topNodes = ks@geneData["SigTerms"])
  
  # Return
  return(results)
  
>>>>>>> 8038c7547053747d8c5b58ae7c7fccdb642c2412
}
