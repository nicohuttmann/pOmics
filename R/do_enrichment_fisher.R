#' Performs overexpression enrichment on logical protein vector with specified Ontology
#'
#' @param proteins character vector
#' @param database database to use
#' @param algorithm algorithm to use ("classic", "elim", "weight", "weight01")
#' @param threshold p-value/confidence threshold to exclude terms
#' @param add.info Add additional information (takes longer)
#'
#' @return
#' @export
#'
#'
do_enrichment_fisher <- function(proteins, database, algorithm, threshold, add.info = F) {

  # Gene Ontology
  if (database %in% c("CC", "BP", "MF")) results <- enrichment_fisher_GO(proteins = proteins,
                                                                         ontology = database,
                                                                         algorithm = algorithm,
                                                                         threshold = threshold,
                                                                         add.info = add.info)
  # KEGG
  else if (database %in% c("Kegg", "KEGG", "kegg")) results <- enrichment_fisher_kegg(proteins = proteins,
                                                                                      threshold = threshold,
                                                                                      add.info = add.info)


  # Return results
  return(results)

}
