#' Performs overexpression enrichment on logical protein vector with specified Ontology
#'
#' @param proteins character vector
#' @param database database to use
#' @param algorithm algorithm to use ("classic", "elim", "weight", "weight01")
#' @param threshold p-value/confidence threshold to exclude terms
#' @param add.info add additional information (takes longer)
#' @param pAdjustMethod one of "none","hochberg" (Benjamini-Hochberg correction), "bonferroni", "holm", "hommel", "BH", "BY", "fdr"
#' @param dataset dataset
#' @param return.all return enrichResult object; useful for further analysis of enrichment results
#'
#' @return
#' @export
#'
#'
do_ORA <- function(proteins, database, algorithm, threshold, pAdjustMethod = "none", dataset, return.all = F, add.info = F) {

  # Gene Ontology
  if (database %in% c("CC", "BP", "MF")) results <- do_ORA_GO(proteins = proteins,
                                                                         ontology = database,
                                                                         algorithm = algorithm,
                                                                         threshold = threshold,
                                                                         add.info = add.info)
  # KEGG
  else if (database %in% c("Kegg", "KEGG", "kegg")) results <- do_ORA_KEGG(proteins = proteins,
                                                                           pvalueCutoff = threshold,
                                                                           pAdjustMethod = pAdjustMethod,
                                                                           dataset = dataset,
                                                                           return.all = return.all,
                                                                           add.info = add.info)


  # Return results
  return(results)

}
