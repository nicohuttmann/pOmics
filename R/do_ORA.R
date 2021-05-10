#' Performs overexpression enrichment on logical protein vector with specified Ontology
#'
#' @param proteins character vector
#' @param database database to use
#' @param pvalueCutoff p-value/confidence threshold to exclude terms
#' @param pAdjustMethod one of "none", "BH" (Benjamini-Hochberg correction), "hochberg", "bonferroni", "holm", "hommel", "BY", "fdr"
#' @param qvalueCutoff q-value cutoff for annotations
#' @param minGSSize minimum number of proteins for annotation to be used for enrichment
#' @param maxGSSize maximum number of proteins for annotation to be used for enrichment
#' @param algorithm algorithm to use ("classic", "elim", "weight", "weight01")
#' @param dataset dataset
#' @param return.all return enrichResult object; useful for further analysis of enrichment results
#' @param add.info add additional information (takes longer)
#'
#' @return
#' @export
#'
#'
do_ORA <- function(proteins, database, pvalueCutoff = 0.05, pAdjustMethod = "none", qvalueCutoff = 0.2, minGSSize = 10,
                   maxGSSize = 500, algorithm = "classic", dataset, return.all = F, add.info = F) {

  # Gene Ontology
  if (database %in% c("CC", "BP", "MF")) results <- do_ORA_GO(proteins = proteins,
                                                              ontology = database,
                                                              pvalueCutoff = pvalueCutoff,
                                                              pAdjustMethod = pAdjustMethod,
                                                              algorithm = algorithm,
                                                              add.info = add.info)
  # KEGG
  else if (database %in% c("Kegg", "KEGG", "kegg")) results <- do_ORA_KEGG(proteins = proteins,
                                                                           pvalueCutoff = pvalueCutoff,
                                                                           pAdjustMethod = pAdjustMethod,
                                                                           minGSSize = minGSSize,
                                                                           dataset = dataset,
                                                                           return.all = return.all,
                                                                           add.info = add.info)


  # Return results
  return(results)

}
