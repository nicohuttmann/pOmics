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
#'
#' @return
#' @export
#'
#'
do_ORA <- function(proteins, database, pvalueCutoff = 0.05, pAdjustMethod = "none", qvalueCutoff = 0.2, minGSSize = 10,
                   maxGSSize = 500, algorithm = "classic", dataset) {

  # Check if any significant proteins are present
  if (sum(proteins) == 0) return(NULL)

  # Gene Ontology
  if (database %in% c("CC", "BP", "MF")) results <- do_ORA_GO(proteins = proteins,
                                                              pvalueCutoff = pvalueCutoff,
                                                              pAdjustMethod = pAdjustMethod,
                                                              qvalueCutoff = qvalueCutoff,
                                                              minGSSize = minGSSize,
                                                              maxGSSize = maxGSSize,
                                                              database = database,
                                                              dataset = dataset)
  # KEGG
  else if (database %in% c("Kegg", "KEGG", "kegg")) results <- do_ORA_KEGG(proteins = proteins,
                                                                           pvalueCutoff = pvalueCutoff,
                                                                           pAdjustMethod = pAdjustMethod,
                                                                           qvalueCutoff = qvalueCutoff,
                                                                           minGSSize = minGSSize,
                                                                           maxGSSize = maxGSSize,
                                                                           dataset = dataset)

  # Reactome
  else if (database %in% c("Reactome", "reactome", "REACTOME")) results <- do_ORA_Reactome(proteins = proteins,
                                                                                           pvalueCutoff = pvalueCutoff,
                                                                                           pAdjustMethod = pAdjustMethod,
                                                                                           qvalueCutoff = qvalueCutoff,
                                                                                           minGSSize = minGSSize,
                                                                                           maxGSSize = maxGSSize,
                                                                                           dataset = dataset)

  # CORUM
  else if (database %in% c("CORUM", "Corum", "corum")) results <- do_ORA_CORUM(proteins = proteins,
                                                                               pvalueCutoff = pvalueCutoff,
                                                                               pAdjustMethod = pAdjustMethod,
                                                                               qvalueCutoff = qvalueCutoff,
                                                                               minGSSize = minGSSize,
                                                                               maxGSSize = maxGSSize,
                                                                               dataset = dataset)

  # unknown database
  else return(NULL)

  # Return results
  return(results)

}
