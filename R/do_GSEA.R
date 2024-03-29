#' Performs gene set enrichment on numeric protein vector with specified Ontology
#'
#' @param proteins numeric score vector
#' @param database database to use
#' @param inverse enrich for terms in higher values/scores
#' @param pvalueCutoff p-value cutoff for annotations
#' @param pAdjustMethod one of "none", "BH" (Benjamini-Hochberg correction), "hochberg", "bonferroni", "holm", "hommel", "BY", "fdr"
#' @param qvalueCutoff q-value cutoff for annotations
#' @param minGSSize minimum number of annotated proteins to be included
#' @param maxGSSize maximum number of annotated proteins to be included
#' @param dataset dataset
#' @param return.all Should the enrichrObject be returned
#' @param add.info Add additional information (takes longer)
#'
#' @return
#' @export
#'
#'
do_GSEA <- function(proteins,
                    database,
                    inverse = F,
                    pvalueCutoff = 0.05,
                    pAdjustMethod = "none",
                    qvalueCutoff = 0.2,
                    minGSSize = 10,
                    maxGSSize = 120,

                    dataset,
                    return.all = F,
                    add.info = F) {

  # inverse enrichment
  if(inverse) proteins <- -proteins

  # GO enrichment
  # if (database %in% c("topCC", "topBP", "topMF"))
  #   results <- do_GSEA_topGO(proteins = proteins,
  #                            ontology = database,
  #                            pvalueCutoff = pvalueCutoff,
  #                            pAdjustMethod = pAdjustMethod,
  #                            minGSSize = minGSSize,
  #                            maxGSSize = maxGSSize,
  #                            algorithm = algorithm,
  #                            dataset = dataset,
  #                            add.info = add.info)

  # KEGG
  else if (database %in% c("Kegg", "KEGG", "kegg"))
    results <- do_GSEA_KEGG(proteins = proteins,
                            pvalueCutoff = pvalueCutoff,
                            pAdjustMethod = pAdjustMethod,
                            minGSSize = minGSSize,
                            maxGSSize = maxGSSize,
                            dataset = dataset,
                            view = T,
                            return.all = return.all,
                            add.info = add.info)
  #

  # Return results
  return(results)

}
