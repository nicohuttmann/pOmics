#' Performs over-representation analysis using MSigDB annotations
#'
#' @param proteins numeric/logical vector of proteins indicating group
#' @param pvalueCutoff p-value cutoff for annotations
#' @param pAdjustMethod one of "none", "BH" (Benjamini-Hochberg correction), "hochberg", "bonferroni", "holm", "hommel", "BY", "fdr"
#' @param qvalueCutoff q-value cutoff for annotations
#' @param minGSSize minimum number of proteins for annotation to be used for enrichment
#' @param maxGSSize maximum number of proteins for annotation to be used for enrichment
#' @param dataset dataset
#' @param view view results
#' @param return.all return enrichResult object; useful for further analysis of enrichment results
#' @param add.info add additional information to the results data frame
#'
#' @return
#' @export
#'
#'
do_ORA_MSigDB <- function(proteins, pvalueCutoff = 0.05, pAdjustMethod = "none", qvalueCutoff = 0.2, minGSSize = 10,
                          maxGSSize = 500, dataset, view = T, return.all = F, add.info = F) {





  library(msigdbr)

  m_df <- msigdbr::msigdbr(species = "Mus musculus", category = "C4")


  m_t2g <- m_df %>%
    dplyr::select(gs_name, entrez_gene)


  em <- clusterProfiler::enricher(gene = entrez,
                                  universe = background.entrez,
                                  pAdjustMethod = "none",
                                  TERM2GENE = m_t2g)
  View(em@result)

  #em2 <- GSEA(geneList, TERM2GENE = m_t2g)
  head(em)






  kegg.results <- clusterProfiler::enrichKEGG(gene = sig.proteins,
                                              universe = background,
                                              organism = organism_code,
                                              keyType = "uniprot",
                                              pAdjustMethod = pAdjustMethod,
                                              minGSSize = 10,
                                              pvalueCutoff = pvalueCutoff)


  results <- tibble::as_tibble(kegg.results@result) %>%
    dplyr::filter(pvalue < 0.05) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(Proteins = paste(strsplit(geneID, split = "/")[[1]], collapse = ";"), .before = geneID) %>%
    dplyr::select(-c(geneID)) %>%
    dplyr::mutate(Genes = paste(p2g(strsplit(Proteins, split = ";")[[1]]), collapse = ";"), .before = Proteins)

  # View data frame immediately
  if (view) View(results)

  # Return
  if (!return.all) invisible(results)

  else return(list(results = results,
                   enrichResult = kegg.result))

}
