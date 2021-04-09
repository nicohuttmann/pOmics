#' Performs gene set enrichment analysis using Reactome annotations
#'
#' @param proteins numeric vector of proteins scores
#' @param pvalueCutoff p-value cutoff for annotations
#' @param pAdjustMethod one of "none","hochberg" (Benjamini-Hochberg correction), "bonferroni", "holm", "hommel", "BH", "BY", "fdr"
#' @param view view results
#' @param return.all return enrichResult object; useful for further analysis of enrichment results
#' @param add.info add additional information to the results data frame
#'
#' @return
#' @export
#'
#'
do_GSEA_Reactome <- function(proteins, pvalueCutoff = 0.05, pAdjustMethod = "none", view = T, return.all = F, add.info = F) {

  stop("FINISH")


  entrez <- clusterProfiler::bitr(proteins, fromType="UNIPROT", toType="ENTREZID", OrgDb="org.Mm.eg.db")[[2]]
  background.entrez <- clusterProfiler::bitr(background, fromType="UNIPROT", toType="ENTREZID", OrgDb="org.Mm.eg.db")[[2]]

  proteins.sorted <- sort(rank(proteins), decreasing = T)


  # Perform enrichment
  kegg.results <- clusterProfiler::gseKEGG(geneList      = proteins.sorted,
                                           organism      = organism_code,
                                           minGSSize     = 10,
                                           maxGSSize     = 120,
                                           pvalueCutoff  = pvalueCutoff,
                                           pAdjustMethod = pAdjustMethod,
                                           verbose       = FALSE,
                                           keyType       = "uniprot",
                                           scoreType     = "pos")




  results <- tibble::as_tibble(kegg.results@result) %>%
    dplyr::filter(pvalue < 0.05) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(Proteins = paste(strsplit(core_enrichment, split = "/")[[1]], collapse = ";"), .before = core_enrichment) %>%
    dplyr::select(-c(core_enrichment, leading_edge, rank)) %>%
    dplyr::mutate(Genes = paste(p2g(strsplit(Proteins, split = ";")[[1]]), collapse = ";"), .before = Proteins)

  # View data frame immediately
  if (view) View(results)

  # Return
  if (!return.all) invisible(results)

  else return(list(results = results,
                   enrichResult = kegg.result))




}
