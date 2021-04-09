#' Performs over-representation analysis using Reactome annotations
#'
#' @param proteins numeric/logical vector of proteins indicating group
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
do_ORA_Reactome <- function(proteins, pvalueCutoff = 0.05, pAdjustMethod = "none", view = T, return.all = F, add.info = F) {


  stop("FINISH")


  entrez <- clusterProfiler::bitr(proteins, fromType="UNIPROT", toType="ENTREZID", OrgDb="org.Mm.eg.db")[[2]]
  background.entrez <- clusterProfiler::bitr(background, fromType="UNIPROT", toType="ENTREZID", OrgDb="org.Mm.eg.db")[[2]]


  reactome.results <- ReactomePA::enrichPathway(gene = entrez,
                                                universe = background.entrez,
                                                organism = "mouse",
                                                pAdjustMethod = "none",
                                                readable = T)

  View(reactome.results@result)

  sig.proteins <- names(proteins)[proteins == 1]

  background <- names(proteins)



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
