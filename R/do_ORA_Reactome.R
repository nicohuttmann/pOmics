#' Performs over-representation analysis using Reactome annotations
#'
#' @param proteins numeric/logical vector of proteins indicating group
#' @param pvalueCutoff p-value cutoff for annotations
#' @param pAdjustMethod one of "none", "BH" (Benjamini-Hochberg correction), "hochberg", "bonferroni", "holm", "hommel", "BY", "fdr"
#' @param qvalueCutoff q-value cutoff for annotations
#' @param minGSSize minimum number of annotated proteins to be included
#' @param maxGSSize maximum number of annotated proteins to be included
#' @param dataset dataset
#' @param view view results
#' @param return.all return enrichResult object; useful for further analysis of enrichment results
#' @param add.info add additional information to the results data frame
#'
#' @return
#' @export
#'
#'
do_ORA_Reactome <- function(proteins, pvalueCutoff = 0.05, pAdjustMethod = "none", qvalueCutoff = 0.2, minGSSize = 10,
                            maxGSSize = 500, dataset, view = F, return.all = F, add.info = F) {

  # Get dataset
  dataset <- get_dataset(dataset)
  if (is_dataset("all")) dataset <- "all"

  # Prepare protein vectors
  sig.proteins <- names(proteins)[proteins == 1]

  sig.proteins.eg <- translate_Ids(Ids = sig.proteins,
                                   fromType = "UNIPROT",
                                   toType = "ENTREZID",
                                   dataset = dataset,
                                   drop = TRUE,
                                   silent = TRUE)

  background <- names(proteins)

  background.eg <- translate_Ids(Ids = background,
                                 fromType = "UNIPROT",
                                 toType = "ENTREZID",
                                 dataset = dataset,
                                 drop = TRUE,
                                 silent = TRUE)


  # Organism name
  organism <- get_dataset_attr(which = "common_name", dataset = dataset)



  reactome.results <- ReactomePA::enrichPathway(gene = sig.proteins.eg,
                                                universe = background.eg,
                                                organism = organism,
                                                pvalueCutoff = pvalueCutoff,
                                                pAdjustMethod = pAdjustMethod,
                                                qvalueCutoff = qvalueCutoff,
                                                minGSSize = minGSSize,
                                                maxGSSize = maxGSSize,
                                                readable = F)

  # If enrichment failed
  if (is.null(reactome.results)) return(NULL)

  results <- tibble::as_tibble(reactome.results@result) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(Proteins = paste(translate_Ids(strsplit(geneID, split = "/")[[1]],
                                                 fromType = "ENTREZID",
                                                 toType = "UNIPROT",
                                                 dataset = dataset),
                                   collapse = ";"), .before = geneID) %>%
    dplyr::select(-c(geneID)) %>%
    dplyr::mutate(Genes = paste(translate_Ids(strsplit(Proteins, split = ";")[[1]],
                                    fromType = "UNIPROT",
                                    toType = "SYMBOL",
                                    dataset = dataset), collapse = ";"), .before = Proteins) %>%
  dplyr::ungroup() %>%
  dplyr::filter(p.adjust < pvalueCutoff)

  # View data frame immediately
  #if (view) View(results)

  # Return
  if (!return.all) return(results)

  else return(list(results = results,
                   enrichResult = reactome.results))

}
