#' Performs over-representation analysis using Gene Ontology annotations
#'
#' @param proteins numeric/logical vector of proteins indicating group
#' @param pvalueCutoff p-value cutoff for annotations
#' @param pAdjustMethod one of "none", "BH" (Benjamini-Hochberg correction),
#' "hochberg", "bonferroni", "holm", "hommel", "BY", "fdr"
#' @param qvalueCutoff q-value cutoff for annotations
#' @param minGSSize minimum number of annotated proteins to be included
#' @param maxGSSize maximum number of annotated proteins to be included
#' @param database which GO database to use
#' @param dataset dataset
#' @param view view results
#'
#' @return
#' @export
#'
#'
do_ORA_GO <- function(proteins,
                      pvalueCutoff = 0.05,
                      pAdjustMethod = "none",
                      qvalueCutoff = 0.2,
                      minGSSize = 10,
                      maxGSSize = 500,
                      database,
                      dataset,
                      view = F) {

  # Get dataset
  dataset <- get_dataset(dataset)
  if (is_dataset("all")) dataset <- "all"

  # Annotation database for organism
  OrgDb <- get_OrgDb(dataset = dataset)


  # Prepare protein vectors
  sig.proteins <- names(proteins)[proteins == 1]


  background <- names(proteins)



  # Performing enrichment
  go.results <- clusterProfiler::enrichGO(gene = sig.proteins,
                                          universe = background,
                                          OrgDb = OrgDb,
                                          ont = database,
                                          keyType = "UNIPROT",
                                          pvalueCutoff = pvalueCutoff,
                                          pAdjustMethod = pAdjustMethod,
                                          qvalueCutoff = qvalueCutoff,
                                          minGSSize = minGSSize,
                                          maxGSSize = maxGSSize,
                                          readable = T)

  # If enrichment failed
  if (is.null(go.results)) return(NULL)

  # # Re-mapping of entrezids
  # mapping.eg2p <- select_org(keys = sig.proteins.eg,
  #                            columns = "UNIPROT",
  #                            output = "mapping.na",
  #                            keytype = "ENTREZID",
  #                            OrgDb = OrgDb,
  #                            dataset = dataset)
  #
  # mapping.eg2p <- mapping.eg2p[mapping.eg2p[, 2] %in% sig.proteins, ]
  #
  # mapping.eg2p <- data2vector(mapping.eg2p)

  # # Mapping to Symbols
  # mapping.p2g <- select_org(keys = sig.proteins,
  #                            columns = "SYMBOL",
  #                            output = "vector.keep",
  #                            #keytype = "UNIPROT",
  #                            OrgDb = OrgDb,
  #                            dataset = dataset)



  # # Prepare results data
  # results <- tibble::as_tibble(go.results@result) %>%
  #   dplyr::rowwise() %>%
  #   dplyr::mutate(Proteins = paste(mapping.eg2p[strsplit(geneID,
  #   split = "/")[[1]]], collapse = ";"), .before = geneID) %>%
  #   dplyr::select(-c(geneID)) %>%
  #   dplyr::mutate(Genes = paste(mapping.p2g[strsplit(Proteins,
  #   split = ";")[[1]]], collapse = ";"), .before = Proteins) %>%
  #   dplyr::ungroup() %>%
  #   dplyr::filter(p.adjust < pvalueCutoff)

  # View data frame immediately
  if (view) View(go.results@result)

  # Return
  #if (!return.all)
  return(invisible(go.results))

  # else return(list(results = results,
  #                  enrichResult = go.results))

}
