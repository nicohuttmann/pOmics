#' Performs over-representation analysis using Gene Ontology annotations
#'
#' @param proteins numeric/logical vector of proteins indicating group
#' @param pvalueCutoff p-value cutoff for annotations
#' @param pAdjustMethod one of "none", "BH" (Benjamini-Hochberg correction), "hochberg", "bonferroni", "holm", "hommel", "BY", "fdr"
#' @param qvalueCutoff q-value cutoff for annotations
#' @param minGSSize minimum number of proteins for annotation to be used for enrichment
#' @param maxGSSize maximum number of proteins for annotation to be used for enrichment
#' @param database which GO database to use
#' @param dataset dataset
#' @param view view results
#' @param return.all return enrichResult object; useful for further analysis of enrichment results
#' @param add.info add additional information to the results data frame
#'
#' @return
#' @export
#'
#'
do_ORA_GO <- function(proteins, pvalueCutoff = 0.05, pAdjustMethod = "none", qvalueCutoff = 0.2, minGSSize = 10, maxGSSize = 500,
                      database, dataset, view = T, return.all = F, add.info = F) {

  # Get dataset
  dataset <- get_dataset(dataset)

  # Annotation database for organism
  OrgDb <- get_OrgDb(dataset = dataset)

  # Database
  #if (database == "GO") database <- "ALL"

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
                                          maxGSSize = maxGSSize)

  # If enrichment failed
  if (is.null(go.results)) return(NULL)

  # Prepare results data
  results <- tibble::as_tibble(go.results@result) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(Proteins = paste(strsplit(geneID, split = "/")[[1]], collapse = ";"), .before = geneID) %>%
    dplyr::select(-c(geneID)) %>%
    dplyr::mutate(Genes = paste(p2g(strsplit(Proteins, split = ";")[[1]]), collapse = ";"), .before = Proteins) %>%
    dplyr::ungroup() %>%
    dplyr::filter(p.adjust < pvalueCutoff)

  # View data frame immediately
  #if (view) View(results)

  # Return
  if (!return.all) invisible(results)

  else return(list(results = results,
                   enrichResult = go.results))

}
