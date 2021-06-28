#' Title
#'
#' @param data_
#'
#' @return
#' @export
#'
#'
do_hclust_fun_enrich <- function(data_, max.k = 8, simplify = F, database = "BP", input = "dend_y") {

  # Check input
  if (!hasArg(data_)) {

    message("No data given.")

    return(invisible(NULL))

  }

  # Check if list or dataframe given
  list.input <- !is.data.frame(data_) & is.list(data_)

  # Check list input
  if (list.input & !input %in% names(data_)) {

    message("Data could not be found. Please specify correct <input>.")

    return(invisible(data_))

  }

  # Get data
  if (list.input) data <- data_[[input]]

  else data <- data_



  cluster_list <- list()

  for (k in 2:max.k) {

    cluster_list[[as.character(k)]] <- data %>%
      cutree_(k = k) %>%
      fun_enrich(database = database, view = F) %>%
      merge_enrichment_results_recursive()

  }


  cluster_enrichment <- merge_enrichment_results(cluster_list, append.name = F)
  cluster_enrichment <- cluster_enrichment[!duplicated(cluster_enrichment$ID), ]




  cluster_enrichment_Diet <- cluster_enrichment

  cluster_enrichment <- cluster_enrichment_Diet


  if (simplify) {

    cluster_enrichment <- cluster_enrichment %>%
      simplify_enrichment_results(overlap.threshold = 1)

  }





  data_[["cluster_enrichment"]] <- cluster_enrichment


  View(cluster_enrichment)

  View(simplify_enrichment_results(cluster_enrichment))


  dat <- plot_gg_heatmap_terms(data_,
                               TERMS = c("lipid metabolic process",
                                         "plasma membrane",
                                         "ion transport",
                                         "iron ion binding",
                                         "mitochondrion",
                                         "carboxylic acid metabolic process"),
                               TERM2GENE,
                               observations.order = get_observations_data(groups) %>% unique() %>% as.character(),

                               label.proteins = F,
                               label.observations = T,
                               label.terms = T,
                               indicate.clusters = T,
                               k = 8,
                               term.lines.height = 0.5,
                               term.lines.width = 0.9,
                               color.term.lines = "#A18276",
                               color.cluster.annotation = "#463730",

                               ratio.hm = 2,
                               abs.height = 6,
                               rel.width.hm.terms = 1,
                               rel.width.dend.y = 0.2,
                               rel.width.labels.y = 0.3,
                               rel.height.dend.x = 0.05,
                               rel.height.labels.x = 0.5,

                               terms.distance.method = "euclidean",
                               terms.clustering.method = "complete",


                               export = T, file = "heatmap2.pdf")



dat <- plot_gg_heatmap_terms(data_,
                      TERMS = c("mitochondrion",
                                "nucleus",
                                "macromolecule metabolic process",
                                "cellular amino acid metabolic process",
                                "Metabolism of RNA",
                                "fatty acid oxidation"),
                      TERM2GENE,

                      label.proteins = F,
                      label.observations = T,
                      label.terms = T,
                      indicate.clusters = T,
                      k = 8,
                      term.lines.height = 0.5,
                      term.lines.width = 0.9,
                      color.term.lines = "#A18276",
                      color.cluster.annotation = "#463730",

                      ratio.hm = 2,
                      abs.height = 6,
                      rel.width.hm.terms = 1,
                      rel.width.dend.y = 0.2,
                      rel.width.labels.y = 0.3,
                      rel.height.dend.x = 0.05,
                      rel.height.labels.x = 0.5,

                      terms.distance.method = "euclidean",
                      terms.clustering.method = "complete",


                      export = T, file = "heatmap2.pdf")


  # Prepare return
  if (list.input) data_[[paste0("enrichment_", input)]] <- data

  else data_ <- data

  # Return
  return(data_)

}
