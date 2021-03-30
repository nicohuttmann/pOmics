#' Performs overexpression enrichment on logical protein vector with specified GO ontology
#'
#' @param proteins character vector
#' @param ontology GO ontology
#' @param algorithm algorithm to use ("classic", "elim", "weight", "weight01")
#' @param threshold p-value/confidence threshold to exclude terms
#' @param add.info Add additional information (takes longer)
#'
#' @return
#' @export
#'
#'
do_ORA_GO <- function(proteins, ontology = "CC", algorithm = "classic", threshold = 0.05, add.info = F) {

  # Get and update GO object
  GOdata <- get_GOdata(proteins = proteins, ontology = ontology)

  #
  fisher <- suppressMessages(topGO::runTest(GOdata, algorithm = algorithm, statistic = "fisher"))


  results <- topGO::GenTable(GOdata,
                             p.value = fisher,
                             orderBy = algorithm,
                             topNodes = sum(fisher@score < 0.05)) %>%
    tibble::as_tibble()


  # Add information
  if (add.info) {

    results <- results %>%
      dplyr::mutate(Genes = NA_character_, Proteins = NA_character_)

    # Add proteins
    for (i in 1:nrow(results)) {

      # Get proteins
      variables <- results[["GO.ID"]][i] %>%
        get_GO_proteins(include.child.terms = T) %>%
        intersect(names(proteins[proteins == 1]))


      # Proteins
      results[i, "Proteins"] <- variables %>%
        paste(collapse = ";")

      # Genes
      results[i, "Genes"] <- variables %>%
        p2g %>%
        paste(collapse = ";")


    }

  }


  # Return
  return(results)

}
