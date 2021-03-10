#' Performs gene set enrichment on numeric protein vector with specified Ontology
#'
#' @param proteins numeric score vector
#' @param inverse enrich for terms in higher values/scores
#' @param ontology GO ontology
#' @param algorithm algorithm to use ("classic", "elim", "weight", "weight01")
#' @param threshold p-value/confidence threshold to exclude terms
#' @param add.info Add additional information (takes longer)
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
#'
enrichment_ks_GO <- function(proteins, inverse = F, ontology = "CC", algorithm = "classic", threshold = 0.05, add.info = F) {

  # Get and update GO object
  if (!inverse) {
    GOdata <- get_GOdata(proteins = proteins, ontology = ontology)
  } else {
    GOdata <- get_GOdata(proteins = -proteins, ontology = ontology)
  }


  #
  ks <- suppressMessages(topGO::runTest(GOdata, algorithm = algorithm, statistic = "ks"))


  results <- topGO::GenTable(GOdata,
                             p.value = ks,
                             orderBy = "ks",
                             topNodes = sum(ks@score < 0.05)) %>%
    tibble::as_tibble() %>%
    dplyr::select(-c("Significant", "Expected"))


  # Add information
  if (add.info) {

    results <- results %>%
      dplyr::mutate(Mean = NA_real_, SD = NA_real_, Genes = NA_character_, Proteins = NA_character_)

    # Add proteins
    for (i in 1:nrow(results)) {

      # Get proteins
      variables <- results[["GO.ID"]][i] %>%
        get_GO_proteins(include.child.terms = T) %>%
        intersect(names(proteins))

      # Proteins
      variables <- proteins[variables]

      # Mean and SD
      results[i, "Mean"] <- round(x = mean(variables), digits = 2)
      results[i, "SD"] <- round(x = sd(variables), digits = 2)


      variables <- variables %>%
        sort() %>%
        names()

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
