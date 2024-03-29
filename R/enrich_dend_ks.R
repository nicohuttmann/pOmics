#' Performs KS test on all cluster levels of given dendrogram
#'
#' @param dend.table dendrogram represented as data frame
#' @param protein.scores vector of protein scores
#' @param n maximum number of clusters
#' @param inverse enrichment for higher scores
#'
#' @return
#' @export
#'
#'
enrich_dend_ks <- function(dend.table, protein.scores, n = 10, inverse = F) {

  # Extract data as matrix
  dend.table.pvalue <- as.matrix(dend.table[, c(1:n + 1)])

  # Set rownames
  rownames(dend.table.pvalue) <- dplyr::pull(dend.table, var = 1)

  protein.scores <-
    protein.scores[intersect(names(protein.scores),
                             dplyr::pull(dend.table, var = 1))]

  # Inverse enrichment
  if(inverse) protein.scores <- -protein.scores

  # Save position data and proteins vector for convenience
  dend.table.cluster <- dend.table.pvalue
  proteins <- dplyr::pull(dend.table, var = 1)


  # Fill dend.table.pvalue with enrichment scores
  for (i in 1:n) {

    for (j in 1:i) {

      dend.table.pvalue[dend.table.cluster[, i] == j, i] <-
        ks_test(protein.scores = protein.scores,
                annotated.proteins = proteins[dend.table.cluster[, i] == j])

    }

  }

  # Return
  return(list(dend.table.cluster = dend.table.cluster,
              dend.table.pvalue = dend.table.pvalue))

}
