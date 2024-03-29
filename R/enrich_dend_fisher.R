#' Performs Fisher's exact test on all cluster levels of given dendrogram
#'
#' @param dend.table dendrogram represented as data frame
#' @param annotated.proteins vector of annotated proteins
#' @param n maximum number of clusters
#'
#' @return
#' @export
#'
#'
enrich_dend_fisher <- function(dend.table,
                               annotated.proteins,
                               n = 10,
                               inverse = F) {

  # Extract data as matrix
  dend.table.pvalue <- as.matrix(dend.table[, c(1:n + 1)])

  # Set rownames
  rownames(dend.table.pvalue) <- dplyr::pull(dend.table, var = 1)

  # Save position data for convenience
  dend.table.cluster <- dend.table.pvalue

  # Regular enrichment
  if (!inverse) {

    # Fill dend.table.pvalue with enrichment scores
    for (i in 1:n) {

      for (j in 1:i) {

        dend.table.pvalue[dend.table.cluster[, i] == j, i] <- fisher_test(proteins = dplyr::pull(dend.table, var = as.character(i)) == j,
                                                                          annotated.proteins = annotated.proteins)

      }

    }


  } else {

    # Inverse enrichment
    # Fill dend.table.pvalue with enrichment scores
    for (i in 1:n) {

      for (j in 1:i) {

        dend.table.pvalue[dend.table.cluster[, i] == j, i] <- fisher_test(proteins = dplyr::pull(dend.table, var = as.character(i)) != j,
                                                                          annotated.proteins = annotated.proteins)

      }

    }

  }


  # Return
  return(list(dend.table.cluster = dend.table.cluster, dend.table.pvalue = dend.table.pvalue))

}
