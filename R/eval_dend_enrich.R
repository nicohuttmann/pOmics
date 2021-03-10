#' Evalueates dendrogram enrichment
#'
#' @param dend.enrich output from enrich_dend_fisher/_ks
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
#'
eval_dend_enrich <- function(dend.enrich, cut = T) {

  # Check data
  if (!hasArg(dend.enrich) || !"dend.table.cluster" %in% names(dend.enrich) || !"dend.table.pvalue" %in% names(dend.enrich))
    stop("Problematic input.")

  # Smallest p-value
  p.value <- min(dend.enrich[["dend.table.pvalue"]])

  # Find cluster cut
  k <- apply(X = dend.enrich[["dend.table.pvalue"]],
             MARGIN = 2,
             FUN = function(x) p.value %in% x) %>%
    which() %>%
    first_element() %>%
    unname()

  # Collapse table
  if (cut) {
    dend.table.pvalue.cut <- dend.enrich[["dend.table.pvalue"]][!duplicated(dend.enrich[["dend.table.cluster"]][, k]), 1:k]
    dend.table.cluster.cut <- dend.enrich[["dend.table.cluster"]][!duplicated(dend.enrich[["dend.table.cluster"]][, k]), 1:k]
  } else {
    dend.table.pvalue.cut <- dend.enrich[["dend.table.pvalue"]][!duplicated(dend.enrich[["dend.table.cluster"]][, ncol(dend.enrich[["dend.table.cluster"]])]), ]
    dend.table.cluster.cut <- dend.enrich[["dend.table.cluster"]][!duplicated(dend.enrich[["dend.table.cluster"]][, ncol(dend.enrich[["dend.table.cluster"]])]), ]
  }


  rownames(dend.table.pvalue.cut) <- c()
  rownames(dend.table.cluster.cut) <- c()

  # Cluster identifier
  l <- dend.table.cluster.cut[(dend.table.pvalue.cut[, k] == p.value) %>% which() %>% first_element, k] %>% unname()


  dend.enrich[["dend.table.cluster.cut"]] <- dend.table.cluster.cut
  dend.enrich[["dend.table.pvalue.cut"]] <- dend.table.pvalue.cut



  dend.enrich[["p.value"]] <- p.value
  dend.enrich[["k"]] <- k
  dend.enrich[["l"]] <- l

  # Add data
  dend.enrich[["proteins"]] <- which_names(dend.enrich[["dend.table.cluster"]][, k] == l)

  # Return
  invisible(dend.enrich)

}
