#' Performs overexpression enrichment on protein group vectors
#'
#' @param proteins character vector
#' @param database database to use
#' @param algorithm algorithm to use ("classic", "elim", "weight", "weight01")
#' @param threshold p-value/confidence threshold to exclude terms
#' @param add.info Add additional information (takes longer)
#'
#' @return
#' @export
#'
#'
do_ORA_groups <- function(proteins, database, algorithm, threshold, add.info = F) {

  list.enrichment <- tibble::lst()

  # Check input
  for (group in unique(proteins)) {

    #
    list.enrichment[[as.character(group)]] <- do_ORA(proteins = ifelse(proteins == group, 1, 0),
                                                                   database = database,
                                                                   algorithm = algorithm,
                                                                   threshold = threshold,
                                                                   add.info = add.info)
  }

  # Return
  return(list.enrichment)

}
