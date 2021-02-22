#' Adds table representation of dendrogram
#'
#' @param cor_list cor_list object
#'
#' @return
#' @export
#'
#'
dendrogram2table_ <- function(cor_list) {

  #
  dend_table <- tibble::tibble(variables = attr(cor_list, "variables"))


  # Add branches to table
  for (i in 1:length(attr(cor_list, "variables"))) {

    dend_table <- dend_table %>%
      dplyr::mutate(new = cutree(cor_list[["dendrogram"]], k = i)) %>%
      dplyr::rename(!!as.character(i) := "new")

  }

  # Add table
  cor_list[["dend.table"]] <- dend_table

  invisible(cor_list)

}
