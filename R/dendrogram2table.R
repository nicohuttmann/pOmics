#' Adds table representation of dendrogram
#'
#' @param data_ data list
#'
#' @return
#' @export
#'
#'
dendrogram2table <- function(data_, dendrogram = "dend_x") {

  #
  dend_table <- tibble::tibble(variables = data_[[dendrogram]]$labels)


  # Add branches to table
  for (i in seq_along(dend_table$variables)) {

    dend_table <- dend_table %>%
      dplyr::mutate(!!as.character(i) := cutree(data_[[dendrogram]], k = i))

  }

  # Add table
  data_[["dend.table"]] <- dend_table

  # Return
  return(invisible(data_))

}

