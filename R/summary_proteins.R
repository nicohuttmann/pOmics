#' Views a list of proteins
#'
#' @param proteins vector of proteins
#' @param sort.names order table by protein names
#' @param view view protein summary table
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
#'
summary_proteins <- function(proteins, sort.names = F, view = T) {

  # Check input
  # vector
  if (is.vector(proteins)) table <- tibble::tibble(Protein = proteins)

  # Matrix
  else if (is.matrix(proteins)) table <- tibble::tibble(Protein = rownames(proteins),
                                                        tibble::as_tibble(proteins))

  # Tibble
  else if (tibble::is_tibble(proteins)) table <- proteins %>%
      dplyr::rename(Protein = 1)

  # Add gene and protein names
  table <- table %>%
    dplyr::mutate(Gene = get_variables_data("GENES", Protein)) %>%
    dplyr::mutate(Name = get_variables_data("PROTEIN-NAMES", Protein)) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), function(x) {signif(x = x, digits = 3)}))

  # sort
  if (sort.names) {
    table <- dplyr::arrange(table, Name)
  }

  # View table
  if (view) save2cache(data = table, view = TRUE)


  # Return
  invisible(table)

}
