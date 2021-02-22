#' Builds annotation list from DisGeNet
#'
#' @param save Save?
#'
#' @return
#' @export
#'
#'
new_annotations_data_DisGeNet <- function(nodeSize = 5, save = T) {

  # Collect all variables
  variables <- get_variables_data(var = All, name = "GENES")



  # Retrieve annotations
  annotations <- select_DisGeNet(variables) %>%
    annotation_data2list(sep = "; ") %>%
    topGO::inverseList()

  # Remove diseases with less than nodeSize entries
  annotations <- annotations[unlist(lapply(annotations, length)) >= nodeSize]

  # Save new annotations database
  if (save) add_database(annotations, id = "DisGeNet", type = "Annotations")

  # Return
  invisible(annotations)

}
