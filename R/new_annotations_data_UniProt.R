#' Builds annotation list from UniProt data
#'
#' @param annotation annotation type
#' @param dataset dataset
#' @param save Save?
#' @param return Return?
#'
#' @return
#' @export
#'
#'
new_annotations_data_UniProt <- function(annotation = "GO-ID", dataset, save = T, return = F) {

  # Collect all variables
  variables <- get_all_variables()


  # Check
  if (!check_database(id = get_dataset_attr(which = "taxId", dataset = dataset), type = "UniProt")) stop("UniProt.ws not found")

  # Retrieve annotatoins
  annotations <- .databases[["UniProt"]][[as.character(get_dataset_attr(which = "taxId", dataset = dataset))]] %>%
  select_UniProt(keys = variables,
                 columns = annotation,
                 keytype = "UNIPROTKB") %>%
  annotation_data2list(sep = "; ")

  # Save new annotations database
  if (save) add_database(annotations, id = annotation, type = "Annotations")

  # Return
  if (return) return(annotations)

}
