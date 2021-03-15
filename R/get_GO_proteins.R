#' Returns GO terms from given terms
#'
#' @param terms GO ids
#' @param include.child.terms include child terms
#'
#' @return
#' @export
#'
#'
get_GO_proteins <- function(terms, include.child.terms = F) {


  # Check if database exists
  if (check_database(id = "GO-ID", type = "Annotations")) {
    database <- get_database(id = "GO-ID", type = "Annotations")
    # Not set up
  } else {
    database <- new_annotations_data_UniProt(annotation = "GO-ID",
                                             dataset = dataset,
                                             save = T)
  }


  # Inverse database list
  database <- topGO::inverseList(database)


  # Names to GO-IDs
  if (hasarg(terms)) {

    for (i in seq_along(terms)) {

      if (!grepl("GO:", terms[i])) {

        terms[i] <- which_names(terms[i] == Term(names(database)))

      }

    }

  }


  # No argument or terms match
  if (!hasArg(terms) || (all(!terms %in% names(database)) && !include.child.terms)) {

    terms <- choose_GO_terms(return.ID = TRUE)

  }

  # Get child terms
  if (include.child.terms) {
    terms <- get_child_terms(terms = terms)
  }


  if (any(!terms %in% names(database))) {

    if (!include.child.terms) {
      message("Following terms not found: ")
      print(terms[!terms %in% names(database)])
      message("Continue with remaing terms.")
    }

    terms <- terms[terms %in% names(database)]

  }


  # Collect proteins
  proteins <- c()
  for (i in terms) {
    proteins <- unique(c(proteins, database[[i]]))
  }


  # Return
  return(proteins)

}
