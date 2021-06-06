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


  database <- a


  # Names to GO-IDs
  if (hasArg(terms)) {

    for (i in seq_along(terms)) {

      if (!grepl("GO:", terms[i])) {

        terms[i] <- which_names(terms[i] == AnnotationDbi::Term(names(database)))

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
